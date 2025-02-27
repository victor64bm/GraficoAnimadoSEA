# Creaci�n de grafico de barras animado

# Cargamos librerias
library(readxl) # Leer excel
library(dplyr) # Manipulacion de datos
library(ggplot2) # Crear graficos
library(tidyr) # Ordenar datos
library(lubridate) # Trabajo con fechas
library(ggstance) # Generar grafico de barras horizontal
library(gganimate) # Dar animaci�n a graficos
library(stringr) # Trabajo con expresiones regulares
library(ggthemes) # Trabajar con temas de ggplot

# Cargamos datos
proyectos <- readxl::read_excel("Proyectos.xlsx",sheet = 1)

# Vemos la informaci�n a nivel de tipologias. Considerar:
# - Desde la p�gina del SEA se puede extraer directamente la subtipolg�a, sin embargo para obtener la topologia 
#     solo es necesario extraer la letra que se encuentra en la subtipologia. 
# - Vamos a utilizar trimestres para agrupar la informaci�n.
# - Se utilizan nombres cortos en las tipologias, para que dichos nombres puedan mostrarse en el gr�fico.
# - Obtenemos la suma de proyectos por A�o_Trimestre y por Tipologia.
tipologias_base <- proyectos %>% select(Tipologia = typology,Fecha_ingreso = entry_date) %>% 
  mutate(Tipologia = stringr::str_extract(Tipologia,"[:alpha:]")) %>%
  mutate(A�o = year(as.Date(Fecha_ingreso)),
         Quartil = lubridate::quarter(as.Date(Fecha_ingreso)),
         A�o_Quartil = paste0(A�o," - Trimestre ",Quartil),
         Tipologia = case_when(
           Tipologia == "a" ~ "Acueductos, embalses o tranques y sifones",
           Tipologia == "b" ~ "L�neas de tranmisi�n el�ctrica",
           Tipologia == "c" ~ "Centrales el�ctricas",
           Tipologia == "d" ~ "Reactores y establecimientos nucleares",
           Tipologia == "e" ~ "Aeropuertos, terminales, vias ferreas y autopistas",
           Tipologia == "f" ~ "Puertos y terminales mar�timos",
           Tipologia == "g" ~ "Proyectos de desarrollo urbano o tur�stico",
           Tipologia == "h" ~ "Proyectos industriales o inmobiliarios",
           Tipologia == "i" ~ "Proyectos mineros",
           Tipologia == "j" ~ "Oleoductos, ductos mineros u otros similares",
           Tipologia == "k" ~ "Instalaciones fabriles",
           Tipologia == "l" ~ "Agroindustrias",
           Tipologia == "m" ~ "Proyectos forestales",
           Tipologia == "n" ~ "Explotaci�n de recursos hidrobiol�gicos",
           Tipologia == "�" ~ "Producci�n y disposici�n de sustancias peligrosas",
           Tipologia %in% c("o","p","q","r","s","t","u") ~ "Saneamiento ambiental (alcantarillados, rellenos sanitarios, disp. de residuos)")) %>% 
  group_by(Tipologia,A�o_Quartil) %>% 
  summarise(Proyectos = n()) %>% ungroup() 

# - Como no hay observaciones para todos los meses, vamos a generar un nuevo dataframe que tenga todas las
#     combinaciones posibles de A�o_Quartil y Tipologia. 
# - Luego, con un left_join vamos a unir a nuestro nuevo dataframe, los registros de nuestro dataframe base. 
# - Luego, en donde el n�mero de proyectos sea NA, esto indicar� que en dichos trimestres no hay proyectos. En este
#     caso, vamos a completar con valores 0, indicando que para tal periodo no se presentaron nuevos proyectos.
# - Finalmente, obtenemos la suma acumulada de proyectos.
tipologias_q <- tipologias_base %>%  distinct(Tipologia,A�o_Quartil) %>% expand(Tipologia,A�o_Quartil) %>% 
  left_join(tipologias_base,by = c("Tipologia" = "Tipologia", "A�o_Quartil" = "A�o_Quartil")) %>% 
  mutate(Proyectos = ifelse(is.na(Proyectos),0,Proyectos)) %>% group_by(Tipologia) %>% 
  mutate(Proyectos_Acumulados = cumsum(Proyectos)) %>% ungroup()

# Extraemos el top 10 de tipologias con mas proyectos por A�oTrimestre.
top_tipologias_q <- tipologias_q %>% group_by(A�o_Quartil) %>%  mutate(Top10 = row_number(-Proyectos_Acumulados)) %>% 
  filter(Top10 <= 10) %>% ungroup() %>% filter(Proyectos_Acumulados > 0)

# Generamos el grafico base
grafico_q <- top_tipologias_q  %>%  ggplot(aes(x = Proyectos_Acumulados,y = desc(Top10))) + 
  geom_barh(aes(fill = Tipologia),stat="identity",alpha = 0.3,show.legend = FALSE) + 
  geom_text(aes(x=Proyectos_Acumulados/2,label = paste0(Tipologia,": ",round(Proyectos_Acumulados,1)), 
                hjust=0), color = "gray30")  +
  theme_fivethirtyeight() +
  transition_states(A�o_Quartil,wrap = FALSE) + 
  labs(title = 'Top 10 de Tipolog�as con m�s proyectos ingresados al {closest_state}') +
  theme(panel.grid = element_blank(), 
        legend.position = "none",
        axis.title = element_text(size = 12),
        axis.title.x = element_text(vjust = -1.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "italic")) + 
  labs(y="", x="Proyectos presentados al SEA") + 
  labs(caption = "Fuente: Web scraping realizado a la Web del SEA en Febrero 2021") + 
  view_follow(fixed_x = c(0,NA), fixed_y = TRUE ) + enter_grow() #view_follow permite fijar ejes

# - Renderizamos el video configurando el ancho, alto, resoluci�n y frames por segundo.
# - Adem�s agregamos frames en el inicio y al final del video para pausarlo.
# - El video se genera en mp4 en el directorio que estemos trabajando.
df <- animate(grafico_q, renderer = av_renderer('AnimacionSEATipologias.mp4'), 
              width = 1280, height = 720, res = 104, fps = 50,duration = 42, end_pause = 200,start_pause = 20)
utils::browseURL('AnimacionSEATipologias.mp4')
  