library(haven)
library(plotly)
library(shiny)
library("DescTools")
library(shinydashboard)
library(dplyr)
library(zoo)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
library(statar)
library("viridis") 
library( rmapshaper)
library(sp)
library(rgeos)
library(readr)
library(raster)
library(ggmap)
library(maptools)
library(rgdal)
library(mapproj)
library(maps)
library(janitor)
library(tidyverse)
library(readxl)
library(wesanderson)
library(MASS)
library(robvis)
library(sf)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(shinycssloaders)
options(scipen=999)


theme_dataton <- function(){ 
  font <- "Georgia"   
  theme_minimal() %+replace%    
    theme(
      axis.ticks = element_blank(),          
      plot.title = element_text(family = font, size = 16,face = 'bold',hjust = 0,vjust = 2),               
      plot.subtitle = element_text(family = font,size = 14),               
      plot.caption = element_text( family = font,size = 9,hjust = 1),               
      axis.title.x = element_blank(),
      axis.title.y = element_text(family = font, size = 12,angle=90,
                                  margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.text = element_text(family = font,size = 9),                
      axis.text.x = element_text(margin=margin(t=2, b = 10), size=11),
      axis.text.y = element_text( size=11)
    )
}

municipios_names<-read_dta("www/poblacion.dta")

choices = data.frame(
  var = unique(municipios_names$nom_ent),
  num =  unique(municipios_names$state_cod))

# List of choices for selectInput
mylist <- as.list(choices$num)
# Name it
names(mylist) <- choices$var


css = HTML("
  .leaflet-top, .leaflet-bottom {
    z-index: unset !important;
  }

  .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
    z-index: 10000000000 !important;
  }
  .shiny-output-error{color: white;}
  
  
         .skin-blue .left-side, .skin-blue .wrapper {
                        background-color: #ecf0f5;
                        }
         
")

total_orig<-read_csv("www/asegurados_total_202008.csv")
sexo<- read_csv("www/asegurados_x_sexo_202008.csv") 
edad<-read_csv("www/asegurados_x_edad_202008.csv")
pymes<-read_csv("www/asegurados_pyme_202008.csv")
salario<-read_csv("www/asegurados_uma50_202008.csv")
semaforo <- read.csv("www/crec_acum_semaforo_202008.csv")

tabla_resumen <- read_excel("www/tabla_resumen.xlsx")
mapa1Mex<- read_dta("www/mapa_mexico_sexo_edad.dta")
mapa2Mex<- read_dta("www/mapa_mexico_sector_tam_emp.dta")
cajeros<- read_csv("www/crecimiento_acumulado_transacciones_cnbv_202008.csv")

library(readxl)

perdida_empleo1 <- read_excel("www/perdida_empleo.xlsx",  sheet = "feb-jul")
perdida_empleo2 <- read_excel("www/perdida_empleo.xlsx",  sheet = "jul-agosto")
# Bases de datos ----
# Procesamiento previo:
map <- sf::st_read("www/01_Datos/mpios.geojson") 


semaforo_mapa <- read_csv("www/01_Datos/semaforo_mapa.csv") %>%
  mutate(CVEGEO = case_when(str_length(cve_mun) == 4 ~ paste0("0",cve_mun), str_length(cve_mun) == 5 ~ paste0(cve_mun)))


semaforo_mapa$valor=factor(semaforo_mapa$valor, labels = c("Menor a -10","Entre -10 y 0", "Mayor a 0") , ordered =is.ordered(semaforo_mapa$valor))

# Generar datos
left_join(map, semaforo_mapa) %>%
  saveRDS("www/01_Datos/datosMapaYun.rds")

estado2 <- readRDS("www/01_Datos/datosMapaYun.rds")


# Funciones propias ----

# Generacion de mapa 
gen_edo <- function(estado){
  
  ## Prueba Debug
  # nombre_entidad <- "Morelos"
  # estado <- estado2 %>% 
  #   filter(NOM_ENT == nombre_entidad)  
  
  # Paleta de colores 
  pal <- colorFactor(palette = c("chartreuse","darkorange", "darkred" ), 
                     rev = TRUE,
                     domain = sort(unique(estado$valor)))
  
  # Generacion de popups
  popup <- paste0("<b style = 'color:green;'>Entidad: </b>", estado$NOM_ENT, "<br>",
                  "<b>Municipio: </b>", estado$NOM_MUN, "<br>",
                  "<b>Semáforo: </b>", estado$valor, "<br>"
  )
  
  # Generacion de etiquetas
  etiquetas <- lapply(paste0("<b style = 'color:green;'>Entidad: </b>",
                             estado$NOM_ENT, "<br>",
                             "<b>Municipio: </b>", estado$NOM_MUN), htmltools::HTML)
  
  # Generacion de mapa interactivo
  leaflet(estado) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(fillColor = pal(estado$valor), 
                fillOpacity = 1, 
                popup = popup, 
                color = "white", 
                opacity = 1,
                weight = 0.9,
                label = etiquetas,
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 4, 
                                                    bringToFront = TRUE)) %>% 
    addLegend(title = "Semáforo", 
              pal = pal, 
              values = estado$valor, 
              position = "bottomleft") %>% 
    addFullscreenControl()
  
  

  
  
}
