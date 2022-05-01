
###### MAPAS DE COVID 19 - PARA #######################################################
# Será realizado uma Espacialização dos microdados sobre COVID19 para o Estado do Pará
# Utilizando a Base de Dados Cartográfica do IBGE e Brasil.io

#######################################################################################

############### Limpar a Memória ######################################################
rm(list = ls())
#######################################################################################


###### INSTALACAO DOS PACOTES #########################################################
######
# Ser?o Instalados os Diversos Pacotes

install.packages(c("sf","geobr","magrittr","dplyr","colorspace",
                   "ggplot2","gifski","gganimate","leaflet","maps", "sp"))
#######################################################################################


###### ATIVACAO DOS PACOTES ###########################################################
######
library(sf)         # Ler aquivos tipo .shap 
library(geobr)      # Base Cartográfica/IBGE
library(magrittr)   # utilizar operador pipe
library(dplyr)      # fazer manipula??o no banco
library(readr)
library(colorspace) # usar paleta de cores
library(ggplot2)    # gerar mapa por camadas
library(gifski)     # Highest Quality GIF Encoder 
library(gganimate)  # gerar animação no gráfico
library(leaflet)    # gerar mapas interativos
library(maps)       # Draw Geographical Maps
library(sp)         # Classes and Methods for Spatial Data 
library(crul)
library(htmltools)
library(covr)
library(rgdal)
library(knitr)
library(rmarkdown)
#######################################################################################


###### Download da Base de COVID19 ############################################################################
######
# Base de dados Sobre O COVID-19 p/ UF foi utilizado 
# do site https://brasil.io/home/
# DATA : 27/03/2021
#######################################################################################

summary(casosPA)


###### Manipulação de Leitura da Base Cartográica ###############################################
# Ler base cartográfica do IBGE via Pacote geobr
BASE_PA <- read_municipality(code_muni = "PA", year = 2020)
BASE_SP <- read_municipality(code_muni = "SP", year = 2020)
BASE_RJ <- read_municipality(code_muni = "RJ", year = 2020)

# Mapa simples via pacote base
plot(BASE_PA)
plot(BASE_SP)
plot(BASE_RJ)

# Mapa simples via pacote ggplo2
# Mapa do Pará
ggplot(BASE_PA)+
  geom_sf(fill= "#2D3E50", color= "#FEBF57", 
          size=0.15, show.legend = FALSE)


# Mapa de São Paulo
ggplot(BASE_SP)+
  geom_sf(fill= "#2D3E50", color= "#FEBF57", 
          size=0.15, show.legend = FALSE)

# Mapa dO Rio de Janeiro
ggplot(BASE_RJ)+
  geom_sf(fill= "#2D3E50", color= "#FEBF57", 
          size=0.15, show.legend = FALSE)

########################################################################################


###### Manipulação da Base de COVID19 ##############################################


COVID19_MAR_PA <- read_csv("COVID19_MAR_PA.csv")



######
# Limpar Casos
casosPA <- COVID19_MAR_PA
linhas <- c(55,146)
casosPA <- casosPA[-linhas,]

# Remover Colunas/(variáveis)
colunas <- c(3,4) 
BASE_PA <- BASE_PA[,-colunas]

colunas <- c(1,2,4,7)
casosPA <- casosPA[,-colunas] 
######################################################################################


####### Juntar as Bases (geobr+ COVID19_PA) ##########################################
#######
PA_Casos_Covid <- merge(BASE_PA, casosPA, 
                        by.x= "code_muni", 
                        by.y="city_ibge_code")

# Gerar o Mapa (gerobr + COVID19_PA)
Mapa_PA <- leaflet(PA_Casos_Covid) %>% addTiles()
Mapa_PA %>% addPolygons()

Mapa_PA %>% addPolygons(
  weight = 1.5,
  opacity = 0.5,
  color = "blue",
  dashArray = "1",
  smoothFactor = 1.5,
  fillOpacity = 0,)
######################################################################################


###### Definir as Categorias da Legenda e Paletas de cores ###########################
######
# Bins e Cores/ pacote (colorspace)
Categoria_Legenda <- c(0,10,20, 50, 100,1000, Inf)
pal <- colorBin("YlOrRd", domain = PA_Casos_Covid$deaths, bins = Categoria_Legenda)

Mapa_PA %>% addPolygons(
  fillColor = ~pal(deaths),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = 1,
  smoothFactor = 1.5,
  fillOpacity = 0.7,)
#######################################################################################


###### Adicionar Interatividade no Mapa ###############################################
######
Mapa_PA %>% addPolygons(
  fillColor = ~pal(deaths),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "1",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    smoothFactor = 2.5,
    bringToFront = TRUE))
########################################################################################


###### Customizando Informa??es/gerar html #############################################
######
label1 <- sprintf(
  "<strong>%s</strong></br>%g Acidentes</br>%g Vitima Fatal",
  PA_Casos_Covid$name_muni, PA_Casos_Covid$confirmed, PA_Casos_Covid$fatal) %>% lapply(htmltools::HTML)

# Mapa Customizado

Mapa_PA %>% addPolygons(
  fillColor = ~pal(deaths),
  weight =2,
  opacity =1,
  color ="white",
  dashArray ="1",
  smoothFactor = 1.5,
  fillOpacity =0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray ="",
    fillOpacity =0.7,
    bringToFront =TRUE),
  label = label1,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction= "auto"
  )) %>% addLegend(pal= pal,
                   values = ~deaths,
                   opacity = 0.5,
                   title = "Vitma Fatal no Transito",
                   position= "bottomright")

#################################################################################################

####### Salvar o Mapa ###########################################################################
ggsave(plot = meu.plot,filename="C:/Users/mario Dhiego/Documents/Mapas_Covid19_PARA/Mapas.COVID19.PARA/mapa1_vitima.jpeg",
       width= 5,height=5)
#################################################################################################








