rm(list=ls())

setwd("~/Downloads/")
library(ggplot2)
library(dplyr)
library(reshape)

actas_2038 <-read.csv("acta.2019.10.22.20.38.53.csv")
actas_2223 <-read.csv("acta.2019.10.22.23.49.51.csv")

summary(actas_2223) 
electores <- 7315364



actas_rshp %>% 
  group_by(Partido) %>%
  summarise(TotalVotos = sum(value),
            PropVotos  = sum(value)/electores)%>%
  ggplot(mapping = aes(x = Partido, y=PropVotos)) + 
  geom_col()

actas_rshp %>% 
  group_by(Partido) %>%
  summarise(TotalVotos = sum(value),
            PropVotos  = sum(value)/electores)%>%
  ggplot(mapping = aes(x = Partido, y=PropVotos)) + 
  geom_col()

actas_rshp %>% 
  mutate(Exterior = ifelse(País=="Bolivia", "Nacional", "Extranjero")) %>%
  filter(Elección == "Presidente y Vicepresidente") %>%
  group_by(Partido, Exterior) %>%
  summarise(TotalVotos = sum(value),
            PropVotos =  sum(value)/sum(Votos.Válidos))%>%
  ggplot(mapping = aes(x = Partido, y=TotalVotos, fill = Exterior)) + 
  geom_col()

# Selecting mesas #####

actas_rshp %>%
  filter(Elección == "Presidente y Vicepresidente", 
         Número.Mesa == 40259)

mesas <- sample(x = actas_rshp$Número.Mesa,size = 5)
plotme <- list()

for(i in 1: length(mesas)){
  plotme [[i]] <- actas_rshp %>%
    filter(Elección == "Presidente y Vicepresidente", 
           País=="Bolivia", 
           Número.Mesa == mesas[i]) %>% 
    group_by(Partido) %>%
    summarise(TotalVotos = sum(value))%>%
    ggplot(mapping = aes(x = Partido, y=TotalVotos)) + 
    geom_col() + ggtitle(
      paste("Resultado Mesa", as.character(mesas[i]))
    )
}

plotme[[1]]
plotme[[2]]
plotme[[3]]
plotme[[4]]
plotme[[5]]

levels(actas_rshp$Recinto)

actas_rshp %>%
  filter(Elección == "Presidente y Vicepresidente", 
         País=="Bolivia"
  ) %>% 
  group_by(Partido, Departamento, Recinto) %>%
  summarise(TotalVotos = sum(value))%>%
  ggplot(mapping = aes(x = Partido, y=TotalVotos)) + 
  geom_col() + ggtitle(
    paste("Resultado Mesa", as.character(mesas[i]))
  )

# 1. votos validos vs. votos partidos.  1500 suma nada que ver 

rm(list=ls())

setwd("~/Dropbox/Downloads/")

library(ggplot2)
library(reshape)
library(dplyr)

actas <- read.csv("acta.2019.10.23.06.43.43.csv")

actas$Votos.Totales <- 
  actas[,c("CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL")] %>%
  rowSums()

actas$Diferencia.Votos <- actas$Votos.Totales - actas$Votos.Válidos

actas$Irreg<- ifelse(
  actas$Diferencia.Votos!=0, "Irregular","Regular")

actas_pres_irr <- actas %>% 
  filter(
    Elección  == "Presidente y Vicepresidente", 
    Irreg ==  "Irregular")

head(actas_pres_irr)

actas_pres <- actas %>% melt(
  measure.vars = c("CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL"),
  variable_name= "Partido") %>% 
  filter(Elección == "Presidente y Vicepresidente")

actas_pres %>% group_by(Número.Mesa) %>%
  summarise(votosdif = sum(value)- mean(Votos.Válidos)) %>%
  ggplot(mapping = aes(x = Número.Mesa, y=votosdif)) + 
  geom_col()

mesas_id <- actas_pres %>% group_by(Número.Mesa) %>%
  summarise(votosdif = sum(value)- mean(Votos.Válidos)) %>% 
  filter(votosdif!=0)

summary(mesas_id$votosdif)

zerodiff <- actas_rshp %>%
  filter(Elección == "Presidente y Vicepresidente") %>% 
  group_by(Número.Mesa, Departamento) %>%
  summarise(votosdif = sum(value)- mean(Votos.Válidos))    





sum(zerodiff$votosdif!=0)
sum(abs(zerodiff$votosdif))

sum(abs(zerodiff$votosdif))/7315364*100






# 2. promedio por recinto.  Mesas donde se invierte completamente 
