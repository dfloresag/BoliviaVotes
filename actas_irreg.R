# 1. votos validos vs. votos partidos.  1500 suma nada que ver 

rm(list=ls())

setwd("~/Dropbox/Downloads/")

library(ggplot2)
library(reshape)
library(dplyr)
actas      <- read.csv("acta.2019.10.24.01.40.43.csv")

actas      <- read.csv("acta.2019.10.24.01.40.43.csv")

actas_pres <- actas %>%  
  filter(Elección  == "Presidente y Vicepresidente") 

partidos <- c(
  "CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL"
)

emitidos <-c(partidos, "Nulos", "Blancos") 

actas_pres$Votos.Partidos <- 
  actas_pres[,partidos] %>% 
  rowSums()

actas_pres$Votos.Emitidos <- 
  actas_pres[,emitidos] %>% 
  rowSums()

actas_pres$Diferencia.Partidos <- 
  actas_pres$Votos.Partidos - actas_pres$Votos.Válidos

actas_pres %>% 
  mutate(
    Irreg = ifelse(
      Diferencia.Partidos!=0,"Irregular","Regular") 
    ) -> actas_pres

## 

actas_pres %>% 
  filter(Irreg=="Irregular")  %>%
  select(Diferencia.Partidos) %>%
  summary()

actas_pres %>% 
  filter(Irreg=="Irregular")  %>%
  select(Diferencia.Partidos) %>%
  abs() %>%
  sum() -> 

actas_pres %>% 
  select(Votos.Emitidos) %>% 
  sum() 


actas_pres_tidy %>% melt(
  measure.vars  = 
    c("CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL"),
  variable_name= "Partido")


# 2. promedio por recinto.  Mesas donde se invierte completamente 

ComoVotaMiRecinto <- function(recinto, porcentajes =FALSE){
   if(!(recinto %in% levels(actas$Recinto))) {
     stop(message("Nombre de recinto no valido."))
   } else {
     data <- actas %>% melt(
       measure.vars  = 
         c("CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL", "Nulos", "Blancos"),
       variable_name= "Partido") %>%
       mutate(Porcentaje.por.Partido =  value/Votos.Totales*100) %>%
       filter(
         Elección == "Presidente y Vicepresidente",
         Recinto  == recinto) %>%
       group_by(Número.Mesa, Partido)
       
       if(!porcentajes){
        plots <- data %>% 
          ggplot(mapping = aes(x = Partido, 
                               y=value, 
                               fill = factor(Número.Mesa))) +
          geom_col(position ="dodge")+
          facet_grid(.~Departamento+Provincia+Localidad+Irreg, scales = "free_y") + 
          ggtitle(paste("Localidad :", recinto, "- Total")) + 
          labs(x = "Partido", y = "Total de Votos")
       } else if (porcentajes){
         plots <- data %>% 
           ggplot(mapping = aes(x = Partido, 
                                y = Porcentaje.por.Partido, 
                                fill = factor(Número.Mesa))) +
           geom_col(position ="dodge")+
           facet_grid(.~Departamento+Provincia+Localidad+Irreg, scales = "free_y") + 
           ggtitle(paste("Localidad :", recinto, "- Porcentaje"))+ 
           labs(x = "Partido", y = "Porcentaje de Votos")
       }
     plots
   }
}


ComoVotaMiRecinto("Colegio German Busch", porcentajes = FALSE)
ComoVotaMiRecinto("Colegio German Busch", porcentajes = TRUE)

ComoVotaMiRecinto("Escuela Carlos Medinaceli", porcentajes = FALSE)
ComoVotaMiRecinto("Escuela Carlos Medinaceli", porcentajes = TRUE)

ComoVotaMiRecinto("Colegio Bolivia", porcentajes = FALSE)
ComoVotaMiRecinto("Colegio Bolivia", porcentajes = TRUE)

ComoVotaMiRecinto("Escuela Simón Bolivar", porcentajes = FALSE)
ComoVotaMiRecinto("Escuela Simón Bolivar", porcentajes = TRUE)

ComoVotaMiRecinto("Escuela Alto Achumani", porcentajes = FALSE)
ComoVotaMiRecinto("Escuela Alto Achumani", porcentajes = TRUE)

ComoVotaMiRecinto("U. E. Ff. Aa. De La Nación", porcentajes = FALSE)
ComoVotaMiRecinto("U. E. Ff. Aa. De La Nación", porcentajes = TRUE)

ComoVotaMiRecinto("Esc. San Jose", porcentajes = FALSE)
ComoVotaMiRecinto("Esc. San Jose", porcentajes = TRUE)

ComoVotaMiRecinto("U.E. Ignacio Warnes", porcentajes = FALSE)
ComoVotaMiRecinto("U.E. Ignacio Warnes", porcentajes = TRUE)



p2 <- actas %>% melt(
  measure.vars  = 
    c("CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL", "Nulos", "Blancos"),
  variable_name= "Partido") %>%
  mutate(Porcentaje.por.Partido =  value/Votos.Totales*100) %>%
  filter(
    Elección == "Presidente y Vicepresidente",
    # País == "Bolivia", 
    # Departamento == "Cochabamba", 
    # Provincia == "Cercado", 
    # Municipio
    # Localidad=="Chipaya",
    Recinto == "Colegio German Busch") %>%
  group_by(Número.Mesa, Partido) %>% 
  # summarise(Votos= sum(value))%>%
  ggplot(mapping = aes(x = Partido, y=Porcentaje.por.Partido, fill = factor(Número.Mesa))) + 
  geom_col(position ="dodge")+facet_grid(.~Departamento+Provincia+Localidad+Irreg, scales = "free_y") + 
  ggtitle("Localidad : Colegio German Busch - Porcentual") + 
  labs(x = "Partido", y = "Porcentaje de Votos")

p1
p2


actas %>% melt(
  measure.vars  = 
    c("CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL", "Nulos", "Blancos"),
  variable_name= "Partido") %>%
  mutate(Porcentaje.por.Partido =  value/Votos.Totales*100) %>%
  filter(
    Elección == "Presidente y Vicepresidente",
    # País == "Bolivia", 
    # Departamento == "Cochabamba", 
    Provincia == "Murillo",
    Recinto == "Colegio German Busch") %>%
  group_by(Número.Mesa, Partido) %>% 
  # summarise(Votos= sum(value))%>%
  ggplot(mapping = aes(x = Partido, y=Porcentaje.por.Partido, fill = factor(Número.Mesa))) + 
  geom_col(position ="dodge")+facet_grid(.~Departamento+Provincia+Localidad+Irreg, scales = "free_y") + 
  ggtitle("Localidad : Colegio German Busch") + 
  labs(x = "Partido", y = "Votos")

##### 

names(actas)


actas_tidy <- actas %>% 
  melt(
    measure.vars  = partidos,
    variable_name= "Partido")%>% 
  filter(Elección == "Presidente y Vicepresidente")

actas_rcnt<-actas_tidy %>% 
  group_by(País, Departamento,Provincia, Municipio, Localidad, Recinto, Partido) %>% 
  summarise(Votos.Por.Recinto = sum(value),
            Porcentaje.Por.Recinto = sum(value)/mean(Votos.Totales))

dim(actas_rcnt)

actas_mesa<-actas_tidy %>% 
  group_by(País,Departamento,Provincia,Municipio, Localidad, Recinto, Número.Mesa, Partido) %>% 
  summarise(Votos.Por.Mesa = sum(value), 
            Porcentaje.por.Mesa = sum(value)/mean(Votos.Totales))



actas_mesa %>%
  filter(
    País == "Bolivia",
    Departamento == "Cochabamba",
    Provincia == "Campero",
    Localidad == "Pasorapa") %>%
  ggplot(mapping = aes(x = Partido, y = Votos.Por.Mesa , fill = factor(Número.Mesa)))+ 
  geom_col(position = "dodge") 
           

actas_mesa %>%
  filter(
    País == "Bolivia",
    Departamento == "Cochabamba",
    Provincia == "Cercado") %>%
  ggplot(mapping = aes(x = Partido, y = Votos.Por.Mesa , fill = factor(Número.Mesa)))+ 
  geom_col(position = "dodge") 

actas_mesa %>%
  filter(
    País == "Bolivia",
    Departamento == "Cochabamba",
    Provincia == "Cercado") %>%
  ggplot(mapping = aes(x = Partido, y = Porcentaje.Por.Mesa , fill = factor(Número.Mesa)))+ 
  geom_col(position = "dodge") 




length(levels(actas$Localidad))




head(actas_rcnt)




ggplot(data = actas_rcnt) +
  geom_mosaic(actas_rcnt aes(x = Partido, fill=value), na.rm=TRUE) +
  labs(x="Is it rude recline? ", title='f(RudeToRecline)') 


for(localidad in levels(actas$Localidad)) {
  actas_loc <- actas %>% 
    filter(
      Elección == "Presidente y Vicepresidente", 
      Localidad == localidad
      )
  for(recinto in levels(actas_loc$Recinto)){
    actas_loc_rec <- actas_loc %>% filter(Recinto==recinto)
    aggregate(
      actas[, partidos], FUN = sum, by = list(actas_loc_rec$Recinto)
    )
  }
}



actas_pres <- subset(actas, subset = )

for(localidad in levels(actas$Localidad)) {
  actas_loc <- actas %>% 
    filter(
      Elección == "Presidente y Vicepresidente", 
      Localidad == localidad
    )
  for(recinto in levels(actas_loc$Recinto)){
    actas_loc_rec <- actas_loc %>% filter(Recinto==recinto)
    aggregate(
      actas[, partidos], FUN = sum, by = list(actas_loc_rec$Recinto)
    )
  }
}

#######

rm(list=ls())

setwd("~/Dropbox/Downloads/")

library(ggplot2)
library(reshape)
library(dplyr)

actas <- read.csv("acta.2019.10.23.22.46.43.csv")

actas_pres <- subset(actas, subset = (Elección == "Presidente y Vicepresidente"))
partidos <- c("CC","FPV","MTS","UCS","MAS...IPSP","X21F","PDC","MNR","PAN.BOL", "Nulos", "Blancos")

ids.mesas<-vector()
pvs.mesas<-vector()

for(localidad in levels(actas_pres$Localidad)) {
  actas_locl <- subset(actas_pres, subset = (Localidad == localidad))
  actas_locl$Recinto <- droplevels(actas_locl$Recinto)
  actas_locl$Recinto <- droplevels(actas_locl$Recinto)
  for(recinto in levels(actas_locl$Recinto)){
    actas_rcnt <- subset(actas_locl, subset = (Recinto == recinto))
    id   <- actas_locl$Número.Mesa
    if(nrow(actas_rcnt)>1){
      Votos.por.MesaVot  <- actas_rcnt[, partidos]
      Votos.por.Recinto  <- colSums(Votos.por.MesaVot)
      if(sum(Votos.por.Recinto)>0){
        p <-  Votos.por.Recinto/sum(Votos.por.Recinto) 
        for(mesa in 1:nrow(actas_rcnt)){
          tests <-try(chisq.test(x=as.vector(Votos.por.MesaVot[mesa,]) , p), silent=TRUE)
          if(class(tests) != "try-error"){
            pvs.mesas <-c(pvs.mesas, tests$p.value)
            ids.mesas <-c(ids.mesas, id[mesa])
          } else {
            print("ALERTA! ALERTA!")
          }
        } 
      }
    }
  }
}
sum(pvs.mesas<=0.01)

idsmesas <-ids.mesas[which(pvs.mesas<=0.01)]

actas_pres_dvnt <- filter()
head(actas_pres_dvnt)        
        

select
Votos.por.MesaVot[as.character(mesa),]

localidad

length(as.vector(Votos.por.MesaVot[mesa,]))
length(p)



actas_mesa %>%
  filter(
    N) %>%
  ggplot(mapping = aes(x = Partido, y = Porcentaje.Por.Mesa , fill = factor(Número.Mesa)))+ 
  geom_col(position = "dodge") 

