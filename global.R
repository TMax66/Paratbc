library(tidyverse)
library(googlesheets)
library(lubridate)
library(shinyjs)

rm(list=ls())
sheet <- gs_title("datasetparatbc")
dati<-gs_read(sheet)
dati$datareg<-mdy(dati$dtreg)
#dati<-mutate(dati,anno=year(datareg))
#dati$anno<-as.Date((paste(dati$anno,"-01","-01",sep="")))

#data$dtreg<-as.Date(data$dtreg, origin=as.Date("1970-01-01"))
dati$codaz<-casefold(dati$codaz, upper=TRUE)
dati$veterinario<-casefold(dati$veterinario, upper=TRUE)
dati$proprietario<-casefold(dati$proprietario, upper=TRUE)

dati<-dati %>% 
  mutate(prevalenza= round((P/(P+D+N))*100)) %>% 
  select(-dtreg,-prova,-matrice,-codaz2,-finalità)






