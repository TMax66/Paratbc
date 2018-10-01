require(RColorBrewer)
require(Hmisc)
require(epitools)
require(survival)
require(spdep)
require(maptools)
require(maps)
require(mapdata)R
require(xtable)
require(ggplot2)
require(rgdal)
require(gpclib)
require(geoR)
require(lattice)
require(fields)
require(googleVis)
#source('limiti.r')
#require(EnQuireR)

library(tidyverse)
library(dplyr)
library(tibble)



rm(list=ls())
df <- read.csv('OLDriskData.csv', sep=';', header=T, dec=',')
df$riskazienda<-casefold(df$riskazienda, upper = TRUE)
righe<-dim(df)[1]
ddf<-df%>%
  gather(key = item, value = risk, -riskazienda, -dtrisk) %>% 
  arrange(riskazienda) 

settore<-rep(c(rep("Sala Parto", 8), rep("Vitelli pre-svezzamento", 6), rep("Vitelle/Manzette svezzate",5), rep("Manze gravide",5),rep("Vacche e tori
",4), rep("Animali acquistati", 4)), righe)

rdata<-cbind(ddf,settore)


rdata %>% as.tibble %>% 
  group_by(riskazienda,settore)%>%
  summarise(punteggio=sum(risk))%>%
  
 


library(pivottabler)
   
   pt <- PivotTable$new()
   pt$addData(rdata)
   pt$addColumnDataGroups("settore")
   pt$addRowDataGroups("riskazienda")
   pt$defineCalculation(calculationName="punteggio", summariseExpression="sum(risk)")
   pt$renderPivot()
   
   
   
   
   
   
   
   
  rdata<-na.omit(rdata)
   
   aggregate(rdata$risk, by=list(rdata$riskazienda,rdata$settore), sum)
   
   
   

ddf$settore<- factor(ddf$settore, labels=c('Sala parto','Pre svezzamento','Svezzamento','Manze gravide','Vacche e Tori','Introduzione', 'Rischio'))



rfiles<-list.files(file.path("riskdata"), full.names=TRUE)
rdata<-lapply(rfiles, read.csv, stringsAsFactors=FALSE)
rdata<-do.call(rbind, rdata)
rdata$dtrisk<-as.Date(rdata$dtrisk, origin=as.Date("1970-01-01"))
rdata$riskazienda<-casefold(rdata$riskazienda, upper=TRUE)
righe<-dim(rdata)[1]

settore<-rep(c(rep("Sala Parto", 8), 
               rep("Vitelli pre-svezzamento", 6), 
               rep("Vitelle/Manzette svezzate",5), 
               rep("Manze gravide",5),rep("Vacche e tori",4), 
               rep("Animali acquistati", 4)), righe)
settore<-factor(settore, levels=c("Sala Parto","Vitelli pre-svezzamento","Vitelle/Manzette svezzate","Manze gravide","Vacche e tori" ,"Animali acquistati"))

x<-rdata %>% as.tibble() %>% 
  gather(key = item, value = risk, -riskazienda, -dtrisk) %>%
  arrange(riskazienda) %>% 
  cbind(settore) %>% 
  group_by(riskazienda,settore)%>%
  summarise(punteggio=sum(risk))%>% 
  filter(riskazienda=="011BG001")




  
  
x %>%data.frame %>% 
summarise(riskazienda = "",
            settore="Totale",
            punteggio = sum(punteggio))%>%
  bind_rows(x, .) %>% 
  do.call(cbind, maxpunti) %>% 
  select(settore,maxpunti,punteggio) %>% 
  mutate("% di settore"=(punteggio/maxpunti)*100) %>% 
  mutate("% sul totale"= (punteggio/276)*100)





maxpunti<-c(80,60,35,25,16,60, 276)

summarise(cvar = "add",
          nvar1 = sum(nvar1),
          nvar2 = sum(nvar2)) %>%
  bind_rows(data, .)


cvar <- c("2015-11-01","2015-11-02","All")
nvar1 <- c(12,10,5)
nvar2 <- c(7,5,6)
data <- cbind.data.frame(cvar,nvar1,nvar2)


data %>% as.tibble() %>% 
  rbind(c("add",sum(nvar1),sum(nvar2)))
  
summarise(cvar = "add",
          nvar1 = sum(nvar1),
          nvar2 = sum(nvar2)) %>%
  bind_rows(data, .)


x<-data.frame(x)

















risk <- subset(ddf, ddf$settore=='Rischio')
risk <- risk[, c(2,4,5,6,7)]

risk$azprev <- factor(risk$az, levels = risk$az[order(risk$prevalenza)])
risk$azliv <- factor(risk$az, levels = risk$az[order(risk$livello)])

p1 <- ggplot(risk, aes(x=azprev, y=prevalenza)) + geom_bar(stat='identity', fill='gray50')+coord_flip()+xlab("Azienda") +ylab("prevalenza(%)")
p2 <- ggplot(risk, aes(x=azliv, y=livello)) + geom_bar(stat='identity', fill='gray50')+coord_flip()+xlab("Azienda") +ylab("Livello di Rischio")



x <- subset(ddf,ddf$settore!='Rischio')
xx <- x[,c(2,6,7)]
#xx$azliv <- factor(xx$az, levels = xx$az[order(xx$livello)])
#ggplot(xx, aes(x = az)) + geom_bar(aes(weight=livello, fill = item), position = 'fill')+ scale_fill_manual(values = rev(brewer.pal(6, "Blues"))) + coord_flip()



p <- ggplot(xx)
pp <- p + geom_bar(aes(az, livello), stat = "identity",fill = "gray")
p3 <- pp + geom_bar(aes(az, livello, fill = settore),stat = "identity", position = "dodge")+xlab("Azienda") +ylab("Livello di Rischio")+ scale_fill_brewer(palette = "Blues")







x<-read.csv("OLDriskData.csv", sep=";")
write.csv(x, file="oldData.csv", row.names = FALSE)









