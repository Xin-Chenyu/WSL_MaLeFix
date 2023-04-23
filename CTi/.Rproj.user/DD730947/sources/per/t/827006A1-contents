library(reshape2)
library(ggplot2)
library(ggthemes)


## Mammals ##

Tmax_boar = 22
Tmax_fox = 30
Tmax_muskrat =30
Tmax_wolf = 32.5



df_meteo <- read.csv("E:/Internship/Climate variables/df_meteo.csv")
meteo_ZH <- df_meteo[,c(1,4,7,10)]

get_pk <- function(Tmax,local_T){
  ifelse (local_T>Tmax,1,0)
}


get_pk_cattle <- function(THI_mild,THI_moderate,
                          THI_severe,local_T,local_H){
  THI = (1.8*local_T+32)-(0.55-0.0055*local_H)*(1.8*local_T-26)
  if(THI>THI_severe) {return(1)}
  if((THI>THI_moderate)&(THI<=THI_severe)){return(0.5)}
  if((THI>THI_mild)&(THI<=THI_moderate)){return(0.2)}
  if(THI<=THI_mild){return(0)}
}


meteo_ZH$CTi_boar <- NA
meteo_ZH$CTi_fox <- NA
meteo_ZH$CTi_muskrat <- NA
meteo_ZH$CTi_wolf <- NA



for(i in 1:length(meteo_ZH$Date)){
  meteo_ZH$pk_boar[i] = get_pk(Tmax = Tmax_boar, local_T = meteo_ZH$Zurch_txxx[i])
  meteo_ZH$pk_fox[i] = get_pk(Tmax = Tmax_fox, local_T = meteo_ZH$Zurch_txxx[i])
  meteo_ZH$pk_muskrat[i] = get_pk(Tmax = Tmax_muskrat, local_T = meteo_ZH$Zurch_txxx[i])
  meteo_ZH$pk_wolf[i] = get_pk(Tmax = Tmax_wolf, local_T = meteo_ZH$Zurch_txxx[i])
  if(i>14){
    meteo_ZH$CTi_boar[i] = mean(meteo_ZH$pk_boar[i-14:i])
    meteo_ZH$CTi_fox[i] = mean(meteo_ZH$pk_fox[i-14:i])
    meteo_ZH$CTi_muskrat[i] = mean(meteo_ZH$pk_muskrat[i-14:i])
    meteo_ZH$CTi_wolf[i] = mean(meteo_ZH$pk_wolf[i-14:i])
  }
}


meteo_ZH <- meteo_ZH[-(1:14),]
meteo_ZH <- meteo_ZH[,c(1,5:8)]

meteo_ZH_ggp <- melt(meteo_ZH, id.vars="Date")
meteo_ZH_ggp$Date <- as.Date(meteo_ZH_ggp$Date)
meteo_ZH_ggp$species <- c(rep("Wild boar",nrow(meteo_ZH)),
                         rep("Red fox",nrow(meteo_ZH)),
                         rep("Muskrat",nrow(meteo_ZH)),
                         rep("Wolf",nrow(meteo_ZH)))

meteo_ZH_ggp_noboar <- meteo_ZH_ggp[-(1:122),]


zuma <- ggplot(meteo_ZH_ggp_noboar,aes(Date,value, col=species)) +
  geom_point()+
  geom_line(size = 1.5, alpha = 0.5)+
  labs(y = expression(italic("CTi")), x = "Month",
       title = "CTi for mammals in subcatchment of Zurich") +
  theme_hc() +
  theme(legend.position = "right")+
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_x_date(date_labels = "%b")


## Livestock ##


THI_mild_cattle = 72
THI_moderate_cattle = 79
THI_severe_cattle = 89

THI_mild_sow = 72
THI_moderate_sow = 78
THI_severe_sow = 84

THI_mild_horse = 68
THI_moderate_horse = 72
THI_severe_horse = 80


df_meteo <- read.csv("E:/Internship/Climate variables/df_meteo.csv")
meteo_ZH <- df_meteo[,c(1,4,7,10)]


get_pk_cattle <- function(THI_mild,THI_moderate,
                          THI_severe,local_T,local_H){
  THI = (1.8*local_T+32)-(0.55-0.0055*local_H)*(1.8*local_T-26)
  if(THI>THI_severe) {return(1)}
  if((THI>THI_moderate)&(THI<=THI_severe)){return(0.5)}
  if((THI>THI_mild)&(THI<=THI_moderate)){return(0.2)}
  if(THI<=THI_mild){return(0)}
}


meteo_ZH$CTi_cattle <- NA
meteo_ZH$CTi_sow <- NA
meteo_ZH$CTi_horse <- NA

for(i in 1:length(meteo_ZH$Date)){
  meteo_ZH$pk_cattle[i] = get_pk_cattle(THI_mild = THI_mild_cattle,
                                             THI_moderate = THI_moderate_cattle,
                                             THI_severe = THI_severe_cattle,
                                             local_T = meteo_ZH$Zurch_txxx[i],
                                             local_H = meteo_ZH$Zurch_relh[i])
  meteo_ZH$pk_sow[i] = get_pk_cattle(THI_mild = THI_mild_sow,
                                             THI_moderate = THI_moderate_sow,
                                             THI_severe = THI_severe_sow,
                                             local_T = meteo_ZH$Zurch_txxx[i],
                                             local_H = meteo_ZH$Zurch_relh[i])
  meteo_ZH$pk_horse[i] = get_pk_cattle(THI_mild = THI_mild_horse,
                                     THI_moderate = THI_moderate_horse,
                                     THI_severe = THI_severe_horse,
                                     local_T = meteo_ZH$Zurch_txxx[i],
                                     local_H = meteo_ZH$Zurch_relh[i])
  if(i>14){
    meteo_ZH$CTi_cattle[i] = mean(meteo_ZH$pk_cattle[i-14:i])
    meteo_ZH$CTi_sow[i] = mean(meteo_ZH$pk_sow[i-14:i])
    meteo_ZH$CTi_horse[i] = mean(meteo_ZH$pk_horse[i-14:i])
  }
}


meteo_ZH <- meteo_ZH[-(1:14),]
meteo_ZH <- meteo_ZH[,c(1,5:7)]

meteo_ZH_ggp <- melt(meteo_ZH, id.vars="Date")
meteo_ZH_ggp$Date <- as.Date(meteo_ZH_ggp$Date)
meteo_ZH_ggp$species <- c(rep("Cattle",nrow(meteo_ZH)),
                          rep("Sow",nrow(meteo_ZH)),
                          rep("Horse",nrow(meteo_ZH)))
                         

custom.col <- c("#C4961A","#D16103", "#52854C", "#4E84C4", "#293352")
zuli <- ggplot(meteo_ZH_ggp,aes(Date,value, col=species)) +
  geom_point(alpha = 0.5)+
  geom_line(size = 1.5,alpha = 0.5)+
  labs(y = expression(italic("CTi")), x = "Month",
       title = "CTi for livestock in subcatchment of Zurich") +
  theme_hc() +
  theme(legend.position = "right")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_x_date(date_labels = "%b")


ggarrange(zuma,zuli,
          labels = c("A1", "A2"),
          ncol = 1, nrow = 2)


