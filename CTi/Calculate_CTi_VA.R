library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggpubr)


## Mammals ##

Tmax_boar = 22
Tmax_fox = 30
Tmax_muskrat =30
Tmax_wolf = 32.5



df_meteo <- read.csv("E:/Internship/Climate variables/df_meteo.csv")
meteo_VA <- df_meteo[,c(1,3,6,9)]

get_pk <- function(Tmax,local_T){
  ifelse (local_T>Tmax,1,0)
}



meteo_VA$CTi_boar <- NA
meteo_VA$CTi_fox <- NA
meteo_VA$CTi_muskrat <- NA
meteo_VA$CTi_wolf <- NA



for(i in 1:length(meteo_VA$Date)){
  meteo_VA$pk_boar[i] = get_pk(Tmax = Tmax_boar, local_T = meteo_VA$Vala_txxx[i])
  meteo_VA$pk_fox[i] = get_pk(Tmax = Tmax_fox, local_T = meteo_VA$Vala_txxx[i])
  meteo_VA$pk_muskrat[i] = get_pk(Tmax = Tmax_muskrat, local_T = meteo_VA$Vala_txxx[i])
  meteo_VA$pk_wolf[i] = get_pk(Tmax = Tmax_wolf, local_T = meteo_VA$Vala_txxx[i])
  if(i>14){
    meteo_VA$CTi_boar[i] = mean(meteo_VA$pk_boar[i-14:i])
    meteo_VA$CTi_fox[i] = mean(meteo_VA$pk_fox[i-14:i])
    meteo_VA$CTi_muskrat[i] = mean(meteo_VA$pk_muskrat[i-14:i])
    meteo_VA$CTi_wolf[i] = mean(meteo_VA$pk_wolf[i-14:i])
  }
}


meteo_VA <- meteo_VA[-(1:14),]
meteo_VA <- meteo_VA[,c(1,5:8)]

meteo_VA_ggp <- melt(meteo_VA, id.vars="Date")
meteo_VA_ggp$Date <- as.Date(meteo_VA_ggp$Date)
meteo_VA_ggp$species <- c(rep("Wild boar",nrow(meteo_VA)),
                          rep("Red fox",nrow(meteo_VA)),
                          rep("Muskrat",nrow(meteo_VA)),
                          rep("Wolf",nrow(meteo_VA)))

meteo_VA_ggp_noboar <- meteo_VA_ggp[-(1:122),]


vama <- ggplot(meteo_VA_ggp_noboar,aes(Date,value, col=species)) +
  geom_point()+
  geom_line(size = 2.5, alpha = 0.5)+
  labs(y = expression(italic("CTi")), x = "Month",
       title = "CTi for mammals in subcatchment of Valais") +
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
meteo_VA <- df_meteo[,c(1,3,6,9)]


get_pk_cattle <- function(THI_mild,THI_moderate,
                          THI_severe,local_T,local_H){
  THI = (1.8*local_T+32)-(0.55-0.0055*local_H)*(1.8*local_T-26)
  if(THI>THI_severe) {return(1)}
  if((THI>THI_moderate)&(THI<=THI_severe)){return(0.5)}
  if((THI>THI_mild)&(THI<=THI_moderate)){return(0.2)}
  if(THI<=THI_mild){return(0)}
}


meteo_VA$CTi_cattle <- NA
meteo_VA$CTi_sow <- NA
meteo_VA$CTi_horse <- NA

for(i in 1:length(meteo_VA$Date)){
  meteo_VA$pk_cattle[i] = get_pk_cattle(THI_mild = THI_mild_cattle,
                                        THI_moderate = THI_moderate_cattle,
                                        THI_severe = THI_severe_cattle,
                                        local_T = meteo_VA$Vala_txxx[i],
                                        local_H = meteo_VA$Vala_relh[i])
  meteo_VA$pk_sow[i] = get_pk_cattle(THI_mild = THI_mild_sow,
                                     THI_moderate = THI_moderate_sow,
                                     THI_severe = THI_severe_sow,
                                     local_T = meteo_VA$Vala_txxx[i],
                                     local_H = meteo_VA$Vala_relh[i])
  meteo_VA$pk_horse[i] = get_pk_cattle(THI_mild = THI_mild_horse,
                                       THI_moderate = THI_moderate_horse,
                                       THI_severe = THI_severe_horse,
                                       local_T = meteo_VA$Vala_txxx[i],
                                       local_H = meteo_VA$Vala_relh[i])
  if(i>14){
    meteo_VA$CTi_cattle[i] = mean(meteo_VA$pk_cattle[i-14:i])
    meteo_VA$CTi_sow[i] = mean(meteo_VA$pk_sow[i-14:i])
    meteo_VA$CTi_horse[i] = mean(meteo_VA$pk_horse[i-14:i])
  }
}


meteo_VA <- meteo_VA[-(1:14),]
meteo_VA <- meteo_VA[,c(1,5:7)]

meteo_VA_ggp <- melt(meteo_VA, id.vars="Date")
meteo_VA_ggp$Date <- as.Date(meteo_VA_ggp$Date)
meteo_VA_ggp$species <- c(rep("Cattle",nrow(meteo_VA)),
                          rep("Sow",nrow(meteo_VA)),
                          rep("Horse",nrow(meteo_VA)))



vali <- ggplot(meteo_VA_ggp,aes(Date,value, col=species)) +
  geom_point(alpha = 0.5)+
  geom_line(size = 2.5,alpha = 0.5)+
  labs(y = expression(italic("CTi")), x = "Month",
       title = "CTi for livestock in subcatchment of Valais") +
  theme_hc() +
  theme(legend.position = "right")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_x_date(date_labels = "%b")

