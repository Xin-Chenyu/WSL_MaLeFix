Amphibian_CTi <- read_csv("E:/Internship/Swiss species/Tables/Amphibian_CTi.csv")
df_txxx <- df_meteo[,c(1,8:10)]

get_pk <- function(Tmax,local_T){
  ifelse (local_T>Tmax,1,0)
}


# get_pk_cattle <- function(THI_mild,THI_moderate,
#                           THI_severe,local_T,local_H){
#   THI = (1.8*local_T+32)-(0.55-0.0055*local_H)*(1.8*local_T-26)
#   if(THI>THI_severe) {return(1)}
#   if((THI>THI_moderate)&(THI<=THI_severe)){return(0.5)}
#   if((THI>THI_mild)&(THI<=THI_moderate)){return(0.2)}
#   if(THI<=THI_mild){return(0)}
# }


# df_meteo$CTi_Tici_boar <- NA
# df_meteo$CTi_Vala_boar <- NA
# df_meteo$CTi_Zuri_boar <- NA
# 
# df_meteo$CTi_Tici_Sala <- NA
# df_meteo$CTi_Vala_Sala <- NA
# df_meteo$CTi_Zuri_Sala <- NA
# 
# df_meteo$CTi_Tici_cattle <- NA
# df_meteo$CTi_Vala_cattle <- NA
# df_meteo$CTi_Zuri_cattle <- NA

for(i in 1:length(df_txxx$Date)){
  for (j in 1:length(Amphibian_CTi)) {
 
   
  df_txxx$paste0("pk_Tici_",j)[i] = get_pk(Tmax = Amphibian_CTi[j,2], local_T = df_txxx$Tici_txxx[i])
  df_txxx$paste0("pk_Vala_",j)[i] = get_pk(Tmax = Amphibian_CTi[j,2], local_T = df_txxx$Vala_txxx[i])
  df_txxx$paste0("pk_Zuri_",j)[i] = get_pk(Tmax = Amphibian_CTi[j,2], local_T = df_txxx$Zurch_txxx[i])
  
  # if(i>14){
  #   
  #   df_meteo$CTi_Tici_Sala[i] = mean(df_meteo$pk_Tici_Sala[i-14:i])
  #   df_meteo$CTi_Vala_Sala[i] = mean(df_meteo$pk_Vala_Sala[i-14:i])
  #   df_meteo$CTi_Zuri_Sala[i] = mean(df_meteo$pk_Zuri_Sala[i-14:i])
  #   
  # }
  }
  
}


df_meteo <- df_meteo[-(1:14),]


colname_order <- c("Date", "CTi_Tici_cattle", "CTi_Tici_boar","CTi_Tici_Sala",
                   "CTi_Vala_cattle", "CTi_Vala_boar","CTi_Vala_Sala",
                   "CTi_Zuri_cattle", "CTi_Zuri_boar", "CTi_Zuri_Sala")

df_CTi <- df_meteo[, colname_order]

write.csv(df_CTi, "E:/Internship/Climate variables/df_CTi.csv",row.names = FALSE)
df_CTi <- read.csv("E:/Internship/Climate variables/df_CTi.csv")

df_CTi_ggp <- melt(df_CTi, id.vars="Date")
df_CTi_ggp$Date <- as.Date(df_CTi$Date)
df_CTi_ggp$location <- c(rep("Ticino",3*nrow(df_CTi)),
                         rep("Valais",3*nrow(df_CTi)),
                         rep("Zurich",3*nrow(df_CTi)))
df_CTi_ggp$species <- rep(c(rep("cattle",nrow(df_CTi)),
                            rep("sus scrofa",nrow(df_CTi)),
                            rep("salamandra atra",nrow(df_CTi))),3)


ggplot(df_CTi_ggp, aes(Date,value, col=species)) + 
  geom_point()+
  geom_line()+
  scale_color_viridis(discrete=TRUE) +
  facet_grid(location ~ .)+
  labs(y = expression(italic("CTi")), x = "Month",
       title = "Critical Temperature index (CTi) in subcatchments of canton Ticino, Valais and Zurich",
       subtitle = "Using cattle, alpine salamander and wild boar as examples") +
  scale_x_date(date_labels = "%b")
