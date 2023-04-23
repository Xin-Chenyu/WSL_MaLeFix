library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggpubr)

Tmax_burbot = 29
Tmax_trout = 27.4
Tmax_pike =29

## Rhone ##

Rp <- ggplot(rho_sim, aes(Days,Temp)) + 
  geom_line(size = 1.5, alpha = 0.8, color = "#00AFBB")+
  labs(title = "Simulation of water temperature of River Rhone",
       y = "Temperature", 
       x = "Days")+
  theme_hc()
  #scale_color_discrete(name = NULL)

## Doubs ##
Dp <-ggplot(dou_sim, aes(Days,Temp)) + 
  geom_line(size = 1.5, alpha = 0.8, color = "#00AFBB")+
  labs(title = "Simulation of water temperature of River Doubs",
       y = "Temperature", 
       x = "Days")+
  theme_hc()

ggarrange(Rp,Dp,
          labels = c("a", "b"),
          ncol = 2, nrow = 1)

get_pk <- function(Tmax,local_T){
  ifelse (local_T>Tmax,1,0)
}




rho_sim$CTi_burbot <- NA
rho_sim$CTi_trout <- NA
rho_sim$CTi_pike <- NA



for(i in 1:length(rho_sim$Days)){
  rho_sim$pk_burbot[i] = get_pk(Tmax = Tmax_burbot, local_T = rho_sim$Temp[i])
  rho_sim$pk_trout[i] = get_pk(Tmax = Tmax_trout, local_T = rho_sim$Temp[i])
  rho_sim$pk_pike[i] = get_pk(Tmax = Tmax_pike, local_T = rho_sim$Temp[i])
  if(i>14){
    rho_sim$CTi_burbot[i] = mean(rho_sim$pk_burbot[i-14:i])
    rho_sim$CTi_trout[i] = mean(rho_sim$pk_trout[i-14:i])
    rho_sim$CTi_pike[i] = mean(rho_sim$pk_pike[i-14:i])
  }
}


rho_sim <- rho_sim[-(1:14),]
rho_sim <- rho_sim[,c(1,3:5)]

rho_sim_ggp <- melt(rho_sim, id.vars="Days")
rho_sim_ggp$species <- c(rep("Burbot",nrow(rho_sim)),
                          rep("Lake trout",nrow(rho_sim)),
                          rep("Northern pike",nrow(rho_sim)))
                          


CTi_rho <- ggplot(rho_sim_ggp,aes(Days,value, col=species)) +
  geom_point()+
  geom_line(size = 2, alpha = 0.5)+
  labs(y = expression(italic("CTi")), x = "Days",
       title = "CTi for fish in River Rhone") +
  theme_few() +
  theme(legend.position = "right")+
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) 

CTi_dou <- ggplot(rho_sim_ggp,aes(Days,value, col=species)) +
  geom_point()+
  geom_line(size = 2, alpha = 0.5)+
  labs(y = expression(italic("CTi")), x = "Days",
       title = "CTi for fish in River Doubs") +
  theme_few() +
  theme(legend.position = "right")+
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) 


ggarrange(CTi_rho, CTi_dou, 
          labels = c("a", "b"),
          ncol = 1, nrow = 2)

