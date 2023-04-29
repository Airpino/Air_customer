#some analysis
load("My_table3.RData")
library(tidyverse)
library(patchwork)
source("WASS_funct_discrete.R")
source("WASS_funct_regression.R")
source("WASS_principal_components_discr.R")
source("WASS_viz_discrete.R")
source("compute_moments.R")
#Accorciamo le labels
#aggiungere a fin tibble L1 e L2 per poi fare le analisi

labs<-Fin_tibble$LAB
labs<-gsub("BARI.","BA.",labs)
labs<-gsub("BRINDISI.","BR.",labs)
labs<-gsub("2015.","15.",labs)
labs<-gsub("2016.","16.",labs)
labs<-gsub("2017.","17.",labs)
labs<-gsub("ROMA.","RM.",labs)
labs<-gsub("MILANO.","MI.",labs)
labs<-gsub("OTH_NAZ.","NAZ.",labs)
labs<-gsub("INTERN.","IN.",labs)
labs<-gsub("ALITALIA.","AZ.",labs)
labs<-gsub("RYANAIR.","RY.",labs)
labs<-gsub("OTHER.","OT.",labs)
labs<-gsub(".Business",".Bu",labs)
labs<-gsub(".Leisure",".Le",labs)
labs<-gsub("INV.","I.",labs)
labs<-gsub("EST.","E.",labs)



basics<-m_sd_skew_NestTibb_nol(Fin_tibble[,2:15])

# connect rows by time
categ<-everything %>% select(AIRPORT,YEAR,SEASON,FLIGHT,COMPANY,TYPE)
categ<-categ %>% mutate(SEA_YEAR=paste0(SEASON,"_",YEAR),
                        allcat=factor(paste0(AIRPORT,"_",FLIGHT,"_",COMPANY,"_",TYPE)))
categ<-categ %>% mutate(SY=factor(SEA_YEAR,levels=c("EST_2015","INV_2015",
                                                    "EST_2016","INV_2016",
                                                    "EST_2017","INV_2017")))
#farei una PCA/interpreterei gli assi
res<-WH.MultiplePCA_discr(Fin_tibble,list.of.vars = c(2:13),quantiles = 20,ncp_in = 5)
resL12<-WH.MultiplePCA_discr(Fin_tibble,list.of.vars = c(14:15),quantiles = 20,ncp_in = 5)

### EYE-iris plots
## 126 and 142 the farthest
Performance_plot(Fin_tibble[,2:13],selected = 91,labels=labs)+
  Performance_plot(Fin_tibble[,2:13],selected = 92,labels=labs)+
  Performance_plot(Fin_tibble[,2:13],selected = 14,labels=labs)+
  Performance_plot(Fin_tibble[,2:13],selected = 142,labels=labs)+
  Performance_plot(Fin_tibble[,2:13],selected = 5,labels=labs)+
  Performance_plot(Fin_tibble[,2:13],selected = 126,labels=labs)+
    Performance_plot(Fin_tibble[,2:13],selected = 162,labels=labs)+
  Performance_plot(Fin_tibble[,2:13],selected = 133,labels=labs)+
    Performance_plot(Fin_tibble[,2:13],selected = 79,labels=labs)
plot_spacer()+Performance_plot(Fin_tibble[,14:15],selected = 92,labels=labs)+plot_spacer()+
  Performance_plot(Fin_tibble[,14:15],selected = 142,labels=labs)+
  Performance_plot(Fin_tibble[,14:15],selected = 5,labels=labs)+
  Performance_plot(Fin_tibble[,14:15],selected = 126,labels=labs)+
  plot_spacer()+
  Performance_plot(Fin_tibble[,14:15],selected = 133,labels=labs)+
  plot_spacer()
# plotterei gli EYE-iris al posto dei punti
# collegherei le traiettorie per gli anni, o la stagione/anni in interattivo
# ma fai attenzione perchè non tutti hanno 6 punti

# Estrai gli scores dalle due PCA
# Costruisci la frontiera e mettici i punti in base agli scores

# Calcola la distanza con la frontiera (Approssima le distribuzioni tenendo conto
# delle distribuzioni discrete)(ottimizza con la migliore distribuzione discreta?)
# descrivi la distanza con la frontiera con transport plots



