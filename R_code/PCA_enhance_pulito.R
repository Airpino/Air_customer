# Do PCA

## INIT --------
#some analysis
load("Data/My_table3.RData")
library(tidyverse)
library(patchwork)
library(class)
library(HistDAWass)
library(progress)
library(png)
source("R_code/WASS_funct_discrete.R")
source("R_code/WASS_funct_regression.R")
source("R_code/WASS_principal_components_discr.R")
source("R_code/WASS_viz_discrete.R")
source("R_code/compute_moments.R")
## END INIT --------------


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
levels(categ$SY)<-list(E_15="EST_2015",I_15="INV_2015",
                       E_16="EST_2016",I_16="INV_2016",
                       E_17="EST_2017",I_17="INV_2017")

## DO PCA ----------
doPCA=TRUE
quantiles=25
if(doPCA) {
  res<-WH.MultiplePCA_discr(Fin_tibble,list.of.vars = c(2:13),
                          quantiles = quantiles,ncp_in = 8)
resL12<-WH.MultiplePCA_discr(Fin_tibble,list.of.vars = c(14:15),
                             quantiles = 20,ncp_in = 5)
}

## DO A GRID
# get the min and max of the plane coordinates
plane<- c(1,2)
coords <- res$ind$coord[,plane]
mins<-apply(coords,2,min)
maxs<-apply(coords,2,max)
ratio<-(maxs-mins)[1]/(maxs-mins)[2]
number_of_neurons<-162
height<-ceiling(sqrt(number_of_neurons/ratio))
base<-ceiling(number_of_neurons/height)

# get the grid 

map<-somgrid(xdim = base, ydim = height, topo = c("hexagonal"))

new_points_x<-(map$pts[,1]-min(map$pts[,1]))/diff(range(map$pts[,1]))*(maxs-mins)[1]+mins[1]
new_points_y<-(map$pts[,2]-min(map$pts[,2]))/diff(range(map$pts[,2]))*(maxs-mins)[2]+mins[2]
new_co<-data.frame(nx=new_points_x,ny=new_points_y)
# reconstruct points -----
ncp<-2
New_qua<-reconstr_MFA(res,ncp,new_co,quantiles)

df1<-data.frame(nx=sort(unique(new_co$nx)),ny=0)
df2<-data.frame(nx=0,ny=sort(unique(new_co$ny))) %>% select(nx,ny)
## Eye iris on single axes ------------
New_qua_ax1<-reconstr_MFA(res,ncp,df1,quantiles)
New_qua_ax2<-reconstr_MFA(res,ncp,df2,quantiles)

dom<-c(1:10)
#lets create a new tibble
labels<-paste0("ID",1:nrow(New_qua))# generate the labels
variab.names<-colnames(Fin_tibble[,2:13])

Recon_to_discr<-function(New_qua,qunatiles,dom=c(1:10),variab.names){
  nv<-ncol(New_qua)/(quantiles+1)
  
  x<-New_qua[1,1:(quantiles+1)] ## PROBLEM NOT MONOTONE! HOW TO DO?
  # Which is the best increasing step function approximating
  p<-c(0:quantiles)/quantiles
  
  #create the variables
  tml_v<-vector("list", nv)
  pb <- progress_bar$new(
    format = "  discretizing recons. grid [:bar] :percent eta: :eta",
    total = nv, clear = FALSE, width= 80)
  for(j in 1:nv){
    tmp_l<-vector("list", nrow(New_qua))
    ini<-(j-1)*(quantiles+1)+1
    end<-j*(quantiles+1)
    a<-Sys.time()
    for (i in 1: nrow(New_qua)){
      #print(paste0("V_",j,"_ID_",i))
      obg<-discretize_continuous_distr(New_qua[i,ini:end],p,dom)
      if(nrow(obg)<3) browser()
      if(max(obg$cdf)<1) browser()
      tmp_l[[i]]<-discretize_continuous_distr(New_qua[i,ini:end],p,dom)
    }
    tml_v[[j]]<-tmp_l
    pb$tick()
    Sys.sleep(1 / 100)
  }
  

# let's compute the number of variables



hexa_tibb<-tibble((!!variab.names[1]):=tml_v[[1]])
for(j in 2:nv){
  hexa_tibb<-cbind(hexa_tibb, tibble((!!variab.names[j]):=tml_v[[j]]))
}
  return(hexa_tibb)
}

hexa_tibb<-Recon_to_discr(New_qua,qunatiles,dom=c(1:10),variab.names)
hexa_tibb<-hexa_tibb %>% mutate(labels=labels,
                                coor_x=new_points_x,
                                coor_y=new_points_y)#add the coordinates

BSTAT_hexa<-m_sd_skew_NestTibb_nol(hexa_tibb[,1:12])
SKEWM<-as.data.frame(cbind(BSTAT_hexa$skew,hexa_tibb$coor_y))
SKEWM %>% group_by(V13) %>% 
  summarize(B1=mean(B1),B2=mean(B2),C1=mean(C1),D1=mean(D1),D2=mean(D2),
            E1=mean(E1),E2=mean(E2),E3=mean(E3),E4=mean(E4),
            F1=mean(F1),F2=mean(F2),F3=mean(F3))
stop()
## Fine nuova tibble per plots ----
correla<-corr_mat_Wass_discr(Fin_tibble[,2:13])
hclu_v<-hclust(as.dist(sqrt(2*(1-correla))),method = "single")
myorder<-hclu_v$order
myorder<-c(1:12)
nn<-nrow(hexa_tibb)
pb <- progress_bar$new(
  format = "  saving eye-iris [:bar] :percent eta: :eta",
  total = nn, clear = FALSE, width= 80)
for(i in 1:nn){
  fname<-paste0("./Images/im_h_",i,".png")
  pp<-Performance_plot(hexa_tibb[,(myorder+3)],selected = i,labels=labels,TITLE=F,bg="white")
  
  #CairoPNG(filename = fname, bg = "transparent")
  ggsave(filename = fname,
         plot = pp,
         width = 3, 
         height = 3,
         dpi=100#,bg='transparent'
  )
  pb$tick()
  Sys.sleep(1 / 100)
}

df_im_h<-data.frame(ID=c(1:nn),x=hexa_tibb$coor_x,y=hexa_tibb$coor_y) %>% mutate(adr=paste0("Images/im_h_",ID,".png"))

library(ggimg)
ori_coord<-as.data.frame(res$ind$coord)
ph<-ggplot(df_im_h,aes(x=x, 
                 y=y))+
  #geom_point()+
  #geom_text(aes(label=ID))+
  geom_point_img(aes(
    x = x,
    y = y,
    img = adr
  ), size = 1.2,alpha=0.8,interpolate = FALSE)+ 
  geom_point(inherit.aes = F,data = ori_coord,aes(x=Dim.1,y=Dim.2),
             color='red', fill='darkblue', shape=21,size=2)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  coord_fixed()+
  theme_minimal()
ph


## create a grid exa ----
init<-paste0("layout <- c(")
cont<-""
rg<-8
cg<-4
mult<-4
for(i in 1:rg){#
  for(j in 1:cg){
    if(i%%2==1){ st=0}else{(st=1)}
    cont<-paste0(cont,"area(",(i-1)*mult+1-(i-1),",",
                 st+(j-1)*mult+1-(j-1),",",
                 (i-1)*mult+(mult)-(i-1),",",
                 st+(j*mult)-(j-1))
    if((i==rg)&(j==cg)){
      cont<-paste0(cont,")")
    }else{
      
      cont<-paste0(cont,"),")
    }
    
  }
}
fin<-")"
paste0(init,cont,fin)
eval(parse(text=paste0(init,cont,fin)))


## Proviamo a generare i plottini e poi fare l'inset sul grafico fel primo piano fattoriale -----
list_of_per_plots<-vector("list", nrow(hexa_tibb))
for(i in 1:nrow(hexa_tibb)){
  tmp<-Performance_plot(hexa_tibb[,(myorder+3)],selected = i,labels=labels,TITLE=F,notick = T,bg="transparent")
  list_of_per_plots[[i]]<-tmp
}
firstplane<-data.frame(X=res$ind$coord[,1],Y=res$ind$coord[,2])
DX<-min(abs(diff(sort(unique(hexa_tibb$coor_x)))))
DY<-min(abs(diff(sort(unique(hexa_tibb$coor_y)))))/2
RX<-range(firstplane$X)+c(-DX,DX)
RY<-range(firstplane$Y)+c(-DY,DY)

 p0<-ggplot(firstplane,aes(x=X,y=Y))+geom_blank()+theme_void()+
#   geom_point(inherit.aes = F, data=hexa_tibb,aes(x=coor_x,y=coor_y),color="black")+
   coord_fixed(ratio=1)+coord_cartesian(xlim = RX,
                                        ylim = RY,expand = F)
base_plot<-ggplot(firstplane,aes(x=X,y=Y))+
  geom_blank()+theme_void()+
  theme(
    panel.border = element_rect(color="black",fill = "black"),
    panel.background = element_rect(color="black",fill = "black"),
    plot.background = element_rect(fill = "black", color = "black"))+
  coord_fixed(ratio=1)+coord_cartesian(xlim = RX,
                                       ylim = RY,expand = F)

## prepariamo le coordinate relative al plot per le celle esagonali  -------
hexa_coord<-hexa_tibb[,1:3]
hexa_coord$nx<-(hexa_coord$coor_x-min(RX))/diff(RX)
hexa_coord$ny<-(hexa_coord$coor_y-min(RY))/diff(RY)
hexa_coord$nx_min<-(hexa_coord$coor_x-min(RX))/diff(RX)-DX/diff(RX)
hexa_coord$nx_max<-(hexa_coord$coor_x-min(RX))/diff(RX)+DX/diff(RX)
hexa_coord$ny_min<-(hexa_coord$coor_y-min(RY))/diff(RY)-DY/diff(RY)
hexa_coord$ny_max<-(hexa_coord$coor_y-min(RY))/diff(RY)+DY/diff(RY)

 exp_fact=1.3
plotsk<-F
str<-""
for(i in 1:nrow(hexa_coord)){

  str<-paste0(str,"inset_element(")
  str<-paste0(str,"Performance_plot(hexa_tibb[,4:15],selected = ",i,",labels=labels,TITLE=F,notick=T,alpha=0.8,")
  if(plotsk){
  str<-paste0(str,("skewness_plo = T),"))}else{
  str<-paste0(str,("skewness_plo = F),"))}
  str<-paste0(str,"left=  hexa_coord$nx_min[",i,"]*exp_fact-hexa_coord$nx[",i,"]*(exp_fact-1),")
  str<-paste0(str,"bottom=hexa_coord$ny_min[",i,"]*exp_fact-hexa_coord$ny[",i,"]*(exp_fact-1),")
  str<-paste0(str,"right= hexa_coord$nx_max[",i,"]*exp_fact-hexa_coord$nx[",i,"]*(exp_fact-1),")
  str<-paste0(str,"top=   hexa_coord$ny_max[",i,"]*exp_fact-hexa_coord$ny[",i,"]*(exp_fact-1))")
if(i<nrow(hexa_coord)){
  str<-paste0(str,"+")}  
}
str<-paste0("ppp <- base_plot+",str)

file_name<-"map_iris.png"

eval(parse(text=str))### ora in ppp c'
### save the file in a background image
ggsave(filename = file_name,
               plot = ppp,
                width = 18.5/2, 
                  height = (9-0.5)/2,
                 dpi=150,bg='black'
       )
knitr::plot_crop(file_name)
browser()

img <- png::readPNG("map_iris.png")
background_image <- function(raster.img){
  annotation_raster(raster.img,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf)
}
#ppp+inset_element(p0+geom_point(color="black",size=4)+geom_hline(yintercept=0)+geom_vline(xintercept=0)+
      
ppp2<-ggplot(firstplane,aes(x=X,y=Y))+theme_void()+
  #theme(
  #  panel.border = element_rect(color=NA,fill = "black"),
  #  panel.background = element_rect(color="black",fill = "black"),
  #  plot.background = element_rect(fill = "black", color = "black"))+
  coord_fixed(ratio=1)+coord_cartesian(xlim = RX,
                                       ylim = RY,expand = F)+
  #base_plot+
  background_image(img)+
  geom_hline(yintercept=0,color="white",alpha=1)+
  geom_vline(xintercept=0,color="white",alpha=1)+
  geom_point(color="yellow",fill="red",shape=21,size=3,stroke = 1.5)+
  theme_bw()


## VIVA PLOTLY -----
ppp2_2<-ggplot(firstplane,aes(x=X,y=Y))+theme_void()+
  coord_fixed(xlim = RX,ylim = RY,expand = F)+
  geom_point(color="yellow",fill="red",shape=21,size=3,stroke = 1)+
  theme_bw()+theme(axis.text.x = element_text(face="bold", color="white"),
                   axis.text.y = element_text(face="bold", color="white"),
                   axis.line = element_line(colour = "white"),
                   panel.background = element_rect(colour = "white",linewidth = 3)
                   )

  
ppp3<-plotly::ggplotly(ppp2_2)
plotly::layout(ppp3,
               plot_bgcolor = "black",
               paper_bgcolor = "black",
               xaxis=list(
                 linecolor='white',
                 linewidth= 2,
                 mirror=T
               ),
               yaxis=list(
                 linecolor='white',
                 linewidth= 2,
                 mirror= T
               ),
               images = list(
                 list(
                   # Add images
                   #source =  "https://images.plot.ly/language-icons/api-home/r-logo.png?raw=true",
                   source = base64enc::dataURI(file = "map_iris.png") , 
                   xref = "x",
                   yref = "y",
                   x = RX[1],
                   y = RY[2],
                   sizex = diff(RX),
                   sizey = diff(RY),
                   sizing = "stretch",
                   opacity = 1,
                   layer = "below"
                 )
               )
)




stop()
## CLUSTERING TO SEE!----
hc<-WD2_HC(Fin_tibble[,2:13],method="complete")
K8<-cutree(hc,6)
Cl<-6
sel<-unname(which(K8==Cl))
command_text<-paste0("Performance_plot(Fin_tibble[,2:13],selected = ",
                     sel[1],",labels=labs)")
if(length(sel)>1){
  for(i in 2:length(sel)){
    command_text<-paste0(command_text,"+Performance_plot(Fin_tibble[,2:13],selected = ",
                         sel[i],",labels=labs)")
    
  }
}

eval(parse(text=command_text))

# extract the convex hull
sel_L_dim=1
sel_data_dim=2

DF_tmp<-data.frame(X=resL12$ind$coord[,sel_data_dim],
                   Y=res$ind$coord[,sel_L_dim])
firstplane2<-DF_tmp %>% mutate(X2=X-min(X),Y2=Y-min(Y))
firstplane2<-rbind(firstplane2,c(min(DF_tmp$X),min(DF_tmp$Y),
                                 0,0))
firstplane2<-rbind(firstplane2,c(min(DF_tmp$X),max(DF_tmp$Y),0,
                                 max(DF_tmp$Y)-min(DF_tmp$Y)))
firstplane2<-rbind(firstplane2,c(max(DF_tmp$X),min(DF_tmp$Y),
                                 max(DF_tmp$X)-min(DF_tmp$X),0))
hull <- firstplane2 %>%slice(chull(X, Y)) #contiene la sequenza dei punti
p <- ggplot(firstplane2, aes(X, Y)) + 
  geom_point(shape = 21,aes(text=as.character(c(1:nrow(firstplane2)))))+
  geom_polygon(data = hull, alpha = 0.5)
plotly::ggplotly(p)
npch<-nrow(hull)
#extract segments
#PCA L1 "atteso" OK! e caccio lo score --------
resQ_expected<-WH.1d.PCA_discr(Fin_tibble,var = 14,
                             quantiles = 30)
rotatedLoadingsExp <- varimax(resQ_expected$PCAout$var$cor[,1:2])
oricoord<-resQ_expected$PCAout$ind$coord[,1:3]
parcoord<-oricoord[,1:3]%*%rotatedLoadings$rotmat
#convex hull

# extract the convex hull
sel_L_dim=1
sel_data_dim=2
highliths<-c(70:96,142:165)


 gra<-plot_CH_map(1,2,
             resu=resQ_expected$PCAout,highliths,alpha = 0.6)
# gra<-plot_CH_map2(1,2,
#                  resu=resQ_expected$PCAout,highliths,alpha = 0.6)
gra$p2
# #PCA L2 "percepita"---------
# resQ_perceived<-WH.1d.PCA_discr(Fin_tibble,var = 15,
#                                quantiles = 30)
# rotatedLoadingsPerc <- varimax(resQ_perceived$PCAout$var$cor[,1:3])
# plot_CH_map(sel_L_dim,sel_data_dim,
#             resu=resQ_perceived$PCAout,highliths)
df_h<-data.frame(nx=gra$ch$X,ny=gra$ch$Y)
rcon_CH<-reconstr_PCA(resQ_expected$PCAout,ncp,df_h,quantiles)
lbs<-row.names(gra$ch)
#reconstruct frontier points
tmpl<-list()
p<-c(0:quantiles)/quantiles
for(i in 1:length(lbs)){
  IDtmp<-as.numeric(lbs[i])
  if (IDtmp%in% c(1:nrow(resQ_expected$PCAout$ind$coord))){
    tmpl[[i]]=Fin_tibble[[14]][[i]]
  }else{
    tmpl[[i]]<-discretize_continuous_distr(rcon_CH[i,c(3,3,3:(quantiles+1))],p,dom)
  }
}
EXP<-tibble(EXP=tmpl) %>% mutate(ID=as.numeric(lbs),
                                 labs2=if_else(ID<=nrow(resQ_expected$PCAout$ind$coord),"true_p","artifact"))
EXP<-cbind(EXP,df_h)
routing<-c(1:nrow(EXP),1)
#per i punti 2017 come muoverli verso la frontiera
eigens<-resQ_expected$PCAout$eig[1:2,1]
coeffs<-sqrt(eigens[1:2,1])#/sum(resQ_expected$PCAout$eig[1:2,1])
slope<-coeffs[2]/coeffs[1]
DD<-targets_points(highliths,routing,gra,slope)

# identificare il target
orilab->hilights

pp<-gra$p
pp+geom_segment(data=DD$DF_segmenti,aes(x=ini_x,y=ini_y,xend=end_x,yend=end_y))
# calcolare il trasporto ottimo (QQ plot)
# misurare lo sforzo
# mettere tutto in ordine
# effort e scomposizione
# 28 03 2023 struttura del paper. E divisioni compiti.
medie<-means_stds_NestTibb_nol(Fin_tibble[,2:15])
regr_L1<-WH.regression.two.components_discr(Fin_tibble,14,c(2:13),mY = medie$means[,13],mX=medie$means[,1:12])
predictions<-WH.two_comp_predict(Fin_tibble,c(2:13),mX = medie$means[,1:12],regr_L1)
for(i in 1:nrow(predictions)){
  predictions[[1]][[i]]<-Discretize_a_distr(predictions[[1]][[i]])
  
}
(GOFS<-WH.GOFs_discr(Fin_tibble[,14:15],predictions))

medie<-means_stds_NestTibb_nol(Fin_tibble[,2:15])
regr_L2<-WH.regression.two.components_discr(Fin_tibble,15,c(2:13),mY = medie$means[,14],mX=medie$means[,1:12])
predictions<-WH.two_comp_predict(Fin_tibble,c(2:13),mX = medie$means[,1:12],regr_L2)
for(i in 1:nrow(predictions)){
  predictions[[1]][[i]]<-Discretize_a_distr(predictions[[1]][[i]])
  
}
(GOFS<-WH.GOFs_discr(Fin_tibble[,15:14],predictions))




my_ex<-Wass_SQ_discr_MULT(Fin_tibble[92,2:13],Fin_tibble[61,2:13])
pippo<-Transp_plots_MULT(Fin_tibble[92,2:13],Fin_tibble[61,2:13])

va<-6
a<-pippo %>% filter(IDV==va)
#a<-rbind(a,c(1,max(a$one_q),max(a$ref_q)),c(1,min(a$one_q),min(a$ref_q)))
a<-rbind(a,c(1,10,10),c(1,1,1))
ggplot(a,aes(x=ref_q,y=one_q))+geom_polygon(fill="red")+theme_bw()+
  xlab("Target quantiles")+ylab("Data quantiles")+
  xlim(c(1,10))+ylim(c(1,10))

for(vv in 1:12){
  print(vv)
  
regr_L1<-WH.regression.two.components_discr(Fin_tibble,
                                            Yvar = vv+1,Xvars = 14,
                                            mY = medie$means[,vv],
                                            mX=medie$means[,13])
print(regr_L1)
predictions<-WH.two_comp_predict(Fin_tibble,
                                 Xvars = 14,
                                 mX = medie$means[,13],
                                 regr_L1)
  # for(i in 1:nrow(predictions)){
  #   predictions[[1]][[i]]<-Discretize_a_distr(predictions[[1]][[i]])
  #   
  # }
print(WH.GOFs_discr(Fin_tibble[,vv+c(1,2)],predictions))
}
