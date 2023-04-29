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
doPCA=FALSE
quantiles=30
if(doPCA) res<-WH.MultiplePCA_discr(Fin_tibble,list.of.vars = c(2:13),
                          quantiles = quantiles,ncp_in = 8)

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
# reconstruct points
ncp<-2
New_qua<-reconstr_MFA(res,ncp,new_co,quantiles)
# let's compote the number of variables
nv<-ncol(New_qua)/(quantiles+1)



# discretize_continuous_distr<-function(x,p,dom,quantiles){
#   x<-monotonize_seq(x,quantiles)
#   dom_agg<-(dom[1:(length(dom)-1)]+dom[2:length(dom)])*0.5
#  # dom_agg<-sort(unique(c(dom,dom_agg)))
#   di<-distributionH(x,p)
#   dom_agg<-sort(unique(c(dom_agg,range(dom))))
#   pro<-sapply(dom_agg,function(x)compP(di,x))
#   df<-as.data.frame(cbind(dom,freq=diff(pro),cdf=cumsum(diff(pro))))
#   return(df)
# }
x<-New_qua[1,1:(quantiles+1)] ## PROBLEM NOT MONOTONE! HOW TO DO?
# Which is the best increasing step function approximating
p<-c(0:quantiles)/quantiles
dom<-c(1:10)
#lets create a new tibble
labels<-paste0("ID",1:nrow(New_qua))# generate the labels
hexa_tibb<-tibble(labels=labels,coor_x=new_points_x,coor_y=new_points_y)#add the coordinates
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
    #   if((i==108) && (j==2)) {print(j)
    #     browser()
    #     obg<-discretize_continuous_distr(New_qua[i,ini:end],p,dom)}
       tmp_l[[i]]<-discretize_continuous_distr(New_qua[i,ini:end],p,dom)
     }
    #tmp_l2<-lapply(1: nrow(New_qua),FUN = function(x)discretize_continuous_distr(New_qua[x,ini:end],p,dom))
    tml_v[[j]]<-tmp_l
    pb$tick()
    Sys.sleep(1 / 100)
}

variab.names<-colnames(Fin_tibble[,2:13])

for(j in 1:nv){
  hexa_tibb<-cbind(hexa_tibb, tibble((!!variab.names[j]):=tml_v[[j]]))
}


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

#browser()

# p1<-Performance_plot(Fin_tibble[,2:13],selected = 1,labels=labels,TITLE=F,notick=T)
#p2<-Performance_plot(Fin_tibble[,hclu_v$order+1],selected = 162,labels=labels,TITLE=F)

# layout <- c(
#   area(t = 2, l = 1, b = 5, r = 4),
#   area(t = 1, l = 3, b = 3, r = 5),
#     area(t = 3, l = 2, b = 6, r = 7)
# )
# layout <- c(
#  area(1,1,2,2),#1
#   area(1,3,2,4),#2
#  area(1,5,2,6),#3
#  area(1,7,2,8) ,#4
#  area(3,2,4,3),#5
#  area(3,4,4,5) ,#6
#  area(3,6,4,7),#7
#  area(3,8,4,9),#8
#  area(1+4,1,2+4,2),#1
#  area(1+4,3,2+4,4),#2
#  area(1+4,5,2+4,6),#3
#  area(1+4,7,2+4,8) ,#4
#  area(3+4,2,4+4,3),#5
#  area(3+4,4,4+4,5) ,#6
#  area(3+4,6,4+4,7),#7
#  area(3+4,8,4+4,9) #8
# )
## create a grid exa
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
# p1 + p2+p3 +p4+p5+p6+p7+p8+p1 + p2+p3 +p4+p5+p6+p7+p8+
#   p1 + p2+p3 +p4+p5+p6+p7+p8+p1 + p2+p3 +p4+p5+p6+p7+p8+
#   plot_layout(design = layout,widths = 1,heights = 1)
# 
# mult2<-1
# df_unit<-data.frame(x=c(0,0,1,1)*mult2,y=c(0,1,0,1)*mult2)
# p0<-ggplot(df_unit, aes(x = x, y = y)) + geom_blank()+coord_cartesian(expand = F)
# 
# p0+inset_element(p2+theme(plot.background = element_rect(color = "black")),left=0,bottom=0,right=0.25,top=0.25)+
#         inset_element(p3+theme(plot.background = element_rect(color = "black")),left=0.25,bottom=0,right=0.5,top=0.25)


## Proviamo a generare i plottini e poi fare l'inset sul grafico fel primo piano fattoriale
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
  geom_point(inherit.aes = F, data=hexa_tibb,aes(x=coor_x,y=coor_y),color="black")+
  coord_fixed(ratio=1)+coord_cartesian(xlim = RX,
                                       ylim = RY,expand = F)
base_plot<-ggplot(firstplane,aes(x=X,y=Y))+
  geom_blank()+theme_void()+
  coord_fixed(ratio=1)+coord_cartesian(xlim = RX,
                                       ylim = RY,expand = F)

## prepariamo le coordinate relative al plot per le celle esagonali
hexa_coord<-hexa_tibb[,1:3]
hexa_coord$nx<-(hexa_coord$coor_x-min(RX))/diff(RX)
hexa_coord$ny<-(hexa_coord$coor_y-min(RY))/diff(RY)
hexa_coord$nx_min<-(hexa_coord$coor_x-min(RX))/diff(RX)-DX/diff(RX)
hexa_coord$nx_max<-(hexa_coord$coor_x-min(RX))/diff(RX)+DX/diff(RX)
hexa_coord$ny_min<-(hexa_coord$coor_y-min(RY))/diff(RY)-DY/diff(RY)
hexa_coord$ny_max<-(hexa_coord$coor_y-min(RY))/diff(RY)+DY/diff(RY)

# ID_n<-10
# l=hexa_coord$nx_min[ID_n]
# b=hexa_coord$ny_min[ID_n]
# r=hexa_coord$nx_max[ID_n]
# t=hexa_coord$ny_max[ID_n]
# cp=c((b+t)*0.5,(l+r)*0.5)
 exp_fact=1.3
# nl=(l-cp[2])*exp_fact+cp[2]
# nb=(b-cp[1])*exp_fact+cp[1]
# nr=(r-cp[2])*exp_fact+cp[2]
# nt=(t-cp[1])*exp_fact+cp[1]
plotsk<-F
str<-""
for(i in 1:nrow(hexa_coord)){

  str<-paste0(str,"inset_element(")
  str<-paste0(str,"Performance_plot(hexa_tibb[,4:15],selected = ",i,",labels=labels,TITLE=F,notick=T,alpha=0.75,")
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
str<-paste0("ppp<-p0+",str)

eval(parse(text=str))### ora in ppp c'
### save the file in a background image
ggsave(filename = "map_iris.png",
               plot = ppp,
                width = 18.5/2, 
                  height = (9-0.5)/2,
                 dpi=150#,bg='transparent'
       )

browser()

img <- png::readPNG("map_iris.png")
background_image <- function(raster.img){
  annotation_raster(raster.img,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf)
}
#ppp+inset_element(p0+geom_point(color="black",size=4)+geom_hline(yintercept=0)+geom_vline(xintercept=0)+
      
ppp2<-base_plot+background_image(img)+
  geom_point(color="black",size=4)+geom_hline(yintercept=0)+geom_vline(xintercept=0)
#ggplot(iris, aes(Species, Sepal.Length))+
  background_image(img)#+
#  geom_boxplot(aes(fill = Species), color = "white")+
#  fill_palette("jco")


  
# p0+  
#   inset_element(p2,
#                 left=  hexa_coord$nx_min[ID_n]*exp_fact-hexa_coord$nx[ID_n]*(exp_fact-1),
#                 bottom=hexa_coord$ny_min[ID_n]*exp_fact-hexa_coord$ny[ID_n]*(exp_fact-1),
#                 right= hexa_coord$nx_max[ID_n]*exp_fact-hexa_coord$nx[ID_n]*(exp_fact-1),
#                 top=   hexa_coord$ny_max[ID_n]*exp_fact-hexa_coord$ny[ID_n]*(exp_fact-1))+
#   inset_element(p3,
#                 left=  hexa_coord$nx_min[ID_n+1]*exp_fact-hexa_coord$nx[ID_n+1]*(exp_fact-1),
#                 bottom=hexa_coord$ny_min[ID_n+1]*exp_fact-hexa_coord$ny[ID_n+1]*(exp_fact-1),
#                 right= hexa_coord$nx_max[ID_n+1]*exp_fact-hexa_coord$nx[ID_n+1]*(exp_fact-1),
#                 top=   hexa_coord$ny_max[ID_n+1]*exp_fact-hexa_coord$ny[ID_n+1]*(exp_fact-1))+
#   inset_element(p3,
#                 left=  hexa_coord$nx_min[ID_n+18]*exp_fact-hexa_coord$nx[ID_n+18]*(exp_fact-1),
#                 bottom=hexa_coord$ny_min[ID_n+18]*exp_fact-hexa_coord$ny[ID_n+18]*(exp_fact-1),
#                 right= hexa_coord$nx_max[ID_n+18]*exp_fact-hexa_coord$nx[ID_n+18]*(exp_fact-1),
#                 top=   hexa_coord$ny_max[ID_n+18]*exp_fact-hexa_coord$ny[ID_n+18]*(exp_fact-1))
# # +
#      inset_element(p0+geom_point(color="red")+geom_hline(yintercept=0)+geom_vline(xintercept=0)+
#                    theme_void(),left = 0,bottom = 0,right = 1,top = 1)



# 
# plot(New_qua[1,1:(quantiles+1)],type="l")
# lines(New_qua[22,1:(quantiles+1)],col="blue")
# lines(New_qua[42,1:(quantiles+1)],col="red")
# lines(New_qua[64,1:(quantiles+1)],col="green")
# lines(New_qua[84,1:(quantiles+1)],col="orange")
# 
# plot(New_qua[84,1:(quantiles+1)],type="l")
# lines(New_qua[104,1:(quantiles+1)],col="blue")
# lines(New_qua[123,1:(quantiles+1)],col="red")
# lines(New_qua[143,1:(quantiles+1)],col="green")
# lines(New_qua[162,1:(quantiles+1)],col="orange")
# approx_discrete_distr_from_quantiles<-function(x,p,dom=c(1:10)){
#   breaks_dom<-(dom[1:(length(dom)-1)]+dom[2:(length(dom))])*0.5
#   myd<-HistDAWass::distributionH(x,p)
# }

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

