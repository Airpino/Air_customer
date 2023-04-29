### discretizing and visualizing through performance plot distributional data
library(HistDAWass)
library(tidyverse)
library(Rfast)
source("R_code/WASS_funct_discrete.R")
source("R_code/WASS_funct_regression.R")
source("R_code/WASS_principal_components_discr.R")
source("R_code/WASS_viz_discrete.R")
source("R_code/compute_moments.R")
# --------------------
# plot(DATA,type="DENS")
# nr<-get.MatH.nrows(DATA)
# nc<-get.MatH.ncols(DATA)
# min_DATA<-colMins(get.MatH.stats(DATA,stat="min")$mat)
# max_DATA<-colMaxs(get.MatH.stats(DATA,stat="max")$mat)
# #create a tibble
# my_l<-list()
# for (j in 1:nc){
#   tmp<-list()
#   for (i in 1:nr){
#     tmp[[i]]<-data.frame(x=DATA@M[i,j][[1]]@x,cdf=DATA@M[i,j][[1]]@p)
#     
#   }
#   my_l[[colnames(DATA@M)[j]]]<-tmp
# }
# 
# tibb<-as_tibble(my_l)
# tibb2<-tibb
# # transform into a discretize
# ls<-10
# dom<-c(1:ls)
# for(j in 1:nc){
#   lims<-seq(min_DATA[j],max_DATA[j],length.out=2*ls-1)
#  
#  lims2<-lims[c(which((1:(2*ls-1))%%2==0),2*ls-1)]
#  
#   for(i in 1:nr){
#     tmp_c<-sapply(lims2,FUN = function(x){compP(DATA@M[i,j][[1]],x)})
#     tmp_d<-c(tmp_c[1],diff(tmp_c))
#     tibb2[[j]][[i]]<-data.frame(x=dom,freq=tmp_d,cdf=cumsum(tmp_d),true_x=lims2)%>% filter(freq>0)
#     
#   }
# }
# tibb2$ID_N<-get.MatH.rownames(DATA)
# transform a MatH into a tibble with discrete values
## -----------

cont_to_distr_tibble<-function(DATA,ls=10,both_tib=F){
  nr<-get.MatH.nrows(DATA)
  nc<-get.MatH.ncols(DATA)
  min_DATA<-Rfast::colMins(get.MatH.stats(DATA,stat="min")$mat,value=T)
  max_DATA<-Rfast::colMaxs(get.MatH.stats(DATA,stat="max")$mat,value=T)
  #create a tibble
  my_l<-list()
  my_l2<-list()
  
  dom<-c(1:ls)
  for (j in 1:nc){
    tmp<-list()
    tmp2<-list()
    lims<-seq(min_DATA[j],max_DATA[j],length.out=2*ls-1)
    lims2<-lims[c(which((1:(2*ls-1))%%2==0),2*ls-1)]
    
    for (i in 1:nr){
      tmp[[i]]<-data.frame(x=DATA@M[i,j][[1]]@x,cdf=DATA@M[i,j][[1]]@p)
      
      tmp_c<-sapply(lims2,FUN = function(x){compP(DATA@M[i,j][[1]],x)})
      
      tmp_d<-c(tmp_c[1],diff(tmp_c))
      tmp_e<-data.frame(x=dom,freq=tmp_d,cdf=cumsum(tmp_d),true_x=lims2)#%>% filter(freq>0)
      tmp2[[i]]<-tmp_e[tmp_e$freq>0,]
    }
    my_l[[colnames(DATA@M)[j]]]<-tmp
    my_l2[[colnames(DATA@M)[j]]]<-tmp2
  }
  
  tibb<-as_tibble(my_l)
  tibb2<-as_tibble(my_l2)
 
  tibb2$ID_N<-get.MatH.rownames(DATA)
  resu<-list()
  resu$fin_tib<-tibb2
  if(both_tib){  
    resu$cont_tib<-tibb
  }
  return(resu)
}
DATA<-China_Seas
# DATA<-China_Month[,13:24]
# DATA<-BLOOD

resu<-cont_to_distr_tibble(DATA,both_tib = T)


# 
# Discretize_a_distr(df)
# Performance_plot_MUL(tibb2[,1:56],selected=c(1:7),labels =tibb2$ID_N[1:7])
# vsel<-c(5:16,49:56)
# p1<-Performance_plot_MUL(tibb2[,vsel],selected=c(1:7),labels =tibb2$ID_N[1:7])
# p2<-Performance_plot_MUL(tibb2[,vsel],selected=c(8:14),labels =tibb2$ID_N[8:14])
# p3<-Performance_plot_MUL(tibb2[,vsel],selected=c(15:21),labels =tibb2$ID_N[15:21])
# p4<-Performance_plot_MUL(tibb2[,vsel],selected=c(22:28),labels =tibb2$ID_N[22:28])
# p5<-Performance_plot_MUL(tibb2[,vsel],selected=c(29:35),labels =tibb2$ID_N[29:35])
# p6<-Performance_plot_MUL(tibb2[,vsel],selected=c(36:42),labels =tibb2$ID_N[36:42])
# p7<-Performance_plot_MUL(tibb2[,vsel],selected=c(43:49),labels =tibb2$ID_N[43:49])
# p8<-Performance_plot_MUL(tibb2[,vsel],selected=c(50:56),labels =tibb2$ID_N[50:56])
# p9<-Performance_plot_MUL(tibb2[,vsel],selected=c(57:60),labels =tibb2$ID_N[57:60])
# show(p1|p2|p3|p4|p5|p6|p7|p8|p9)

# let's try to create a heatmap using 10 levels

hcrow<-WH_hclust(DATA)
hccol<-hclust(as.dist(sqrt(1 - round(WH.correlation(DATA)^2,5))))

heatmap_distrib_stack<-function(TIB,
                                labels=as.character(c(1:nrow(TIB))),
                                xcomp=0,ycomp=0,#compressing rows
                                hcrow=F,hccol=F,#reorder rows or columns
                                resHrow=NULL, resHcol=NULL, #get results
                                ratio=c(4,5)){
  if(hcrow){
    TIB<-TIB[resHrow$order,]
    
  }
  if(hccol){
    TIB<-TIB[,resHcol$order]
    
  }
  
  nr<-nrow(TIB)
  nc<-ncol(TIB)
  col_name<-colnames(TIB)
  color_map<-c("0" = "#A50026", "1" = "#A50026", "2" = "#D73027", "3" = "#F46D43",
               "4" = "#FDAE61", "5" = "#FEE08B", "6" = "#D9EF8B", "7" = "#66BD63",
               "8" = "#1A9850", "9" = "#006837", "10" = "#003300")
  
  xmin<-xmax<-ymin<-ymax<-fill_co<-numeric()
  
  for(i in 1:nr){
    for(j in 1:nc){
      tmp<-TIB[[j]][[i]]
      x_m<-(unique(c(0,tmp$cdf))-0.5)*(1-xcomp)+0.5+j
      fill_m<-c(tmp[1,1],tmp[,1])
      xmin<-c(xmin,c(x_m[1:(length(x_m)-1)]))
      xmax<-c(xmax,c(x_m[2:length(x_m)]))
      ymin<-c(ymin,rep(i-0.5+ycomp/2,(length(x_m)-1)))
      ymax<-c(ymax,rep(i+0.5-ycomp/2,(length(x_m)-1)))
      fill_co<-c(fill_co,tmp[,1])
      
    }
  }

  DF<-data.frame(xmin=xmin,xmax=xmax,
                 ymin=ymin,ymax=ymax,
                 fill_co=fill_co)
  
  p<-ggplot(DF)+geom_rect(aes(xmin=xmin,xmax=xmax,
                              ymin=ymin,ymax=ymax,
                              fill=as.factor(fill_co)))+
    scale_fill_manual(
      values=color_map#colorspace::diverge_hsv(10)
    )
  ddata1 <- dendro_data(resHrow, type = "rectangle")
  p1 <- ggplot(segment(ddata1)) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
    coord_flip() + 
    scale_y_reverse(expand = c(0, 0))+theme_dendro()+
    #theme(plot.margin = margin(0,0,0,0,"cm"))+
    theme_bw()
   ddata2 <- dendro_data(resHcol, type = "rectangle")
  # p2 <- ggplot(segment(ddata2)) +scale_y_continuous(expand = c(0, 0))+ 
  #   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + theme_dendro()+
  #   theme(plot.margin = margin(0,0,0,0,"cm"))
   if(hcrow){
     rhori=nc/ratio[1]
     tmp<-segment(ddata1)
     tmpy<-c(tmp$y,tmp$yend)
    #browser()
  p<-p+
    geom_segment(inherit.aes = F,data=tmp,aes(y = x, 
                                              x = -(y-min(tmpy))/diff(range(tmpy))*rhori+0.9, 
                                              yend = xend, 
                                              xend = -(yend-min(tmpy))/diff(range(tmpy))*rhori+0.9))
   }
   
   if(hcrow){ 
     rvert=nr/ratio[2]
     tmp2<-segment(ddata2)
     tmpy2<-c(tmp2$y,tmp2$yend)
  p<-p+geom_segment(inherit.aes = F,
                    data=tmp2,
                    aes(x = x+0.5, 
                        y = (y-min(tmpy2))/diff(range(tmpy2))*rvert+(nr+1),
                        xend = xend+0.5, 
                        yend = (yend-min(tmpy2))/diff(range(tmpy2))*rvert+(nr+1)))
   }
  # browser()
  # pfin<-#plot_spacer()+p2+
  #   p1+p+plot_layout(widths = c(1, 4))
  #   #plot_layout(heights = c(1, 5), widths = c(1, 4))
  return(p)
}
tmpDF<-heatmap_distrib_stack(resu$fin_tib[,1:(ncol(resu$fin_tib)-1)],
                             xcomp=0.05,ycomp=0.2,
                             hcrow = T,hccol = T,
                             resHrow = hcrow,resHcol = hccol,ratio = c(6,8)
                              )
# color_map<-c("0" = "#A50026", "1" = "#A50026", "2" = "#D73027", "3" = "#F46D43",
#              "4" = "#FDAE61", "5" = "#FEE08B", "6" = "#D9EF8B", "7" = "#66BD63",
#              "8" = "#1A9850", "9" = "#006837", "10" = "#003300")
# ggplot(tmpDF)+geom_rect(aes(xmin=xmin,xmax=xmax,
#                             ymin=ymin,ymax=ymax,
#                             fill=as.factor(fill_co)))+
#   scale_fill_manual(
#     values=colorspace::diverge_hsv(10)
#   )

# rows_to_plot <-hcrow$order[c(1:15)]
# p1<-Performance_plot_MUL(resu$fin_tib[,1:12],selected=c(rows_to_plot),
#                      labels =resu$fin_tib$ID_N[rows_to_plot])+
#   theme(strip.text.y = element_text(angle = 0))
# rows_to_plot <-hcrow$order[c(15:30)]
# p2<-Performance_plot_MUL(resu$fin_tib[,1:12],selected=c(rows_to_plot),
#                          labels =resu$fin_tib$ID_N[rows_to_plot])+
#   theme(strip.text.y = element_text(angle = 0))
# rows_to_plot <-hcrow$order[c(31:45)]
# p3<-Performance_plot_MUL(resu$fin_tib[,1:12],selected=c(rows_to_plot),
#                          labels =resu$fin_tib$ID_N[rows_to_plot])+
#   theme(strip.text.y = element_text(angle = 0))
# rows_to_plot <-hcrow$order[c(46:60)]
# p4<-Performance_plot_MUL(resu$fin_tib[,1:12],selected=c(rows_to_plot),
#                          labels =resu$fin_tib$ID_N[rows_to_plot])+
#   theme(strip.text.y = element_text(angle = 0))
# p1|p2|p3|p4
