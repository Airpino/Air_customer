#res comes from MFA
library(FactoMineR)
library(HistDAWass)
source("WASS_funct_discrete.R")
source("WASS_funct_regression.R")
source("WASS_principal_components_discr.R")
source("WASS_viz_discrete.R")
source("compute_moments.R")
quantiles=20
res<-WH.MultiplePCA_discr(Fin_tibble,list.of.vars = c(2:13),
                          quantiles = quantiles,ncp_in = 5)
recon<- FactoMineR::reconst(res, ncp=2)*sqrt(quantiles+1)
vars<-ncol(recon)/(quantiles+1)
seque<-rep(c(1:vars),each=21)
pp<-c(0:quantiles)/quantiles
TMP<-distributionH(x=recon[142,1:21],p=pp)
scale<-seq(0.5,10,by=0.5)
# ora stimiamo le probabilità ai valori della scala
p2<-numeric()
for(i in scale){
  p2<-c(p2,compP(TMP,i))
}
tmp2<-cbind(scale,p2,c(0,diff(p2)))
scale2<-seq(1,10)
p3<-numeric()
for(i in scale2){
  if(i==min(scale2)){ 
    p3<-c(p3,sum(tmp2[1:3,3]))
  }else{
    if (i==max(scale2)){
      p3<-c(p3,tmp2[nrow(tmp2),3])
    }else{
      p3<-c(p3,
      sum(tmp2[which((tmp2[,1]>(i-0.5))&(tmp2[,1]<=(i+0.5))),3])
      )
    }
  } 
}
discr2<-data.frame(x=scale2,freq=p3)
ggplot(discr2)+geom_bar(aes(x=x,y=freq),stat="identity")
ggplot(Fin_tibble[[2]][[142]])+geom_bar(aes(x=B1,y=freq),stat="identity")
