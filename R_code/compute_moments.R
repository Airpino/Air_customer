
compute_three_mom<-function(x,w){


m<-sum(x*(w/sum(w)))
s<-sqrt(sum(x^2*(w/sum(w)))-m^2)
sk<-mean(((x-m)/s)^3*(w/sum(w)))
 res<- c(m=m,s=s,sk=sk)
 return(res)
}

simulate_distributions<-function(dom,rep=1000){
  mat<-matrix(0,rep,3)
  for(i in 1:rep){
    w=round(runif(length(dom))^3,3)
    mat[i,]<-compute_three_mom(dom,w)
  }
  return(mat)
}

compute_groeneveld_skewness<-function(x,w,grain=100){
  #dd<-data.frame(x=x,freq=w/sum(w))
  epsilon<-1e-8
  vals<-seq(0.5+epsilon,1-epsilon,length.out=grain)
  med<-e1071::qdiscrete(0.5, w, values = x)
  value<-rep(NA_real_,length(vals))
  for(i in 1:length(vals)){
    twos<-e1071::qdiscrete(c(vals[i],1-vals[i]), w, values = x)
    if((twos[1]-twos[2])>0){
      value[i]<-(twos[1]+twos[2]-2*med)/(twos[1]-twos[2])
        }
    
  }
  
  DF<-data.frame(vals,value)
  return(DF)
}

# M<-simulate_distributions(c(1:10),rep=10000)
# M<-as.data.frame(M)
# p1<-ggplot(M, aes(x = V1, y = V2,fill = ..level..)) +
#   stat_density_2d(geom = "polygon")
# p2<-ggplot(M, aes(x = V2, y = V3,fill = ..level..)) +
#   stat_density_2d(geom = "polygon")
# p3<-ggplot(M, aes(x = V1, y = V3,fill = ..level..)) +
#   stat_density_2d(geom = "polygon")
# library(patchwork)
# p1+p2+p3

# x<-Fin_tibble[[2]][[1]][[1]]
# w<-Fin_tibble[[2]][[1]][["freq"]]

