
nn<-nrow(hexa_tibb)

for(i in 1:nn){
  fname<-paste0("./Images/im_h_",i,".png")
  pp<-Performance_plot(hexa_tibb[,3:15],selected = i,labels=labels,TITLE=F)
  
  #CairoPNG(filename = fname, bg = "transparent")
  ggsave(filename = fname,
         plot = pp,
         width = 3, 
         height = 3,
         dpi=100#,bg='transparent'
  )
}