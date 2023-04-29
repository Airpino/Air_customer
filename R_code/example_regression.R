#regression try
library(tidyverse)
MM<-means_stds_NestTibb_nol(Fin_tibble[,2:15])
coeffs<-WH.regression.two.components_discr(as_tibble(Fin_tibble),
                                           Yvar = 2,
                                           Xvars = c(3:4),
                                           mY = MM$means[,2],
                                           mX=MM$means[,c(3:4)])

coeffs2<-WH.regression.two.components_discr_boot(as_tibble(Fin_tibble),
                                           Yvar = 2,
                                           Xvars = c(3:4),
                                           mY = MM$means[,2],
                                           mX=MM$means[,c(3:4)],boot_rep = 1000)
