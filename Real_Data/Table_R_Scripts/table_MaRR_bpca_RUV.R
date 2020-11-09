setwd("../saved_objects")
load("Operator_bpca_RUV_load.R")
load("Charmion_bpca_RUV_load.R")
load("Metabolon_bpca_RUV_load.R")
#load("Charmion_random_load0419.R")
#saved_charmion
c170<- c((length(which(saved_mshalf$spikesf>70))*100)/3,
         (length(which(saved_mshalf$batchesf>70))*100)/9,
         (length(which(saved_mshalf$tspikesf>70))*100)/27,
         (length(which(saved_charmion$rall3>70))*100)/choose(131,2),
         (length(which(saved_charmion$rabbcca2>70))*100)/393,
         (length(which(saved_metabolon$rall3>70))*100)/choose(1130,2))
c180<- c((length(which(saved_mshalf$spikesf>80))*100)/3,
         (length(which(saved_mshalf$batchesf>80))*100)/9,
         (length(which(saved_mshalf$tspikesf>80))*100)/27,
         (length(which(saved_charmion$rall3>80))*100)/choose(131,2),
         (length(which(saved_charmion$rabbcca2>80))*100)/393,
         (length(which(saved_metabolon$rall3>80))*100)/choose(1130,2))

c190<- c((length(which(saved_mshalf$spikesf>90))*100)/3,
         (length(which(saved_mshalf$batchesf>90))*100)/9,
         (length(which(saved_mshalf$tspikesf>90))*100)/27,
         (length(which(saved_charmion$rall3>90))*100)/choose(131,2),
         (length(which(saved_charmion$rabbcca2>90))*100)/393,
         (length(which(saved_metabolon$rall3>90))*100)/choose(1130,2))


         c470<- c((length(which(saved_mshalf$pall>70))*100)/860,
                  (length(which(saved_mshalf$pallm>70))*100)/860,
                  (length(which(saved_mshalf$pallb>70))*100)/860,
                  (length(which(saved_charmion$pall>70))*100)/2860,
                  (length(which(saved_charmion$pabbcca>70))*100)/2860,
                  (length(which(saved_metabolon$pall>70))*100)/995)

         c480<- c((length(which(saved_mshalf$pall>80))*100)/860,
                  (length(which(saved_mshalf$pallm>80))*100)/860,
                  (length(which(saved_mshalf$pallb>80))*100)/860,
                  (length(which(saved_charmion$pall>80))*100)/2860,
                  (length(which(saved_charmion$pabbcca>80))*100)/2860,
                  (length(which(saved_metabolon$pall>80))*100)/995)

         c490<- c((length(which(saved_mshalf$pall>90))*100)/860,
                  (length(which(saved_mshalf$pallm>90))*100)/860,
                  (length(which(saved_mshalf$pallb>90))*100)/860,
                  (length(which(saved_charmion$pall>90))*100)/2860,
                  (length(which(saved_charmion$pabbcca>90))*100)/2860,
                  (length(which(saved_metabolon$pall>90))*100)/995)

 library(xtable)
 xtable(data.frame(c170,c180,c190,c470,c480,c490))
