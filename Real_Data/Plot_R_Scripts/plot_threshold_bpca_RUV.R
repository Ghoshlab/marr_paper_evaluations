setwd("../saved_objects")

load("Charmion_bpca_RUV_load.R")
load("Metabolon_bpca_RUV_load.R")
#saved_metabolon
#png("../met.png")
################################################
# Metabolites
###############################################
Fig7a<-hist(saved_charmion$rabbcca2,breaks=seq(0,100,1))
Fig7b<-hist(saved_charmion$rall3,breaks=seq(0,100,1))
Fig7c<-hist(saved_metabolon$rall3,breaks=seq(0,100,1))
plotlist<-list()
per_met1<-saved_charmion$rabbcca2
df1<-data.frame(per_met1)
#par(mfrow=c(1,3))
par(mar=c(6,5.5,2,2))
plotlist[[1]]<-ggplot(df1, aes(x=per_met1)) + geom_histogram(binwidth=1,color="black", fill="white")+
  ylab("Sample\npair")+
  xlab("a) Percent reproducible metabolites per \n sample pair for 393 pairwise subjects")+xlim(0,100)
per_met2<-saved_charmion$rall3
df2<-data.frame(per_met2)
par(mar=c(6,2,2,2))
#Fig7b<-ggplot(df2, aes(x=per_met2)) + geom_histogram(binwidth=0.10,color="black", fill="white")
plotlist[[2]]<-ggplot(df2, aes(x=per_met2)) + geom_histogram(binwidth=1,color="black", fill="white")+
  ylab("Sample\npair")+
  xlab("b) Percent reproducible metabolites per \n sample pair for 8515 pairwise subjects")+xlim(0,100)


per_met3<-saved_metabolon$rall3
df3<-data.frame(per_met3)
par(mar=c(6,2,2,2))
plotlist[[3]]<-ggplot(df3, aes(x=per_met3)) + geom_histogram(binwidth=1,color="black", fill="white")+
  ylab("Sample\npair")+
  xlab("c) Percent reproducible metabolites per \n sample pair for 637885 pairwise subjects")+xlim(0,100)


############################ XXXX ###########################
require(gridExtra)


ml2 <- marrangeGrob(plotlist, nrow = 1, ncol = 3,top=NULL)
#ml1  5.04 inches by 3.78
#############################################
# Samples
Fig6a<-hist(saved_charmion$pabbcca,breaks=seq(0,100,1))
Fig6b<-hist(saved_charmion$pall,breaks=seq(0,100,1))
Fig6c<-hist(saved_metabolon$pall,breaks=seq(0,100,1))
plotlist1<-list()
per_met11<-saved_charmion$pabbcca
df11<-data.frame(per_met11)
par(mar=c(6,5.5,2,2))
plotlist1[[1]]<-ggplot(df11, aes(x=per_met11)) + geom_histogram(color="black", fill="white")+
  ylab("Metabolite")+
  xlab("a) Percent reproducible sample pairs per \nmetabolite for 393 pairwise technical replicates")

per_met12<-saved_charmion$pall
df12<-data.frame(per_met12)
par(mar=c(6,2,2,2))
plotlist1[[2]]<-ggplot(df12, aes(x=per_met12)) + geom_histogram(binwidth=1,color="black", fill="white")+
 ylab("Metabolite")+
  xlab("b) Percent reproducible sample pairs per \nmetabolite for 8515 pairwise subjects")


per_met13<-saved_metabolon$pall
df13<-data.frame(per_met13)
par(mar=c(6,2,2,2))
plotlist1[[3]]<-ggplot(df13, aes(x=per_met13)) + geom_histogram(binwidth=1,color="black", fill="white")+
  ylab("Metabolite")+
  xlab("c) Percent reproducible sample pairs per \nmetabolite for 637885 pairwise subjects")

############################ XXXX ###########################
require(gridExtra)


ml3 <- marrangeGrob(plotlist1, nrow = 3, ncol = 1,top=NULL)
#ml1  5.04 inches by 3.78
ggsave("bpca_RUVFig11.pdf", ml3,
       width = 5.04, height = 3.78, units = "in")
################################################
ml31 <- marrangeGrob(plotlist, nrow = 3, ncol = 1,top=NULL)
#ml1  5.04 inches by 3.78
ggsave("bpca_RUVFig10.pdf", ml31,
       width = 5.04, height = 3.78, units = "in")
