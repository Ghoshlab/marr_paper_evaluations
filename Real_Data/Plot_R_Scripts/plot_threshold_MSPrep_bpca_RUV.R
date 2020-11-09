setwd("../saved_objects")

load(file="Operator_bpca_RUV_load.R")
par(mfrow=c(3,1))
hist(spikesf,main="3 Batches-MSPrep (Top layer)",breaks=seq(0,100,20),xlab="a) Reproducible metabolites")
hist(batchesf, main="9 Spike-MSPrep in (2nd layer)",breaks=seq(0,100,20),xlab="b) Reproducible metabolites")
hist(tspikesf, main="27 technical replicates--MSPrep (Bottom Layer)",breaks=seq(0,100,20),xlab="c) Reproducible metabolites")

###############################################
Fig7a<-hist(saved_mshalf$spikesf,breaks=seq(0,100,1))
Fig7b<-hist(saved_mshalf$batchesf,breaks=seq(0,100,1))
Fig7c<-hist(saved_mshalf$tspikesf,breaks=seq(0,100,1))
plotlist<-list()
per_met1<-saved_mshalf$spikesf
df1<-data.frame(per_met1)
#par(mfrow=c(1,3))
par(mar=c(6,5.5,2,2))
plotlist[[1]]<-ggplot(df1, aes(x=per_met1)) + geom_histogram(binwidth=2,color="black", fill="white")+
  ylab("Sample\npair")+
  xlab("a) Percent reproducible metabolites per \n sample pair for 3 pairwise batches")+xlim(0,100)
per_met2<-c(saved_mshalf$batchesf)
df2<-data.frame(per_met2)
par(mar=c(6,2,2,2))
#Fig7b<-ggplot(df2, aes(x=per_met2)) + geom_histogram(binwidth=0.10,color="black", fill="white")
plotlist[[2]]<-ggplot(df2, aes(x=per_met2)) + geom_histogram(binwidth=2,color="black", fill="white")+
  ylab("Sample\npair")+
  xlab("b) Percent reproducible metabolites per \n sample pair for 9 spike ins")+xlim(0,100)


per_met3<-c(saved_mshalf$tspikesf)
df3<-data.frame(per_met3)
par(mar=c(6,2,2,2))
plotlist[[3]]<-ggplot(df3, aes(x=per_met3)) + geom_histogram(binwidth=2,color="black", fill="white")+
  ylab("Sample\npair")+
  xlab("c) Percent reproducible metabolites per \n sample pair for 27 replicates")+xlim(0,100)


############################ XXXX ###########################
require(gridExtra)


################################################
# Samples
Fig6a<-hist(saved_mshalf$pall,breaks=seq(0,100,1))

plotlist1<-list()
per_met11<-c(saved_mshalf$pall)
df11<-data.frame(per_met11)
par(mar=c(6,5.5,2,2))
plotlist1[[1]]<-ggplot(df11, aes(x=per_met11)) + geom_histogram(color="black", fill="white")+
  ylab("Metabolite")+
  xlab("a) Percent reproducible sample pairs per \nmetabolite for 3 batches")

per_met12<-c(saved_mshalf$pallm)
df12<-data.frame(per_met12)
par(mar=c(6,5.5,2,2))
plotlist1[[2]]<-ggplot(df12, aes(x=per_met12)) + geom_histogram(color="black", fill="white")+
  ylab("Metabolite")+
  xlab("b) Percent reproducible sample pairs per \nmetabolite for 9 spike-ins")

per_met13<-c(saved_mshalf$pallb)
df13<-data.frame(per_met13)
par(mar=c(6,5.5,2,2))
plotlist1[[3]]<-ggplot(df13, aes(x=per_met13)) + geom_histogram(color="black", fill="white")+
  ylab("Metabolite")+
  xlab("c) Percent reproducible sample pairs per \nmetabolite for 27 technical replicates")

############################ XXXX ###########################
require(gridExtra)


ml3 <- marrangeGrob(plotlist1, nrow = 3, ncol = 1,top=NULL)
#ml1  5.04 inches by 3.78
ggsave("bpca_RUVFig9.pdf", ml3,
       width = 5.04, height = 3.78, units = "in")
################################################
ml31 <- marrangeGrob(plotlist, nrow = 3, ncol = 1,top=NULL)
#ml1  5.04 inches by 3.78
ggsave("bpca_RUVFig8.pdf", ml31,
       width = 5.04, height = 3.78, units = "in")
