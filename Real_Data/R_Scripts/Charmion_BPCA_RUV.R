
source("../src/source_MaRR.R")
library(openxlsx)
require(gdata)
library(readxl)
library(MSPrep)
dataex5<-read_xlsx("../data/COPD 131 HILIC Pos2.xlsx",sheet=1)
#dim 662, 401
dataex6<-read_xlsx("../data/COPD 131 C18 Pos2.xlsx", sheet=1)
#2337,401

dataex51<-data.matrix(dataex5[,2:394])
dataex61<-data.matrix(dataex6[,2:394])
datac51<-data.matrix(rbind(dataex51,dataex61))


raw.data<- datac51
raw.data[raw.data == 0] <- NA

int.mat <- data.matrix(raw.data)
minConc <- min(int.mat[int.mat>0], na.rm=T)/2;
good.inx <- apply(is.na(int.mat), 1, sum)/ncol(int.mat)<0.2

init.mat <- as.data.frame(int.mat[good.inx,]);
#raw.data<- init.mat

#raw.data[,394]<-array(0,dim=c(2999))
raw.data<- data.frame(init.mat)


raw.data[1:5,1:5]
dim(raw.data)
rownames(raw.data)=paste0("metname", seq(1,dim(raw.data)[1]))
#new_names <- vector(mode = "character", length = 401)
colnames(raw.data)=paste0("X", seq(1,dim(raw.data)[2]))
#rownames(replicates) <- replicates$X
#replicates$X <- NULL
library(rlang)
library(tidyverse)
##replicates <- as.data.frame(t(as.matrix(replicates)))
data.table::setDT(raw.data, keep.rownames = TRUE)

# replicates pipeline
#library(rlang)
#library(tidyverse)
tidied_replicates <- ms_tidy(raw.data, met_id = "rn",
                             col_names = "subject_id")

summarized_replicates = ms_summarize(tidied_replicates, met_id = "rn",
                                     cvmax = 0.50, min_proportion_present = 1/3,
                                     missing_val = 0)
library(rlang)
library(tidyverse)
filtered_replicates = ms_filter(summarized_replicates, filter_percent=0.8)
#filtered_replicates<- summarized_replicates
options(warn=0)
library(pcaMethods)
imputed_replicates = ms_impute(filtered_replicates, imputeMethod ="bpca", k_knn = 5,n_pcs =5)

normalized_replicates = ms_normalize(imputed_replicates, normalizeMethod ="RUV",
                                     controls = NULL,  n_control = 5, k_ruv = 5, transform = "log2")

returned_replicates <- ms_return(normalized_replicates)
dim(returned_replicates$data)
print(Sys.time())
newdata<- data.frame(returned_replicates$data)
#newdata2<- array(0,dim=c(dim(raw.data)[1],dim(raw.data)[2]))
col.order<- paste0("X", seq(1,dim(raw.data)[2]-1))
newdata2<- newdata[,c("met_id",col.order)]
newdata3<- newdata2[,-1]
dim(newdata3)
#col.order <- c("colname4","colname3","colname2","colname5","colname1")
#M[,col.order]
library(bestNormalize)
raw.dat3<- array(0,dim=c(dim(raw.data)[1],(dim(raw.data)[2]-1)))
i=3



options(warn=-1)
for(i in 1:(dim(raw.data)[2]-1))
{
  raw.dat3[,i]<- newdata3[,i]
}

raw.data<-raw.dat3
kAB<-kBC<-kCA<-array(0,dim=c(131))
i=1
datA<-datB<-datC<-datAB<-datBC<-datCA<-array(0,dim=c(131,2860))
for(i in 1:131)
{
  # Technical replicates (patient 01)
  datA[i,] <- rank(-raw.dat3[,(1+3*(i-1))],ties.method="average")
  datB[i,] <- rank(-raw.dat3[,(2+3*(i-1))],ties.method="average")
  datC[i,] <- rank(-raw.dat3[,(3+3*(i-1))],ties.method="average")

  datAB[i,] <- apply(cbind(datA[i,],datB[i,]),1,max)

  kAB[i]=MaRR(datAB[i,],alpha=.01)[[2]]
  datBC[i,] <- apply(cbind(datB[i,],datC[i,]),1,max)

  kBC[i]=MaRR(datBC[i,],alpha=.01)[[2]]

  datCA[i,] <- apply(cbind(datC[i,],datA[i,]),1,max)
  kCA[i]=MaRR(datCA[i,],alpha=.01)[[2]]
}
### Top layer (3 firstly picked patients)

# 3 spike ins  (Top layer)
datranks=array(0,dim=c(131,2860))
for(i in 1:131)
{
  datranks[i,] <- rank(-apply(raw.dat3[,c(1+3*(i-1),2+3*(i-1),3+3*(i-1))],1,sum),
                       ties.method="average")
  #datranks[i,]=datranksi
}
#dat.max.rankij=res.marij=k=array(0,dim=c(131,131))
#i=1
dat.max.rankij=array(0,dim=c(131,131,2860))
rall<-k<-array(0,dim=c(131,131))
for(i in 1:131)
{
  for(j in i:131)
  {

    dat.max.rankij[i,j,]<- apply(cbind(datranks[i,],datranks[j,]),1,max)

  }
}




##################  % Reproducible metabolites pairwise

raw.data<-raw.dat3
rAB<-rBC<-rCA<-array(0,dim=c(131))
i=1
countab=countbc=countca=c(0)
# Suppress warning messages
options(warn = -1)
datA<-datB<-datC<-datAB<-datBC<-datCA<-array(0,dim=c(131,2860))
for(i in 1:131)
{
  # Technical replicates (patient 01)
  datA[i,] <- rank(-raw.dat3[,(1+3*(i-1))],ties.method="average")
  datB[i,] <- rank(-raw.dat3[,(2+3*(i-1))],ties.method="average")
  datC[i,] <- rank(-raw.dat3[,(3+3*(i-1))],ties.method="average")

  datAB[i,] <- apply(cbind(datA[i,],datB[i,]),1,max)

  rAB[i]=length(MaRR(datAB[i,],alpha=.01)[[5]])
  mAB_rep_i=MaRR(datAB[i,],alpha=.01)[[5]]
  posab<-array(0,dim=c(2860))
  posab[mAB_rep_i]=rep(1,length(mAB_rep_i))
  #mAB_irrep_i=setdiff(c(1:2999),mAB_rep_i)
  #countab=c(0)
  countab=countab+(posab==1)*1
  datBC[i,] <- apply(cbind(datB[i,],datC[i,]),1,max)

  rBC[i]=length(MaRR(datBC[i,],alpha=.01)[[5]])
  mBC_rep_i=MaRR(datBC[i,],alpha=.01)[[5]]
  posbc<-array(0,dim=c(2860))
  posbc[mBC_rep_i]=rep(1,length(mBC_rep_i))
  #mAB_irrep_i=setdiff(c(1:2999),mAB_rep_i)
  #countbc=c(0)
  countbc=countbc+(posbc==1)*1

  datCA[i,] <- apply(cbind(datC[i,],datA[i,]),1,max)
  rCA[i]=length(MaRR(datCA[i,],alpha=.01)[[5]])
  mCA_rep_i=MaRR(datCA[i,],alpha=.01)[[5]]
  posca<-array(0,dim=c(2860))
  posca[mCA_rep_i]=rep(1,length(mCA_rep_i))
  #mAB_irrep_i=setdiff(c(1:2999),mAB_rep_i)
  #countbc=c(0)
  countca=countca+(posca==1)*1
}

### Histograms of reproducible metabolites:
hist(rAB)
summary(rAB)

hist(rBC)
summary(rBC)

hist(rCA)
summary(rCA)
hist(c(rAB,rBC,rCA))
summary(c(rAB,rBC,rCA))
pabbcca<-(countab+countbc+countca)/3.93
hist(pabbcca)
summary(pabbcca)
which(pabbcca>50)
length(which(pabbcca>50))
length(which(pabbcca>80))
countall=c(0)
##  131 pairwise subjects
options(warn = -1)
for(i in 1:131)
{
  for(j in i:131)
  {if(i < j){
    listMaRR<- MaRR(dat.max.rankij[i,j,],alpha=.01)
    rall[i,j] <- length(listMaRR[[5]])
    mall_rep_i=listMaRR[[5]]
    posall<-array(0,dim=c(2860))
    posall[mall_rep_i]=rep(1,length(mall_rep_i))
    #mAB_irrep_i=setdiff(c(1:1005),mAB_rep_i)
    #countbc=c(0)

    countall=countall+(posall==1)*1
  }
    #k[i,j]=MaRR(dat.max.rankij[i,j,],alpha=.01)[[2]]
  }
  print(i)
}

rall_new=upper.tri(rall,diag=F)
rall_new[1:3,1:3]
rall2<-rall[rall_new]
summary(rall2)
hist(rall2)
rall3<-(rall2*100)/2860
pall<-(countall*100)/(choose(131,2))
hist(pall)
summary(pall)
which(pall>50)
length(which(pall>50))
length(which(pall>80))

par(mfrow=c(1,2))
hist(rall3,xlab = "Reproducible metabolites per metabolite")
hist(rall2)
rabbcca<-c(rAB,rBC,rCA)
rabbcca2<-(rabbcca*100)/2860
par(mfrow=c(1,2))
hist(rabbcca2,xlab="a) Reproducible metabolites \nper sample",
     main="393 pairwise technical\nreplicates")

hist(pabbcca,xlab="b) Reproducible samples \nper metabolite",
     main="393 pairwise technical \nreplicates")


par(mfrow=c(1,3))
hist(rall3,xlab="a) Reproducible metabolites \nper sample pair",
     main="8515 pairwise subjects")

hist(pall,xlab="b) Reproducible samples \nper metabolite",
     main="8515 pairwise subjects")

hist(pabbcca,xlab="b) Reproducible samples \nper metabolite",
     main="393 pairwise technical \nreplicates")


mrall2<-(rall2/2860)*1004
mrall3<-rep(mrall2,74)
mrall4<-c(mrall3,rall2[1:7775])

mpall2<-(pall/8515)*637885
mpall3<-mpall2[1:1004]


par(mfrow=c(1,2))
hist(mrall4,xlab="a) Reproducible metabolites per sample",
     main="637885 pairwise subjects")

hist(mpall3,xlab="b) Reproducible samples per metabolite",
     main=" 637885 pairwise subjects")

saved_charmion<-list(rabbcca=rabbcca,rabbcca2=rabbcca2,pabbcca=pabbcca,
                     rall=rall,rall2=rall2,rall3=rall3,pall=pall)

save(saved_charmion,file="../saved_objects/Charmion_BPCA_RUV_load.R")

rm(list = ls())
