source("../src/source_MaRR.R")
library(MSPrep)
library(tidyverse)

# Load example quantification dataset and view
data(msquant)
as_data_frame(msquant)
names(msquant)
#subject2<-data.matrix(data.frame(msquant[,c(1,2,30:56)]))
######
subject2<-data.matrix(data.frame(msquant[,c(30:56)]))
#subject1<-data.matrix(data.frame(msquant[,c(1,2,3:29)]))

raw.dat <- data.frame(subject2)


for(i in 1:27)
{
  raw.dat[,i]=raw.dat[,i]
}

raw.dat[raw.dat == 1] <- NA



int.mat <- data.matrix(raw.dat)
minConc <- min(int.mat[int.mat>0], na.rm=T)/2;
good.inx <- apply(is.na(int.mat), 1, sum)/ncol(int.mat)<0.2
init.mat <- as.data.frame(int.mat[good.inx,]);

#raw.data<- init.mat
#### Prototype: change it to operator, replicates


raw.data<- data.frame(init.mat)
#raw.data[,394]<-array(0,dim=c(2999))
raw.data<- data.frame(init.mat)


raw.data[1:5,1:5]
dim(raw.data)
rownames(raw.data)=paste0("metname", seq(1,dim(raw.data)[1]))
#new_names <- vector(mode = "character", length = 401)
colnames(raw.data)=paste0("X", seq(1,dim(raw.data)[2]))
#rownames(operator) <- operator$X
#operator$X <- NULL
library(rlang)
library(tidyverse)
##operator <- as.data.frame(t(as.matrix(operator)))
data.table::setDT(raw.data, keep.rownames = TRUE)

# operator pipeline
#library(rlang)
#library(tidyverse)
tidied_operator <- ms_tidy(raw.data, met_id = "rn",
                           col_names = "subject_id")

summarized_operator = ms_summarize(tidied_operator, met_id = "rn",
                                   cvmax = 0.50, min_proportion_present = 1/3,
                                   missing_val = 1)
library(rlang)
library(tidyverse)
filtered_operator = ms_filter(summarized_operator, filter_percent=0.8)
#filtered_operator<- summarized_operator
options(warn=0)
#library(pcaMethods)
imputed_operator = ms_impute(filtered_operator, imputeMethod ="bpca",n_pcs =5)

normalized_operator = ms_normalize(imputed_operator, normalizeMethod ="median",
                                   controls = NULL,  n_control = 5, transform = "log2")

returned_operator <- ms_return(normalized_operator)
dim(returned_operator$data)
print(Sys.time())
newdata<- data.frame(returned_operator$data)
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

raw28<-array(0,dim=c(860))
raw.dat3<-cbind(raw.dat3,raw28)
dim(raw.dat3)


#######
c0<-array(0,dim=c(860))
# Technical replicate: A (Batch 01)
datA.rank1x01 <- rank(-apply(raw.dat3[,c(1,28)],1,sum),ties.method="average")
datA.rank2x01 <- rank(-apply(raw.dat3[,c(2,28)],1,sum),ties.method="average")
datA.rank4x01 <- rank(-apply(raw.dat3[,c(3,28)],1,sum),ties.method="average")

dat.max.rankA12x01 <- apply(cbind(datA.rank1x01,datA.rank2x01),1,max)
res.marA12x01 <- MaRR(dat.max.rankA12x01,alpha=.01)
k1x01ab=length(res.marA12x01[[5]])

dat.max.rankA14x01 <- apply(cbind(datA.rank1x01,datA.rank4x01),1,max)
res.marA14x01 <- MaRR(dat.max.rankA14x01,alpha=.01)
k1x01ac=length(res.marA14x01[[5]])

dat.max.rankA24x01 <- apply(cbind(datA.rank2x01,datA.rank4x01),1,max)
res.marA24x01 <- MaRR(dat.max.rankA24x01,alpha=.01)
k1x01bc=length(res.marA24x01[[5]])
# Technical replicate: B

datB.rank1x01 <- rank(-apply(raw.dat3[,c(4,28)],1,sum),ties.method="average")
datB.rank2x01 <- rank(-apply(raw.dat3[,c(5,28)],1,sum),ties.method="average")
datB.rank4x01 <- rank(-apply(raw.dat3[,c(6,28)],1,sum),ties.method="average")

dat.max.rankB12x01 <- apply(cbind(datB.rank1x01,datB.rank2x01),1,max)
res.marB12x01 <- MaRR(dat.max.rankB12x01,alpha=.01)
k1x02ab=length(res.marB12x01[[5]])

dat.max.rankB14x01 <- apply(cbind(datB.rank1x01,datB.rank4x01),1,max)
res.marB14x01 <- MaRR(dat.max.rankB14x01,alpha=.01)
k1x02ac=length(res.marB14x01[[5]])

dat.max.rankB24x01 <- apply(cbind(datB.rank2x01,datB.rank4x01),1,max)
res.marB24x01 <- MaRR(dat.max.rankB24x01,alpha=.01)
k1x02bc=length(res.marB24x01[[5]])

# Technical replicate: C
datC.rank1x01 <- rank(-apply(raw.dat3[,c(7,28)],1,sum),ties.method="average")
datC.rank2x01 <- rank(-apply(raw.dat3[,c(8,28)],1,sum),ties.method="average")
datC.rank4x01 <- rank(-apply(raw.dat3[,c(9,28)],1,sum),ties.method="average")

dat.max.rankC12x01 <- apply(cbind(datC.rank1x01,datC.rank2x01),1,max)
res.marC12x01 <- MaRR(dat.max.rankC12x01,alpha=.01)
k1x03ab=length(res.marC12x01[[5]])

dat.max.rankC14x01 <- apply(cbind(datC.rank1x01,datC.rank4x01),1,max)
res.marC14x01 <- MaRR(dat.max.rankC14x01,alpha=.01)
k1x03ac=length(res.marC14x01[[5]])

dat.max.rankC24x01 <- apply(cbind(datC.rank2x01,datC.rank4x01),1,max)
res.marC24x01 <- MaRR(dat.max.rankC24x01,alpha=.01)
k1x03bc=length(res.marC24x01[[5]])
# Technical replicate: A (Batch 02)
datA.rank1x02 <- rank(-apply(raw.dat3[,c(10,28)],1,sum),ties.method="average")
datA.rank2x02 <- rank(-apply(raw.dat3[,c(11,28)],1,sum),ties.method="average")
datA.rank4x02 <- rank(-apply(raw.dat3[,c(12,28)],1,sum),ties.method="average")

dat.max.rankA12x02 <- apply(cbind(datA.rank1x02,datA.rank2x02),1,max)
res.marA12x02 <- MaRR(dat.max.rankA12x02,alpha=.01)
k2x01ab=length(res.marA12x02[[5]])

dat.max.rankA14x02 <- apply(cbind(datA.rank1x02,datA.rank4x02),1,max)
res.marA14x02 <- MaRR(dat.max.rankA14x02,alpha=.01)
k2x01ac=length(res.marA14x02[[5]])

dat.max.rankA24x02 <- apply(cbind(datA.rank2x02,datA.rank4x02),1,max)
res.marA24x02 <- MaRR(dat.max.rankA24x02,alpha=.01)
k2x01bc=length(res.marA24x02[[5]])

# Technical replicate: B

datB.rank1x02 <- rank(-apply(raw.dat3[,c(13,28)],1,sum),ties.method="average")
datB.rank2x02 <- rank(-apply(raw.dat3[,c(14,28)],1,sum),ties.method="average")
datB.rank4x02 <- rank(-apply(raw.dat3[,c(15,28)],1,sum),ties.method="average")

dat.max.rankB12x02 <- apply(cbind(datB.rank1x02,datB.rank2x02),1,max)
res.marB12x02 <- MaRR(dat.max.rankB12x02,alpha=.01)
k2x02ab=length(res.marB12x02[[5]])

dat.max.rankB14x02 <- apply(cbind(datB.rank1x02,datB.rank4x02),1,max)
res.marB14x02 <- MaRR(dat.max.rankB14x02,alpha=.01)
k2x02ac=length(res.marB14x02[[5]])

dat.max.rankB24x02 <- apply(cbind(datB.rank2x02,datB.rank4x02),1,max)
res.marB24x02 <- MaRR(dat.max.rankB24x02,alpha=.01)
k2x02bc=length(res.marB24x02[[5]])

# Technical replicate: C
datC.rank1x02 <- rank(-apply(raw.dat3[,c(16,28)],1,sum),ties.method="average")
datC.rank2x02 <- rank(-apply(raw.dat3[,c(17,28)],1,sum),ties.method="average")
datC.rank4x02 <- rank(-apply(raw.dat3[,c(18,28)],1,sum),ties.method="average")


dat.max.rankC12x02 <- apply(cbind(datC.rank1x02,datC.rank2x02),1,max)
res.marC12x02 <- MaRR(dat.max.rankC12x02,alpha=.01)
k2x03ab=length(res.marC12x02[[5]])

dat.max.rankC14x02 <- apply(cbind(datC.rank1x02,datC.rank4x02),1,max)
res.marC14x02 <- MaRR(dat.max.rankC14x02,alpha=.01)
k2x03ac=length(res.marC14x02[[5]])

dat.max.rankC24x02 <- apply(cbind(datC.rank2x02,datC.rank4x02),1,max)
res.marC24x02 <- MaRR(dat.max.rankC24x02,alpha=.01)
k2x03bc=length(res.marC24x02[[5]])
# Technical replicate: A (Batch 03)
datA.rank1x03 <- rank(-apply(raw.dat3[,c(19,28)],1,sum),ties.method="average")
datA.rank2x03 <- rank(-apply(raw.dat3[,c(20,28)],1,sum),ties.method="average")
datA.rank4x03 <- rank(-apply(raw.dat3[,c(21,28)],1,sum),ties.method="average")

dat.max.rankA12x03 <- apply(cbind(datA.rank1x03,datA.rank2x03),1,max)
res.marA12x03 <- MaRR(dat.max.rankA12x03,alpha=.01)
k4x01ab=length(res.marA12x03[[5]])

dat.max.rankA14x03 <- apply(cbind(datA.rank1x03,datA.rank4x03),1,max)
res.marA14x03 <- MaRR(dat.max.rankA14x03,alpha=.01)
k4x01ac=length(res.marA14x03[[5]])

dat.max.rankA24x03 <- apply(cbind(datA.rank2x03,datA.rank4x03),1,max)
res.marA24x03 <- MaRR(dat.max.rankA24x03,alpha=.01)
k4x01bc=length(res.marA24x03[[5]])

# Technical replicate: B

datB.rank1x03 <- rank(-apply(raw.dat3[,c(22,28)],1,sum),ties.method="average")
datB.rank2x03 <- rank(-apply(raw.dat3[,c(23,28)],1,sum),ties.method="average")
datB.rank4x03 <- rank(-apply(raw.dat3[,c(24,28)],1,sum),ties.method="average")

dat.max.rankB12x03 <- apply(cbind(datB.rank1x03,datB.rank2x03),1,max)
res.marB12x03 <- MaRR(dat.max.rankB12x03,alpha=.01)
k4x02ab=length(res.marB12x03[[5]])

dat.max.rankB14x03 <- apply(cbind(datB.rank1x03,datB.rank4x03),1,max)
res.marB14x03 <- MaRR(dat.max.rankB14x03,alpha=.01)
k4x02ac=length(res.marB14x03[[5]])

dat.max.rankB24x03 <- apply(cbind(datB.rank2x03,datB.rank4x03),1,max)
res.marB24x03 <- MaRR(dat.max.rankB24x03,alpha=.01)
k4x02bc=length(res.marB24x03[[5]])


# Technical replicate: C
datC.rank1x03 <- rank(-apply(raw.dat3[,c(25,28)],1,sum),ties.method="average")
datC.rank2x03 <- rank(-apply(raw.dat3[,c(26,28)],1,sum),ties.method="average")
datC.rank4x03 <- rank(-apply(raw.dat3[,c(27,28)],1,sum),ties.method="average")

dat.max.rankC12x03 <- apply(cbind(datC.rank1x03,datC.rank2x03),1,max)
res.marC12x03 <- MaRR(dat.max.rankC12x03,alpha=.01)
k4x03ab=length(res.marC12x03[[5]])

dat.max.rankC14x03 <- apply(cbind(datC.rank1x03,datC.rank4x03),1,max)
res.marC14x03<- MaRR(dat.max.rankC14x03,alpha=.01)
k4x03ac=length(res.marC14x03[[5]])

dat.max.rankC24x03 <- apply(cbind(datC.rank2x03,datC.rank4x03),1,max)
res.marC24x03<- MaRR(dat.max.rankC24x03,alpha=.01)
k4x03bc=length(res.marC24x03[[5]])

####  Batches 2nd layer

# Spike-in:1x
dat1x.rank01 <- rank(-apply(raw.dat3[,c(1:3)],1,sum),ties.method="average")
dat1x.rank02<- rank(-apply(raw.dat3[,c(4:6)],1,sum),ties.method="average")
dat1x.rank03 <- rank(-apply(raw.dat3[,c(7:9)],1,sum),ties.method="average")

dat.max.rank1x12 <- apply(cbind(dat1x.rank01,dat1x.rank02),1,max)
res.mar1x12 <- MaRR(dat.max.rank1x12,alpha=.01)
k1x12=length(res.mar1x12[[5]])
k1x12
dat.max.rank1x13 <- apply(cbind(dat1x.rank01,dat1x.rank03),1,max)
res.mar1x13<- MaRR(dat.max.rank1x13,alpha=.01)
k1x13=length(res.mar1x13[[5]])
k1x13
dat.max.rank1x23 <- apply(cbind(dat1x.rank02,dat1x.rank03),1,max)
res.mar1x23 <- MaRR(dat.max.rank1x23,alpha=.01)
k1x23=length(res.mar1x23[[5]])
k1x23
# Spike-in: 2x
dat2x.rank01 <- rank(-apply(raw.dat3[,c(10:12)],1,sum),ties.method="average")
dat2x.rank02<- rank(-apply(raw.dat3[,c(13:15)],1,sum),ties.method="average")
dat2x.rank03 <- rank(-apply(raw.dat3[,c(16:18)],1,sum),ties.method="average")

dat.max.rank2x12 <- apply(cbind(dat2x.rank01,dat2x.rank02),1,max)
res.mar2x12 <- MaRR(dat.max.rank2x12,alpha=.01)
k2x12=length(res.mar2x12[[5]])
k2x12
dat.max.rank2x13 <- apply(cbind(dat2x.rank01,dat2x.rank03),1,max)
res.mar2x13<- MaRR(dat.max.rank2x13,alpha=.01)
k2x13=length(res.mar2x13[[5]])
k2x13
dat.max.rank2x23 <- apply(cbind(dat2x.rank02,dat2x.rank03),1,max)
res.mar2x23 <- MaRR(dat.max.rank2x23,alpha=.01)
k2x23=length(res.mar2x23[[5]])
k2x23
# Spike-in: 3x
dat3x.rank01 <- rank(-apply(raw.dat3[,c(19:21)],1,sum),ties.method="average")
dat3x.rank02<- rank(-apply(raw.dat3[,c(22:24)],1,sum),ties.method="average")
dat3x.rank03 <- rank(-apply(raw.dat3[,c(25:27)],1,sum),ties.method="average")

dat.max.rank3x12 <- apply(cbind(dat3x.rank01,dat3x.rank02),1,max)
res.mar3x12 <- MaRR(dat.max.rank3x12,alpha=.01)
k4x12=length(res.mar3x12[[5]])
k4x12
dat.max.rank3x13 <- apply(cbind(dat3x.rank01,dat3x.rank03),1,max)
res.mar3x13<- MaRR(dat.max.rank3x13,alpha=.01)
k4x13=length(res.mar3x13[[5]])
k4x13
dat.max.rank3x23 <- apply(cbind(dat3x.rank02,dat3x.rank03),1,max)
res.mar3x23 <- MaRR(dat.max.rank3x23,alpha=.01)
k4x23=length(res.mar3x23[[5]])
k4x23
# 3 spike ins  (Top layer)
datrank1x <- rank(-apply(raw.dat3[,c(1:9)],1,sum),ties.method="average")
datrank2x<- rank(-apply(raw.dat3[,c(10:18)],1,sum),ties.method="average")
datrank4x <- rank(-apply(raw.dat3[,c(19:27)],1,sum),ties.method="average")

dat.max.rank12x <- apply(cbind(datrank1x,datrank2x),1,max)
res.mar12x <- MaRR(dat.max.rank12x,alpha=.01)
k1x2x=length(res.mar12x[[5]])
k1x2x
dat.max.rank14x <- apply(cbind(datrank1x,datrank4x),1,max)
res.mar14x <- MaRR(dat.max.rank14x,alpha=.01)
k1x4x=length(res.mar14x[[5]])
k1x4x
dat.max.rank24x <- apply(cbind(datrank2x,datrank4x),1,max)
res.mar24x <- MaRR(dat.max.rank24x,alpha=.01)
k2x4x=length(res.mar24x[[5]])
k2x4x

######### MARR
#summary(raw.dat3[,3:29])
##################################################

#cor(raw.dat3[,3:29], use="pairwise.complete.obs", method="spearman")

#########  Spike in 2nd layer

# Spikein 1x ()
dat01.rank1x <- rank(-apply(raw.dat3[,c(1,4,7)],1,sum),ties.method="average")
dat01.rank2x<- rank(-apply(raw.dat3[,c(2,5,8)],1,sum),ties.method="average")
dat01.rank4x <- rank(-apply(raw.dat3[,c(3,6,9)],1,sum),ties.method="average")

dat.max.rank12x01 <- apply(cbind(dat01.rank1x,dat01.rank2x),1,max)
res.mar12x01 <- MaRR(dat.max.rank12x01,alpha=.01)
k011x2x=length(res.mar12x01[[5]])
k011x2x
dat.max.rank14x01 <- apply(cbind(dat01.rank1x,dat01.rank4x),1,max)
res.mar14x01 <- MaRR(dat.max.rank14x01,alpha=.01)
k011x4x=length(res.mar14x01[[5]])
k011x4x
dat.max.rank24x01 <- apply(cbind(dat01.rank2x,dat01.rank4x),1,max)
res.mar24x01 <- MaRR(dat.max.rank24x01,alpha=.01)
k012x4x=length(res.mar24x01[[5]])
k012x4x
# BATCH OPERATOR 02
dat02.rank1x <- rank(-apply(raw.dat3[,c(10,13,16)],1,sum),ties.method="average")
dat02.rank2x<- rank(-apply(raw.dat3[,c(11,14,17)],1,sum),ties.method="average")
dat02.rank4x <- rank(-apply(raw.dat3[,c(12,15,18)],1,sum),ties.method="average")

dat.max.rank12x02 <- apply(cbind(dat02.rank1x,dat02.rank2x),1,max)
res.mar12x02 <- MaRR(dat.max.rank12x02,alpha=.01)
k021x2x=length(res.mar12x02[[5]])
k021x2x
dat.max.rank14x02 <- apply(cbind(dat02.rank1x,dat02.rank4x),1,max)
res.mar14x02 <- MaRR(dat.max.rank14x02,alpha=.01)
k021x4x=length(res.mar14x02[[5]])
k021x4x
dat.max.rank24x02 <- apply(cbind(dat02.rank2x,dat02.rank4x),1,max)
res.mar24x02 <- MaRR(dat.max.rank24x02,alpha=.01)
k022x4x=length(res.mar24x02[[5]])
k022x4x
# BATCH OPERATOR 03
dat03.rank1x <- rank(-apply(raw.dat3[,c(19,22,25)],1,sum),ties.method="average")
dat03.rank2x<- rank(-apply(raw.dat3[,c(20,23,26)],1,sum),ties.method="average")
dat03.rank4x <- rank(-apply(raw.dat3[,c(21,24,27)],1,sum),ties.method="average")

dat.max.rank12x03 <- apply(cbind(dat03.rank1x,dat03.rank2x),1,max)
res.mar12x03 <- MaRR(dat.max.rank12x03,alpha=.01)
k031x2x=length(res.mar12x03[[5]])
k031x2x
dat.max.rank14x03 <- apply(cbind(dat03.rank1x,dat03.rank4x),1,max)
res.mar14x03 <- MaRR(dat.max.rank14x03,alpha=.01)
k031x4x=length(res.mar14x03[[5]])
k031x4x
dat.max.rank24x03 <- apply(cbind(dat03.rank2x,dat03.rank4x),1,max)
res.mar24x03 <- MaRR(dat.max.rank24x03,alpha=.01)
k032x4x=length(res.mar24x03[[5]])
k032x4x
# 3 Batches (Top Layer)
datrank01 <- rank(-apply(raw.dat3[,c(1:3,10:12,19:21)],1,sum),ties.method="average")
datrank02<- rank(-apply(raw.dat3[,c(4:6,13:15,22:24)],1,sum),ties.method="average")
datrank03 <- rank(-apply(raw.dat3[,c(7:9,16:18,25:27)],1,sum),ties.method="average")

dat.max.rank0102 <- apply(cbind(datrank01,datrank02),1,max)
res.mar0102 <- MaRR(dat.max.rank0102,alpha=.01)
k0102=length(res.mar0102[[5]])
k0102
dat.max.rank0103 <- apply(cbind(datrank01,datrank03),1,max)
res.mar0103 <- MaRR(dat.max.rank0103,alpha=.01)
k0103=length(res.mar0103[[5]])
k0103
dat.max.rank0203 <- apply(cbind(datrank02,datrank03),1,max)
res.mar0203 <- MaRR(dat.max.rank0203,alpha=.01)
k0203=length(res.mar0203[[5]])
k0203

#### 0419


datranks=array(0,dim=c(3,860))

datranks[1,]<- rank(-apply(raw.dat3[,c(1:3,10:12,19:21)],1,sum),ties.method="average")
datranks[2,]<- rank(-apply(raw.dat3[,c(4:6,13:15,22:24)],1,sum),ties.method="average")
datranks[3,]<- rank(-apply(raw.dat3[,c(7:9,16:18,25:27)],1,sum),ties.method="average")
#dat.max.rankij=res.marij=k=array(0,dim=c(3,3))
#i=1
dat.max.rankij=array(0,dim=c(3,3,860))
rall<-k<-array(0,dim=c(3,3))
for(i in 1:3)
{
  for(j in i:3)
  {

    dat.max.rankij[i,j,]<- apply(cbind(datranks[i,],datranks[j,]),1,max)

  }
  print(i)
}




countall=c(0)
##  3 pairwise subjects
options(warn = -1)
for(i in 1:3)
{
  for(j in i:3)
  {if(i < j){
    listMaRR<- MaRR(dat.max.rankij[i,j,],alpha=.01)
    rall[i,j] <- length(listMaRR[[5]])
    mall_rep_i=listMaRR[[5]]
    posall<-array(0,dim=c(860))
    posall[mall_rep_i]=rep(1,length(mall_rep_i))
    #mAB_irrep_i=setdiff(c(1:860),mAB_rep_i)
    #countbc=c(0)

    countall=countall+(posall==1)*1
  }
    #k[i,j]=MaRR(dat.max.rankij[i,j,],alpha=.01)[[2]]
  }
  print(i)
}

##########################
tspike01<-c(k1x01ab,k1x01ac,k1x01bc,k1x02ab,k1x02ac,k1x02bc,k1x03ab,k1x03ac,k1x03bc)
tspike02<-c(k2x01ab,k2x01ac,k2x01bc,k2x02ab,k2x02ac,k2x02bc,k2x03ab,k2x03ac,k2x03bc)
tspike04<-c(k4x01ab,k4x01ac,k4x01bc,k4x02ab,k4x02ac,k4x02bc,k4x03ab,k4x03ac,k4x03bc)
library(xtable)
xtable(data.frame(tspike01,tspike02,tspike04))

data.frame(tspike01,tspike02,tspike04)
hist(cbind(tspike01,tspike02,tspike04), main="27 technical replicates")
barplot(cbind(tspike01,tspike02,tspike04), main="27 technical replicates")
batch1x<-c(k1x12,k1x13,k1x23)
batch2x<-c(k2x12,k2x13,k2x23)
batch4x<-c(k4x12,k4x13,k4x23)

data.frame(batch1x,batch2x,batch4x)

hist(cbind(batch1x,batch2x,batch4x), main="9 batches (batch 2nd layer)")

barplot(cbind(batch1x,batch2x,batch4x), main="9 batches (batch 2nd layer)")
spikes<-c(k1x2x,k1x4x,k2x4x)
spikes
hist(spikes,main=" Spike in")
barplot(spikes,main="3 spike-ins-top layer")



###  Batch top layer

spike01<-c(k011x2x,k011x4x,k012x4x)
spike02<-c(k021x2x,k021x4x,k022x4x)
spike03<-c(k031x2x,k031x4x,k032x4x)
#Spike 2nd layer
data.frame(spike01,spike02,spike03)
hist(cbind(tspike01,tspike02,tspike04), main="27 technical replicates",xlab="Reproducible metabolites")
barplot(cbind(tspike01,tspike02,tspike04), main="27 technical replicates")
hist(cbind(batch1x,batch2x,batch4x), main="9 Spike in (2nd layer)",xlab="Reproducible metabolites")

barplot(cbind(batch1x,batch2x,batch4x), main="9 spikes (spike 2nd layer)")
spikes<-c(k0102,k0203,k0203)
spikes
### Most important
hist(spikes,main="3 Batches (Top layer)",xlab="Reproducible metabolites")

spikesf<-(spikes*100)/860
batchesf1<-cbind(batch1x,batch2x,batch4x)
batchesf<-(batchesf1*100)/860
tspikesf1<-cbind(tspike01,tspike02,tspike04)
tspikesf<-(tspikesf1*100)/860


datranksb=array(0,dim=c(27,860))
for(i in 1:27)
{
  datranksb[i,] <- rank(-(raw.dat3[,c(i)]),
                        ties.method="average")
  #datranks[i,]=datranksi
}
#dat.max.rankij=res.marij=k=array(0,dim=c(27,27))
#i=1
dat.max.rankijb=array(0,dim=c(27,27,860))
rallb<-kb<-array(0,dim=c(27,27))
for(i in 1:27)
{
  for(j in i:27)
  {

    dat.max.rankijb[i,j,]<- apply(cbind(datranksb[i,],datranksb[j,]),1,max)

  }
  print(i)
}




##################  % Reproducible metabolites pairwise

#raw.dat3<-new_metabolon_data

countallb=c(0)
##  27 pairwise subjects
options(warn = -1)
for(i in 1:27)
{
  for(j in i:27)
  {if(i < j){
    listMaRRb<- MaRR(dat.max.rankijb[i,j,],alpha=.01)
    rallb[i,j] <- length(listMaRRb[[5]])
    mall_rep_ib=listMaRRb[[5]]
    posallb<-array(0,dim=c(860))
    posallb[mall_rep_ib]=rep(1,length(mall_rep_ib))
    #mAB_irrep_i=setdiff(c(1:860),mAB_rep_i)
    #countbc=c(0)

    countallb=countallb+(posallb==1)*1
  }
    #k[i,j]=MaRR(dat.max.rankij[i,j,],alpha=.01)[[2]]
  }
  print(i)
}

datranksm=array(0,dim=c(9,860))

datranksm[1,]<- rank(-apply(raw.dat3[,c(1:3)],1,sum),ties.method="average")
datranksm[2,]<- rank(-apply(raw.dat3[,c(4:6)],1,sum),ties.method="average")
datranksm[3,]<- rank(-apply(raw.dat3[,c(7:9)],1,sum),ties.method="average")
datranksm[4,]<- rank(-apply(raw.dat3[,c(10:12)],1,sum),ties.method="average")
datranksm[5,]<- rank(-apply(raw.dat3[,c(13:15)],1,sum),ties.method="average")
datranksm[6,]<- rank(-apply(raw.dat3[,c(16:18)],1,sum),ties.method="average")
datranksm[7,]<- rank(-apply(raw.dat3[,c(19:21)],1,sum),ties.method="average")
datranksm[8,]<- rank(-apply(raw.dat3[,c(22:24)],1,sum),ties.method="average")
datranksm[9,]<- rank(-apply(raw.dat3[,c(25:27)],1,sum),ties.method="average")
#dat.max.rankij=res.marij=k=array(0,dim=c(3,3))

#dat.max.rankij=res.marij=k=array(0,dim=c(9,9))
#i=1
dat.max.rankijm=array(0,dim=c(9,9,860))
rallm<-km<-array(0,dim=c(9,9))
for(i in 1:9)
{
  for(j in i:9)
  {

    dat.max.rankijm[i,j,]<- apply(cbind(datranksm[i,],datranksm[j,]),1,max)

  }
  print(i)
}




##################  % Reproducible metabolites pairwise

#raw.dat3<-new_metabolon_data

countallm=c(0)
##  9 pairwise subjects
options(warn = -1)
for(i in 1:9)
{
  for(j in i:9)
  {if(i < j){
    listMaRRm<- MaRR(dat.max.rankijm[i,j,],alpha=.01)
    rallm[i,j] <- length(listMaRRm[[5]])
    mall_rep_im=listMaRRm[[5]]
    posallm<-array(0,dim=c(860))
    posallm[mall_rep_im]=rep(1,length(mall_rep_im))
    #mAB_irrep_i=setdiff(c(1:860),mAB_rep_i)
    #countbc=c(0)

    countallm=countallm+(posallm==1)*1
  }
    #k[i,j]=MaRR(dat.max.rankij[i,j,],alpha=.01)[[2]]
  }
  print(i)
}





pall<-(countall*100)/(choose(3,2))
pallm<-(countallm*100)/9
pallb<-(countallb*100)/27
saved_mshalf<-list(batchesf=batchesf,spikesf=spikesf,tspikesf=tspikesf,pall=pall,pallb=pallb,pallm=pallm)


save(saved_mshalf,file="../saved_objects/Operator_bpca_median_load.R")
par(mfrow=c(3,1))
hist(spikesf,main="3 Batches-MSPrep (Top layer)",breaks=seq(0,100,20),xlab="a) Reproducible metabolites")
hist(batchesf, main="9 Spike-MSPrep in (2nd layer)",breaks=seq(0,100,20),xlab="b) Reproducible metabolites")
hist(tspikesf, main="27 technical replicates--MSPrep (Bottom Layer)",breaks=seq(0,100,20),xlab="c) Reproducible metabolites")

