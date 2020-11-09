source("../src/source_MaRR.R")

library(MSPrep)
library(tidyverse)
library(KEGGREST)
load(file = "../data/COPDGene_Metabolon_Metabolite_MetaData12Sep2018.Rda")

kegg_cid <- metabo[!is.na(metabo$KEGG),"KEGG"]

sum(!is.na(metabo$KEGG))


kegg_cid2 <-unlist(strsplit(kegg_cid, "[,]"))
length(kegg_cid2)



hsapath <- unique(keggLink("pathway", "hsa"))

n_pathway <- length(hsapath)

keggGet(hsapath[1])[[1]]$COMPOUND
keggGet("path:hsa00010")[[1]]$COMPOUND

#load("copd_metabolon_imputed_20Aug2018.Rda")
load("../data/COPDGene_Metabolon_P2_12Sep2018.Rda")
#nrow(metabo_t_p2)
#ncol(metabo_t_p2)
nrow(metabo_t_p2)
ncol(metabo_t_p2)
names(metabo_t_p2)[1:31]

metabo_t_p2[1,1:32]

table(metabo_t_p2$TIME_POINT)

outliers <- c("16293Z","14879T","19368T","10314D","24332F","13252W")

metabo_t_p2_v2<- metabo_t_p2[-(which(rownames(metabo_t_p2) %in% outliers)), ]
#metabo_t_p2_v2 <- metabo_t_p2[metabo_t_p2$TIME_POINT=="Visit 2" &
# (!metabo_t_p2$SID %in% outliers) ,]
#data5<-data.matrix(metabo_t_p2_v2[,31:1035])
data5<-data.matrix(metabo_t_p2_v2)
#dim(metabo_t_p2_v1)
clindat <- read.table("../data/COPDGene_subset.txt",
                      header=T, sep=" ")

nrow(clindat)
ncol(clindat)
names(clindat)
clindat[1:2,]

table(clindat$visitnum)


v2 <- clindat[clindat$visitnum==2,]

raw.data<-data.frame(t(data5))

int.mat <- data.matrix(raw.data)
minConc <- min(int.mat[int.mat>0], na.rm=T)/2;
good.inx <- apply(is.na(int.mat), 1, sum)/ncol(int.mat)<0.2

init.mat <- as.data.frame(int.mat[good.inx,]);
#raw.data[,394]<-array(0,dim=c(2999))
raw.data<- data.frame(init.mat)


raw.data[1:5,1:5]
dim(raw.data)
rownames(raw.data)=paste0("metname", seq(1,dim(raw.data)[1]))
#new_names <- vector(mode = "character", length = 401)
colnames(raw.data)=paste0("X", seq(1,dim(raw.data)[2]))

data.table::setDT(raw.data, keep.rownames = TRUE)


library(rlang)
library(tidyverse)
library(tibble)

#imputed_metabolon = ms_impute(filtered_metabolon, imputeMethod ="knn", k_knn = 10,n_pcs =5)
imputed_metabolon = readRDS("Metabolon_knn_object.rds")

normalized_metabolon = ms_normalize(imputed_metabolon, normalizeMethod ="RUV",
                                    controls = NULL,  n_comp = 2, n_control = 10)

returned_metabolon <- ms_return(normalized_metabolon)
dim(returned_metabolon$data)
newdata<- data.frame(returned_metabolon$data)
#newdata2<- array(0,dim=c(dim(raw.data)[1],dim(raw.data)[2]))
col.order<- paste0("X", seq(1,dim(raw.data)[2]-1))
newdata2<- newdata[,c("met_id",col.order)]
newdata3<- newdata2[,-1]
dim(newdata3)

new_metabolon_data<-array(0,dim=c(dim(raw.data)[1],(dim(raw.data)[2]-1)))
i=3



options(warn=-1)
for(i in 1:(dim(raw.data)[2]-1))
{
  new_metabolon_data[,i]<- newdata3[,i]
}
i=2


datranks=array(0,dim=c(1130,995))
for(i in 1:1130)
{
  datranks[i,] <- rank(-(new_metabolon_data[,c(i)]),
                       ties.method="average")
  #datranks[i,]=datranksi
}
#dat.max.rankij=res.marij=k=array(0,dim=c(1130,1130))
#i=1
dat.max.rankij=array(0,dim=c(1130,1130,995))
rall<-k<-array(0,dim=c(1130,1130))
for(i in 1:1130)
{
  for(j in i:1130)
  {

    dat.max.rankij[i,j,]<- apply(cbind(datranks[i,],datranks[j,]),1,max)

  }
  print(i)
}




##################  % Reproducible metabolites pairwise

#raw.dat3<-new_metabolon_data

countall=c(0)
##  1130 pairwise subjects
options(warn = -1)
for(i in 1:1130)
{
  for(j in i:1130)
  {if(i < j){
    listMaRR<- MaRR(dat.max.rankij[i,j,],alpha=.05)
    rall[i,j] <- length(listMaRR[[5]])
    mall_rep_i=listMaRR[[5]]
    posall<-array(0,dim=c(995))
    posall[mall_rep_i]=rep(1,length(mall_rep_i))
    #mAB_irrep_i=setdiff(c(1:995),mAB_rep_i)
    #countbc=c(0)

    countall=countall+(posall==1)*1
  }
    #k[i,j]=MaRR(dat.max.rankij[i,j,],alpha=.05)[[2]]
  }
  print(i)
}

rall_new=upper.tri(rall,diag=F)
rall_new[1:3,1:3]
rall2<-rall[rall_new]
summary(rall2)
hist(rall2)
rall3<-(rall2*100)/995
pall<-(countall*100)/(choose(1130,2))
paltu<-(countall*100)/(637885)
hist(pall)
summary(pall)
which(pall>50)
length(which(pall>50))
length(which(pall>80))

saved_metabolon<-list(rall2=rall2,rall3=rall3,pall=pall)
save(saved_metabolon,file="../saved_objects/Metabolon_knn_RUV_load.R")
