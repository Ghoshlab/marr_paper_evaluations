library(dplyr)
library(tidyr)
library(readr) 
library(stringr)

##### MaRR procedure ####

## The MaRR procedure file is at the very bottom.  ##
require(MASS)

# ----------- Functions for generating data used in simulations --------

# get.ranks.fixedn1: a function to produce a set of paired ranks following Li et al. 2011
# INPUTS: n = number of signals
#         n1 = number of reproducible signals
#         tau1 = correlation between original reproducible signals
#         mu1 = mean of 'true' signals (sd assumed to be 1)
# OUTPUTS: x = first set of ranks, y = second set of ranks
#          ks = a vector of zeroes and ones indicating whether gene is reproducible (1) or not (0)
#          which.sig = a vector of the indices of reproducible signals
#          Which.notsig = a vector of the indices of irreprodicible signals
#         x.idr = vector of signals suitable for use with the idr package
get.ranks.fixedn1 <- function(n=1000, n1=650, tau1=.84, mu1=2.5) 
{
  omega1=1-tau1
  Zijs = matrix(NA,2,n)
  n0 <- n-n1
  ks = sample(c(rep(0,n0),rep(1,n1)))
  which.sig = which(ks==1); which.notsig = which(ks==0)
  Zs = rnorm(n1,mu1,sqrt(tau1))
  Zijs[,which.sig] = sapply(Zs,function(x){return(rnorm(2,mean=x,sd=sqrt(omega1)))})
  Zijs[,which.notsig] = matrix(rnorm(2*n0),2,n0)
  Pvals = pnorm(Zijs,lower.tail=F)
  rank.Pvals = rbind(rank(Pvals[1,]),rank(Pvals[2,]))
  x <- rank.Pvals[1,]; y <- rank.Pvals[2,] 
  max.rank = apply(cbind(x,y),1,max)
  return(list(max.rank=max.rank,x=x,y=y,ks=ks,which.sig=which.sig,which.notsig=which.notsig,n1=n1,x.idr = t(-log(Pvals))))
}

# get.ranks.n1sep: a function to produce a set of paired ranks under the ideal setting
#                  of a perfect split
# INPUTS: n = number of signals
#         n1 = number of reproducible signals
#         tau1 = correlation between original reproducible signals
#         mu1 = mean of 'true' signals (sd assumed to be 1)
# OUTPUTS: x = first set of ranks, y = second set of ranks
#          ks = a vector of zeroes and ones indicating whether gene is reproducible (1) or not (0)
#          which.sig = a vector of the indices of reproducible signals
#          Which.notsig = a vector of the indices of irreprodicible signals
#          max.rank = a vector of maximum rank statistics
get.ranks.n1sep <- function(n=1000, n1=650, tau1=.84, mu1=2.5) 
{
  omega1=1-tau1
  max.rank = rep(NA,n)
  n0 <- n-n1
  ks = sample(c(rep(0,n0),rep(1,n1)))
  which.sig = which(ks==1); which.notsig = which(ks==0)
  Zs = rnorm(n1,mu1,sqrt(tau1))
  Zijs.rep = sapply(Zs,function(x){return(rnorm(2,mean=x,sd=sqrt(omega1)))})
  Zijs.irrep = matrix(rnorm(2*n0),2,n0)
  Pvals.rep = pnorm(Zijs.rep,lower.tail=F)
  max.rank.rep = apply(cbind(rank(Pvals.rep[1,]),rank(Pvals.rep[2,])),1,max)
  Pvals.irrep = pnorm(Zijs.irrep,lower.tail=F)
  x.ranks = y.ranks = rep(NA,n)
  x.ranks[which.sig] = rank(Pvals.rep[1,]); y.ranks[which.sig]=rank(Pvals.rep[2,])  
  x.ranks[which.notsig] = rank(Pvals.irrep[1,])+n1; y.ranks[which.notsig] = rank(Pvals.irrep[2,])+n1;
  max.rank.irrep = apply(cbind(rank(Pvals.irrep[1,]),rank(Pvals.irrep[2,])),1,max)+n1
  max.rank[which.sig] = max.rank.rep; max.rank[which.notsig] = max.rank.irrep
  return(list(ks=ks,which.sig=which.sig,which.notsig=which.notsig,max.rank=max.rank,x=x.ranks,y=y.ranks))
}

# get.ranks.dep: a function to produce a set of paired ranks with correlated irreproducible signals
# INPUTS: n = number of signals
#         n1 = number of reproducible signals
#         tau1 = correlation between original reproducible signals
#         mu1 = mean of 'true' signals (sd assumed to be 1)
#         rho= - correlation between irreproducible signals
# OUTPUTS: x = first set of ranks, y = second set of ranks
#          ks = a vector of zeroes and ones indicating whether gene is reproducible (1) or not (0)
#          which.sig = a vector of the indices of reproducible signals
#          Which.notsig = a vector of the indices of irreprodicible signals
#          x.idr = vector of signals suitable for use with the idr package
get.ranks.dep <- function(n=1000, n1=650, tau1=.84, mu1=2.5,rho0=0) 
{
  omega1=1-tau1
  Zijs = matrix(NA,2,n)
  n0 <- n-n1
  ks = sample(c(rep(0,n0),rep(1,n1)))
  which.sig = which(ks==1); which.notsig = which(ks==0)
  Zs = rnorm(n1,mu1,sqrt(tau1))
  Zijs[,which.sig] = sapply(Zs,function(x){return(rnorm(2,mean=x,sd=sqrt(omega1)))})
  Zijs[,which.notsig] = t(mvrnorm(n=n0,mu=c(0,0),Sigma=matrix(c(1,rho0,rho0,1),2)))
  Pvals = pnorm(Zijs,lower.tail=F)
  rank.Pvals = rbind(rank(Pvals[1,]),rank(Pvals[2,]))
  x <- rank.Pvals[1,]; y <- rank.Pvals[2,] 
  max.rank = apply(cbind(x,y),1,max)
  return(list(max.rank=max.rank,x=x,y=y,ks=ks,which.sig=which.sig,which.notsig=which.notsig,n1=n1,x.idr = t(-log(Pvals))))
}

# get.ranks.real: a function to produces a set of paired ranks that looks like the SEQC data
# INPUTS: n = number of signals
#         n1 = number of reproducible signals
#         r1 = minimum correllation between reproducible signals
#         ms = c(m1,m2) = the min and max for Z_{i1}, reproducible signal
# OUTPUTS: x = first set of ranks, y = second set of ranks
#          ks = a vector of zeroes and ones indicating whether gene is reproducible (1) or not (0)
#          which.sig = a vector of the indices of reproducible signals
#          Which.notsig = a vector of the indices of irreprodicible signals
#          x.idr = vector of signals suitable for use with the idr package
get.ranks.real <- function(n=1000, n1=650, r1=1, ms=c(2,5),sigma=1) 
{
  Zijs = matrix(NA,2,n)
  n0 <- n-n1
  ks = sample(c(rep(0,n0),rep(1,n1)))
  which.sig = which(ks==1); which.notsig = which(ks==0)
  
  Zs = Zijs[1,which.sig] =  runif(n1,ms[1],ms[2]) #rnorm(n1,mean(ms)); ms=range(Zs) 
  Zijs[2,which.sig] = sapply(Zs,function(x){return(rnorm(1,mean=x,sd=sqrt(1-((1-r1)/(ms[2]-ms[1])*(x-ms[1])+r1)^2)))})
  Zijs[,which.notsig] = rnorm(2*n0,mean=0,sd=sigma)
  Pvals = pnorm(Zijs,lower.tail=F)
  rank.Pvals = rbind(rank(Pvals[1,]),rank(Pvals[2,]))
  x <- rank.Pvals[1,]; y <- rank.Pvals[2,] 
  max.rank = apply(cbind(x,y),1,max)
  return(list(max.rank=max.rank,x=x,y=y,ks=ks,which.sig=which.sig,which.notsig=which.notsig,n1=n1,x.idr = t(-log(Pvals))))
}

# ----------- Functions for performing the MaRR procedure ------------------------

# get.SS: a function to produce a vector of sums of squared differences between observed
#         and actual survival functions for k-hat=0,...,n-1
# INPUTS: max.rank = a vector of maximum rank statistics
# OUTPUTS: mySS = a vector of SS(i/k) values as described in manuscript
get.SS = function(max.rank)
{
  n = length(max.rank)
  x = 1:n/n
  mySS=rep(NA,n)
  my.W = sapply(1:n,function(i){sum(max.rank==i)})
  surv.function = ( n-cumsum(my.W))/n
  for(k in 0:(n-1))
  {
    i=k+1; pi1=k/n; pi0=1-pi1
    temp.W =surv.function[i:n]
    Sn = pi0*(1-(x-pi1)^2/pi0^2)[i:n]
    temp.diff = temp.W-Sn
    sq.diff = (temp.diff*temp.diff)/n
    mySS[i] = sum(sq.diff)/pi0
  }
  return(mySS)
}

# est.fdr: a function to produce a vector of estimated false discovery rats based on 
#          a vector of maximum rank statistics and a khat.
# INPUTS: max.rank = a vector of maximum rank statistics
#         khat = a value of khat calculated from the argmin of mySS
# OUTPUTS: a vector of estimated FDR values for each potential threshold Nhat=1,...,n
est.fdr = function(khat,max.rank)
{
  n=length(max.rank)
  Nhat = (khat+1):n
  Q.khat = sum(max.rank<=khat)
  R.Nhat = sapply(Nhat,function(i){sum(max.rank<=i)})
  indicate.R = as.numeric(R.Nhat>0)
  R.Nhat[indicate.R==0] = 1
  temp = Nhat-khat
  numer=temp*temp
  denom = (n-khat)*R.Nhat
  FDR.Nhatkhat = (numer/denom)*indicate.R
  return(c(rep(0,khat),FDR.Nhatkhat))
}

# MaRR: a function that performs the MaRR procedure based on a vector of maximum rank statistics.
# INPUTS: max.rank = a vector of maximum rank statistics
#         cutoff = a value between 0 and 1 that provides the maximum allowed value for pi-hat.
#         alpha = desired level of FDR control
#         khat.to.zero = T/F, whether or not to set k-hat to zero for mFDR calculation (recommended for very small pi1)
# OUTPUTS: khat = n*pi-hat, discrete estimate of where irreproducible signals begin
#          Nhat = estimated cut-off for maximum ranks that will control FDR at level alpha
#          est.fdr = the estimated fdr value for each of potential N-hat
#          SS = vector of values for SS loss function evaluated at i/n =0, 1/n, 2/n,..., 1
#          which.sig = vector of indices for maximum ranks declared to be reproducible
MaRR = function(max.rank,cutoff=.9,alpha=.05,khat.to.zero=F){
  maxx=floor(cutoff*length(max.rank))
  mySS = get.SS(max.rank)
  khat = which(mySS[1:maxx]==min(mySS[1:maxx]))-1
  if(khat.to.zero==T){khat=0}
  temp.fdr = est.fdr(khat,max.rank)
  Nhat = max(which(temp.fdr<=alpha))
  which.sig=which(max.rank<=Nhat)
  return(list(Nhat=Nhat,khat=khat,est.fdr=temp.fdr,SS=mySS,which.sig=which.sig))
}

# ----------- Auxiliary functions needed for simulation studies 

# ofinterest - a function that calculates achieved fdr and 1-ndr.
ofinterest = function(sig.index,rep.index,irrep.index)
{
  fdr = ndr = 0
  k=length(sig.index)
  if(k>0)
  {
    fdr = length(intersect(sig.index,irrep.index))/length(sig.index)
    ndr = length(intersect(sig.index,rep.index))/length(rep.index)
  }
  return(c(fdr,ndr))
}

#QL.randomstarts - a function that performs the IDR-procedure (copula mixture model) a set number of times and
#                  reports the results with the highest likelihood
QL.randomstarts = function(signals,starts=25,omegas=c(.4,.84),means=c(1.5,3.5),ps=c(0.01,1),alpha=.05)
{
  r.omegas = runif(starts,min=omegas[1],max=omegas[2])
  r.means = runif(starts,min=means[1],max=means[2])
  r.ps = runif(starts,min=ps[1],max=ps[2])
  likes = rep(NA,starts)
  for(i in 1:starts)
  {
    likes[i] = -est.IDR(signals,mu=r.means[i],sigma=1,rho=r.omegas[i],p=r.ps[i],eps=0.001,max.ite=30)$loglik
  }
  chosen = which(likes==min(likes))
  idr.out = est.IDR(signals,mu=r.means[chosen],sigma=1,rho=r.omegas[chosen],p=r.ps[chosen],eps=.001,max.ite=30)
  idr.sig = which(idr.out$IDR<=alpha)
  return(idr.sig)
}