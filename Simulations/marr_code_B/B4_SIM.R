 
require(doParallel)
require(idr)
setwd("../src")
source('source_MaRR.R')

# a function that generates a single data set, performs the MaRR procedure, fits the copula mixture model, and then reports results
# Use for simulation B
set.seed(3456)
one.data.setreal = function(K,setting,n,p,r1,ms,rho0,mu2,v2,alpha=.05,max.prop = .9,set.khat=F)
{
  require(idr)
  n1 <- floor(n*p)
  maxx = floor(max.prop*n)

  # set up data
  mydata = get.ranks.real(n,n1,r1,ms,rho0,mu2,v2)
  max.rank = mydata$max.rank

  # Perform MaRR procedure
  mySS = get.SS(max.rank)
  khat = which(mySS[1:maxx]==min(mySS[1:maxx]))-1
  if(set.khat==T){khat =0}
  temp.fdr = est.fdr(khat,max.rank)
  Nhat = max(which(temp.fdr<=alpha))

  #get fdr and ndr from MaRR procedure
  temp.results = ofinterest(which(max.rank<=Nhat),mydata$which.sig,mydata$which.notsig)
  my.fdr=temp.results[1]; my.ndr=temp.results[2]
  my.pi=length(which(max.rank<=Nhat))/n


  pdf(paste("k=",K, "setting=",setting,"SS.pdf", sep="_"),width=4,height=4)
  plot(mySS,main=paste("MaRR",round(my.fdr,2),round(my.ndr,2), "IDR=",round(my.fdr,2),round(my.ndr,2), sep=" "))
  dev.off()
  return(c(khat,Nhat,my.fdr,my.pi,my.ndr))
}

### set up simulations of interest ----

# The number of simulated datasets to run on
n.cores <- 3    # The number of the computer's cores to use

### simulation A settings  ----
n <- 2860; alpha <- .05; n.sims <- 1000; set.khat=F


### Simulation B settings ----

setting <- "B4"; ms <- c(4,5); mu2<-3.2;v2<- (0.05)^2;
r1 <- 0.4; p <- .9 ;  max.prop = .9; set.khat=F;rho0=0
#B2# setting <- "B2"; ms <- c(1,5); r1 <- .95; p <- .6 ;  omegas=c(.7,.99); ps=c(.5,.9); max.prop = .9; set.khat=F; n=10000; means <- c(1,5)
#B3# setting <- "B3"; ms <- c(1,5); r1 <- .7;  p <- .7 ;  omegas=c(.7,.99); ps=c(.5,.9); max.prop = .8; set.khat=F; n=10000; means <- c(1,5)
#B4# setting <- "B4"; ms <- c(1,5); r1 <- .3;  p <- .6 ;  omegas=c(.3,.99); ps=c(.5,.9); max.prop = .9; set.khat=F; n=10000; means <- c(1,5)

### For running B settings simulations ----
registerDoParallel(n.cores)

t1 <- proc.time() # Start the timer
sim.results <- foreach(k = 1:n.sims,
                       .errorhandling="remove",
                       .combine="rbind",
                       .inorder=FALSE) %dopar% {

                         cat("Starting", k, "th job.\n", dep="")
                         output.k <- one.data.setreal(k,setting,n,p,r1,ms,rho0,mu2,v2,alpha,max.prop,set.khat)
                         cat("Finishing", k, "th job.\n", dep="")
                         output.k
                       }
t2 <- proc.time() # End the timer
cat(t2-t1, "\n")

# save simulation results
save(sim.results, r1, ms,rho0, max.prop, n,
     file=paste(setting,  "sim_results.Rdata", sep="_"))






#library(ggplot2)
#plotlist<-list()

#load(file="B1_sim_results.Rdata")
#b1fdr<-sim.results[,3]
#b1_pi<-sim.results[,4]
#b1ndr<-sim.results[,5]

