require(doParallel)
require(idr)
setwd("../src")
source('source_MaRR.R')

### Two functions to set up running the simulations in parallel ----
# a function that generates a single data set, performs the MaRR procedure, fits the copula mixture model, and then reports results
# Use for simulation A
set.seed(3456)
one.data.set = function(K,setting,n,p,mu1,mu2,v1,v2,rho1,
                        alpha=.05,max.prop = .9,set.khat=F)
{
  require(idr)
  n1 <- floor(n*p)
  maxx = floor(max.prop*n)

  # set up data
  mydata = get.ranks.fixedn1(n,n1,mu1,mu2,v1,v2,rho1)
  max.rank = mydata$max.rank


  ##########################################################
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


  #pdf(paste("k=",K, "setting=",setting,"SS.pdf", sep="_"),width=4,height=4)
  #plot(mySS,main=paste("MaRR",round(my.fdr,2),round(my.ndr,2), "IDR=",round(my.fdr,2),round(my.ndr,2), sep=" "))
  #dev.off()
  return(c(khat,Nhat,my.fdr,my.pi,my.ndr))
  ##########################################################

}


### set up simulations of interest ----

n.sims <- 1000   # The number of simulated datasets to run on
n.cores <- 3    # The number of the computer's cores to use

### simulation A settings  ----
n <- 2860; alpha <- .05;means <- c(0.5,2.5); starts <- 10; n.sims <- 100; set.khat=F
setting <- "C2";  mu1 <- 4.01; rho1<- 0.45 ; p<- 0.4;
max.prop = .9; mu2<- 3.2; v1<- (0.17)^2; v2<- (0.05)^2
#A2# setting <- "A2";  mu1 <- 2.5; tau1 <- .4;  p <- .3;  omegas=c(.3,.5);   ps=c(.2,.4);   max.prop = .9;  means=c(2,3)
#A3# setting <- "A3";  mu1 <- 2;   tau1 <- .85; p <- .1;  omegas=c(.75,.95); ps=c(.01,.3);  max.prop = .9;  means=c(1.5,2.5)
#A4# setting <- "A4";  mu1 <- 2;   tau1 <- .4;  p <- .65; omegas=c(.3, .5);  ps=c(.55,.75); max.prop = .75; means=c(1.5,2.5)
#A5# setting <- "A5";  mu1 <- 1;   tau1 <- .4;  p <- .8;  omegas=c(.3,.5);   ps=c(.7,.9);   max.prop = .75; means=c(.5,1.5)
#A6# setting <- "A6";  mu1 <- 1;   tau1 <- .85; p <- .1;  omegas=c(.75,.95); ps=c(.01,.3);  max.prop = .9;  means=c(.5,1.5)

### For running A settings simulations ----
#  registerDoParallel(cores=n.cores)
t1 <- proc.time()
sim.results <- foreach(k = 1:n.sims,
                       .errorhandling="remove",
                       .combine="rbind",
                       .inorder=FALSE) %dopar% {
                         output.k <- one.data.set(k,setting,n,p,
                                                  mu1,mu2,v1,v2,rho1,alpha,max.prop,set.khat)
                         output.k
                       }
t2 <- proc.time()
cat(t2-t1, "\n")

# save simulation results
save(sim.results,mu1,mu2,v1,v2,rho1, p,n,
     file=paste(setting,  "sim_results.Rdata", sep="_"))
