
setwd("../marr_code_A45")
library(ggplot2)
plotlist<-plotlist1k<-list()

##########################n=1000

load(file="B1_sim_results.Rdata")
b1fdr<-sim.results[,3]
b1_pi<-sim.results[,4]
b1ndr<-sim.results[,5]
load(file="B2_sim_results.Rdata")
b2fdr<-sim.results[,3]
b2_pi<-sim.results[,4]
b2ndr<-sim.results[,5]
load(file="B3_sim_results.Rdata")
b3fdr<-sim.results[,3]
b3_pi<-sim.results[,4]
b3ndr<-sim.results[,5]

load(file="B4_sim_results.Rdata")
b4fdr<-sim.results[,3]
b4_pi<-sim.results[,4]
b4ndr<-sim.results[,5]
dat <- data.frame(cond = factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)),
                  FDR = c(b1fdr,b2fdr,b3fdr,b4fdr))
datpi <- data.frame(cond = factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)), pi1 = c(b1_pi,b2_pi,b3_pi,b4_pi))
datpi0 <- data.frame(cond = factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)),
                     pi10 = c((b1_pi-0.2),(b2_pi-0.4),(b3_pi-0.75),(b4_pi-0.9)))


  p_fdr <- ggplot(dat, aes(x=cond, y= FDR)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+
  ggtitle(expression(paste(~rho[R],"=0.45,"~mu[R],"=3.89")))+ylim(0,0.10)+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0.05,lwd=0.3)

datn <-data.frame(cond =  factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)), NDR = c(b1ndr,b2ndr,b3ndr,b4ndr))
#p <- ggplot(dat, aes(x=cond, y= FDR)) + geom_boxplot()+ xlab(expression(paste("Value is ", sigma,",", R^{2},'=',r2.value)))
p_ndr <- ggplot(datn, aes(x=cond, y= NDR)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+ylab("1-NDR")+
  ggtitle(expression(paste(~rho[R],"=0.45,"~mu[R],"=3.89")))+
  theme(plot.title = element_text(hjust = 0.5))


#p <- ggplot(dat, aes(x=cond, y= FDR)) + geom_boxplot()+ xlab(expression(paste("Value is ", sigma,",", R^{2},'=',r2.value)))
p_pi <- ggplot(datpi, aes(x=cond, y= pi1)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+
  ylab(expression(paste(hat(pi[1]))))+
  ggtitle(expression(paste(n[1],"=1000,"~rho,"=0.4")))+ylim(0,1)+
  theme(plot.title = element_text(hjust = 0.5))

p_pi0 <- ggplot(datpi0, aes(x=cond, y= pi10)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+
  ylab(expression(paste(pi[1]-hat(pi[1]))))+
  ggtitle(expression(paste(~rho[R],"=0.45,"~mu[R],"=3.89")))+ylim(-0.12,0.12)+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0,lwd=0.3)



plotlist[[13]]<-plotlist1k[[1]]<-p_fdr

plotlist[[14]]<-p_pi
plotlist[[15]]<-plotlist1k[[2]]<-p_pi0
plotlist[[16]]<-plotlist1k[[3]]<-p_ndr
load(file="C1_sim_results.Rdata")
b1fdr<-sim.results[,3]
b1_pi<-sim.results[,4]
b1ndr<-sim.results[,5]
load(file="C2_sim_results.Rdata")
b2fdr<-sim.results[,3]
b2_pi<-sim.results[,4]
b2ndr<-sim.results[,5]
load(file="C3_sim_results.Rdata")
b3fdr<-sim.results[,3]
b3_pi<-sim.results[,4]
b3ndr<-sim.results[,5]

load(file="C4_sim_results.Rdata")
b4fdr<-sim.results[,3]
b4_pi<-sim.results[,4]
b4ndr<-sim.results[,5]
dat <- data.frame(cond = factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)),
                  FDR = c(b1fdr,b2fdr,b3fdr,b4fdr))
datpi <- data.frame(cond = factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)), pi1 = c(b1_pi,b2_pi,b3_pi,b4_pi))
datpi0 <- data.frame(cond = factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)),
                     pi10 = c((b1_pi-0.2),(b2_pi-0.4),(b3_pi-0.75),(b4_pi-0.9)))

p_fdr <- ggplot(dat, aes(x=cond, y= FDR)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+
  ggtitle(expression(paste(~rho[R],"=0.45,"~mu[R],"=4.01")))+ylim(0,0.10)+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0.05,lwd=0.3)
#~rho[R],"=0.45,"~mu[R],"=4.01"
##NDR

datn <- data.frame(cond =  factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)), NDR = c(b1ndr,b2ndr,b3ndr,b4ndr))
#p <- ggplot(dat, aes(x=cond, y= FDR)) + geom_boxplot()+ xlab(expression(paste("Value is ", sigma,",", R^{2},'=',r2.value)))
p_ndr <- ggplot(datn, aes(x=cond, y= NDR)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+ylab("1-NDR")+
  ggtitle(expression(paste(~rho[R],"=0.45,"~mu[R],"=4.01")))+
  theme(plot.title = element_text(hjust = 0.5))




#p <- ggplot(dat, aes(x=cond, y= FDR)) + geom_boxplot()+ xlab(expression(paste("Value is ", sigma,",", R^{2},'=',r2.value)))
p_pi <- ggplot(datpi, aes(x=cond, y= pi1)) + geom_boxplot()+ xlab(expression(paste(pi[1])))+
  ylab(expression(paste(hat(pi[1]))))+
  ggtitle(expression(paste(~rho,"=0.6")))+ylim(0,1)+
  theme(plot.title = element_text(hjust = 0.5))

p_pi0 <- ggplot(datpi0, aes(x=cond, y= pi10)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+
  ylab(expression(paste(pi[1]-hat(pi[1]))))+
  ggtitle(expression(paste(~rho[R],"=0.45,"~mu[R],"=4.01")))+ylim(-0.12,0.12)+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0,lwd=0.3)


plotlist[[17]]<-plotlist1k[[4]]<-p_fdr

plotlist[[18]]<-p_pi
plotlist[[19]]<-plotlist1k[[5]]<-p_pi0
plotlist[[20]]<-plotlist1k[[6]]<-p_ndr

load(file="D1_sim_results.Rdata")
b1fdr<-sim.results[,3]
b1_pi<-sim.results[,4]
b1ndr<-sim.results[,5]
load(file="D2_sim_results.Rdata")
b2fdr<-sim.results[,3]
b2_pi<-sim.results[,4]
b2ndr<-sim.results[,5]
load(file="D3_sim_results.Rdata")
b3fdr<-sim.results[,3]
b3_pi<-sim.results[,4]
b3ndr<-sim.results[,5]

load(file="D4_sim_results.Rdata")
b4fdr<-sim.results[,3]
b4_pi<-sim.results[,4]
b4ndr<-sim.results[,5]

dat <- data.frame(cond = factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)),
                  FDR = c(b1fdr,b2fdr,b3fdr,b4fdr))
datpi <- data.frame(cond = factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)), pi1 = c(b1_pi,b2_pi,b3_pi,b4_pi))
datpi0 <- data.frame(cond = factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)),
                     pi10 = c((b1_pi-0.2),(b2_pi-0.4),(b3_pi-0.75),(b4_pi-0.9)))

p_fdr <- ggplot(dat, aes(x=cond, y= FDR)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+
  ggtitle(expression(paste(~rho[R],"=0.45,"~mu[R],"=4.13")))+ylim(0,0.10)+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0.05,lwd=0.3)
#~rho[R],"=0.45,"~mu[R],"=4.13"
##NDR

datn <- data.frame(cond =  factor(rep(c("0.2","0.4","0.75","0.9"), each=1000)), NDR = c(b1ndr,b2ndr,b3ndr,b4ndr))
#p <- ggplot(dat, aes(x=cond, y= FDR)) + geom_boxplot()+ xlab(expression(paste("Value is ", sigma,",", R^{2},'=',r2.value)))
p_ndr <- ggplot(datn, aes(x=cond, y= NDR)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+ylab("1-NDR")+
  ggtitle(expression(paste(~rho[R],"=0.45,"~mu[R],"=4.13")))+
  theme(plot.title = element_text(hjust = 0.5))


#p <- ggplot(dat, aes(x=cond, y= FDR)) + geom_boxplot()+ xlab(expression(paste("Value is ", sigma,",", R^{2},'=',r2.value)))
p_pi <- ggplot(datpi, aes(x=cond, y= pi1)) + geom_boxplot()+ xlab(expression(paste(pi[1])))+
  ylab(expression(paste(hat(pi[1]))))+
  ggtitle(expression(paste(~rho,"=0.99")))+ylim(0,1)+
  theme(plot.title = element_text(hjust = 0.5))

p_pi0 <- ggplot(datpi0, aes(x=cond, y= pi10)) + geom_boxplot(fatten=1,lwd=0.3,outlier.size=0)+ xlab(expression(paste(pi[1])))+
  ylab(expression(paste(pi[1]-hat(pi[1]))))+
  ggtitle(expression(paste(~rho[R],"=0.45,"~mu[R],"=4.13")))+ylim(-0.12,0.12)+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0,lwd=0.3)




plotlist[[21]]<-plotlist1k[[7]]<-p_fdr

plotlist[[22]]<-p_pi
plotlist[[23]]<-plotlist1k[[8]]<-p_pi0
plotlist[[24]]<-plotlist1k[[9]]<-p_ndr
require(gridExtra)


ml2 <- marrangeGrob(plotlist1k, nrow = 1, ncol = 1,top=NULL)
#ml1  5.04 inches by 3.78

#ml2fdr <- marrangeGrob(list(plotlist1k[[1]],plotlist1k[[4]],plotlist1k[[7]],
#                            plotlist1k[[1]],plotlist1k[[4]],plotlist1k[[7]]),
#                      nrow = 2, ncol = 3,byrow=F,top=NULL)

ml2fdr <- marrangeGrob(list(plotlist1k[[1]],plotlist1k[[4]],plotlist1k[[7]]),
                       nrow = 1, ncol = 3,byrow=F,top=NULL)

mlpi0 <- marrangeGrob(list(plotlist1k[[2]],plotlist1k[[5]],plotlist1k[[8]]),
                      nrow = 1, ncol = 3,top=NULL)


ml2ndr <- marrangeGrob(list(plotlist1k[[3]],plotlist1k[[6]],plotlist1k[[9]]),
                       nrow = 1, ncol = 3,top=NULL)
#ggsave("/Users/ghoshtu/Desktop/Wednesday_1211pres/MaRR0419_simplots.pdf", ml2,
#   width = 5.04, height = 3.78, units = "in")
##../MaRRsimA45/
ggsave("../MaRRsimA45/Fig5a.pdf", ml2fdr, width = 6.5, height = 3.78, units = "in")

ggsave("../MaRRsimA45/Fig7a.pdf", mlpi0, width = 6.5, height = 3.78, units = "in")
#width = 5.04, height = 3.78, units = "in")

ggsave("../MaRRsimA45/Fig6a.pdf", ml2ndr, width = 6.5, height = 3.78, units = "in")
#width = 5.04, height = 3.78, units = "in")


