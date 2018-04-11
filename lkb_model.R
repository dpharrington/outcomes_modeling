##############################################################################################
#############The packages we use: maxLik to solve maximum likelihood function,################
#############pakcage ROCR for drawing ROC curve and calculating Area Under the curve##########
##############################################################################################
install.packages("maxLik")
install.packages("ROCR")
library(maxLik)
library(ROCR)
##############################################################################################
#############Read the Dose-Volume Histogram data and the response#############################
#############the datalist stores all patients' DVH, R is the response#########################
##############################################################################################
setwd("C:/Users/xliu203/Desktop/Vector")
fileName <- dir()
read.file <- function(File){
  read.table(File,header=TRUE)
} 
datalist <- lapply(fileName,read.file)
R=rep(0,length(fileName))
R[c(72,63,30,25,73,38,71,57,3,50,70,17,34,15,81)]=1
##############################################################################################
##############LKB modeling, dntcpl is the function name#######################################
##############param is short for parameters, TD50, m0(m) and n################################
##############################################################################################
dntcpl <- function(param){
  TD_50=param[1];
  m0=param[2];
  n=param[3];
  lntcp=0;
  if(TD_50>50&&TD_50<100&&m0>0&&m0<1&&n>0&&n<1){  ##I set a boundary for parameters for better estimation##
    
    for(i in 1:79){
      
        d=datalist[[i]][[1]]
        eud=(sum(d^(1/n))/length(d))^(n)
        t=(eud-TD_50)/(m0*TD_50)
        lntcp=lntcp+R[i]*log(pnorm(t))+(1-R[i])*log(1-pnorm(t))
     
      
    }
  }else {lntcp=-100}
  return(lntcp)
}
model=maxLik(dntcpl,start=c(65,0.2,0.3),method="nm") ####The maxLik is the function to solve the parameters####
summary(model)
coef=coef(model)
TD_50=coef[1]
m0=coef[2]
n=coef[3] #########Three estimated parameters###########

#############################################################################################
##############Parameter confidence interval #################################################
##############Here we use estimated Hessian Matrix as estimated covariance matrix############
#############################################################################################
theta.start <- coef
md<-function(param) -dntcpl(param)
out <- nlm(md, theta.start,hessian=TRUE)
theta.hat <- out$estimate
fish=out$hessian
theta.hat
conf.level <- 0.95
crit <- qnorm((1 + conf.level)/2)
inv.fish <- solve(fish)
theta.hat[1] + c(-1, 1) * crit * sqrt(inv.fish[1, 1])/sqrt(79)
theta.hat[2] + c(-1, 1) * crit * sqrt(inv.fish[2, 2])/sqrt(79)
theta.hat[3] + c(-1, 1) * crit * sqrt(inv.fish[3, 3])/sqrt(79)

###############################################################################################
##############Calculate training AUC, LKB model################################################
###############################################################################################
entcp <- function(param){
  
  lntcp=NULL
  TD_50=param[1];
  m0=param[2];
  n=param[3];
  for(i in 1:length(fileName)){
    
    d=datalist[[i]][[1]]
    eud=(sum(d^(1/n)/length(d)))^(n)
    t=(eud-TD_50)/(m0*TD_50)
    lntcp=c(lntcp,pnorm(t))
  }
  
  pred0=prediction(lntcp,R)
  auc.tmp=performance(pred0,"auc")
  auc=as.numeric(auc.tmp@y.values)
  return(auc)
}
auc1=entcp(coef(model))       ###the AUC  with no cross-validation

###############################################################################################
###############Univariate Logistic Regression Model############################################
###############Store all the dose percentiles in the matrix z0#################################
###############Calculating training AUC########################################################
###############################################################################################
z0=matrix(0,79,17)
for(i in 1:79){
  for(j in 1:17){
    d=datalist[[i]][[1]]
    z0[i,j]=quantile(datalist[[i]][[1]],0.05*j+0.05)
  }
}
colnames(z0)=c("D90","D85","D80","D75","D70","D65","D60","D55",
               "D50","D45","D40","D35","D30","D25","D20","D15","D10")
logit0=glm(R~D50, data=data.frame(R,z0), family=binomial) ########you can try different dose indices##########
summary(logit0)
ntcp=predict(logit0, type="response")
pred=prediction(ntcp, R)
perf0=performance(pred,"tpr","fpr")
auc.tmp=performance(pred,"auc")
auc=as.numeric(auc.tmp@y.values)
auc

###############################################################################################
####################goodness of fit############################################################
###############################################################################################
z0=matrix(0,79,17)
for(i in 1:79){
  for(j in 1:17){
    d=datalist[[i]][[1]]
    z0[i,j]=quantile(datalist[[i]][[1]],0.05*j+0.05)
  }
}
colnames(z0)=c("D90","D85","D80","D75","D70","D65","D60","D55","D50",
               "D45","D40","D35","D30","D25","D20","D15","D10")
logit=glm(R~D15,data=data.frame(R,z0),family="binomial") #############you can try different dose indices######
logit2=glm(R~1,data=data.frame(R,z0),family="binomial")
lrtest(logit,logit2)
