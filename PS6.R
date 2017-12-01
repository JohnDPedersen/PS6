#Metropolis-Hastings Regression

setwd ("C:/Users/John/Desktop/Macro/PS6")

data=read.csv('card.csv',header=TRUE)

#Create OLS Estimate
OLS=lm(log(wage)~educ+smsa+black+exper+south,data=data)
stats=summary(OLS)$coef[,1:2]
beta_OLS=stats[,1]
sd_OLS=stats[,2]
e=resid(OLS)
data$constant <- rep(1,nrow(data))
X=data.frame('educ','smsa','black','black','exper','south')
lwage=log(data$wage)


prior<-function(inter,beta,sd){
  p_beta=dnorm(beta,mean=0, sd = 5,log=T)
  p_inter=dunif(inter,min=0,max=10,log=T)
  p_sd=dunif(sd,min=0,max=1,log=T)
  
  p_sum=sum(p_beta,p_inter,p_sd)
  
  return(p_sum)
}

LL<-function(inter,beta,sd){
  yhat<-inter + beta[1]*data$educ+beta[2]*data$exper+beta[3]*data$smsa+beta[4]*data$black+beta[5]*data$south
  likelihood_num=dnorm(data$lwage,mean=yhat,sd=sd,log=T)
  
  LL_sum=sum(likelihood_num)
  
  return(LL_sum)
}

##Posterior
posterior<-function(inter,beta,sd){
  return(prior(inter,beta,sd)+LL(inter,beta,sd))
  
}

proposal<-function(par){
  
  v<-sapply(c(std_errors,.00001), function (x) rnorm(1,0,x)) #Standard error residuals? hmm
  par_1<-par+v
  sd<-par_1[length(par)]
  
  if(sd <= 0) {
    par_1<-par
  }
  
  return(par_1)
}

theta=c(7,.065,.03,.1,.1,.1,.1)
iter=1000
value=matrix(0,iter,length(theta))
accept=NULL

for(i in 1:iter){
  
  value[i,]=theta
  suggest=proposal(value[i,])
  
  
  Lratio=LL(suggest[1],suggest[2:6],suggest[7])-LL(value[i,1],value[i,2:6],value[i,7])
  
  Lratio=exp(Lratio)
  
  a=runif(1,0,1)
  
  if (a<Lratio){
    accept[i]<-TRUE
    theta<-suggest
  }else{
    accept[i]<-FALSE
    theta=theta
  }
  
}

educ+smsa+black+exper+south

labels<-c("Intercept","Beta Educ.","Beta Exp","Beta SMSA","Beta Black","Beta South","Std. Dev of Resid.")


par(mfrow=c(2,2))
for(j in 1:length(theta)){
  
  png=(filename=c("C:/Users/John/Desktop/Macro/PS6/Hist",labels[j]))
  hist(value[,j], breaks=50,main=c(labels[j]," 1000 Iterations"),xlab=labels[j])
  
}



#Get Stats from Posterior Distributions
for(k in 1:length(theta)){
  print(c(mean(value[,k]),sd(value[,k])))
}

