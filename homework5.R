fishmatrix<-matrix(c(0,0,0,6.2,0.2,0.05,0,0,0,0.7,0.05,0,0,0,0.8,0.8),nrow=4,ncol=4,byrow=TRUE)
fishmatrix
n0<-matrix(c(100,0,0,0),nrow=4,ncol=1,byrow=TRUE)
n0

#--------------------------------------
ans<-n0
ans
n.mult <- 12
  
for(i in seq(n.mult)){
  ans <- fishmatrix %*% ans 
}
ans
#------------------------------------------------------
nt<-n0
nt
fullmatrix<-matrix(nrow=4,ncol=51)  #creating a matrix to store all the stages
percmatrix<-matrix(nrow=4,ncol=51)  #creating a matrix to store the percentages
summatrix<-matrix(nrow=1,ncol=51)  #creating a matrix to store the sums

fullmatrix[1:4,1]<-n0  #filling the first column for n0
summatrix[1,1]<-sum(n0)  #filling the first column for n0
percmatrix[1:4,1]<-(n0/sum(n0))*100  #filling the first column for n0

for (j in 2:ncol(fullmatrix))  {
    nt <- fishmatrix %*% nt 
    fullmatrix[,j]<-nt
    summatrix[,j]<-sum(nt)
    percmatrix[,j]<-(nt/sum(nt))*100
}

fullmatrix
summatrix
percmatrix
#------------------------------------------------------

#create a matrix for years, to bind to matrix of sums
yearsmatrix<-matrix(0:30,nrow=1,ncol=31)
yearsmatrix
plot(yearsmatrix,summatrix,main="Projected Future Population Size",xlab="Years", ylab="Population size")

#plots stacked proportions 
barplot(percmatrix,main="Percentage of population in 4 life stages",xlab="Years in Future",ylab="Population proportions",col=rainbow(4))  
names.arg<-c("newly hatched eggs","juveniles","subadults","adults")
names.arg
legend(38,40,names.arg,cex=0.5,fill=rainbow(4))
#plots proportions side by side
barplot(percmatrix,main="Percentage of population in 4 life stages",xlab="Years in Future",ylab="Population proportions",beside=TRUE,col=rainbow(4))  
legend("topright",c("newly hatched eggs","juveniles","subadults","adults"),cex=0.6,bty="n",fill=rainbow(4))

#------------------------------------------------------
#calculating lambdas using summatrix
l=length(summatrix)
l
lambdas<-((summatrix[2:l])/(summatrix[1:(l-1)]))
lambdas<-as.matrix(lambdas)
lambdas
plot(lambdas,type="o",xlab="years in future")

#-----------------------------------------------------------
eigen(fishmatrix)

#-----------------BORDERIA CHOUARDII--------------------------------------------
plantmatrix<-matrix(c(0.500,0,0,0.032,0.220,0.528,0.030,0,0,0.002,0.011,0.026,0,0.772,0.856,0.046,0.002,0,0,0.019,0.095,0.759,0.079,0,0,0,0,0.179,0.838,0.151,0,0,0,0.003,0.074,0.845),nrow=6,ncol=6,byrow=TRUE)
plantmatrix
eigen(plantmatrix)

n0<-matrix(c(100,0,0,0,0,0),nrow=6,ncol=1,byrow=TRUE)
n0

nt<-n0
nt
fullmatrix<-matrix(nrow=6,ncol=51)  #creating a matrix to store all the stages
percmatrix<-matrix(nrow=6,ncol=51)  #creating a matrix to store the percentages
summatrix<-matrix(nrow=1,ncol=51)  #creating a matrix to store the sums

fullmatrix[1:6,1]<-n0  #filling the first column for n0
summatrix[1,1]<-sum(n0)  #filling the first column for n0
percmatrix[1:6,1]<-(n0/sum(n0))*100  #filling the first column for n0

for (j in 2:ncol(fullmatrix))  {
  nt <- plantmatrix %*% nt 
  fullmatrix[,j]<-nt
  summatrix[,j]<-sum(nt)
  percmatrix[,j]<-(nt/sum(nt))*100
}

fullmatrix
summatrix
percmatrix

#calculating lambdas
l=length(summatrix)
l
lambdas<-((summatrix[2:l])/(summatrix[1:(l-1)]))
lambdas<-as.matrix(lambdas)
lambdas
plot(lambdas,type="o",xlab="years in future")

yearsmatrix<-matrix(c(1995:2045),nrow=1,ncol=51)
yearsmatrix
plot(yearsmatrix,summatrix,main="Projected Future Population Size",xlab="Year", ylab="Population size")

#plots stacked proportions 
barplot(percmatrix,main="Percentage of population in 6 life stages",xlab="Year",ylab="Population proportions",col=rainbow(6))  
names.arg<-c("seeds in bank","plants <1year","juveniles","small females","med females","large females")
names.arg
legend(45,98,names.arg,cex=0.5,fill=rainbow(6))
#plots proportions side by side
barplot(percmatrix,main="Percentage of population in 4 life stages",xlab="Years in Future",ylab="Population proportions",beside=TRUE,col=rainbow(4))  
legend("topright",c("newly hatched eggs","juveniles","subadults","adults"),cex=0.6,bty="n",fill=rainbow(4))

#--------------------------------
barplot(plantmatrix,col=rainbow(6))
plot(plantmatrix)

#---------------------------------
#remove the new seed and new seedling values
survivalplantmatrix<-matrix(c(0.500,0,0,0,0,0,0.030,0,0,0,0,0,0,0.772,0.856,0.046,0.002,0,0,0.019,0.095,0.759,0.079,0,0,0,0,0.179,0.838,0.151,0,0,0,0.003,0.074,0.845),nrow=6,ncol=6,byrow=TRUE)
survivalplantmatrix
#eigen(survivalplantmatrix)

n0<-matrix(c(0,0,0,0,0,1),nrow=6,ncol=1,byrow=TRUE)
n0
nt<-n0
nt
fullmatrix<-matrix(nrow=6,ncol=301)  #creating a matrix to store all the stages
percmatrix<-matrix(nrow=6,ncol=301)  #creating a matrix to store the percentages
summatrix<-matrix(nrow=1,ncol=301)  #creating a matrix to store the sums

fullmatrix[1:6,1]<-n0  #filling the first column for n0
summatrix[1,1]<-sum(n0)  #filling the first column for n0
percmatrix[1:6,1]<-(n0/sum(n0))*100  #filling the first column for n0

for (j in 2:ncol(fullmatrix))  {
  nt <- survivalplantmatrix %*% nt 
  fullmatrix[,j]<-nt
  summatrix[,j]<-sum(nt)
  percmatrix[,j]<-(nt/sum(nt))*100
}

fullmatrix
summatrix
percmatrix

yearsmatrix<-matrix(c(1:length(summatrix)),nrow=1,ncol=length(summatrix))
yearsmatrix
#matplot(s.yearsmatrix,s.summatrix,main="Probability of a seed surviving",xlab="Years in future", ylab="Probability*10",type="l")
plot(years,prob.surv,main="Probability of survival",xlab="years in future",ylab="probability of survival",type="l")
lines(years,prob.surv,type="l",col="6")

#s.summatrix<-stack(as.data.frame(summatrix))
#s.yearsmatrix<-stack(as.data.frame(yearsmatrix))


barplot(fullmatrix,main="Probabilities of surviving to different ages for seeds",xlab="years",ylab="prob.*10",beside=TRUE,col=rainbow(6))
legend("topright",c("seeds","F0","F1","F2","F3","F4"),cex=0.6,bty="n",fill=rainbow(6))

prob.surv<-(summatrix[1:length(summatrix)-1]-summatrix[2:length(summatrix)])
prob.surv
years<-1:(length(summatrix)-1)
years
sum(years*prob.surv)

