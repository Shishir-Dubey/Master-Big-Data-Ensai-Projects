#################################### Preparing the data set ################################################
# Installation of packages and loading them ###########################


#### Installation of necessary packages, need to do it only one time

#install.packages("corrplot")
# install.packages("caret")
#install.packages("randomForest")
# install.packages("MASS")
# install.packages("rpart")
# install.packages("e1071")
#install.packages("glmnet")
#install.pacakges("plotly")
#install.packages("missMDA")
#install.packages("pROC")
#install.packages("DMwR")
#install.packages("gbm")
# install.packages("rattle")
# install.packages("rpart.plot")
# install.packages("RColorBrewer")
# install.packages("party")
# install.packages("partykit")

#### Loading the necessary packages
library(pROC)  #for the roc curve methods
library("MASS")
library("rpart")
library("randomForest")
library("e1071")
library("glmnet")
library(plotly)
library(ggplot2)
library(missMDA)
library(caret)
library(DMwR)
library(rpart)				        
library(rattle)					
library(rpart.plot)				
library(RColorBrewer)				
library(party)					
library(partykit)				
library(caret)					

##############################################################################
####################          set The directory           ####################
##############################################################################
setwd("/home/moustapha/Energiency Big Data Project/Archive")

## loading the data set resampled in one hour interval###

data = read.table("all1h1.csv", header = TRUE, sep = ",")

DateTS <- as.POSIXlt(data$X, format = "%Y-%m-%d %H:%M:%S")

data$X = DateTS ; colnames(data)[1] = "date" ;rownames(data) = data$date





### First View of the Data

summary(data)

graph1 <- ggplot() +
  geom_line(data = data, aes(x = date, y = prodh, color = "prodh")) +
  geom_line(data = data, aes(x = date, y = elec, color = "elec")) +
  geom_line(data = data, aes(x = date, y = (planstop*10), color = "planstop")) +
  geom_line(data = data, aes(x = date, y = prod, color = "prod"))
graph1
ggplotly()


# reshaping the gram for add it in the plot
data$gram = ifelse(data$gram > 100,(data$gram) / 10000,data$gram)

graph2 <- ggplot() +
  geom_line(data = data, aes(x = date, y = prodh, color = "prodh")) +
  geom_line(data = data, aes(x = date, y = elec, color = "elec")) +
  geom_line(data = data, aes(x = date, y = (planstop*10), color = "planstop")) +
  geom_line(data = data, aes(x = date, y = prod, color = "prod")) +
  geom_line(data = data, aes(x = date, y = gram, color = "gram"), color="black")

ggplotly(graph2)

summary(data)


## selecting the Data set containing only the complete plan stop
nomissing = data[which(is.na(data$planstop) == F),]
summary(nomissing)

graph3 <- ggplot() +
  geom_line(data = nomissing, aes(x = date, y = prodh, color = "prodh")) +
  geom_line(data = nomissing, aes(x = date, y = elec, color = "elec")) +
  geom_line(data = nomissing, aes(x = date, y = (planstop*10), color = "planstop")) +
  geom_line(data = nomissing, aes(x = date, y = prod, color = "prod")) +
  geom_line(data = nomissing, aes(x = date, y = gram), color="black") +
  theme(legend.position="bottom")

ggplotly(graph3)

### according to the summary and the plot we will consider the variables that have less missing values
## it is prodh and elec
### we will also consider the data up to "2016-01-26 07:00:00"

data1 = nomissing[1:which(nomissing$date == "2016-01-26 07:00:00"),]
summary(data1)

## we replace the missing values in prodh by the value in prod
data1$prodh[which(is.na(data1$prodh)==T)]=data1$prod[which(is.na(data1$prodh)==T)]
summary(data1)

dd=as.data.frame(is.na(data1))
freq=table(col(dd), as.matrix(dd))
rownames(freq)=colnames(dd)
freq
freq=as.data.frame(freq)

graph4<-ggplot(freq, aes(Var1, Freq)) +   
  geom_bar(aes(fill = Var2), position = "fill", stat="identity")+
  theme(legend.position="top")
graph4
ggplotly()
## it still left some missing values in prodh "48" and elec "24", we will input them with the method inputpca in missMDA

data2=data1[,c(1,2,3,5)]  ## data2 with only the interesting variables

nb= estim_ncpPCA(data2[-1],ncp.min = 0,ncp.max = 5)
nb
res= imputePCA(data2[-1], ncp = 1)
dd=as.data.frame(res$completeObs)
dd$prodh=ifelse(dd$prodh<0,0,dd$prodh)
summary(dd)

data2[-1]=dd

graph5 = ggplot()+
  geom_step(data = data2, aes(x = date, y=prodh, color="prodh"))+
  geom_line(data= data2, aes(x = date, y=elec, color="elec"))+
  geom_line(data= data2, aes(x = date, y=planstop*20, color="planstop"))
graph5
ggplotly(graph5)

## Creation of the variable faillure

## we make the assumption that when the production is less than 20 and there are no stop planned, there are a faillure.

data2$State = ifelse((data2$prodh <= 20),"Notworking","Normal")
data2$State[which(data2$State == "Notworking")] = ifelse((data2$planstop[which(data2$State ==
                                                                                 "Notworking")] == 0),"Normal","Faillure")
data2$State=as.factor(data2$State)

summary(data2)
(table(data2$State)/nrow(data2))*100

### creation of the variable first faillure
ffail=c()
test=T
for (i in 1:nrow(data2)){
  if (test==T){
    ffail[i]=data2$State[i]
    if (data2$State[i]=="Faillure"){
      test=F
    }
  } else {
    ffail[i]="fix"
    if (data2$State[i]=="Normal") {
      test=T
      ffail[i]=data2$State[i]
    }
    
  }
}
#### we will have two variables :
#### first faillure that consider only the first faillure like and the next one like a normal state
ffail=ifelse(ffail==1,"Faillure",ffail)
ffail=ifelse(ffail==2,"Normal",ffail)
ffail2=ifelse(ffail!="Faillure","Normal",ffail)

data2$ffail=as.factor(ffail)
(table(data2$ffail)/nrow(data2))*100

data2$ffail2=as.factor(ffail2)
(table(data2$ffail2)/nrow(data2))*100


#### for the moment we have only five variables in our data set, lets create other 
###  ones from our data set, that we will use for our modelisations

### Lets create the variable working time wich count the working time of the machine
worktime=c()
cou=0
for (i in 1:nrow(data2)){
  if(data2$State[i]=="Normal"){
    cou=cou+1
    worktime[i]=cou
  }
  else {
    cou=0
    worktime[i]=cou
  }
}

data2$worktime=worktime

### Creation of varible, for consider the production and the electricity variation

## prodh
dd=c(data2$prodh[2:nrow(data2)], NA)
delta= dd-data2$prodh
data2$prodhdelta=c(NA, delta[-nrow(data2)])

## elec
dd=c(data2$elec[2:nrow(data2)], NA)
delta= dd - data2$elec
data2$elecdelta=c(NA, delta[-nrow(data2)])

data2=na.omit(data2)
### Creation of a function, for make a transformation on our variable (log(var+1), sqrt, power2,power3,poly)

transformation = function(vect) {
  vect.log1=log(vect+1)     ## vect+1 because with log we have infinite
  vect.sqrt=sqrt(vect)
  vect.p2 = vect^2
  vect.p3 = vect^3
  vect.poly= poly(vect, degree = 5, simple = T)
  
  return (data.frame(vect.log1,vect.sqrt,vect.p2,vect.p3,vect.poly))
}


### applying the function transformation on prodh and elec
# prodh
pp=transformation(data2$prodh)
colnames(pp)=paste("prodh",colnames(pp),sep = "-")
data2= data.frame(data2,pp)
# elec
pp=transformation(data2$elec)
colnames(pp)=paste("elec",colnames(pp),sep = "-")
data2= data.frame(data2,pp)

####### Decomposition of the date in months and weekdays and adding it like variables
data2$months=as.factor(months(data2$date))
data2$wday= as.factor(weekdays(data2$date))


## a function for new variable from time decalage

varcreation = function(number,data,var) {
  n = length(data[,1])
  k = which(colnames(d1) == var)
  for (i in 1:number) {
    p = c(rep(NA,i), data[,k][-c((n - i + 1):n)])
    data[length(data) + 1] = p
  }
  return (data)
}

d1 = data2



## creation of decaled state
decal=function(var, numb=24){
  n = length(d1)
  numb = 24
  d1 = varcreation(numb,d1,var)
  namm1 = paste(rep(var,numb),c(1:numb),sep = ".")
  colnames(d1)[(n + 1):(n + numb)] <- paste(rep(var,numb),c(1:numb),sep = "..")
  return(d1)
}

tode=colnames(d1)[-c(1)]
for (i in 1:length(tode)){
  pp=decal(tode[i],numb = 24)
  d1=pp
}

## factor probleme management
numb=24
namm1 = paste(rep("State",numb),c(1:numb),sep = "..")
namm2 = paste(rep("ffail",numb),c(1:numb),sep = "..")
namm3 = paste(rep("ffail2",numb),c(1:numb),sep = "..")
namm4 = paste(rep("months",numb),c(1:numb),sep = "..")
namm5 = paste(rep("wday",numb),c(1:numb),sep = "..")
for (i in 1:numb) {
  d1[,which(colnames(d1) == namm1[i])] = as.factor(d1[,which(colnames(d1) == namm1[i])])
  levels(d1[,which(colnames(d1) == namm1[i])])=levels(d1$State)
  
  d1[,which(colnames(d1) == namm2[i])] = as.factor(d1[,which(colnames(d1) == namm2[i])])
  levels(d1[,which(colnames(d1) == namm2[i])])=levels(d1$ffail)
  
  d1[,which(colnames(d1) == namm3[i])] = as.factor(d1[,which(colnames(d1) == namm3[i])])
  levels(d1[,which(colnames(d1) == namm3[i])])=levels(d1$ffail2)
  
  d1[,which(colnames(d1) == namm4[i])] = as.factor(d1[,which(colnames(d1) == namm4[i])])
  levels(d1[,which(colnames(d1) == namm4[i])])=levels(d1$months)
  
  d1[,which(colnames(d1) == namm5[i])] = as.factor(d1[,which(colnames(d1) == namm5[i])])
  levels(d1[,which(colnames(d1) == namm5[i])])=levels(d1$wday)
}

## creation of the working data set

df = d1
df=df[,-c(1:3,8:28)]

df = na.omit(df)

## Using DF for fit our different models

x = df[,-c(2,3,4)]
y1 = df[,2]
y2 = df[,4]
y3 = df[,3]

df=data.frame(y1,y2,y3,x)

pp=lapply(x,as.numeric)
x=as.data.frame(pp)
rownames(x)=rownames(df)

##########################        Descriptives statistics         ##############################
summary(y1)
pie(table(y1),labels = levels(y1), col=rainbow(2),main="All the the faillure proportion")

summary(y2)
pie(table(y2),labels = levels(y1), col=rainbow(2),main="First Faillure Proportion")

summary(y3)
pie(table(y3),labels = levels(y3), col=rainbow(3),main="Faillure and fixig proportion")
###################### Models Fitting Begining #####################################################
## first with y1 then with y2 and finally with y3

### make training and test data set  #############



## making the test and trainning splits
set.seed(1234)
set.seed(24)

ind <- createDataPartition(df$y1, p = .70,list = FALSE,times = 1)
indT= 1:8000
## the predictors
xtrain <- x[ind,]
xtest = x[-ind,]

xtrainT <- x[indT,]
xtestT = x[-indT,]
## for type one
ytrain1 <- y1[ind]
ytest1 <- y1[-ind]

dat1=data.frame(ytrain1,xtrain)
## lets see the proportion
prop.table(table(ytrain1))
prop.table(table(ytest1))

ytrain1T <- y1[indT]
ytest1T <- y1[-indT]

dat1T=data.frame(ytrain1T,xtrainT)
## lets see the proportion
prop.table(table(ytrain1T))
prop.table(table(ytest1T))

### for only the first faillure
ytrain2 <- y2[ind]
ytest2 <- y2[-ind]

dat2=data.frame(ytrain2,xtrain)
## lets see the proportion
prop.table(table(ytrain2))
prop.table(table(ytest2))


ytrain2T <- y2[indT]
ytest2T <- y2[-indT]

dat2T=data.frame(ytrain2T,xtrainT)

## lets see the proportion
prop.table(table(ytrain2T))
prop.table(table(ytest2T))

### Test data frame
dtest1= data.frame(ytest1,xtest)
dtest2= data.frame(ytest2,xtest)
dtest1T = data.frame(ytest1T,xtestT)
dtest2T = data.frame(ytest2T,xtestT)

## creating test data with smote for 
## have more faillures in our dataset and mayde improve our results
library(DMwR)
dfSmote1 <- SMOTE(ytrain1 ~., data = dat1, perc.over = 100, perc.under=200)
prop.table(table(dfSmote1$ytrain1))
## first faillure case
dfSmote2 <- SMOTE(ytrain2 ~., data = dat2, perc.over= 300, perc.under = 300)
prop.table(table(dfSmote2$ytrain2))

dfSmote2T <- SMOTE(ytrain2T ~., data = dat2T, perc.over= 300, perc.under = 300)
prop.table(table(dfSmote2$ytrain2))

######## Setup multinode ####
library(doMC)
registerDoMC(cores = 5)
library(pROC)
###


########################  Some use finction

### 

opthr= function(fittingProba, ytrain) {
  vecthr = seq(from = 0,to = 1,by = 0.01)   ## vector of thresholding
  sensibility = 0
  specificity = 0
  for (i in 1:length(vecthr)) {
    predicted = as.factor(ifelse(fittingProba[1] > vecthr[i],"Faillure","Normal"))
    sensibility[i] = mean(predicted[ytrain == "Faillure"] == "Faillure")
    specificity[i] = mean(predicted[ytrain == "Normal"] == "Normal")
  }
  
  plot(vecthr,sensibility,type = "l",lwd = 3,col = "blue")
  lines(vecthr,specificity,lwd = 3,col = "orange")
  
  plot(1 - specificity,sensibility,type = "l",lwd = 3)
  abline(0,1)
  
  optimalthr = vecthr[which.max(sensibility + specificity)]
  
  points(
    1 - specificity[which.max(sensibility + specificity)],
    sensibility[which.max(sensibility + specificity)],pch = 8,cex = 3,col = "red"
  )
  return(optimalthr)
}


#### variable importance graphique

descnum=function(varimp, max=1:5){
  top=as.data.frame(varimp$importance)
  ord=order(top[,1], decreasing = T)[max]
  topvar=rownames(top)[ord]
  desc=matrix(0,4,length(topvar))
  desc=data.frame(desc)
  colnames(desc)=topvar
  rownames(desc)[c(1,3)]=c("fail","nfail")
  fact=c()
  for (i in 1:length(topvar)){
    if (is.factor(df[,topvar[i]])){
      fact[i]=i
      desc[1,i]=(table(df[which(y2=="Faillure"), topvar[i]])[1])
      desc[2,i]=(table(df[which(y2=="Faillure"), topvar[i]])[2])
      desc[3,i]=(table(df[which(y2!="Faillure"), topvar[i]])[1])
      desc[4,i]=(table(df[which(y2!="Faillure"), topvar[i]])[2])
    }else{
      desc[1,i]= mean(df[which(y2=="Faillure"), topvar[i]])
      desc[3,i]=mean(df[which(y2!="Faillure"), topvar[i]])
      
    }
    
  }
  
  if (is.null(fact)){
    plt=as.data.frame(t(desc[c(1,3),]))
  }else {
    fact=as.numeric(na.omit(fact))
    plt=as.data.frame(t(desc[c(1,3),-fact]))
  }
  
  global=(apply(df[,rownames(plt)],2,mean))
  plt$global=global
  plt$predictor=rownames(plt)
  
  library(reshape2)
  ll<- melt(plt, id.vars = c("predictor"))
  
    graph=ggplot(ll,aes(predictor,value,fill=variable))+
    geom_bar(position="dodge",stat="identity")
  
  return(graph)
}
