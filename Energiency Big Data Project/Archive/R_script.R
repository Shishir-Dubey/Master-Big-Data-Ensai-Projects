### Installation of packages and loading them ###########################
#########################################################################

#### Installation of necessary packages, need to do it only one time

#install.packages("corrplot")
# install.packages("caret")
#install.packages("randomForest")
# install.packages("MASS")
# install.packages("rpart")
# install.packages("e1071")
#install.packages("glmnet")

#### Loading the necessary packages

library("MASS")
library("rpart")
library("randomForest")
library("e1071")
library("glmnet")

##############################################################################
####################          set The directory           ####################
##############################################################################

setwd("/home/moustapha/Energiency Big Data Project/Archive")

## loading the data set resampled in one hour interval
##

data = read.table("all1h.csv", header = TRUE, sep = ",")
data$date = as.Date(data$X)

# Creation of the binomial stop
data$planstopd = ifelse(data$planstop1h == 1,1,0)

# creating the same  for the gram1h data
data$gram = ifelse(data$gram1h > 100,(data$gram1h) / 10000,data$gram1h)

##
summary(data)

## selecting the Data set containing only the complete plan stop
nomissing = data[which(is.na(data$planstop1h) == F),]
summary(nomissing)

## some plotting
plot(nomissing$prodh1h, type = "l", col = "blue", lwd = 2)
lines(nomissing$elec1h, type = "l", col = "green")
lines(nomissing$planstop1h, type = "l", lwd = 3)
lines(nomissing$planstopd, type = "l", col = "red", lwd = 3)
plot(nomissing$gram, type = "l")

## According To the plot we wiil consider the data till 2015-07-30 20:00:00+00:00

data1 = nomissing[1:which(nomissing$X == "2015-07-30 20:00:00"),]

## some plotting
plot(data1$prodh1h, type = "l", col = "blue", lwd = 2)
lines(glmprob*30, type = 'l')

lines(data1$elec1h, type = "l", col = "green")
lines(data1$planstop1h, type = "l", lwd = 3)
lines(data1$planstopd, type = "l", col = "red", lwd = 3)
plot(data1$gram, type = "l")

## Creation of the variable faillure

## we make the assumption that when the production is less than 5, there are a faillure.

data1$State = ifelse((data1$prodh1h <= 20),"Notworking","Normal")
data1$State[which(data1$State == "Notworking")] = ifelse((data1$planstopd[which(data1$State ==
                                                                                  "Notworking")] == 0),"Normal","Faillure")

(summary(as.factor(data1$State)) / length(data1$X)) * 100

#data1$State<-as.factor(data1$State)

## a function for new variable



varcreation = function(number,data,var) {
  n = length(data[,1])
  k = which(colnames(d1) == var)
  for (i in 1:number) {
    p = c(rep(NA,i), data[,k][-c((n-i+1):n)])
    data[length(data) + 1] = p
  }
  #colnames(data)[(n + 1):(n+number)] <- paste(rep(var,number),c(1:number),sep = ".")
  return (data)
}

d1 = data1

## creation of decaled state
n = length(d1)
number = 24
var = "State"
d1 = varcreation(number,d1,var)
namm1 = paste(rep(var,number),c(1:number),sep = ".")
colnames(d1)[(n + 1):(n + number)] <-
  paste(rep(var,number),c(1:number),sep = ".")
for (i in 1:number) {
  d1[,which(colnames(d1) == namm1[i])] = as.factor(d1[,which(colnames(d1) ==
                                                               namm1[i])])
}


## creation of decaled production
n = length(d1)
number = 24
var = "prodh1h"
d1 = varcreation(number,d1,var)
namm2 = paste(rep(var,number),c(1:number),sep = ".")
colnames(d1)[(n + 1):(n + number)] <-
  paste(rep(var,number),c(1:number),sep = ".")
for (i in 1:number) {
  d1[,which(colnames(d1) == namm2[i])] = as.numeric(d1[,which(colnames(d1) ==
                                                                namm2[i])])
}

## creation of decaled electricity
n = length(d1)
number = 24
var = "elec1h"
d1 = varcreation(number,d1,var)
namm3 = paste(rep(var,number),c(1:number),sep = ".")
colnames(d1)[(n + 1):(n + number)] <-
  paste(rep(var,number),c(1:number),sep = ".")
for (i in 1:number) {
  d1[,which(colnames(d1) == namm3[i])] = as.numeric(d1[,which(colnames(d1) ==
                                                                namm3[i])])
}

## creation of the working data set

df = d1
rownames(df) = df$X
df = df[,10:ncol(df)]

df = na.omit(df)

## Using DF for fit our different models
x = df[,-1]
y = as.factor(df[,1])


##########################        Descriptives statistics         ##############################

# pairs(df[,(26:32)], col=y)
# pairs(df[,(33:39)], col=y)
# pairs(df[,(40:46)], col=y)

plot(df[,2:5])

###################### Models Fitting Begining ############################

############### Logistic Model  glm  #########################
### make training and test data set  #############
##
n=length(y)
ind <- sample(1:n,(n / 3)*2)
x.cal <- x[ind,]
y.cal <- y[ind]
x.test = x[-ind,]
y.test <- y[-ind]
##
glmmod=glm(y.cal~.,data = x.cal, family = binomial)
glmprob=predict(glmmod,newdata=x.cal,type = "response")
glmpred=ifelse(glmprob<0.95,"Faillure","Normal")
##
table(glmpred,y.cal)
mean(as.factor(glmpred)==y.cal)

############### Specificity, sensibility, roc curve

vecthr = seq(from=0,to=1,by=0.001)   ## vector of thresholding
sensibility=0
specificity=0
for(i in 1:length(vecthr)) {
  
  predicted = as.factor(ifelse(glmprob<vecthr[i],"Faillure","Normal"))
  sensibility[i] = mean(predicted[y.cal=="Faillure"]=="Faillure")
  specificity[i] = mean(predicted[y.cal=="Normal"]=="Normal")
}

plot(vecthr,sensibility,type = "l",lwd=3,col="blue")
lines(vecthr,specificity,lwd=3,col="orange")

plot(1-specificity,sensibility,type="l",lwd=3)
abline(0,1)

optimalthr = vecthr[which.max(sensibility+specificity)]

points(1-specificity[which.max(sensibility+specificity)],sensibility[which.max(sensibility+specificity)],pch=8,cex=3,col="red")

probatest = predict(glmmod,type="response",newdata = x.test)
predicted = as.factor(ifelse(probatest<=optimalthr,"Faillure","Normal"))

sensibility = mean(predicted[y.test=="Faillure"]=="Faillure")
specificity = mean(predicted[y.test=="Normal"]=="Normal")

table(predicted,y.test)
mean(predicted==y.test)
sensibility
specificity

lines(data1$prodh1h, type = "l", col = "blue", lwd = 2)
plot(glmprob*40)
plot(predicted,y.test)

lines(data1$elec1h, type = "l", col = "green")
lines(data1$planstop1h, type = "l", lwd = 3)

########################## Support vector machine
n = length(y)
ind <- sample(1:n,(n / 3)*2)
#ind=1:3500
x.cal <- x[ind,]
y.cal <- y[ind]
x.test = x[-ind,]
y.test <- y[-ind]
## Making the ranking on the first part of Data
svmresult <- svm(y.cal ~ ., data = x.cal, probability=T, 
                 method = "C-classification", 
                 kernel = "radial",
                 cost = 10, gamma = 0.05)

svm.proba = predict(svmresult,x.cal, probability = T)
svmpred=predict(svmresult,x.cal, type="class")
svm.proba=attr(svm.proba, "probabilities")

prediction=data.frame(svmpred,svm.proba)
##
table(svmpred,y.cal)
mean(svmpred==y.cal)

############### Specificity, sensibility, roc curve

vecthr = seq(from=0,to=1,by=0.001)   ## vector of thresholding
sensibility=0
specificity=0
for(i in 1:length(vecthr)) {
  
  predicted = as.factor(ifelse(svm.proba[,1]<vecthr[i],"Faillure","Normal"))
  sensibility[i] = mean(predicted[y.cal=="Faillure"]=="Faillure")
  specificity[i] = mean(predicted[y.cal=="Normal"]=="Normal")
}

plot(vecthr,sensibility,type = "l",lwd=3,col="blue")
lines(vecthr,specificity,lwd=3,col="orange")

plot(1-specificity,sensibility,type="l",lwd=3)
abline(0,1)

optimalthr = vecthr[which.max(sensibility+specificity)]

points(1-specificity[which.max(sensibility+specificity)],sensibility[which.max(sensibility+specificity)],pch=8,cex=3,col="red")


svm.proba = predict(svmresult,x.test, probability = T)
svm.proba=attr(svm.proba, "probabilities")
predicted = as.factor(ifelse(svm.proba[,1]<=optimalthr,"Faillure","Normal"))



sensibility = mean(predicted[y.test=="Faillure"]=="Faillure")
specificity = mean(predicted[y.test=="Normal"]=="Normal")

table(predicted,y.test)
mean(predicted==y.test)
sensibility
specificity


############################## Randomforest ###############

rfmod=randomForest(x.cal,y.cal, ntree = 1000, importance = T, do.trace = T)

predicted=predict(rfmod,x.test, type = "class")

sensibility = mean(predicted[y.test=="Faillure"]=="Faillure")
specificity = mean(predicted[y.test=="Normal"]=="Normal")

table(predicted,y.test)
mean(predicted==y.test)
sensibility
specificity


############################ Bayesian Model #########################

## as for the svm even if globally the model is very good 
## it appears that the prediction of faillure state is very bad 

############################ Boosting ###############################




### Function for implement five features selection test
## y vector of variable to predict
## x matrix or dataframe of explicative variables
## p number of columns in x
test5 = function(y,x,p) {
  colnames(x) <- (1:p)
  #######implementation of the t test for get the most releavent varaibles
  #######implementation of the fds test for get the most releavent variables
  fds <- 1:p
  for (i in 1:p) {
    mon <- as.numeric(mean(x[y == 0,i]))
    sdn <- as.numeric(sd(x[y == 0,i]))
    mop <- as.numeric(mean(x[y == 1,i]))
    sdp <- as.numeric(sd(x[y == 0,i]))
    fds[i] <- abs((mop - mon) / (sdn + sdp))
  }
  ofds <- order(fds, decreasing = TRUE)
  ##### Appliying of the decision tree for see the most important variables
  rpartresult <- rpart(y ~ ., as.data.frame(x))
  orpart <- as.numeric(names(rpartresult$variable.importance))
  ##### Appliying of the random forest for see the most important variables
  randfo <- randomForest(x, as.factor(y),importance = T)
  randfo$importance
  MeanDA <- randfo$importance[,3]
  Gini <- randfo$importance[,4]
  orandfo <- order(MeanDA, decreasing = T)
  #### Applying of the support vector machine ####
  colnames(x) <- 1:p
  svmresult <- svm(y ~ .,data = x)
  w = t(svmresult$coefs) %*% svmresult$SV
  ranksvm <- w * w
  osvm = order(ranksvm[1:p], decreasing = T)
  #### making the result in a same vector
  result <- matrix(nrow = p,ncol = 5)
  result[,1] <- 'NA'
  result[,2] <- ofds
  for (i in 1:length(orpart)) {
    result[i,3] <- orpart[i]
  }
  result[,4] <- orandfo
  result[,5] <- osvm
  colnames(result) <- c("T-test","FDS","CART","RandomForest","SVM")
  return(result)
}

test5(y,x,ncol(x))


SVMerrorrate = function(x,y,p = 25) {
  # The function take in parameter the number of observation n and the number of variable p
  ## splitting Data set in two
  n = length(y)
  ind <- sample(1:n,n / 3)
  x1 <- x[ind,]
  y1 <- y[ind]
  x2 = x[-ind,]
  y2 <- y[-ind]
  ## Making the ranking on the first part of Data
  svmresult <- svm(y1 ~ .,data = x1)
  predict(svmresult,x2,type = "class")
  w = t(svmresult$coefs) %*% svmresult$SV
  ranksvm <- w * w
  osvm = order(ranksvm[1:ncol(x)], decreasing = T)
  errormatrix <- matrix(nrow = 50,ncol = p)
  for (i in 1:50) {
    ind2 <- sample(1:length(x2[,1]),length(x2[,1]) / 3)
    validation <- x2[ind2,]
    yval <- y2[ind2]
    calibration <- x2[-ind2,]
    ycal <- y2[-ind2]
    for (k in 2:p) {
      loopsvmresult <- svm(ycal ~ .,data = calibration[,osvm[1:k]])
      ypred = predict(loopsvmresult,validation[,osvm[1:k]],type = "class")
      errormatrix[i,k] = mean(yval != ypred)
    }
  }
  errorvec <- colMeans(errormatrix)
  return(errorvec)
}
## computation of the error rate with SVM model whith 25 and 40 variables

g25 <- SVMerrorrate(x,y,25)
g40 <- SVMerrorrate(x,y,40)
## plotting the error rate with SVM model when n=50,100 and 500 for p=200
plot(
  g25,type = "l",col = "blue", ylab = "Taux d'erreur", xlab = "", main = "25 variables"
)


plot(
  g40,type = "l",col = "green", ylab = "Taux d'erreur", xlab = "", main =
    "40 variables "
)
## choosing the number of optimal varibles with SVM model when n=50,100 and 500 for p=200
which.min(g25)
which.min(g40)

### Cross validation with svm