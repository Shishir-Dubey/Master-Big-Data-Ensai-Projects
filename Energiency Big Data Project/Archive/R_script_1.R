fitting<-predict(model, xtrain)
fittingProba<-predict(model, xtrain, type = "prob")

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
  1 - specificity[which.max(sensibility + specificity)],sensibility[which.max(sensibility +
                                                                                specificity)],pch = 8,cex = 3,col = "red"
)

return(optimalthr)

}
































################# Logistic Model  glm  #########################
glmmod = glm(y.cal ~ .,data = x.cal, family = binomial)

glmprob = predict(glmmod,newdata = x.cal,type = "response")

glmpred = ifelse(glmprob < 0.95,"Faillure","Normal")

summary(glmmod)
##
table(glmpred,y.cal)
mean(as.factor(glmpred) == y.cal)

############### Specificity, sensibility, roc curve

vecthr = seq(from = 0,to = 1,by = 0.01)   ## vector of thresholding
sensibility = 0
specificity = 0
for (i in 1:length(vecthr)) {
  predicted = as.factor(ifelse(glmprob < vecthr[i],"Faillure","Normal"))
  sensibility[i] = mean(predicted[y.cal == "Faillure"] == "Faillure")
  specificity[i] = mean(predicted[y.cal == "Normal"] == "Normal")
}

plot(vecthr,sensibility,type = "l",lwd = 3,col = "blue")
lines(vecthr,specificity,lwd = 3,col = "orange")

plot(1 - specificity,sensibility,type = "l",lwd = 3)
abline(0,1)

optimalthr = vecthr[which.max(sensibility + specificity)]

points(
  1 - specificity[which.max(sensibility + specificity)],sensibility[which.max(sensibility +
                                                                                specificity)],pch = 8,cex = 3,col = "red"
)

probatest = predict(glmmod,type = "response",newdata = x.test)
predicted = as.factor(ifelse(probatest <= 0.97,"FFAIL","Normal"))

sensibility = mean(predicted[y.test == "FFAIL"] == "FFAIL")
specificity = mean(predicted[y.test == "Normal"] == "Normal")

table(predicted,y.test)
mean(predicted == y.test)
sensibility
specificity
########################## Support vector machine  ##################################
n = length(y1)
ind <- sample(1:n,(n / 3) * 2)
#ind=1:3500
x.cal <- x[ind,]
y.cal <- y1[ind]
x.test = x[-ind,]
y.test <- y1[-ind]
## Making the ranking on the first part of Data
svmresult <- svm(
  y.cal ~ ., data = x.cal, probability = T,
  method = "C-classification",
  kernel = "radial",
  cost = 10, gamma = 0.05
)

### most important variables in the model
w = t(svmresult$coefs) %*% svmresult$SV
ranksvm <- w * w
osvm = order(ranksvm[1:100], decreasing = T)
colnames(x.cal)[osvm]
####

svm.proba = predict(svmresult,x.cal, probability = T)
svmpred = predict(svmresult,x.cal, type = "class")
svm.proba = attr(svm.proba, "probabilities")

prediction = data.frame(svmpred,svm.proba)
##
table(svmpred,y.cal)
mean(svmpred == y.cal)

############### Specificity, sensibility, roc curve

vecthr = seq(from = 0,to = 1,by = 0.01)   ## vector of thresholding
sensibility = 0
specificity = 0
for (i in 1:length(vecthr)) {
  predicted = as.factor(ifelse(svm.proba[,1] < vecthr[i],"Faillure","Normal"))
  sensibility[i] = mean(predicted[y.cal == "Faillure"] == "Faillure")
  specificity[i] = mean(predicted[y.cal == "Normal"] == "Normal")
}

plot(vecthr,sensibility,type = "l",lwd = 3,col = "blue")
lines(vecthr,specificity,lwd = 3,col = "orange")

plot(1 - specificity,sensibility,type = "l",lwd = 3)
abline(0,1)

optimalthr = vecthr[which.max(sensibility + specificity)]

points(
  1 - specificity[which.max(sensibility + specificity)],sensibility[which.max(sensibility +
                                                                                specificity)],pch = 8,cex = 3,col = "red"
)


svm.proba = predict(svmresult,x.test, probability = T)
svm.proba = attr(svm.proba, "probabilities")
predicted = as.factor(ifelse(svm.proba[,1] <= optimalthr,"Faillure","Normal"))



sensibility = mean(predicted[y.test == "Faillure"] == "Faillure")
specificity = mean(predicted[y.test == "Normal"] == "Normal")

table(predicted,y.test)
mean(predicted == y.test)
sensibility
specificity

############################## Randomforest ###############




  




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