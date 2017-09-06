##########     gbm      #########################
library(gbm)
ctrl = trainControl( method = "cv", number = 2, classProbs = TRUE)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5), n.trees = c(1,15,30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbm.model=function(xd,yd, ctrl){
  registerDoMC(cores = 5)
  model <- train(x=xd,y=yd,
                 method = "gbm",
                 trControl = ctrl,
                 tuneGrid=gbmGrid
  )
  return (model)
}


########## the first case machine learning approches ####


#### Only the first faillure prediction
ytr=ytrain2
ytest=ytest2
xtr=xtrain
xte=xtest
# fitting the model Stochastique gbm
model=gbm.model(xtr,ytr,ctrl)
# seing the performance choosing the best threshold
fittingProba<-predict(model, xtr ,type = "prob")
optimalthr=opthr(fittingProba = fittingProba, ytr)
# predicting

predProba<-predict(model, xte, type = "prob")
pred = as.factor(ifelse(predProba[1] > optimalthr,"Faillure","Normal"))
sensibility = mean(pred[ytest == "Faillure"] == "Faillure")
specificity = mean(pred[ytest == "Normal"] == "Normal")
table(pred,ytest)
mean(pred==ytest)
auc=roc(as.numeric(ytest), as.numeric(pred))
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)), bg="white")
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp, max=c(1:10))
ggplotly()


#### smote data Prediction of the first faillure
ytr=dfSmote2$ytrain2
ytest=ytest2
xtr=dfSmote2[,-1]
xte=xtest
# fitting the model Stochastique gbm
model=gbm.model(xtr,ytr,ctrl)
# seing the performance choosing the best threshold
fittingProba<-predict(model, xtr ,type = "prob")
optimalthr=opthr(fittingProba = fittingProba, ytr)
# predicting

predProba<-predict(model, xte, type = "prob")
pred = as.factor(ifelse(predProba[1] > optimalthr,"Faillure","Normal"))

sensibility = mean(pred[ytest == "Faillure"] == "Faillure")
specificity = mean(pred[ytest == "Normal"] == "Normal")
table(pred,ytest)
mean(pred==ytest)
auc=roc(as.numeric(ytest), as.numeric(pred))
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)), bg="white")
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp)
ggplotly()


############ second approch in case of time slice ####

#### Predicting all the faillures
# the second case with the first faillure ###
ytr=ytrain2T
ytest=ytest2T
xtr=xtrainT
xte=xtestT
# fitting the model Stochastique gbm
model=gbm.model(xtr,ytr,ctrl)
# seing the performance choosing the best threshold
fittingProba<-predict(model, xtr ,type = "prob")
optimalthr=opthr(fittingProba = fittingProba, ytr)
# predicting

predProba<-predict(model, xte, type = "prob")
pred = as.factor(ifelse(predProba[1] > optimalthr,"Faillure","Normal"))
sensibility = mean(pred[ytest == "Faillure"] == "Faillure")
specificity = mean(pred[ytest == "Normal"] == "Normal")
table(pred,ytest)
mean(pred==ytest)
auc=roc(as.numeric(ytest), as.numeric(pred))
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)), bg="white")
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp, max=1:10)
ggplotly()
###### time slice
#### Prediction of the first faillure # approch with smote data
ytr=dfSmote2T$ytrain2T
ytest=ytest2T
xtr=dfSmote2T[,-1]
xte=xtestT
# fitting the model Stochastique gbm
model=gbm.model(xtr,ytr,ctrl)
# seing the performance choosing the best threshold
fittingProba<-predict(model, xtr ,type = "prob")
optimalthr=opthr(fittingProba = fittingProba, ytr)
# predicting

predProba<-predict(model, xte, type = "prob")
pred = as.factor(ifelse(predProba[1] > optimalthr,"Faillure","Normal"))
sensibility = mean(pred[ytest == "Faillure"] == "Faillure")
specificity = mean(pred[ytest == "Normal"] == "Normal")
table(pred,ytest)
mean(pred==ytest)
auc=roc(as.numeric(ytest), as.numeric(pred))
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)), bg="white")
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp)
ggplotly()