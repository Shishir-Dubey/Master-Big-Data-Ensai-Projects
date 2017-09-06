##########     SVM radial      #########################
## fitting the control parameter
ctrl <- trainControl(method = "cv", number = 2, classProbs = TRUE)
svm.radial=function(xd,yd, ctrl){
  registerDoMC(cores = 5)
  model <- train(x=xd,y=yd,
                 method = "svmRadial",
                 trControl = ctrl,
                 preProc = c("center", "scale"),
                 metrics="ROC"
  )
  return (model)
}

########## the first case machine learning approches ####
##### Predicting all the faillures state
####
ytr=ytrain1
ytest=ytest1
xtr=xtrain
xte=xtest
# fitting the model svm
model=svm.radial(xtr,ytr,ctrl)
# seing the performance choosing the best threshold
fittingProba<-predict(model, xtr ,type = "prob")
optimalthr=opthr(fittingProba = fittingProba, ytr)
# predicting

predProba<-predict(model, xte, type = "prob")
pred = as.factor(ifelse(predProba[1] > optimalthr,"Faillure","Normal"))
sensibility = mean(pred[ytest == "Faillure"] == "Faillure")
specificity = mean(pred[ytest == "Normal"] == "Normal")
table(pred,ytest)
roc(as.numeric(ytest), as.numeric(pred))
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp)
ggplotly()

#### Only the first faillure prediction
ytr=ytrain2
ytest=ytest2
xtr=xtrain
xte=xtest
# fitting the model svm
model=svm.radial(xtr,ytr,ctrl)
# seing the performance choosing the best threshold
fittingProba<-predict(model, xtr ,type = "prob")
optimalthr=opthr(fittingProba = fittingProba, ytr)
# predicting

predProba<-predict(model, xte, type = "prob")
pred = as.factor(ifelse(predProba[1] > optimalthr,"Faillure","Normal"))
sensibility = mean(pred[ytest == "Faillure"] == "Faillure")
specificity = mean(pred[ytest == "Normal"] == "Normal")
table(pred,ytest)
roc(as.numeric(ytest), as.numeric(pred))
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp)
ggplotly()
############ second approch in case of time slice ####

#### Predicting all the faillures

# fitting the model
ytr=ytrain1T
ytest=ytest1T
xtr=xtrainT
xte=xtestT
# fitting the model svm
model=svm.radial(xtr,ytr,ctrl)
# seing the performance choosing the best threshold
fittingProba<-predict(model, xtr ,type = "prob")
optimalthr=opthr(fittingProba = fittingProba, ytr)
# predicting

predProba<-predict(model, xte, type = "prob")
pred = as.factor(ifelse(predProba[1] > optimalthr,"Faillure","Normal"))
sensibility = mean(pred[ytest == "Faillure"] == "Faillure")
specificity = mean(pred[ytest == "Normal"] == "Normal")
table(pred,ytest)
roc(as.numeric(ytest), as.numeric(pred))
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp)
ggplotly()
# the second case with the first faillure ###
ytr=ytrain2T
ytest=ytest2T
xtr=xtrainT
xte=xtestT
# fitting the model svm
model=svm.radial(xtr,ytr,ctrl)
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
roc(as.numeric(ytest), as.numeric(pred))
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp)
ggplotly()

###### third approch with smote data #####
##################################"

### machine learning approches

#### Prediction of the first faillure
ytr=dfSmote2$ytrain2
ytest=ytest2
xtr=dfSmote2[,-1]
xte=xtest
# fitting the model svm
model=svm.radial(xtr,ytr,ctrl)
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
roc(as.numeric(ytest), as.numeric(pred))
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp)
ggplotly()

###### time slice
#### Prediction of the first faillure
ytr=dfSmote2T$ytrain2T
ytest=ytest2T
xtr=dfSmote2T[,-1]
xte=xtestT
# fitting the model svm
model=svm.radial(xtr,ytr,ctrl)
# seing the performance choosing the best threshold
fittingProba<-predict(model, xtr ,type = "prob")
optimalthr=opthr(fittingProba = fittingProba, ytr)
# predicting

predProba<-predict(model, xte, type = "prob")
pred = as.factor(ifelse(predProba[1] > 0.05,"Faillure","Normal"))
sensibility = mean(pred[ytest == "Faillure"] == "Faillure")
specificity = mean(pred[ytest == "Normal"] == "Normal")
table(pred,ytest)
mean(pred==ytest)
roc(as.numeric(ytest), as.numeric(pred))
sensibility
specificity

## the variables importance
varimp <- varImp(model, scale = FALSE)
plot(varimp, top=20)
descnum(varimp = varimp)
ggplotly()
