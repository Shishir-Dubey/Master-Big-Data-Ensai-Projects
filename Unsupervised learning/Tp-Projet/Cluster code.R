####  UNSUPERVISED LEARNING
install.packages("clue")
install.packages("klaR")
install.packages("flexclust")
install.packages("fpc")
install.packages("RWeka")
install.packages("divclust")
install.packages("partitions")
require(cubt)

library(clue)
library(cluster)
library(e1071)
library(flexclust)
library(klaR)
library(fpc)    ### function dbscan
library(RWeka)  ### function cobweb
library(cubt)
library("partitions")
## First creation of the simulated data set

# simulation of the different cluster
LC<-function(n=300,q=0.8,var=9){
c1=sapply(1:var, function(x) sample(1:5,n/3,T,c(q,rep((1-q)/4,4))))
c2=sapply(1:var, function(x) sample(1:5,n/3,T,c(rep((1-q)/4,2),q,rep((1-q)/4,2))))
c3=sapply(1:var, function(x) sample(1:5,n/3,T,c(rep((1-q)/4,4),q)))
data1=rbind(c1,c2,c3)
y<-c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
mod1<-list(Model=as.data.frame(data1),cluster=y)
return(mod1)
}
image((data.m1()$M1))

### second data set

Model3<-function(n=400){
C1= cbind(sample(c(1,3,5),n/4,T),sample(c(1,3,5),n/4,T),sample(1:6,n/4,T))
C2= cbind(sample(c(1,3,5),n/4,T),sample(c(2,4,5),n/4,T),sample(1:6,n/4,T))
C3= cbind(sample(c(2,4,6),n/4,T),sample(1:6,n/4,T),sample(c(1,3,5),n/4,T))
C4= cbind(sample(c(2,4,6),n/4,T),sample(1:6,n/4,T),sample(c(2,4,6),n/4,T))
data3=rbind(C1,C2,C3,C4)
y<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
mod3<-list(Model=as.data.frame(data3),cluster=y)
return(mod3)
}
image(m3)

### Third DATASET
Model4<-function(n=400,q=0.8){
C1= cbind(sample(c(1,3),n/4,T,prob=c(q,1-q)),sample(c(1,3),n/4,T,prob=c(q,1-q)),sample(1:4,n/4,T))
C2= cbind(sample(c(1,3),n/4,T,prob=c(q,1-q)),sample(c(2,4),n/4,T,prob=c(q,1-q)),sample(1:4,n/4,T))
C3= cbind(sample(c(2,4),n/4,T,c(q,1-q)),sample(1:4,n/4,T),sample(c(1,3),n/4,T,prob=c(q,1-q)))
C4= cbind(sample(c(2,4),n/4,T,c(q,1-q)),sample(1:4,n/4,T),sample(c(2,4),n/4,T,prob=c(q,1-q)))
data4=rbind(C1,C2,C3,C4)
y<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
mod4<-list(Model=as.data.frame(data4),cluster=y)
return(mod4)
}

### computation of the centroids of each cluster for the methods when he doesn't exist
centroids=function(k=3,training.data,result.cluster){
  centroids<-matrix(NA,nrow=k,ncol=length(names(training.data)))
  for (j in 1:k){
       for (i in 1:length(names(training.data) )){
         centroids[j,i]<-which.max(table(training.data[,i][result.cluster==j]))
       }
  }
  return(centroids)
  }
  
#### Computation of the prediction
prediction=function(centroids,datatopredict){
  predicty<-rep(NA,length(datatopredict[,1]))
  err<-rep(NA,length(centroids[,1]))
  
    for(j in 1:length(datatopredict[,1])){
      for (i in 1:length(centroids[,1])){
      err[i]<-sum(datatopredict[j,]!=centroids[i,])
    }
      predicty[j]<-which.min(err)
    }
  return(predicty)
}
###################### Give by Badih Ghattas ##########################
error = function(pred=prev,obs=dd[,1],print=F)
{
  # computes a prediction error
  # uses index defined in our paper
  # proportion of observations not being together within the
  # bigger clusters
  if(length(obs) != length(pred)) stop("obs and pred different length")
  n = length(obs)
  nbcl = length(unique(obs))
  nbclusters = length(unique(pred))
  tab = table(obs,pred)
  if(nbcl <= nbclusters) {
    y = solve_LSAP(tab,maximum=T)
    #print(y)
    tr = sum(tab[cbind(seq_along(y), y)])
    if(print) print(tab)
    res = 1 - (tr / n)
  } else {
    if(nbclusters == 1) {
      res = 1 - (max(tab)/n)
    }else {
      zz= combn(nbcl,nbclusters)
      nn = ncol(zz)
      res = rep(NA,nn)
      for(j in 1:nn)    {
        tabp = tab[zz[,j],]
        y = solve_LSAP(tabp,maximum=T)
        tr = sum(tabp[cbind(seq_along(y), y)])
        if(print) print(tabp)
        res[j] = 1 - (tr / n)
      }
      res = min(res)
    }
  }
  c(res,nbclusters)
}
################################################################################################################
################################################################################################################

bootstr<-function(nboot=20,method,numobs){
 errmatt<-matrix(NA,nrow=nboot,ncol=5)

for(bs in 1:nboot){

if (method==1){
####### generating of working datasets
  training<-LC(n=numobs,q=0.8,var=9)
  test<-LC(n=numobs,q=0.8,var=9)
  training.data<-training$Model
  test.data<-test$Model
  clusY<-training$cluster
  observedY<-test$cluster
  k<-3
} else if (method==2){
####### generating of working datasets
training<-Model3(n=numobs)
test<-Model3(n=numobs)
training.data<-training$Model
test.data<-test$Model
clusY<-training$cluster
observedY<-test$cluster
k<-4
}else if (method==3){
####### generating of working datasets
training<-Model4(n=numobs,q=0.8)
test<-Model4(n=numobs,q=0.8)
training.data<-training$Model
test.data<-test$Model
clusY<-training$cluster
observedY<-test$cluster
k<-4
}
############################################################
## With the function Agnes present in the package cluster ##
############################################################
result.agnes=agnes(training.data,method="ward")
                                                     #plot(result.agnes)
cluster.agnes=cutree(result.agnes,h=25,k=k)

## finding the center of this methods
center.<-centroids(k,training.data=training.data,result.cluster=cluster.agnes)
## Predicting the clusters' of each data set
predict.y<-prediction(center,test.data)
Agnes=error(pred = predict.y, obs = observedY,print = F )[1]

#########################################################
# With functions available from basic installation of R #
#########################################################
## by using hclust function ward method
distance=dist(training.data, "manhattan")
obs=row.names(training.data)
result.hclust=hclust(distance,method="ward.D")

#plot(result.hclust,labels=obs,ylab="Distance",main="Dendrogram")

# To obtain a certain level clustering
cluster.hclust<-cutree(result.hclust,k=k)

center.ward<-centroids(k,training.data,cluster.hclust)
predict.y<-prediction(centroids = center.ward,datatopredict=test.data)

HCA<-error(predict.y,observedY)[1]

#########################################################
# With the Kmode present in the package klar #
#########################################################
result.kmodes<-kmodes(training.data,k,iter.max = 10)
obs=row.names(training.data)

#plot(jitter(as.matrix(training.data)), col=result.kmodes$cluster)
#points(result.kmodes$modes, col = 1:5, pch = 8)

##plot(training.data,col=(result.kmodes$cluster+1),pch=20,cex=2)
centers<-result.kmodes$modes

predict.y<-prediction(centroids = centers,datatopredict = test.data)

K_mode<-error(predict.y,observedY)[1]

#########################################################
# With the DSCAN present in the package fpc #
#########################################################
result.dbscan<-dbscan(training.data, eps=3.5, method = "raw", MinPts = 55)

predict.y<-predict(result.dbscan,training.data,test.data)

DBSCAN<-error(predict.y,observedY)[1]

#plot(jitter(training.data), col=result.dbscan$cluster)
#print.dbscan(result.dbscan,training.data)
#########################################################
# With the KCCA function present in the package flexclust#
#########################################################

result.median<-kcca(training.data,k = k,family = kccaFamily("kmedians"))
kmed.cluster<-slot(result.median,"cluster")
predict.y<-predict(result.median,newdata=test.data)

K_median<-error(predict.y,observedY)[1]



errmatt[bs,]<-c(Agnes,HCA,K_mode,DBSCAN,K_median)

}
MPE<-colMeans(errmatt)
names(MPE)<-c("Agnes","HCA","K_mode","DBSCAN","K_median")
return(MPE)
}

#########################################################
# With the CUBT present in the package fpc  #
#########################################################

result.cubt<-cubt(training.data,methods="entropy",minsplit=100)
prune.cubt(arbr)
join.cubt(prun)
Where(join)


