#install.packages("klaR")   ##kmodes
library(klaR)

#install.packages("combinat") ## kcca  
library(combinat)

#install.packages("clue")
library(clue)

#install.packages("RWeka")
library(RWeka)


#install.packages("flexclust")
library(flexclust) 

#install.packages("RWekajars")
library(RWekajars)

#install.packages("rJava")
library(rJava)

#install.packages("cubt")
library(cubt)


#####################################################
#####################################################

# m of levels
# q prob
# n number of observations

LC=function(n=300,p=9,l=5,q=0.9){
  
cluster1=sapply(c(1:p),function(x) sample(c(1:l),prob=c(q,rep((1-q)/(l-1),(l-1))),T,size=n/3)) 
cluster2=sapply(c(1:p),function(x) sample(c(3,1:l)[-4],prob=c(q,rep((1-q)/(l-1),(l-1))),T,size=n/3))
cluster3=sapply(c(1:p),function(x) sample(c(5,1:l)[-6],prob=c(q,rep((1-q)/(l-1),(l-1))),T,size=n/3))
  
cluster=rep(1,n/3)
dataset=rbind(cbind(cluster1,cluster),cbind(cluster2,2),cbind(cluster3,3))
#dataset=dataset[sample(1:n,n),]

}

M2=function(n=300,l=6){
  
cluster1=sapply(c(1:3),function(x) 
if(x==1 || x==2){ sample(seq(from=1,to=l,by=2),prob=rep(2/l,trunc((l+1)/2)),T,size=n/4) }
else if (x==3) {sample(c(1:l),prob=rep(1/l,l),T,size=n/4)} )

cluster2=sapply(c(1:3),function(x) 
if(x==2){ sample(seq(from=2,to=l,by=2),prob=rep(2/l,trunc(l/2)),T,size=n/4) }                   ## creer vecteur 1 pour cluster 2
else if (x==1) {sample(seq(from=1,to=l,by=2),prob=rep(2/l,trunc((l+1)/2)),T,size=n/4)}    ## creer vecteur 2 pour cluster 2
else if (x==3){sample(c(1:l),prob=rep(1/l,l),T,size=n/4)} )                                              ## creer vecteur 3 pour cluster 2
  
cluster3=sapply(c(1:3),function(x) 
if(x==1){ sample(seq(from=2,to=l,by=2),prob=rep(2/l,trunc(l/2)),T,size=n/4) }
else if (x==3) {sample(seq(from=1,to=l,by=2),prob=rep(2/l,trunc((l+1)/2)),T,size=n/4)}
else if (x==2){sample(c(1:l),prob=rep(1/l,l),T,size=n/4)} )
  
cluster4=sapply(c(1:3),function(x) 
if(x==1 || x==3){ sample(seq(from=2,to=l,by=2),prob=rep(2/l,trunc(l/2)),T,size=n/4) } ## creer vecteur 1 et 2 pour cluster 1
else if (x==2) {sample(c(1:l),prob=rep(1/l,l),T,size=n/4)} )                                   ## creer vecteur 3 pour cluster 1
  
cluster=rep(1,n/4)
dataset=rbind(cbind(cluster1,cluster),cbind(cluster2,2),cbind(cluster3,3),cbind(cluster4,4))
#dataset=dataset[sample(1:n,n),]  

}
X=M2(100,6)
X=M3(100,6,0.8)

M3=function(n=300,m=6,p=0.8){
  
cluster1=sapply(c(1:3),function(x) 
if(x==1 || x==2){ sample(seq(from=1,to=m,by=2),prob=c(p,rep(((1-p)/(trunc((m+1)/2))),trunc((m+1)/2)-1)),T,size=n/4) }
else if (x==3) {sample(c(1:m),prob=rep(1,m),T,size=n/4)} )
  
cluster2=sapply(c(1:3),function(x) 
if(x==2){sample(seq(from=2,to=m,by=2),prob=c(p,rep(((1-p)/(trunc((m+1)/2))),trunc((m+1)/2)-1)),T,size=n/4)}            ## creer vecteur 1 pour cluster 2
else if (x==1){sample(seq(from=1,to=m,by=2),prob=c(p,rep(((1-p)/(trunc((m+1)/2))),trunc((m+1)/2)-1)),T,size=n/4) }                   ## creer vecteur 2 pour cluster 2
else if (x==3){sample(c(1:m),prob=rep(1,m),T,size=n/4)} )                                              ## creer vecteur 3 pour cluster 2
  
cluster3=sapply(c(1:3),function(x) 
if(x==1){ sample(seq(from=2,to=m,by=2),prob=c(p,rep(((1-p)/(trunc((m+1)/2))),trunc((m+1)/2)-1)),T,size=n/4) }
else if (x==3) {sample(seq(from=1,to=m,by=2),prob=c(p,rep(((1-p)/(trunc((m+1)/2))),trunc((m+1)/2)-1)),T,size=n/4)}
else if (x==2){sample(c(1:m),prob=rep(1,m),T,size=n/4)} )
  
cluster4=sapply(c(1:3),function(x) 
if(x==1 || x==3){ sample(seq(from=2,to=m,by=2),prob=c(p,rep(((1-p)/(trunc((m+1)/2))),trunc((m+1)/2)-1)),T,size=n/4) } ## creer vecteur 1 et 2 pour cluster 1
else if (x==2) {sample(c(1:m),prob=rep(1,m),T,size=n/4)} )                                   ## creer vecteur 3 pour cluster 1
  
cluster=rep(1,n/4)
dataset=rbind(cbind(cluster1,cluster),cbind(cluster2,2),cbind(cluster3,3),cbind(cluster4,4))
#dataset=dataset[sample(1:n,n),]
  
}

###################

error = function(pred=prev,obs=dd[,1],print=F) {
  # computes a prediction error
  # uses index defined in our paper
  # proportion of observations not being together within the
  # bigger clusters
  if(length(obs) != length(pred)) {stop("obs and pred different length")}
  n = length(obs)
  nbcl = length(unique(obs))
  nbclusters = length(unique(pred))
  tab = table(obs,pred)
  if(nbcl <= nbclusters) {
    y = solve_LSAP(tab,maximum=T)
    #print(y)
    tr = sum(tab[cbind(seq_along(y), y)])
    ##if(print){ print(tab) }
    res = 1 - (tr / n)
  } else if(nbcl > nbclusters){
    if(nbclusters == 1) {
      res = 1 - (max(tab)/n)
    }else{
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
  return(res=c(res,nbclusters))
}

#######################################

manhattan_distance=function(x,y){
  if(length(x) != length(y)) {stop("x and y different length")}
  return(sum(abs(x-as.numeric(y))))
}

simple_matching_distance=function(x,y){
    if(length(x) != length(y)) {stop("x and y different length")}
  return(length(which(x!=y))/length(x))
}

##################
##################

q=0.8
l=6
k_cluster=4
compare=function(n=300,p=9,l=5,q=0.8,b=200,which_data=1,k_cluster=3){
  
  ####### vector which stocks the miss classification rate and prediction error rate over b samples
  miss_classification_rate_k_modes=c()
  prediction_error_k_modes=c()
  
  miss_classification_rate_k_medians=c()
  prediction_error_k_medians=c()
  
  miss_classification_rate_cubt=c()
  prediction_error_cubt=c()
  
  ######## starts bootstrap loop 
  for(i in 1:b){
    
    ######## create a training and a test sample according to the selected model
    if(which_data==1){
      X_test=LC(n,p,l,q)
      X_training=LC(n,p,l,q)
    }else if (which_data==2){
      X_test=M2(n,l)
      X_training=M2(n,l)
    }else{
      X_test=M3(n,m=l,p=q)
      X_training=M3(n,m=l,p=q)
    }
    
    ######## to avoid dimension issues
    n=nrow(X_training)
    p=ncol(X_training)-1
    
    ####### k_modes
    mod_k_modes=kmodes(X_training[,1:p],k_cluster)
    cluster_centroids=mod_k_modes$modes
    nb_Cluster=nrow(cluster_centroids)
    
    ####### An empty matrix, will be fill up with the distance from each observation to the cluster 
    cluster_Distances=matrix(NA,nrow=n,ncol=nb_Cluster)
    
    ####### loop : compute the distance between each cluster's centroid and the observation
    for(j in 1:nb_Cluster){
      cluster_Distances[,j]=apply(X_test[,1:p],1,simple_matching_distance,y=cluster_centroids[j,])
    }
    
    ####### affect each observation to the closest cluster
    cluster_Belonging=apply(cluster_Distances,1,which.min)
    
    ######## using the error function, stocks for each simulation the 
    miss_classification_rate_k_modes[i]=error(mod_k_modes$cluster,X_training[,'cluster'])[1]
    prediction_error_k_modes[i]=error(cluster_Belonging,X_test[,'cluster'])[1]
    
    ####### k-medians
    mod_k_medians=kcca(X_training[,1:p],k_cluster,family=kccaFamily("kmedians"))
    
    ######## using the error function, stocks for each simulation the 
    miss_classification_rate_k_medians[i]=error(slot(mod_k_medians,"cluster"),X_training[,'cluster'])[1]
    prediction_error_k_medians[i]=error(predict(mod_k_medians,newdata=X_test[,1:p]),X_test[,'cluster'])[1]
    
    ####### cubt
    mod_cubt=cubt(X_training[,1:p],critopt ='entropy',minsplit = 0.8*(n/k_cluster) ,minsize = trunc(log(n)), mindev = 0.001)
    mod_cubt=prune.cubt(mod_cubt,X_training[,1:p])
    mod_cubt=join.cubt(mod_cubt,X_training[,1:p], nclass = 3, crit0 = 'entropy')
    
    ######## using the error function, stocks for each simulation the 
    cluster_Belonging=where(mod_cubt)
    miss_classification_rate_cubt[i]=error(cluster_Belonging,X_training[,'cluster'])[1]
    cluster_Belonging=where(predict(mod_cubt,X_test[,1:p]))
    prediction_error_cubt[i]=error(cluster_Belonging,X_test[,'cluster'])[1]
  }
  
  ######## returns a list of data frames containing the mean of the miss classification rate 
  ######## and the mean of the prediction error rate for each method
  data.frame(
    percentage_miss_classification=c(
      mean_k_modes=mean(miss_classification_rate_k_modes*100),
      mean_k_medians=mean(miss_classification_rate_k_medians*100), 
      mean_cubt=mean(miss_classification_rate_cubt*100)
    ),
    percentage_prediction_error=c(
      mean_k_modes=mean(prediction_error_k_modes*100),
      mean_k_medians=mean(prediction_error_k_medians*100),
      mean_cubt=mean(prediction_error_cubt*100)
    ),
    nb_of_observations=c(n,n,n)
  )
}


h=compare(n=300,p=3,l=6,b=100,which_data=2,k_cluster=4)

h=compare(n=100,p=3,l=6,q=0.8,b=100,which_data=3,k_cluster=4)

h=compare(n=300,p=9,l=5,q=0.8,b=100,which_data=1,k_cluster=3)

#################
#################FLECLUST

library(flexclust)
library(help=flexclust)
??flexclust
?kcca  

X=LC()
X=M2()
X=M3(n=30)

mod=kcca(X[,1:3],4,family=kccaFamily("kmedians"))
predict(mod, newdata=X[,1:3])


slot(mod,"cluster")
image(mod)
points(X[,1:3])
barplot(mod)


error(slot(mod,"cluster"),X[,3])

###################  KLAR
###################

library(klaR)
library(help=klaR)
??klaR
?kmodes
#weighted	
#Whether usual simple-matching distance between objects is used, or a weighted version of this distance.

X=LC()
X=M2()
X=M3()

mod=kmodes(X[,1:9],3)

mod$cluster
dim(mod$cluster)
is.vector(mod$cluster)
is.table(mod$cluster)
length(mod$cluster)
as.data.frame(mod$cluster)

mod$withindiff ##withindiff	The within-cluster simple-matching distance for each cluster.
mod$size ##size	The number of objects in each cluster.
mod$iterations ##he number of iterations the algorithm has run.
mod$weighted	##Whether weighted distances were used or not.
mod$modes ##les modes de chaque cluster donc les centres

plot(X[,1:9], col = mod$cluster)
?plot
points(mod$modes, col = 1:5, pch = 8)
length(mod$modes)
error(mod$cluster,X[,'cluster'])
?jitter
x=250

mods_Calculator=function(X){
  
  

 

 
  length(V)
}


############################ CUBT
############################
library(help=cubt)
library(cubt)
?predict.cubt
?cubt
?prune.cubt
?join.cubt
i=9

X=LC()
X=M2()
X=M3()

c=cubt(X[,1:3],critopt ='entropy',minsplit = 60 ,minsize = trunc(log(300)), mindev = 0.001)
c=prune.cubt(c,X[,1:3])
c=join.cubt(c,X[,1:3],nclass = 3, crit0 = 'entropy')
plot(c,type="u")
text(c)
where(c)


where(predict(c,X[,1:3]))

c$classopt
c$nbclasses

where(c)

print(c)
?join.cubt
##############################################"
#############################################
###############################################




######################### RWEKA
#########################

library(RWeka)
library(help=RWeka)
??RWeka
?kmeans

X=write.arff(X,file = "donnees")


C45Saver()WOW("J48")




?C45Loader          
WPM("refresh-cache")
WPM("list-packages", "installed")

WPM("list-packages", "available")
WPM("package-info", "repository", "XMeans")


WPM("refresh-cache")
WPM("list-packages", "installed")
WPM("list-packages", "available")

X=LC()
X=M2()
X=M3()

mod=Cobweb(X[,1:3],control = (N=3))
mod=
  error(slot(mod,"cluster"),X[,4])



library(RWeka)
x <- read.arff(system.file("arff", "contact-lenses.arff", package = "C:\Users\HP-ZBOOK-33\AppData\Local\Temp\Rtmp06zANS\downloaded_packages"))
Apriori(x)

install.packages("rJava")
install.packages("RWeka")
library(RWeka)




