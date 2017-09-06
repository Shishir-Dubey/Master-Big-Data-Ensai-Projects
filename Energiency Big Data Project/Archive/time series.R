#install.packages("seasonal")
library(seasonal)
#install.packages("zoo")
library(zoo)
library(parallel)
install.packages("forecast")
#install.packages("tseries")
library(tseries)

data("AirPassengers")
ap=AirPassengers
ap
cycle(ap)
plot(aggregate(ap,FUN=mean))
boxplot(AirPassengers~cycle(AirPassengers))
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

prod=ts(data$prodh1h, start = data$X[1], frequency = 24)
m <- na.approx(prod)
prod=m
cycle(prod)
plot(aggregate(prod,FUN=mean))
boxplot(prod~cycle(prod))
adf.test(diff(log(prod)), alternative="stationary", k=0)



library(forecast)
fit <- Arima(prod, order=c(2,0,1), xreg=fourier(prod, K=4))
plot(forecast(fit, h=2*24, xreg=fourierf(prod, K=4, h=2*24)))

m1 <- auto.arima(prod)
i=forecast(m1,h=100,)
plot(forecast(m1,h=100,))

#ts(mydata[3],start=c(2013,10),frequency=24*60/10) for ten minute

prod=na.omit(prod)

plot(ap, col="orange")
plot(prod, col="magenta")

class(ap)
plot(ap,col=c("blue"))
plot(decompose(ap))
ap.decom=decompose(ap,type="mult")
plot(ap.decom)
trend=ap.decom$trend
trend
seasonal=ap.decom$seasonal
seasonal
ts.plot(cbind(trend,trend*seasonal),lty=1:2)
plot(stl(ap,"periodic"))

class(prod)
start(prod)
end(prod)


plot(prod,col=c("blue"))

plot(decompose(prod))
prod.decom=decompose(prod,type="mult")
plot(prod.decom)
trend=prod.decom$trend
trend
seasonal=prod.decom$seasonal
seasonal
ts.plot(cbind(trend,trend*seasonal),lty=1:2)
plot(stl(prod,"periodic"))



dayahead=5
simnum=7
Base=15
dist=c(.31, .52,0.6,0.7, .95)
newsim=function(i){arima.sim(n=dayahead,list(ar=0.9),innov=rnorm(dayahead))}
newsim(1)
sim=sapply(1:simnum,function(x)newsim(x))
m2=t(sim)
m2
m3=Base+m2
m3
apply(m3,2,mean)
tile=sapply(1:dayahead,function(x)quantile(m3[,x], dist) )
tile


##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
crossv <- function(rep) {
  err = c()
  for (i in 1:rep) {
    n = length(y)
    ind <- sample(1:n,n / 3)
    x1 <- x[ind,]
    y1 <- y[ind]
    x2 = x[-ind,]
    y2 <- y[-ind]
    ## Making the ranking on the first part of Data
    svmresult <- svm(y2 ~ .,data = x2)
    yt = predict(svmresult,x1,type = "class")
    err[i] = (sum(yt == y1)) / length(y1)
  }
  return(err)
}

err = crossv(1000)
plot(err, type = "l", col = "green")
lines(1 - err, type = "l", col = "red")
mean(err)

