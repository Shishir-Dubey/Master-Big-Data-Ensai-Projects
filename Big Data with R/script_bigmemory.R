devtools::install_github("kaneplusplus/bigmemory")
devtools::install_github("kaneplusplus/bigtabulate")
devtools::install_github("kaneplusplus/biganalytics")
devtools::install_github("kaneplusplus/bigalgebra")
devtools::install_github("kaneplusplus/synchronicity")

### difine the working working directory
setwd("/media/moustapha/TOSHIBA EXT/Msc ENSAI/Big Data Science Courses/Big Data with R")
###

library(bigmemory)

system.time({
  X<- read.big.matrix(file.choose(),
                      header = TRUE, type = "integer",
                      backingfile = "test.bin",
                      descriptorfile = "test.desc")
})

## loading the table a first time taking arround 30 min

library(bigmemory)
library(biganalytics)
library(bigtabulate)
imdb <- dget("test.desc")
imdb

system.time(x <- attach.big.matrix(imdb))
## then loading the matrix is instanta nious
colnames(x)
head(x)
## head(x)
# idactors lname fname mname gender idmovies title years location idgenres genre idseires series_name seasons
# [1,]        3    NA    NA    NA     NA        9    NA  1950       NA        7    NA   564811          NA       3
# [2,]        6    NA    NA    NA     NA       14    NA  2005       NA        6    NA   665813          NA       9
# [3,]        6    NA    NA    NA     NA       14    NA  2005       NA        6    NA        7          NA       5
# [4,]        6    NA    NA    NA     NA       14    NA  2005       NA        6    NA   478763          NA       8
# [5,]        6    NA    NA    NA     NA       14    NA  2005       NA        6    NA   622523          NA      10
# [6,]        6    NA    NA    NA     NA       14    NA  2005       NA        6    NA        8          NA       3
dim(x)
# dim(x)
# [1] 571257330        14

system.time(summary(x))
# user  system elapsed 
# 28.452   8.077  36.598 

## our data was not completly loaded but we have 571 257 330 and 14 
head(x,240)
gg<-as.data.frame(tail(x,5000))
######
## The objective is to answer to several question that we can have through this data set.
## we will first do it on one personnal computer with bigmemory then 
## compare the processing time by doing it with spark in a parllelization context

## the non exhaustive list of some question that we want the answer
## 1. the year of the oldest movie in the base and the newest one
system.time( minyear<-colmin(x,"years", na.rm = T))
system.time( maxyear<-colmax(x,"years", na.rm = TRUE))

# user  system elapsed 
# 0.512   0.461   0.973 

#the time take is 10 time less than the normal min or max

## 2. the number of movies played by actors and the actor that played the highest number of movies
system.time( movPlayAc<-table(x[,"idactors"]))

# user  system elapsed 
# 103.673  32.284 136.065 

system.time( movPlayAc<-bigtable(x,"idactors"))
# user  system elapsed 
# 10.685   0.418  11.208 

# like in the precedent question it appear that the implemented bigmemory function are more faster than the normal R ones
names(which.max(movPlayAc))

## 3. what is the genre that is mostly played

system.time( movGenres<-bigtable(x,"idgenres"))
names(which.max(movGenres))

## 4. how many unique actor, genres and series we have in the database
#unique actors
system.time(uniqActor <- na.omit(unique(x[, "idactors"])))
length(uniqActor)
# unique genres
system.time(uniqGenres <- na.omit(unique(x[, "idgenres"])))
length(uniqGenres)
# unique series
system.time(uniqSeries <- na.omit(unique(x[, "idseires"])))
length(uniqSeries)
# unique movies
# unique series
system.time(uniqMovies <- na.omit(unique(x[, "idmovies"])))
length(uniqMovies)

## the series that have most seasons
system.time(maxSeason<-max(x[,"seasons"], na.rm = TRUE))
system.time(maxSeason<-colmax(x, "seasons", na.rm = TRUE))

system.time(topSeries <- x[mwhich(x, "seasons", maxSeason, "eq"),])
na.omit(unique(topSeries[,"idmovies"]))

## 5. the number of movies by year and the year that have the highest number of movies
system.time(numberMovieYear<-bigtable(x, "years"))
names(which.max(numberMovieYear))


## with the function big K means and the paralellisation lets apply the k means
require(doMC)
registerDoMC(cores = 5)
system.time(kmeanss <- bigkmeans(x,3,dist = "euclid", nstart = 5, iter.max = 100)) 

kmeanss
