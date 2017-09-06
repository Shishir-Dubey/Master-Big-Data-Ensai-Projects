Sys.setenv("HADOOP_CMD"="/usr/local/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.7.2.jar")
#install.packages("rhdfs_1.0.8.tar.gz", repos = NULL, type = "source")
library(rmr2)
library(rhdfs)
hdfs.init()

map <- function(k,lines) {
  words.list <- strsplit(lines, '\\s')
  words <- unlist(words.list)
  return( keyval(words, 1) )
}

reduce <- function(word, counts) {
  keyval(word, sum(counts))
}

wordcount <- function (input, output = NULL) {
  mapreduce(input = input, output = output,
            input.format = "text", map = map, reduce = reduce)
}


hdfs.root <- 'example/wordcount'
hdfs.data <- file.path(hdfs.root, 'data')
hdfs.out <- file.path(hdfs.root, 'out')


out <- wordcount(hdfs.data, hdfs.out)
 
results <- from.dfs(out)
results.df <- as.data.frame(results, stringsAsFactors = FALSE)
 colnames(results.df) <- c('word', 'count')
 results.df
 

 #### second example
 ints <- to.dfs(1:10)
 calc <- mapreduce(input = ints,
                      map = function(k, v) cbind(v, 2 * v))
 from.dfs(calc)
 
 
 
 xx<-read.table(file.choose(), header = T, sep=",") 
 
### spark part
library(rJava)
library(SparkR)
Sys.setenv(SPARK_MEM="8g")
sc <- sparkR.init(sparkPackages="com.databricks:spark-csv_2.11:1.2.0")
sqlContext <- sparkRSQL.init(sc) 


## path <- file.path('', 'home','moustapha','airports.csv')

system.time(imdb <- read.df(sqlContext,
                "/Imdb.csv",
               header='true', 
               source = "com.databricks.spark.csv", 
               inferSchema='true'))

head(imdb)
system.time(dimData=dim(imdb))

# 19908819       14

## the data dimensions checking that is quasi instantanious with Bigmemory, here take arround 40 second with 19 908 819 row

## Now lets try to answer to some question like in the case of bigmemory

## 1. the year of the oldest movie in the base and the newest one
system.time( minyear<-collect(select(imdb, min(imdb$years))))
# user  system elapsed 
# 0.257   0.155  48.438 
# > minyear
# min(years)
# 1       1947
system.time( maxyear<-collect(select(imdb, max(imdb$years))))
# user  system elapsed 
# 0.188   0.167  45.659 
# > maxyear
# max(years)
# 1       2017

## it appear that the newest movies in the base is 2017 one's, let see this movies
system.time(futureMovies <-
              collect(imdb[imdb$years == maxyear[[1]], c("idmovies","title","years","location","genre")]))

## that takes around 40s and it is only one movies Mockingbirds that is a mix of three genres (adventures, mystery, romance)
# user  system elapsed 
# 0.181   0.097  37.865 


## 2. the number of movies played by each actors and the actor that played the highest number of movies
system.time( movPlayAc<-collect(agg(groupBy(imdb, "idactors"), numbMovies=countDistinct(imdb$idmovies),
                                                           numbSeries=countDistinct(imdb$idseires)
                                                           )))
## this query take 37 second

maxMov=movPlayAc[which.max(movPlayAc$numbMovies),"idactors"]
maxSeries=movPlayAc[which.max(movPlayAc$numbSeries),"idactors"]

system.time(maxMoviesActor <-
              collect(imdb[imdb$idactors == maxMov, c("idactors","lname","fname","mname")]))


system.time(maxSeriesActor <-
              collect(imdb[imdb$idactors == maxSeries, c("idactors","lname","fname","mname")]))

# user  system elapsed 
# 1.715   0.114  38.846 
##  for this  we remarque that the actor that played the highest number of movies is "Bonnie Gail"
## And the one played highest number of series is " Abdul Paula"

## 3. what is the genre that is mostly played

system.time( mostGenre<-collect(agg(groupBy(imdb, "genre"), numbMovies=countDistinct(imdb$idmovies),
                                    numbSeries=countDistinct(imdb$idseires))))
# user  system elapsed 
# 0.224   0.095  41.638 
# it appeard that the most genres played are Drama and comedy



## 4. how many unique actor and series we have in the database
#unique actors

system.time(uniqElement<-collect(select(imdb, countDistinct(imdb$idactors), countDistinct(imdb$idgenres), 
                                      countDistinct(imdb$idseires), countDistinct(imdb$idmovies),
                                      countDistinct(imdb$location))))
# user  system elapsed 
# 0.168   0.119  39.431

# count(actors) count(genres) count(seires) count(idmovies) count(location)
# 1       18967           27        328083            6246             105


## 5 . the series that have most seasons
## first find the highest number of season
system.time(maxSeason<-collect(select(imdb, max(imdb$seasons))))
# user  system elapsed 
# 1.592   0.204  39.490

# max(seasons)
# 1       111
## wow there are series that have 111 season let see which on it is

system.time(topSeries <-
              collect(imdb[imdb$seasons == maxSeason[[1]], c("idseires","series_name","years","location","title")]))

# idseires     series_name years      location                 title
# 1      69576 Brittany Murphy  1981 weekend title Entertainment Tonight




## 5. the number of movies by year and the year that have the highest number of movies
system.time(topYear<-collect(agg(groupBy(imdb, "years"), numbMovieYear=countDistinct(imdb$idmovies),
                                     numbSeries=countDistinct(imdb$idseires))))
topyearMovies<-topYear[which.max(topYear$numbMovieYear),1:2]
topyearSeries<-topYear[which.max(topYear$numbSeries),-2]
topyearMovies
topyearSeries

# topyearMovies
# years numbMovieYear
# 63  2009           255
# > topyearSeries
# years numbSeries
# 59  2005      15162

## So for this query it appeared that the year where there are lot of movies released is 2009 with 255 distinct movies
## and the year that have top series recorded is 2005 with 15162 released

## with the function big K means and the paralellisation lets apply the k means







sparkR.stop()
