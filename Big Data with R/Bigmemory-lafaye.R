require(bigmemory)
# Takes around 16mns:
system.time({
    X<- read.big.matrix("/media/moustapha/TOSHIBA EXT/Msc ENSAI/Big Data Science Courses/Big Data with R/Data/airline.csv",
                         header = TRUE, type = "integer",
                         backingfile = "airline.bin",
                         descriptorfile = "airline.desc",
                         extraCols = "age")
  })
dim(x)

# Subsequent sessions can connect to the backing instantaneously,
# and we can interact with it (e.g., compute some statistics):

library(bigmemory)
xdesc <- dget("airline.desc")
xdesc
system.time(x <- attach.big.matrix(xdesc))
colnames(x)
head(x,100)
system.time(a <- x[,1])
max(a)                  # This one will not work!
sytem.time(range(x[,1], na.rm = TRUE))
tail(x, 100)
    
#Can we get all flights from JFK to SFO?

a <- read.table("tmp17.txt", sep = ",")
JFK <- a$V1[a$V2 == "JFK"]
SFO <- a$V1[a$V2 == "SFO"]
gc(reset=TRUE)
system.time(
  y <- x[x[, "Origin"] == "JFK" & x[, "Dest"] == "SFO",]
)
dim(y)
gc()
rm(y)


####
install.packages("biganlytics")
require(biganalytics)
# The column range for the first column
colmean(x, 1, na.rm = TRUE)
# The first column is cached a second operation
# on the column is fast.
colrange(x, 1, na.rm = TRUE)

# When is the best hour of the day to fly to minimize delays? A simple
# computation done in parallel on 3 cores.
install.packages("foreach")
install.packages("doMC")
require(foreach)
require(doMC)
registerDoMC(cores = 3)
probs <- c(0.9, 0.99, 0.999, 0.9999)
desc <- describe(x)

# delays by hour of day.
anshourofday <-
foreach (i = seq(0, colmax(x, "CRSDepTime") - 1, by=60), .combine = cbind)%dopar%
           {
             require(bigmemory)
             x <- attach.big.matrix(desc)
             ind <- mwhich(x, "CRSDepTime", c(i, i + 60),
                           comps = c('ge', 'lt'))
             m <- cbind(probs, quantile(x[ind, "DepDelay"],
                                        probs = probs, na.rm = TRUE))
             colnames(m) <- c("Probabilites", "Quantiles")
             t(m)[2,]
           }

# Do older planes suffer more delays? Maybe. A computationally
# intensive example done in parallel.

uniqTailNum <- na.omit(unique(x[, 11]))
uniqTailNum.len <- length(uniqTailNum)
# 166 different planes whose TailNum is known
planeStart <- big.matrix(nrow = uniqTailNum.len,
                         ncol = 1, shared = TRUE)
psDesc <- describe(planeStart)
foreach(i=1:uniqTailNum.len) %dopar%
{
  require(bigmemory)
  x <- attach.big.matrix(desc)
  planeStart <- attach.big.matrix(psDesc)
  # The first year plane i can be found:
  yearInds <- mwhich(x, "TailNum", uniqTailNum[i],
                     comps = 'eq')
  minYear <- min( x[yearInds, "Year"], na.rm = TRUE )
  # First month in minYear where the plane can be found:
  minMonth <- min( x[yearInds, "Month"], na.rm = TRUE )
  planeStart[i, 1] <- 12 * minYear + minMonth
  return(TRUE)
}

###
BadTailNum <- mwhich(x, 11, NA, 'eq')
x[BadTailNum, 30] <- NA
MoreRecentDate <- max(x[,1]) * 12 + max(x[,2])
system.time(foreach(i=1:uniqTailNum.len) %dopar%
{
  require(bigmemory)
  x <- attach.big.matrix(desc)
  planeStart <- attach.big.matrix(psDesc)
  tmpInds <- mwhich(x, 11, uniqTailNum[i], 'eq')
  x[tmpInds, 30] <- as.integer(MoreRecentDate -
                                 planeStart[i, 1])
})

###
blm1 <- biglm.big.matrix(ArrDelay  ̃ age, data = x)
( out <- summary(blm1) )
names(out)
out$rsq
blm2 <- biglm.big.matrix(ArrDelay  ̃ age + Year, data = x)
summary(blm2)