# for testing
set.seed(1234)

library(cluster)
library(NbClust)

# Load function wssplot
source('lib/wssplot.R')
source('lib/clusters.R')
source("lib/rand.index.R")

# Setting variables
MAXNC <- 5    # max number of cluster
TIMES <- 100    # number of iterations

# Load the spam dataset
spam20 <- read.table('spam20.dat')
spamData<-sapply(1:ncol(spam20), function(j) spam20[,j]/mad(spam20[,j]) )

ct.km <- ct.pm <- ct2.km <- matrix(0, 2, 2)

for(i in 1:TIMES) {
  inx.spam <- sample(1:30, 9)
  inx.ham <- sample(31:nrow(spamData), round(0.3*nrow(spamData)))
  inx.test <- c(inx.spam, inx.ham)

  trainData <- spamData[-inx.test, ]
  testData <- spamData[inx.test, ]

  ## 1
  # wssplot(testData, nc=MAXNC)
  
  ## 2
#   nc <- NbClust(testData, min.nc=2, max.nc=MAXNC, method="kmeans")
#   table(nc$Best.n[1,])
  
#   barplot(
#     table(nc$Best.n[1,]), 
#     xlab="Number of Clusters",
#     ylab="Number of Criteria",
#     main="Number of Clusters Chosen by 26 Criteria"
#   )

  ## 3
  fit.km <- kmeans(trainData, 2, nstart=25)
  #fit.pm <- pam(trainData, 2)
#   fit.km$size
#   fit.km$centers
#   apply(testData, 2, function(x) tapply(x, fit.km$cl, sd))
  
  ## 4
  #nt <- nrow(trainData)
  #true <- c(rep(1, 21), rep(2, nt-21))
  #ct.km <- ct.km + table(true, fit.km$cluster)
  #ct.pm <- ct.pm + table(true, fit.pm$clust)
  
  nt <- nrow(testData)
  true <- c(rep(1, 9), rep(2, nt-9))
  cl <- clusters(testData, fit.km$centers)
  ct2.km <- ct2.km + table(true, cl)
  
#   rand.index(true, fit.km$cl)
#   rand.index(true, fit.pm$clustering)
}

#ct.km/TIMES
ct2.km/TIMES
#ct.pm/TIMES
