# for testing
set.seed(1234)

library(mclust)

# Setting variables
MAXNC <- 5    # max number of cluster
TIMES <- 100    # number of iterations

# Load the spam dataset
spam20 <- read.table('spam20.dat')
spamData<-sapply(1:ncol(spam20), function(j) spam20[,j]/mad(spam20[,j]) )

ct.mc <- matrix(0, 2, 2)

for(i in 1:TIMES) {
  inx.spam <- sample(1:30, 9)
  inx.ham <- sample(31:nrow(spamData), round(0.3*nrow(spamData)))
  inx.test <- c(inx.spam, inx.ham)

  trainData <- spamData[-inx.test, ]
  testData <- spamData[inx.test, ]

  mc <- Mclust(trainData, G=2, modelNames="EEE")
  
  nt <- nrow(testData)
  true <-c (rep(1, 9), rep(2, nt-9))
  cl <- predict.Mclust(mc, newdata=testData)
  ct.mc <- ct.mc + table(true, cl$classification)
}

ct.mc/TIMES