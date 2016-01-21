# for testing
#set.seed(1234)

# Load the libraries
library(rrcov)
library(mclust)

# Load function CovMcdR
source('CovMcdF.R')

# Setting variables
TIMES <- 1    # number of iterations

# Load the spam dataset
spamData <- read.table('spam20.dat')

for(i in 1:TIMES) {
  # Define an 70%/30% train/test split of the dataset
  inx.spam <- sample(1:30, 9)
  inx.ham <- sample(31:nrow(spamData), round(0.3*nrow(spamData)))
  inx.test <- c(inx.spam, inx.ham)
  
  trainData <- spamData[-inx.test, ]
  testData <- spamData[inx.test, ]
  
  rob <- CovMcdF(trainData)
  summary(rob$mah)
  
  drob <- mahalanobis(testData, rob$center, rob$cov)
  summary(drob)
  
  plot(drob, ylim=c(30, 40))
  points(inx.spam, drob[inx.spam], col=2, pch=15)
  
  ### Chi-Squared
  abline(h=qchisq(.975, 20), col="green")
  
  ## Beta
  nt<-nrow(testData)
  abline(h=(nt-1)^2/nt*qbeta(.975, 10, (nt-21)/2), col="blue")
  
  nt<-round(nrow(testData)*(sum(rob$wt))/nrow(trainData))
  abline(h=(nt-1)^2/nt*qbeta(.975, 10, (nt-21)/2), col="red")

  ## F
  p <- ncol(rob$rmcd@X)
  h <- round(nrow(testData)*(sum(rob$wt))/nrow(trainData))
  nt<-nrow(testData)
  m <- madj.fun(h, p, nt)
  qF <- qf(0.975, p, m-p+1)
  abline(h=qF*p*m/(m-p+1), col="orange")
  
  p <- ncol(rob$rmcd@X)
  h <- nrow(testData)
  nt<-nrow(testData)
  m <- madj.fun(h, p, nt)
  qF <- qf(0.975, p, m-p+1)
  abline(h=qF*p*m/(m-p+1), col="purple")
}