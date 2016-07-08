# for testing
set.seed(1234)

# Load the libraries
library(rrcov)
library(caret)

# Load function CovMcdR
source('lib/CovMcdF.R')

# Setting variables
TIMES <- 100    # number of iterations

# Load the spam dataset
spamData <- read.table('spam20.dat')

alpha <- 0.1 #c(0.01, 0.05, 0.10)
chisq <- chisq.fdr <- chisq.fdx <- matrix(0, 2, 2)
beta <- beta.fdr <- beta.fdx <- matrix(0, 2, 2)
f <- f.fdr <- f.fdx <- matrix(0, 2, 2)

for(i in 1:TIMES) {
  # Define an 70%/30% train/test split of the dataset
  inx.spam <- sample(1:30, 9)
  inx.ham <- sample(31:nrow(spamData), round(0.3*nrow(spamData)))
  inx.test <- c(inx.spam, inx.ham)

  trainData <- spamData[-inx.test, ]
  testData <- spamData[inx.test, ]
  
  nt <- nrow(testData)
  true <-c (rep(1, 9), rep(0, nt-9))
  
  rob <- CovMcdF(trainData)
  #summary(rob$mah)
  
  drob <- mahalanobis(testData, rob$center, rob$cov)
  #summary(drob)
  
  #plot(drob, ylim=c(30, 40))
  #points(inx.spam, drob[inx.spam], col=2, pch=15)
  
  ### Chi-Squared
  threshold <- qchisq(.975, 20)
  #abline(h=threshold, col="green")
  
  z <- rep(0, nt)
  z <- ifelse(drob>=threshold, 1, 0)
  chisq <- chisq + table(true, z)
  
  ### Chi-Squared multiple testing
  p.val<-NULL

  for(i in 1:nt) {
    p.val[i] <- 1-pchisq(drob[i], 20)
  }

  ### confusion matrix from FDR
  th.fdr <- rank(p.val)*alpha/nt
  test.fdr <- as.numeric(p.val<th.fdr)
  chisq.fdr <- chisq.fdr + table(true, factor(test.fdr, levels = c(0, 1)))

  ### confusion matrix from FDX
  c <- 0.1
  rr <- rank(p.val)
  th.fdx <- (round(rr*c)+1)*alpha/(nt+1+round(rr*c)-rr)
  test.fdx <- as.numeric(p.val<th.fdx)
  chisq.fdx <- chisq.fdx + table(true, factor(test.fdx, levels = c(0, 1)))

  
  ## Beta
  #nt<-nrow(testData)
  #abline(h=(nt-1)^2/nt*qbeta(.975, 10, (nt-21)/2), col="blue")
  
  nt<-round(nrow(testData)*(sum(rob$wt))/nrow(trainData))
  threshold <- (nt-1)^2/nt*qbeta(.975, 10, (nt-21)/2)
  #abline(h=threshold, col="red")
  
  z <- rep(0, nt)
  z <- ifelse(drob>=threshold, 1, 0)
  beta <- beta + table(true, z)
  
  ### Beta multiple testing
  p.val<-NULL

  ccw1 <- (nt-1)^2/nt
  for(i in 1:nrow(testData)) {
    d <- drob[i]/ccw1
    p.val[i] <- 1-pbeta(d, 10, (nt-21)/2)
  }

  ### confusion matrix from FDR
  th.fdr <- rank(p.val)*alpha/nt
  test.fdr <- as.numeric(p.val<th.fdr)
  beta.fdr <- beta.fdr + table(true, factor(test.fdr, levels = c(0, 1)))

  ### confusion matrix from FDX
  c <- 0.1
  rr <- rank(p.val)
  th.fdx <- (round(rr*c)+1)*alpha/(nt+1+round(rr*c)-rr)
  test.fdx <- as.numeric(p.val<th.fdx)
  beta.fdx <- beta.fdx + table(true, factor(test.fdx, levels = c(0, 1)))


  ## F
  p <- ncol(rob$rmcd@X)
  h <- round(nrow(testData)*(sum(rob$wt))/nrow(trainData))
  nt <- nrow(testData)
  m <- madj.fun(h, p, nt)
  qF <- qf(0.975, p, m-p+1)
  threshold <- qF*p*m/(m-p+1)
  #abline(h=threshold, col="orange")
  
  z <- rep(0, nt)
  z <- ifelse(drob>=threshold, 1, 0)
  f <- f + table(true, z)
  
#   p <- ncol(rob$rmcd@X)
#   h <- nrow(testData)
#   nt<-nrow(testData)
#   m <- madj.fun(h, p, nt)
#   qF <- qf(0.975, p, m-p+1)
#   abline(h=qF*p*m/(m-p+1), col="purple")
  
  ### F multiple testing
  p.val<-NULL

  ccw1 <- p*m/(m-p+1)
  for(i in 1:nrow(testData)) {
    d <- drob[i]/ccw1
    p.val[i] <- 1 - pf(d, p, m-p+1)
  }

  ### confusion matrix from FDR
  th.fdr <- rank(p.val)*alpha/nt
  test.fdr <- as.numeric(p.val<th.fdr)
  f.fdr <- f.fdr + table(true, factor(test.fdr, levels = c(0, 1)))

  ### confusion matrix from FDX
  c <- 0.1
  rr <- rank(p.val)
  th.fdx <- (round(rr*c)+1)*alpha/(nt+1+round(rr*c)-rr)
  test.fdx <- as.numeric(p.val<th.fdx)
  f.fdx <- f.fdx + table(true, factor(test.fdx, levels = c(0, 1)))
}

print("Confusion matrix from Chi Squared")
chisq/TIMES
chisq.fdr/TIMES
chisq.fdx/TIMES

print("Confusion matrix from Beta")
beta/TIMES
beta.fdr/TIMES
beta.fdx/TIMES

print("Confusion matrix from F")
f/TIMES
f.fdr/TIMES
f.fdx/TIMES