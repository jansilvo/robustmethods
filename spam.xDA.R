# for testing
#set.seed(1234)

library(MASS)

# Setting variables
TIMES <- 100    # number of iterations

# Load the spam dataset
spam20 <- read.table('spam20.dat')
spamData<-sapply(1:ncol(spam20), function(j) spam20[,j]/mad(spam20[,j]) )

gr <- c(rep(1, 30), rep(2, nrow(spamData)-30))

ct.lda <- ct.qda <- ct.glm <- matrix(0, 2, 2)

for(i in 1:TIMES) {
  inx.spam <- sample(1:30, 9)
  inx.ham <- sample(31:nrow(spamData), round(0.3*nrow(spamData)))
  inx.test <- c(inx.spam, inx.ham)
  inx.train <- c(1:nrow(spamData))[-inx.test]
  
  trainData <- spamData[-inx.test, ]
  testData <- spamData[inx.test, ]

  spam.lda <- lda(spamData, grouping = gr, subset=inx.train)
  spam.qda <- qda(spamData, grouping = gr, subset=inx.train)
  
  lda.pred <- predict(spam.lda, testData)
  qda.pred <- predict(spam.qda, testData)

  nt <- nrow(testData)
  true <- c(rep(1, 9), rep(2, nt-9))
  
  ct.lda <- ct.lda + table(true, lda.pred$class)
  ct.qda <- ct.qda + table(true, qda.pred$class)
}

ct.lda/TIMES
ct.qda/TIMES
