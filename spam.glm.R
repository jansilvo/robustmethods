# for testing
set.seed(1234)

library(MASS)

# Setting variables
TIMES <- 100    # number of iterations

# Load the spam dataset
spam20 <- read.table('spam20.dat')
spamData<-sapply(1:ncol(spam20), function(j) spam20[,j]/mad(spam20[,j]) )
spamData <- cbind(spamData, c(rep(1, 30), rep(0, nrow(spamData)-30)))
spamData <- data.frame(spamData)

gr <- c(rep(1, 30), rep(0, nrow(spamData)-30))

ct.glm <- matrix(0, 2, 2)

for(i in 1:TIMES) {
  inx.spam <- sample(1:30, 9)
  inx.ham <- sample(31:nrow(spamData), round(0.3*nrow(spamData)))
  inx.test <- c(inx.spam, inx.ham)
  inx.train <- c(1:nrow(spamData))[-inx.test]
  
  trainData <- spamData[-inx.test, ]
  testData <- spamData[inx.test, ]
  
  glm.fit <- glm(X21~., data=spamData, family="binomial", subset = inx.train)
  
  glm.probs <- predict(glm.fit, testData, type="response")
  glm.pred <- rep(0, dim(testData)[1])
  glm.pred[glm.probs > .5] <- 1
  
  nt <- nrow(testData)
  true <- c(rep(1, 9), rep(0, nt-9))
  
  ct.glm <- ct.glm + table(true, glm.pred)
}

ct.glm/TIMES
