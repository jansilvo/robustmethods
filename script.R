# load the libraries
library(rrcov)
library(caret)
library(klaR)

# load function CovMcdR
source('CovMcdF.R')

# setting variables
TIMES<-1

# load the spam dataset
spam20<-read.table('spam21.dat')

# define an 70%/30% train/test split of the dataset
trainIndex <- createDataPartition(spam20$V21, times=TIMES, p=0.70, list=FALSE)

spam20$V21<-NULL

for(i in 1:TIMES) {
  data_train <- spam20[trainIndex[,i],]
  data_test <- spam20[-trainIndex[,i],]
  
  data_train.mad<-sapply(1:ncol(data_train), function(j) data_train[,j]/mad(data_train[,j]))
  spam.mcdF<-CovMcdF(data_train.mad, alpha=0.5)
}

## distance plot (not reported in the example)

n<-length(spam.mcdF$wt)
nw<-sum(spam.mcdF$wt)
p<-ncol(data_train)
ccw1=((nw-1)^2)/nw
quantrmcd<-qbeta(.975,p/2, (nw-p-1)/2)*ccw1
ccw2=(nw^2-1)*p/nw/(nw-p)
quantout<-qf(.975,p, nw-p)*ccw2
plot(spam.mcdF$mah,type='n',ylab='Squared Robust Distance', xlab='')
abline(h=quantout)
abline(h=quantrmcd,lty=2)
inx.bad<-which(spam.mcdF$wt==0)
inx.good<-which(spam.mcdF$wt!=0)
points(inx.good,spam.mcdF$mah[inx.good],pch=1)
points(inx.bad,spam.mcdF$mah[inx.bad],pch=2)