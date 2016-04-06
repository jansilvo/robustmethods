# Load the libraries
library(rrcov)

# Load function CovMcdR
source('lib/CovMcdF.R')

# Load the spam dataset
spamData <- read.table('spam20.dat')

#MLE
mle <- list(estcov=cov(spamData),estcent=colMeans(spamData))
ps<-ncol(spamData)
ns<-nrow(spamData)
cc=((ns-1)^2)/ns
quantmle <- qbeta(.975,ps/2, (ns-ps-1)/2)*cc

mah<-mahalanobis(spamData, mle$estcent, mle$estcov)

### Chi-Squared
threshold <- qchisq(.975, 20)

# Distance plot
plot(sqrt(mah), type='n', xlab="", ylab='Mahalanobis Distance')
abline(h=sqrt(threshold))
inx.bad <- which(mah>threshold)
inx.good <- which(mah<=threshold)
points(inx.good, sqrt(mah[inx.good]), pch=20)
points(inx.bad, sqrt(mah[inx.bad]), pch=20)
