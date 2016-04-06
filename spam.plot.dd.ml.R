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

# RMCD
rmcd <- CovMcdF(spamData, alpha=0.5)
nw<-sum(rmcd$wt)
ccw1=((nw-1)^2)/nw
quantrmcd<-qbeta(.975,ps/2, (nw-ps-1)/2)*ccw1
ccw2=(nw^2-1)*ps/nw/(nw-ps)
quantout<-qf(.975,ps, nw-ps)*ccw2


# Distance-Distance plot Mahlanobis-RMCD
mah<-mahalanobis(spamData, mle$estcent, mle$estcov)
plot(sqrt(mah), sqrt(rmcd$mah), xlab='Mahalanobis distance', ylab='Robust distance',type='n', main='RMCD')
inx.bad <- which(rmcd$wt==0)
inx.good <- which(rmcd$wt!=0)
points(sqrt(mah[inx.good]),sqrt(rmcd$mah[inx.good]),pch=20)
points(sqrt(mah[inx.bad]),sqrt(rmcd$mah[inx.bad]),pch=20)

abline(v=sqrt(quantmle))
abline(h=sqrt(c(quantrmcd, quantout)),lty=c(2, 1))
identify(sqrt(mah), sqrt(rmcd$mah))

