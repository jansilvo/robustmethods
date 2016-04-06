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

mah<-mahalanobis(spamData, mle$estcent, mle$estcov)

# Distance-Distance plot Mahlanobis-MM
mm<-CovMMest(spamData, bdp=0.5)
mm@mah<-mahalanobis(spamData, mm@center, mm@cov)
plot(sqrt(mah), sqrt(mm@mah), xlab='Mahalanobis distance', ylab='Robust distance',type='n', main='MM')
inx.bad <- which(rmcd$wt==0)
inx.good <- which(rmcd$wt!=0)
points(sqrt(mah[inx.good]), sqrt(rmcd$mah[inx.good]), pch=20)
points(sqrt(mah[inx.bad]), sqrt(rmcd$mah[inx.bad]), pch=20)
abline(v=sqrt(qchisq(.975, 20)),h=sqrt(qchisq(.975, 20)))

identify(sqrt(mah), sqrt(mm@mah))