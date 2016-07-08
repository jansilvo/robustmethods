# Load function CovMcdR
source('lib/CovMcdF.R')

# Load the spam dataset
spamData <- read.table('spam20.dat')

#MLE
est2 <- CovMcdF(spamData, alpha=0.75)
nw <- sum(est2$wt)
ccw1 <- ((nw-1)^2) / nw
ps <- ncol(spamData)
ns <- nrow(spamData)
quantrmcd <- qbeta(.975, ps/2, (nw-ps-1)/2) * ccw1
ccw2 <- (nw^2-1)*ps/nw/(nw-ps)
quantout <- qf(.975,ps, nw-ps)*ccw2

# Distance plot
plot(sqrt(est2$mah), ylab='Robust Distance', xlab='Index', main='RMCD', type='n')
abline(h=quantout^0.5)
abline(h=quantrmcd^0.5, lty=2)
inx.bad <- which(est2$wt==0)
inx.good <- which(est2$wt!=0)
points(inx.good,sqrt(est2$mah[inx.good]), pch="*")
points(inx.bad,sqrt(est2$mah[inx.bad]), pch="*")