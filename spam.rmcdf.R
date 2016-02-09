# Load the libraries
library(rrcov)

# Load function CovMcdR
source('CovMcdF.R')

# Load the spam dataset
spamData <- read.table('spam20.dat')
spam.mcdF<-CovMcdF(spamData, alpha=0.5)

n<-length(spam.mcdF$wt)
nw<-sum(spam.mcdF$wt)
p<-ncol(spamData)

ccw1=((nw-1)^2)/nw
quantrmcd<-qbeta(.975,p/2, (nw-p-1)/2)*ccw1

ccw2=(nw^2-1)*p/nw/(nw-p)
quantout<-qf(.975,p, nw-p)*ccw2

# Distance plot
plot(spam.mcdF$mah, type='n', ylab='Squared Robust Distance', xlab='')
abline(h=quantout)
abline(h=quantrmcd, lty=2)
inx.bad<-which(spam.mcdF$wt==0)
inx.good<-which(spam.mcdF$wt!=0)
points(inx.good,spam.mcdF$mah[inx.good], pch=1, col="green")
points(inx.bad,spam.mcdF$mah[inx.bad], pch=2, col="red")

#### Spam 1 and not spam 0
true<-c(rep(1,30),rep(0,n-30))

#### Vector of detections: z==1 detection z==0 otherwise
z<-rep(0,n)
z[inx.good]<-ifelse(spam.mcdF$mah[inx.good]>=quantrmcd ,1,0)
z[inx.bad]<-ifelse(spam.mcdF$mah[inx.bad]>=quantout ,1,0)

### confusion matrix from RMCD
table(true,z)

### Multiple testing
p.val<-NULL
for(i in 1:n)
{
  if(spam.mcdF$wt[i]==1)
  {
    d.good<-spam.mcdF$mah[i]/ccw1
    p.val[i]<-1-pbeta(d.good, p/2,(nw-p-1)/2)
  }	
  if(spam.mcdF$wt[i]==0)
  {
    d.bad<-spam.mcdF$mah[i]/ccw2
    p.val[i]<-1-pf(d.bad,p, nw-p)
  }	
}

for(alpha in c(0.01, 0.05, 0.10)) {
  ## FDR
  th.fdr <- rank(p.val)*alpha/n
  test.fdr <- as.numeric(p.val<th.fdr)
  
  ### confusion matrix from FDR
  writeLines(paste("confusion matrix from FDR - alpha", alpha))
  print(table(true, test.fdr))
  writeLines("")
  
  ### FDX
  c <- 0.1
  rr <- rank(p.val)
  th.fdx <- (round(rr*c)+1)*alpha/(n+1+round(rr*c)-rr)
  test.fdx <- as.numeric(p.val<th.fdx)
  
  ### confusion matrix from FDX
  writeLines(paste("confusion matrix from FDX - alpha", alpha))
  print(table(true, test.fdx))
  writeLines("")
}