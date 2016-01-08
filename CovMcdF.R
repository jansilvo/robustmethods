###########################
##
## Reweighted MCD from Hardin and Rocke
##
############################

ma.fun<-function(h,p,n)
{
	## h is @quan from raw MCD
	alpha<-1-h/n
	q<-qchisq(1-alpha,p)
	ca<-(1-alpha)/pchisq(q,p+2)
	c2<--0.5*pchisq(q,p+2)
	c3<- -0.5*pchisq(q,p+4)
	c4<-3*c3
	b1<- ca*(c3-c4)/(1-alpha)
	b2<-0.5+ca/(1-alpha)*(c3-(q/p)*(c2+(1-alpha)/2))
	forv1add1<- (1-alpha)*(b1^2)*(alpha*(ca*q/p)^2-1)
    forv1add2<- 2*c3*(ca^2)*(3*((b1-p*b2)^2)+(p+2)*b2*(2*b1-p*b2))
    v1<-forv1add1-forv1add2
    forv2<-b1*(b1-p*b2)*(1-alpha)
    forv2<-forv2*ca
    v2<-n*(forv2^2)
    v<-v2/v1	
	m<-2*v/(ca^2)
	m
}

madj.fun<-
function(h,p,n)
{ ma<-ma.fun(h,p,n)
	ma*exp(0.725-0.00663*p-0.078*log(n))}


CovMcdF<-
function (x, cor = FALSE,alpha=0.5, nsamp = 500,
            seed = NULL, trace = FALSE, use.correction = TRUE, control ) 
  {
  	if (!missing(control)) {
        defcontrol <- CovControlMcd()
        if (alpha == defcontrol@alpha) 
            alpha <- control@alpha
        if (nsamp == defcontrol@nsamp) 
            nsamp <- control@nsamp
        if (is.null(seed) || seed == defcontrol@seed) 
            seed <- control@seed
        if (trace == defcontrol@trace) 
            trace <- control@trace
        if (use.correction == defcontrol@use.correction) 
            use.correction <- control@use.correction
    }
    iter <- if (is.numeric(nsamp)) 
        nsamp
    else 0

    xcall <- match.call()
    out<-CovMcd(x = x, alpha = alpha, nsamp = nsamp, seed = seed, 
                  trace = trace, use.correction = use.correction, control=control)
    p<-ncol(out@X)
    n<-nrow(out@X) 
    h<-out@quan
    m<-madj.fun(h,p,n)   
    qF<-qf(0.975,p,m-p+1)
    cutoff<-qF*p*m/(m-p+1)
    raw.mah<-out@raw.mah
    weights <- as.numeric(raw.mah < cutoff)
    sum.w <- sum(weights)
    cov<- cov.wt(out@X, wt = weights, cor = cor)
    center=cov$center
    cov=cov$cov
    cdelta.rew <- robustbase:::MCDcons(p, sum.w/n)
    correct.rew <- if (use.correction) 
      robustbase:::MCDcnp2.rew(p, n,  alpha)
    else 1
    cnp2 <- c(cdelta.rew, correct.rew)
    cov.adj<-cdelta.rew*correct.rew*cov        
    mah.rew<-mahalanobis(out@X, center,cov)
    list(cov=cov.adj, cnp2=cnp2, q=cutoff, center=center, mah.rew=mah.rew,m=m, 
    wt=weights, rmcd.chi=out )
  }
  

