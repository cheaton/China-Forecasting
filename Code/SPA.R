SPA <- function(d,EBL=NULL,numBS=1000,alpha=0.05)
{
  library(tseries)  # For the stationary bootstrap.
  library(np)       # For the optimal block length procedure of Patton, Politis and White (2009).

  T <- dim(d)[1]   # The sample size.
  l <- dim(d)[2]   # The number of series.

  if (is.null(EBL)) # If no expected block length was provided, compute it optimally.
  {
    EBL <- mean(b.star(d)[,"BstarSB"])
    EBL <- max(EBL,1.01)  # tsbootstrap throws an error if EBL <= 1
    EBL <- min(EBL,(T-0.01))  # tsbootstrap throws an error if EBL >= T
  }
# print(paste("EBL =",EBL))
  p <- 1/EBL
  
  meand <- colMeans(d)
  dcentred <- scale(d,center=TRUE,scale=FALSE)
  
  # Compute the variances, w. the formula is given by Hansen (2005) on p.372 halfway down the first column.

  t <- seq(from=1,to=(T-1),by=1)
  kappa <- ((T-t)/T)*((1-p)^t) + (t/T)*((1-p)^(T-t))
  mk <- c(rev(c(1,kappa)),kappa)
  K <- diag(T)
  for (t in 1:T)
  {
    K[,t] <- mk[(T-t+1):(2*T-t)]
  }
  w <- diag(t(dcentred)%*%K%*%dcentred)/T

	# Compute the standardised performance statistics.

  Vbar_stats <- sqrt(T)*(t(meand)/sqrt(w)) 
  
	# Compute the test stastic.
	
	Vbar <- max(Vbar_stats)   # This is the test statistic T^{SPA}_n given at 1. on p.368 of Hansen (2005).
  if (Vbar < 0)
  {                                      
		Vbar = 0
	}

  gc <- meand*(meand>(-sqrt(2*t(w)*log(log(T))/T))) # These are the thresholds for recentering the bootstrapped statistics. See Hansen (2005) p.372 at the bottom of the first column and the top of the second column.
	gcmat <- matrix(rep(1,T),ncol=1)%*%gc 

  Vbaris <- rep(0,numBS)     # An array to hold the bootstrapped statistics.
  
  BSindexes <- tsbootstrap(seq(from=1,to=T,by=1),nb=numBS,b=EBL,type="stationary")
  for (i in 1:numBS) # Generate a matrix with numBS columns, each of which contain the indexes for a stationary bootstrap of T observations.
  {
    BSVbars <- sqrt(T)*colMeans(d[BSindexes[,i],] - gcmat)/sqrt(w) # Compute the vector of l statistics for the i^{th} iteration of the bootstrap...
    Vbaris[i] <- max(BSVbars) #... and save the largest.
  }
  Vbaris[which(Vbaris < 0)] <- 0 # Set to zero any elements which are negative.

  cv <- quantile(Vbaris,probs=(1-alpha))
  pvalue <- mean((Vbaris>Vbar)*1)

  return(list(Vbar=Vbar,cv=cv,pvalue=pvalue))
}