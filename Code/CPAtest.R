# An R translation of the Matlab code at # in http://www.runmycode.org/companion/view/88 which
# implements the tests in Giacomini, R., White, H., 2006. Tests of Conditional Predictive Ability. 
# Econometrica 74, 1545–1578. doi:10.1111/j.1468-0262.2006.00718.x. The only thing extra that I 
# have added is to have the function return NAs for all outputs if either the covariance
# matrix is within machine accuracy of being singular (which happens if the two forecasts are
# identical) or if one of the series of losses contains either an NaN or an Inf (which might
# occur if the loss function is the MAPE and one of the outcomes is zero). Without
# this, there is a risk of simulations crashing if one forecasting model is nested in another
# for which the parameters are estimated.

Newey.West <- function(Z,nlags)
{
# A basic Newey-West estimator which is an R version of the Newey-West estimator
# in http://www.runmycode.org/companion/view/88 which is used for
# Giacomini, R., White, H., 2006. Tests of Conditional Predictive Ability. 
# Econometrica 74, 1545–1578. doi:10.1111/j.1468-0262.2006.00718.x

  n <- length(Z[,1])
  k <- length(Z[1,])
  Z <- scale(Z,center=TRUE,scale=FALSE)
  samplevar <- t(Z)%*%Z/n
  omegahat <- samplevar

  if (nlags >0)
  {
    for (ii in 1:nlags)
    {
      Zlag <- rbind(matrix(rep(0,ii*k),ncol=k),as.matrix(Z[1:(n-ii),]))
      gamma <- (t(Z)%*%Zlag + t(Zlag)%*%Z)/n
      weights <- 1 - ii/(nlags+1)
      omegahat <- omegahat + weights*gamma
    }
  }
  return(omegahat)
}


CPAtest <- function(loss1,loss2,tau,alpha,choice)
{
# An R version of the corresponding function on http://www.runmycode.org/companion/view/88
# which is the matlab code for
# Giacomini, R., White, H., 2006. Tests of Conditional Predictive Ability. 
# Econometrica 74, 1545–1578. doi:10.1111/j.1468-0262.2006.00718.x
# 
#                                        Chris Heaton, March 2016.
#
# This function performs the asymptotic Conditional Predictive Ability Test
# INPUTS: loss1 and loss2, Tx1 vectors of losses over the out of sample period for the two models under consideration
#         tau, the forecast horizon
#         alpha, niminal risk level: 1#, 5#, 10#
#         choice: 1 if unconditional ; 2 if conditional
#
# OUTPUTS: teststat, the test statistic of the conditional predictive ability test
#          critval, the critical value of the test for a 5# level (the test is a chi square)
#          pval, the p-value of the test
#
# Raffaella Giacomini, 2003

  if (sum(is.inf(loss1) + is.nan(loss1) + is.inf(loss2) + is.nan(loss2)) > 0)
  {
    teststat <- NA; critval <- NA; pval <- NA
  }else
  {
    lossdiff1 <- na.omit(loss1) - na.omit(loss2)
    TT <- length(lossdiff1)
    library(sandwich)
  
    if (choice==1)
    {
      instruments <- as.matrix(rep(1,TT))
      lossdiff <- as.matrix(lossdiff1)
      T <- TT
      n.inst <- 1
    }else
    {
      instruments <- as.matrix(cbind(rep(1,(TT-tau)),lossdiff1[1:(TT-tau)]))
      lossdiff <- as.matrix(lossdiff1[(tau+1):TT])
      T <- TT-tau
      n.inst <- 2
    }
  
    reg <- matrix(rep(NA,T*n.inst),ncol=n.inst)
  
    for (j in 1:n.inst)
    {
      reg[,j] <- instruments[,j]*lossdiff
    }
    
    if (tau==1)
    {
      ones <- rep(1,length(reg[,1]))
      if (is.finite(sum(reg)))
      {
        reg.out <- lm(ones~0+reg)
        teststat <- summary(reg.out)$r.squared*T
        q <- length(reg.out$coefficients)
        critval <- qchisq(1-alpha,df=q)
        pval = 1 - pchisq(abs(teststat),df=q)
      }else
      {
        teststat <- NA
        critval <- NA
        pval <- NA
      }
    }else
    { 
      zbar <- colMeans(reg)
      nlags <- tau - 1
      omega <- Newey.West(reg,nlags)
      if (rcond(omega)>.Machine$double.eps)
      {
        teststat <- T*t(zbar)%*%solve(omega)%*%zbar
        q <- length(reg[1,])
        critval <- qchisq(1-alpha,df=q)
        pval <- 1 - pchisq(abs(teststat),df=q)
      }else
      {
        teststat <- NA
        critval <- NA
        pval <- NA
      }
    }
  }
  return(list(teststat = teststat, critval = critval, pval = pval))
}

