mean.forecast <- function(y,horizon)
{
  # Returns a simple mean forecast from 1-period-ahead to horizon-periods-ahead. The forecast is the mean
  # of the data. y must be a ts object and horizon is an integer
  #
  #                                        Chris Heaton, March 2019.
  fc <- rep(1,horizon)*mean(y)
  date.of.first.forecast <- (as.yearmon(time(y))+1/frequency(y))[length(y)]
  fc <- ts(fc,start=date.of.first.forecast,frequency=frequency(y))
  return(fc)
}


season.dummy.forecast <- function(y,xreg=NULL,future.xreg=NULL,horizon=1)
{   # y is a ts object. This function regresses y on a constant and a set of
    # seasonal dummy variables, and outputs a ts object containing the 1-period
    # to horizon-period forecasts of y. xreg is an optional ts or mts object
    # containing explanatory variables that will be included in the regression.
    # If xreg is not null, then future.xreg must also be supplied which contains
    # the future values of xreg that will be used in the forecast (there must be
    # horizon of them or it throws an error).

    library(forecast)

    # Check sanity of input
    if ((is.null(xreg)&!is.null(future.xreg))|(!is.null(xreg)&is.null(future.xreg)))
    {
        stop("Problem with call to seasonal.dummy.forecast. xreg and future.xreg must either both be null or both not be null.")
    }
    if (!is.null(future.xreg))
    {
        if (is.matrix(future.xreg))
        {
            if (length(future.xreg[,1])!=horizon)
            {
                stop("Problem with call to seasonal.dummy.forecast. The length of the columns in future.xreg must be equal to horizon.")
            }
        }else
        {
            if (length(future.xreg)!=horizon)
            {
                stop("Problem with call to seasonal.dummy.forecast. The length of future.xreg must be equal to horizon.")
            }
        }
        if (sum(start(y)==start(xreg))+sum(end(y)==end(xreg))+(frequency(y)==frequency(xreg))!=5)
        {
            stop("Problem with call to seasonal.dummy.forecast. The time series properties of y and xreg do not match.")
        }
    }

    # Estimate the regression model.

    if (is.null(xreg))  
    {
        reg <- tslm(y~season)
    }else 
    {
        reg <- tslm(y~xreg+season)
    }

    # Construct rhs variable matrix for forecast calculation.
    first.future.date.yearmon <- as.yearmon(index(y)[length(y)]) + 1/frequency(y)
    first.future.date <- c(year(first.future.date.yearmon),month(first.future.date.yearmon))
    constant.term <- ts(rep(1,horizon), start=first.future.date, frequency=frequency(y))

    if (is.null(future.xreg))
    {
        rhs <- cbind(constant.term,create.seasonal.dummies(constant.term)[,2:frequency(y)])
    }else
    {
        rhs <- cbind(constant.term,future.xreg,create.seasonal.dummies(constant.term)[,2:frequency(y)])
    }

    # Compute the forecast vector, turn it into a ts object, and return it.
    fc <- ts((as.numeric(rhs%*%as.matrix(reg$coefficients))),start=first.future.date,frequency=frequency(y))
    return(fc)
}


naive.forecast <- function(y,horizon)
{
  # Returns a naive forecast from 1-period-ahead to horizon-periods-ahead. The forecast is the 
  # last observation at all horizons.
  #
  #                                        Chris Heaton, March 2019.
  fc <- rep(1,horizon)*y[length(y)]
  date.of.first.forecast <- (as.yearmon(time(y))+1/frequency(y))[length(y)]
  fc <- ts(fc,start=date.of.first.forecast,frequency=frequency(y))
  return(fc)
}


MA.forecast <- function(y,horizon,order=NULL,criterion=NULL)
{
  # Returns a simple moving average forecast from 1-period-ahead to horizon-periods-ahead. The forecast
  # is the mean of the last order observation at all horizons. criterion must be one
  # of c("MSE","RMSE","MAE","MAPE") or NULL. If NULL, then the MA(order) forecast is returned with the 
  # order. If not then the order is chosen to minimise the criterion using order as the highest order
  # considered, and that forecast is returned with the calculated optimal order. 
  # Note that the order choice is made considering the horizon-periods-ahead forecast only
  # and all the forecasts from 1-step-ahead to horizons-steps-ahead are computed using that order. If you
  # want a sequence of j-step-ahead forecasts from j=1:horizon with the optimal order used for EACH forecast
  # then you will need to call this function in a loop and pick out the last forecast in each iteration.
  #
  #                                        Chris Heaton, March 2019.
  library(TTR)
  
  the.MA <- function(y,this.order,horizon,criterion)
  {
    MAs <- cbind(y,lag(SMA(y,n=this.order),-horizon))
    if (is.null(criterion)){crit.value <- NULL}else{
      if (criterion=="MSE"){crit.value <- mean((MAs[,1]-MAs[,2])^2,na.rm=TRUE)}
      if (criterion=="RMSE"){crit.value <- sqrt(mean((MAs[,1]-MAs[,2])^2,na.rm=TRUE))}
      if (criterion=="MAE"){crit.value <- mean(abs(MAs[,1]-MAs[,2]),na.rm=TRUE)}
      if (criterion=="MAPE"){crit.value <- mean((abs(MAs[,1]-MAs[,2])/MAs[,1])*100,na.rm=TRUE)}
    }
    fc <- MAs[dim(MAs)[1],2]; names(fc) <- ""
    return(list(forecast=fc, crit.value=crit.value))
  }
  
  if (is.null(order)){order <- 2*frequency(y)}
  if (!is.null(criterion))
  {
    best.order <- NA; best.crit <- Inf
    for (i in 1:order)
    {
      out.this.order <- the.MA(y,i,horizon,criterion)
      if (out.this.order$crit.value < best.crit)
      {
        best.order <- i; best.crit <- out.this.order$crit.value
      }
    }
  }else{best.order <- order}
  fc <- the.MA(y,best.order,horizon,criterion)$forecast*rep(1,horizon)
  date.of.first.forecast <- (as.yearmon(time(y))+1/frequency(y))[length(y)]
  fc <- ts(fc,start=date.of.first.forecast,frequency=frequency(y))
  return(list(forecast=fc, order=best.order))
}


SES.forecast <- function(y,horizon,alpha=NULL)
{
  # Returns a SES forecast from 1-period-ahead to horizon-periods-ahead. The forecast is the same
  # for all time periods. alpha is the smoothing parameter. If it is NULL then it will be estimated.
  # otherwise, it must be a number between 0 and 1.
  #
  # This function is just a wrapper for the ses function in the forecast package. It returns a list
  # containing the forecasts and the value of alpha.
  #
  #                                        Chris Heaton, March 2019.
 
  library(forecast)
  out <- ses(y, h=horizon, initial="optimal", alpha = alpha)
  return(list(forecast=out$mean, alpha=out$model$par["alpha"]))
}

HW.forecast <- function(y,horizon,alpha = NULL, beta = NULL, gamma = NULL, seasonal.type = "multiplicative")
{
  # Returns a Holt-Winters forecast from 1-period-ahead to horizon-periods-ahead. 
  # alpha, beta and gamma are the smoothing parameters for the level, trend and
  # seasonal component respectively. If they are NULL then they will be estimated.
  # otherwise, they must be numbers between 0 and 1.
  #
  # This function is just a wrapper for the HoltWinters function in the stats
  # package and the forecast function in the forecast package. It returns a list
  # containing the forecasts and the value of alpha.
  #
  #                                        Chris Heaton, July 2019.
 
  library(forecast)
  out <- HoltWinters(y, horizon, alpha=alpha, beta=beta, gamma=gamma, seasonal=seasonal.type)
  out.fc <- forecast(out,h=horizon)
  return(list(forecast=out.fc$mean, alpha=out$alpha, beta=out$beta, gamma=out$gamma))
}



AR.forecast <- function(y,estimate.order=FALSE,order.max=1,horizon)
{
  # Returns an AR forecast from 1-period-ahead to horizon-periods-ahead. y is a ts object
  # containing the data, estimate.order is a boolean indicating whether the order of the AR
  # should be estimated using the AIC. order.max is the maximum order to consider when
  # estimating the order using the AIC, or it is the actual order used if estimate.order=FALSE.
  # horizon is the maximum number of steps ahead to forecast. The output is a list containing
  # two elements: $forecast is a ts object containing the forecasts; order is a numeric
  # containing the order of the autoregression.
  #
  # Note: this function computes iterated multistep forecasts only. For direct multistep
  # AR forecasts, use AR.direct.forecast.
  #
  # This function is just a wrapper for the ar function in the stats package.
  #
  #                                      Chris Heaton, March 2019.
  AR.out <- ar(y,AIC=estimate.order,order.max=order.max,method="ols",intercept=TRUE,demean=FALSE)
  fc <- predict(AR.out,n.ahead=horizon)$pred
  return(list(forecast=fc, order=AR.out$order))
}


AR.direct <- function(y,estimate.order=FALSE,order.max=1,horizon)
{
  # Estimates a 'direct' h-step-ahead autoregression and produces forecasts from 1-step-ahead to
  # horizon-steps-ahead. If estimate.order=TRUE then the order of the AR
  # is estimated using the AIC, with a maximum order of order.max. If estimate.order=FALSE, 
  # then order.max lags are included on the right-hand-side instead. The function returns the forecast
  # and the estimated order. Note that this estimates the I(0) case of Marcellino, Stock and Watson
  # (2006). If y is in first differences, then the cumulative sum of the outputed forecasts are
  # the forecasts from the I(1) cae of Marcellino, Stock and Watson (2006).
  #
  # Marcellino, M., Stock, J.H., Watson, M.W., 2006. A comparison of direct and iterated 
  # multistep AR methods for forecasting macroeconomic time series. Journal of Econometrics 
  # 135, 499–526. https://doi.org/10.1016/j.jeconom.2005.07.020
  #
  #                                      Chris Heaton, March 2019.
  
  estimate.p <- function(y,horizon,order.max)
  {# Estimate the order by minimising the AIC
    crit <- Inf
    best.p <- NA
    for (p in 1:order.max)
    {
      reg <- dynlm((lag(y,horizon))~lag(y,0:-(p-1)))
      aic <- AIC(reg)
      if (aic < Inf){best.p <- p; crit <- aic}
    }
    return(best.p)
  }
  
  library(dynlm)
  y <- as.zoo(y)
  if (estimate.order)
  {
    p <- estimate.p(y,horizon,order.max)
  }else
  {
    p <- order.max
  }
  
  fc <- NULL
  for (h in 1:horizon)
  {
    reg <- dynlm((lag(y,h))~lag(y,0:-(p-1)))
    fc <- c(fc,sum(reg$coefficients*c(1,tail(na.omit(lag(y,0:-(p-1))),1))))
  }
  date.of.first.forecast <- (as.yearmon(time(y))+1/frequency(y))[length(y)]
  fc <- ts(fc,start=date.of.first.forecast,frequency=frequency(y))
  return(list(forecast=fc, p=p))
}
 

ARMA.forecast <- function(y,estimate.order=FALSE,p.max=1,q.max=1,horizon)
{
  # Returns an ARMA forecast from 1-period-ahead to horizon-periods-ahead. y is a ts object
  # containing the data, estimate.order is a boolean indicating whether the order of the ARMA
  # should be estimated using the AIC. p.max is the maximum autoregressive order to consider when
  # estimating the order using the AIC, or it is the actual order used if estimate.order=FALSE.
  # Similarly, max.q is the maximum MA order, or the actual order.
  # horizon is the maximum number of steps ahead to forecast. The output is a list with 3 elements:
  # $forecast is a ts object containing the forecasts. $p is a numeric containing the autoregressive
  # order and $q is a numeric containing the moving average order.
  #
  # This function is just a wrapper for the auto.arima function in the forecast package.
  #
  #                                      Chris Heaton, March 2019.
  if (estimate.order)
  {
    ARMA.out <- auto.arima(y,stationary=TRUE,seasonal=FALSE,max.p=p.max,max.q=q.max,d=0,D=0,max.P=0,max.Q=0,ic="aic",stepwise=FALSE,parallel=FALSE,allowmean=TRUE)
    fc <- forecast(ARMA.out,h=horizon)$mean
    p <- ARMA.out$arma[1]
    q <- ARMA.out$arma[2]
  }else
  {
    ARMA.out <- Arima(y,order=c(p.max,0,q.max),include.constant=TRUE,method="ML")
    fc <- forecast(ARMA.out,h=horizon)$mean
    p <- p.max
    q <- q.max
  }
  return(list(forecast=fc, p=p, q=q))
}

SARMA.forecast <- function(y,estimate.order=FALSE,p.max=1,q.max=1,P.max=1,Q.max=1,horizon,sarima.method="CSS")
{
  # Returns an SARMA forecast from 1-period-ahead to horizon-periods-ahead. y is a ts object
  # containing the data, estimate.order is a boolean indicating whether the order of the ARMA
  # should be estimated using the AIC. p.max is the maximum non-seasonal autoregressive order 
  # to consider when estimating the order using the AIC, or it is the actual order used if 
  # estimate.order=FALSE. Similarly, max.q is the maximum non-seasonal MA order, or the actual order
  # and P.max and Q.max are the corresponding seasonal AR and MA orders.
  # horizon is the maximum number of steps ahead to forecast. The output is a list with 5 elements:
  # $forecast is a ts object containing the forecasts. $p is a numeric containing the 
  # non-seasonal autoregressive order and $q is a numeric containing the non-seasonal moving average order.
  # P and Q contain the corresponding seasonal orders.
  #
  # This function is just a wrapper for the auto.arima function in the forecast package.
  #
  #                                      Chris Heaton, March 2019.
  if (estimate.order)
  {
    ARMA.out <- auto.arima(y,stationary=TRUE,seasonal=TRUE,max.p=p.max,max.q=q.max,d=0,D=0,max.P=P.max,max.Q=Q.max,ic="aic",stepwise=TRUE,approximation=TRUE,method=sarima.method,parallel=FALSE,allowmean=TRUE)
    fc <- forecast(ARMA.out,h=horizon)$mean
    p <- ARMA.out$arma[1]
    q <- ARMA.out$arma[2]
    P <- ARMA.out$arma[1]
    Q <- ARMA.out$arma[2]
  }else
  {
    ARMA.out <- Arima(y,order=c(p.max,0,q.max),seasonal=c(P.max,0,Q.max),include.constant=TRUE,method=sarima.method)
    fc <- forecast(ARMA.out,h=horizon)$mean
    p <- p.max
    q <- q.max
    P <- P.max
    Q <- Q.max
  }
  return(list(forecast=fc, p=p, q=q, P=P, Q=Q))
}






VAR.forecast <- function(Y,estimate.order=FALSE,p.max=1,horizon)
{ # Returns a VAR forecast from 1-period-ahead to horizon-periods-ahead. Y is an mts object
  # containing the data, estimate.order is a boolean indicating whether the order of the VAR
  # should be estimated using the AIC. p.max is the maximum order to consider when
  # estimating the order using the AIC, or it is the actual order used if estimate.order=FALSE.
  # horizon is the maximum number of steps ahead to forecast. The output is a list containing
  # two elements: $forecast is a ts object containing the forecasts; order is a numeric
  # containing the order of the VAR.
  #
  # This function is just a wrapper for the VAR function in the vars package.
  #
  #                                      Chris Heaton, March 2019.
  
  if (estimate.order)
  {
    VAR.out <- VAR(na.omit(Y),lag.max=model.parameters$VAR.lag.max,ic="AIC",type="const")
  }else
  {
    VAR.out <- VAR(na.omit(Y),p=p.max,type="const")
  }
  fc <- ts(predict(VAR.out,n.ahead=horizon)$fcst[[1]][,1],start=as.yearmon(time(na.omit(na.omit(Y))))[dim(na.omit(na.omit(Y)))[1]]+1/frequency,frequency=frequency(na.omit(na.omit(Y))))
  p <- VAR.out$p
  return(list(forecast=fc, order=p))
}


BVAR.forecast <- function(Y,p=1,horizon)
{ # Returns a Bayesian VAR forecast with the Minnesota prior from 1-period-ahead to 
  # horizon-periods-ahead. Y is an mts object
  # containing the data. p is the order of the BVAR.
  # horizon is the maximum number of steps ahead to forecast. The output is a ts containing
  # the forecasts.
  #
  # This function is just a wrapper for the BVARM function which is a wrapper for the UI
  # of the BMR package.
  #
  #                                      Chris Heaton, March 2019.
  prior <- c(1,1,1)
  BVARp <- BVARM(na.omit(Y),prior,p=p,constant=TRUE,keep=10000,burnin=5000,VType=2,HP1=0.5,HP2=0.5,HP3=100)
  fc <- ts(forecast(BVARp,periods=horizon,shocks=FALSE,save=FALSE,plot=FALSE)$forecast_mean[,1],start=as.yearmon(time(na.omit(na.omit(Y))))[dim(na.omit(na.omit(Y)))[1]]+1/frequency,frequency=frequency(na.omit(na.omit(Y))))
}

Factor.forecast <- function(y,X,k,horizon,criterion=NULL)
{ # Returns a factor forecast where the factors are estimated using Bai and Ng's (2002)
  # principal components estimator. y is a ts containing the variable to be forecast. X
  # is an mts containing the variables used in the factor analysis. horizon is an integer
  # indicating the maximum forecast horizon. If k is an integer and criterion=NULL, then
  # k factors will be estimated. Alternatively, if criterion is one of "PC.1", "PC.2", "PC.3",
  # "IC.1", "IC.2", "IC.3", then that criterion is used to estimate the order
  # See Bai and Ng (2002) p.201 for definitions. The output is a list. The first element
  # is $forecast which is a ts object containing the forecasts. The second element is
  # order which contains the estimated order.
  factor.output <- Bai.Ng.2002(X,k,criterion)  # Do the factor analysis.
  if (!is.null(criterion))
  { # Get k if it was estimated.
    cmd <- paste("k <- factor.output$est.k$",criterion,sep="")
    eval(parse(text=cmd))
  }
  F <- ts(factor.output$F.bar,start=as.yearmon(time(X))[1],frequency=frequency(X)) # Get the factors.
  fc <- NULL
  for (h in 1:horizon)
  {
    reg <- dynlm(y~lag(F,-h))
    fc <- c(fc, sum(reg$coeff*c(1,F[dim(F)[1],])))
  }
  fc <- ts(fc,start=as.yearmon(time(F)[dim(F)[1]])+1/frequency(F),frequency=frequency(F))
  return(list(forecast=fc, order=k))
}

FAVAR.forecast <- function(Y,X,k,p.max=1,horizon,factor.criterion=NULL,estimate.VAR.order=FALSE)
{ # Returns a factor-VAR forecast where the factors are estimated using Bai and Ng's (2002)
  # principal components estimator. Y is an mts containing the variables of the VAR (without
  # the factors. The first element of Y is the variable to be forecast. X
  # is an mts containing the variables used in the factor analysis. horizon is an integer
  # indicating the maximum forecast horizon. If k is an integer and factor.criterion=NULL, then
  # k factors will be estimated. Alternatively, if factor.criterion is one of "PC.1", "PC.2", "PC.3",
  # "IC.1", "IC.2", "IC.3", then that criterion is used to estimate the order
  # See Bai and Ng (2002) p.201 for definitions. p.max is the maximum order to consider when
  # estimating the order using the AIC, or it is the actual order used if estimate.order=FALSE.
  # The output is a list. The first element
  # is $forecast which is a ts object containing the forecasts. The second element is
  # order which contains the estimated order. The third element is the order of the VAR.
  factor.output <- Bai.Ng.2002(X,k,factor.criterion)  # Do the factor analysis.
  { # Get k if it was estimated.
    cmd <- paste("k <- factor.output$est.k$",criterion,sep="")
    eval(parse(text=cmd))
  }  
  F <- ts(factor.output$F.bar,start=as.yearmon(time(X))[1],frequency=frequency(X)) # Get the factors.
  Z2 <- cbind(Y,F)  # Create the vector for the VAR.
  VAR.output <- VAR.forecast(Z2,estimate.order=estimate.VAR.order,p.max=p.max,horizon)
  return(list(forecast=VAR.output$forecast, factor.order=k, VAR.order=VAR.output$order))
}
