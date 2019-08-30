forecast.simulation <- function(y.all,Var.all,FA.all,R,horizon,forecast.models,model.parameters,criterion,orders.names,outlier.multiple,varname,whereami,first.diffs=FALSE)
{
  # Conducts a forecasting simulation with a fixed sample size of R. In the first iteration, the first
  # R observations of y.all are used to estimate any parameters, and forecasts from 1-step-ahead to
  # horizon-steps-ahead are computed using all the forecasting models in forecast.models. The first
  # observation of the sample is then dropped, and the (R+1)st observation is added, and the next
  # set of forecasts is computed. This procedure is repeated, dropping the first observation and 
  # adding another observation, until forecasts have been calculated at all possible dates using
  # all the foreasting models estimated with sample sizes of R. The function
  # returns a list containing lists of all the forecasts at all horizons and, if any of the models
  # have orders that require 'estimation', the estimated orders.
  #
  # INPUTS: y.all = a ts object containing the data.
  #         Var.all = a mts object containing the data for the VAR model with the variable to be forecast
  #                   in the first column.
  #         FA.all = a mts object containing the data for the factor analysis.
  #         R = the size of the sample, e.g. 100.
  #         horizon = the longest horizon for which forecasts are computed. Note that forecasts 
  #                   will be computed for all horizons from 1 to horizon. e.g. 12
  #         forecast.models = a vector of characters indicating which forecasting models will be used.
  #                           Currently, this list must be a subset of forecast.models <- 
  #                           c("Mean","Naive","MA","MA-opt","SES","SES-opt","AR(1)","AR(p)",
  #                           "ARMA(1,1)","ARMA(p,q)","VAR(1)","VAR(p)","BVAR(4)","Factor(2)","Factor(k)",
  #                           "F(2)VAR(1)","F(2)VAR(p)").
  #          model.parameters = a list containing all the parameters needed for the forecasting models. 
  #                             e.g. model.parameters <- list(order.max.AR=24, order.max.ARIMA.AR=2,
  #                             order.max.ARIMA.MA=2, VAR.lag.max=4, SES.alpha=0.5,
  #                             max.num.factors = 10, factor.criterion = "PC.1", MA.order = 4,
  #                             max.MA.order = 12)
  #          criterion = a character indicating which criterion to use to choose the order of the MA
  #                      forecast. Currently, the options are c("MSE","RMSE","MAE","MAPE").
  #          orders.names = a vector of characters indicating the names of the variables that will
  #                         be returned containing the estimated model orders. These might be a
  #                         subset of c("MA.orders", "SES.alpha", "AR.orders", "ARMA.AR.orders",
  #                         "ARMA.MA.orders", "VAR.orders", "factor.orders"). If there are no models
  #                         with estimated orders then orders.names <- NULL.
  #          whereami = a boolean. If true then prints to screen the date of each sample.
  #
  # OUTPUTS: A list containing 2 elements:
  #
  #         $all.forecasts = a list containing horizons elements with names $"h=i" where i is
  #                          an integer from 1 to horizons. Each element contains an mts object
  #                          that contains all the forecsts for that horizon. Note that each forecast
  #                          is aligned with the date of the event that it forecasts. For example, in
  #                          $all.forecasts$"h=1", the numbers in the row marked Jun 2005 are the 
  #                          forecasts computed in May 2005 of the outcomes in Jun 2005.
  #         $orders = NULL if there are no models with estimated orders. Otherwise, an mts object containing
  #                   the estimated orders for each model at each date. Note that the orders are aligned
  #                   with the last date in each subsample. For example, the numbers in the row marked
  #                   Jun 2005 are the orders estimated using the subsample with the last observation
  #                   in Jun 2005, which was then used to compute forecsts of things that happened in time
  #                   periods after Jun 2005.
  #
  #                                                          Chris Heaton, March 2019.
  library(zoo)
  library(foreach)
  # Set some parameters
  T <- length(y.all)
  # T <- dim(na.omit(cbind(y.all,Var.all,FA.all)))[1]
  frequency <- frequency(y.all)
  end.date <- as.yearmon(time(y.all))[R] # The date of the last observation in the first subsample.

  # Run the loop. The output is a list which has an element for each date at which a forecast is made
  # (i.e. the last date in each subsample). Each element contains a list with 2 elements: $forecasts
  # and $orders. $forecasts is a matrix with a row for each forecasting model listed in 
  # forecasting.models, and a column for horizons from 1 to horizon. $orders contains the orders
  # estimated by the forecasting models. If no models with estimated orders were used, $orders will be NULL.

  output <- foreach(r=R:(T), .packages=c('zoo','dynlm','vars'), .export=all.functions) %dopar%
  {
      # Subset the data.
      this.end.date <- as.yearmon(time(y.all)[r]); frequency <- frequency(y.all)
      y <- ts(y.all[(r-R+1):r],end=this.end.date,frequency=frequency)
      Y <- ts(Var.all[(r-R+1):r,],end=this.end.date,frequency=frequency)
      FA <- ts(FA.all[(r-R+1):r,],end=this.end.date,frequency=frequency)

      if (whereami){cat('\r',paste("Estimating ",variable,"models with subsample ending at",this.end.date,"\n"))}

      # Clean up the data.

      # First remove outliers and interpolate missing values.
      if (varname=="EP")
      {
        y <- clean.ts(y,multiple=outlier.multiple,interpolate="none",difference=TRUE)$x.clean
      }else
      {
        y <- clean.ts(y,multiple=outlier.multiple,interpolate="cubic",difference=TRUE)$x.clean
      }

      Y[,1] <- y  # The first element of Y is y
      Y[,2] <- clean.ts(Y[,2],multiple=outlier.multiple,interpolate="cubic",difference=TRUE)$x.clean  # The third element of Y is always Rate which has no missing values.

      for (i in 1:dim(FA)[2])
      {
        FA[,i] <- clean.ts(FA[,i],multiple=5,interpolate="cubic",difference=TRUE)$x.clean
      }

                # Now seasonally adjust.
      if (varname=="EP")
      {
        y <- final(seas(
        x = y,
        xreg = genhol(cny, start = 0, end = 6, center = "calendar"),
        regression.aictest = "td",
        regression.usertype = "holiday",
        transform.function = "none",
        outlier = NULL,
        na.action = na.x13
        ))
      }else
      {
        y <- final(seas(
        x = y,
        xreg = genhol(cny, start = 0, end = 6, center = "calendar"),
        regression.aictest = "td",
        regression.usertype = "holiday",
        outlier = ""
        ))
      }

      Y[,1] <- y # The first element of Y is y
      Y[,2] <- final(seas(
      x = Y[,2],
      xreg = genhol(cny, start = 0, end = 6, center = "calendar"),
      regression.aictest = "td",
      regression.usertype = "holiday",
      outlier = ""
      ))  # The third element of Y is always Rate which is non-seasonal.

      for (i in 1:dim(FA)[2])
      {
          # #         # if (!(i %in% c(26,27,28,32,39))) # These have features that cause problems for X-13 in the first revision.
        if (!(i %in% c(10, 27,28,29,33,39,40))) # These have features that cause problems for X-13 in the second revision.
        {
          # cat(paste("\n Factor variable number ",i,"\n",sep=""))
          FA[,i] <- final(seas(
          x = FA[,i],
          xreg = genhol(cny, start = 0, end = 6, center = "calendar"),
          regression.aictest = "td",
          x11 = "",
          transform.function = "none",
          regression.usertype = "holiday",
          outlier = NULL
          ))
        }
      }

      # Take first differences of y and Y if necessary.
      if (first.diffs)
      {
        y <- diff(y)
        Y <- diff(Y)
      }

      # Apply tranformations to data for factor analysis.
      FA <- transformations(FA,transformation.codes)
      FA <- scale(FA, center=TRUE, scale=TRUE)   # Second revision
      FA[is.na(FA)] <- 0
      # write.csv(data.frame(as.yearmon(time(FA)),FA),file="FA.data.for.checking.csv",row.names=FALSE)

# if (r==200)
# {
#   write.csv(data.frame(as.yearmon(time(y)),y),file="data.for.checking.csv",row.names=FALSE)
# }

      # Compute the forecasts.
      forecast.matrix <- NULL
      order.matrix <- NULL

      if ("Mean" %in% forecast.models)
      {
        Mean <- mean.forecast(y,horizon)
        forecast.matrix <- rbind(forecast.matrix,Mean)
      }

      if ("Dummy.regression" %in% forecast.models)
      {
        CNY <- create.cny.dummy(y,horizon=horizon)
        Dummy.regression <- season.dummy.forecast(y,xreg=CNY$cny.dummy,future.xreg=CNY$future.cny.dummy,horizon=horizon)
        forecast.matrix <- rbind(forecast.matrix,Dummy.regression)
      }

      if ("Naive" %in% forecast.models)
      {
        Naive <- naive.forecast(y,horizon)
        forecast.matrix <- rbind(forecast.matrix,Naive)
      }

      if ("MA" %in% forecast.models)
      {
        MA <- MA.forecast(y,horizon,order=model.parameters$MA.order,criterion=NULL)$forecast
        forecast.matrix <- rbind(forecast.matrix,MA)
      }

      if ("MA-opt" %in% forecast.models)
      {
        MA.out <- MA.forecast(y,horizon,order=model.parameters$max.MA.order,criterion=criterion)
        MA.opt <- MA.out$forecast
        forecast.matrix <- rbind(forecast.matrix,MA.opt)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "MA-opt"
        order.matrix <- c(order.matrix,MA.out$order)
      }

      if ("SES" %in% forecast.models)
      {
        SES <- SES.forecast(y,horizon,alpha=model.parameters$SES.alpha)$forecast
        forecast.matrix <- rbind(forecast.matrix,SES)
      }

      if ("SES-opt" %in% forecast.models)
      {
        SES.out <- SES.forecast(y,horizon,alpha=NULL)
        SES.opt <- SES.out$forecast
        forecast.matrix <- rbind(forecast.matrix,SES.opt)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "SES-opt"
        order.matrix <- c(order.matrix,SES.out$alpha)
      }
  
      if ("HW" %in% forecast.models)
      {
        HW <- HW.forecast(y,horizon,alpha=model.parameters$HW.alpha,beta=model.parameters$HW.beta,gamma=model.parameters$HW.gamma,seasonal.type="additive")$forecast
        forecast.matrix <- rbind(forecast.matrix,HW)
      }

      if ("HW-opt" %in% forecast.models)
      {
        HW.out <- HW.forecast(y,horizon,seasonal.type="additive")
        HW.opt <- HW.out$forecast
        forecast.matrix <- rbind(forecast.matrix,HW.opt)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "HW-opt"
        order.matrix <- c(order.matrix,HW.out$alpha,HW.out$beta,HW.out$gamma)
      }

      if ("AR(1)" %in% forecast.models)
      {
        AR.1 <- AR.forecast(y,estimate.order=FALSE,order.max=1,horizon)
        forecast.matrix <- rbind(forecast.matrix,AR.1$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "AR(1)"
      }
      
      if ("AR(p)" %in% forecast.models)
      {
        AR.p <- AR.forecast(y,estimate.order=TRUE,order.max=model.parameters$order.max.AR,horizon)
        forecast.matrix <- rbind(forecast.matrix,AR.p$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "AR(p)"
        order.matrix <- c(order.matrix,AR.p$order)
      }

      if ("ARd(1)" %in% forecast.models)
      {
        AR.d1 <- AR.direct(y,estimate.order=FALSE,order.max=1,horizon)
        forecast.matrix <- rbind(forecast.matrix,AR.d1$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "ARd(1)"
      }

      if ("ARd(p)" %in% forecast.models)
      {
        AR.dp <- AR.direct(y,estimate.order=TRUE,order.max=model.parameters$order.max.AR,horizon)
        forecast.matrix <- rbind(forecast.matrix,AR.dp$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "ARd(p)"
      }

      if ("SAR(1,1)" %in% forecast.models)
      {
        SAR.11 <- SARMA.forecast(y,estimate.order=FALSE,p.max=1,q.max=0,P.max=1,Q.max=0,horizon,sarima.method)
        forecast.matrix <- rbind(forecast.matrix,SAR.11$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "SAR(1,1)"
      }
      
      if ("SAR(p,P)" %in% forecast.models)
      {
        SAR.pP <- SARMA.forecast(y,estimate.order=TRUE,p.max=model.parameters$order.max.SAR.p,q.max=0,P.max=model.parameters$order.max.SAR.P,Q.max=0,horizon,sarima.method)
        forecast.matrix <- rbind(forecast.matrix,SAR.pP$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "SAR(p,P)"
        order.matrix <- c(order.matrix,SAR.pP$p)
        order.matrix <- c(order.matrix,SAR.pP$P)
      }
      
      if ("ARMA(1,1)" %in% forecast.models)
      {
        ARMA.11 <- ARMA.forecast(y,estimate.order=FALSE,p.max=1,q.max=1,horizon)
        forecast.matrix <- rbind(forecast.matrix,ARMA.11$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "ARMA(1,1)"
      }
      
      if ("ARMA(p,q)" %in% forecast.models)
      {
        ARMA.pq <- ARMA.forecast(y,estimate.order=TRUE,p.max=model.parameters$order.max.ARIMA.AR,q.max=model.parameters$order.max.ARIMA.MA,horizon)
        forecast.matrix <- rbind(forecast.matrix,ARMA.pq$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "ARMA(p,q)"
        order.matrix <- c(order.matrix,ARMA.pq$p)
        order.matrix <- c(order.matrix,ARMA.pq$q)
      }
 
      if ("SARMA(1,1)(1,1)" %in% forecast.models)
      {
        SARMA.11.11 <- SARMA.forecast(y,estimate.order=FALSE,p.max=1,q.max=1,P.max=1,Q.max=1,horizon,sarima.method)
        forecast.matrix <- rbind(forecast.matrix,SARMA.11.11$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "SARMA(1,1)(1,1)"
      }

      if ("SARMA(p,q)(P,Q)" %in% forecast.models)
      {
        SARMA.pPqQ <- SARMA.forecast(y,estimate.order=TRUE,p.max=model.parameters$order.max.SARMA.p,q.max=model.parameters$order.max.SARMA.q,P.max=model.parameters$order.max.SARMA.P,Q.max=model.parameters$order.max.SARMA.Q,horizon,sarima.method)
        forecast.matrix <- rbind(forecast.matrix,SARMA.pPqQ$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "SARMA(p,q)(P,Q)"
        order.matrix <- c(order.matrix,SARMA.pPqQ$p)
        order.matrix <- c(order.matrix,SARMA.pPqQ$q)
        order.matrix <- c(order.matrix,SARMA.pPqQ$q)
        order.matrix <- c(order.matrix,SARMA.pPqQ$Q)
      }

      if ("VAR(1)" %in% forecast.models)
      {
        VAR.1 <- VAR.forecast(Y,estimate.order=FALSE,p.max=1,horizon)
        forecast.matrix <- rbind(forecast.matrix,VAR.1$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "VAR(1)"
      }
      
      if ("VAR(p)" %in% forecast.models)
      {
        VAR.p <- VAR.forecast(Y,estimate.order=TRUE,p.max=model.parameters$VAR.lag.max,horizon)
        forecast.matrix <- rbind(forecast.matrix,VAR.p$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "VAR(p)"
        order.matrix <- c(order.matrix,VAR.p$order)
      }
      
      if ("BVAR" %in% forecast.models)
      {
        BVAR <- BVAR.forecast(Y,p=model.parameters$BVAR.lag,horizon)
        forecast.matrix <- rbind(forecast.matrix,BVAR)
      }
 
      if ("Factor(2)" %in% forecast.models)
      {
        FF2 <- Factor.forecast(y,FA,2,horizon,criterion=NULL)
        forecast.matrix <- rbind(forecast.matrix,FF2$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "Factor(2)"
      }
  
      if ("Factor(k)" %in% forecast.models)
      {
        FFk <- Factor.forecast(y,FA,model.parameters$max.num.factors,horizon,criterion=model.parameters$factor.criterion)
        forecast.matrix <- rbind(forecast.matrix,FFk$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "Factor(k)"
        order.matrix <- c(order.matrix,FFk$order)
      }
 
      if ("F(2)VAR(1)" %in% forecast.models)
      {
        FF2VAR1 <- FAVAR.forecast(Y,FA,2,p.max=1,horizon,factor.criterion=NULL,estimate.VAR.order=FALSE)
        forecast.matrix <- rbind(forecast.matrix,FF2VAR1$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "F(2)VAR(1)"
        order.matrix <- c(order.matrix,FF2VAR1$VAR.order)
      }
    
      if ("F(2)VAR(p)" %in% forecast.models)
      {
        FF2VARp <- FAVAR.forecast(Y,FA,2,p.max=model.parameters$VAR.lag.max,horizon,factor.criterion=NULL,estimate.VAR.order=TRUE)
        forecast.matrix <- rbind(forecast.matrix,FF2VARp$forecast)
        rownames(forecast.matrix)[dim(forecast.matrix)[1]] <- "F(2)VAR(p)"
        order.matrix <- c(order.matrix,FF2VARp$VAR.order)
      }

      colnames(forecast.matrix) <- seq(from=1,to=horizon,by=1)
      names(order.matrix) <- orders.names
      # Combine the forecast and order matrices in a list and return.
      return(list(forecasts=forecast.matrix,orders=order.matrix))
  }
  
  names(output) <- as.character(as.yearmon(time(y.all)))[R:(T-1)] # Rename the list items as dates.
  
  ######################################
  # Now unpack the output of the loop. #
  ######################################
 
  # The following nested loop produces horizons mts objects named forecasts.h.i where i runs from 1 to 
  # horizons. The first column of each of these mts objects is the variable being forecast.
  # The subsequent columns are the forecasts computed for that date by each of the forecasting
  # models in forecasting.models. Note that the forecasts are aligned with the observations
  # that they are forecasting. For example, in forecasts.h.1, the row dated Jun 2009 contains
  # the actual data for Jun 2009 and the forecasts of the outcome in Jun 2009 computed in
  # May 2009. Consequently, forecasting errors may be computed by taking differences of the
  # appropriate columns. The horizons mts objects are put into a list named all.forecasts.
  # The nested loop also produces a single mts object names orders which contains the estimated
  # orders of all models for which the order is estimated. In this case, the date corresponds to
  # the last observation in the subsample. For example, the row of orders that is dated Jan 2015
  # is the set of estimated model orders found when using the subsample that ends in Jan 2015 and
  # is used to compute forecasts from Feb 2015 onwards (assuming monthly data).
  all.forecasts <- list()
  for (h in 1:horizon)  # For each horizon...
  {
    if (!is.null(output[[1]]$orders)) # If there are models with estimated orders...
    {
      num.orders <- length(output[[1]]$orders)
      orders <- ts(matrix(rep(NA,length(output)*num.orders),ncol=num.orders),start=end.date,frequency=frequency) #...then create a ts object to store the estimates of the orders...
      colnames(orders) <- names(output[[1]]$orders) #...and give the columns names.
    }else
    {
      orders <- NULL
    }
    num.models <- dim(output[[1]]$forecasts)[1]
    cmd <- paste("forecasts.h.",h," <- ts(matrix(rep(NA,length(output)*num.models),ncol=num.models),start=end.date,frequency=frequency)",sep="")
    eval(parse(text=cmd))  #... create a ts object to hold the forecasts.
    cmd <- paste("colnames(forecasts.h.",h,") <- rownames(output[[1]]$forecasts)",sep="")
    eval(parse(text=cmd))  #... and give the columns names,  
    for (t in 1:length(output)) # and for each time period in the output...
    {
      if (!is.null(output[[1]]$orders)) # If there are models with estimated orders...
      {
        orders[t,] <- output[[t]]$orders # ...put the estimated orders from the loop output into orders.
      }
      cmd <- paste("forecasts.h.",h,"[t,] <- output[[t]]$forecasts[,'",h,"']",sep="")
      eval(parse(text=cmd)) # Put the forecasts from the loop output in forcasts.h.
    }
 
    cmd <- paste("forecasts.h.",h," <- cbind(y.all,lag(forecasts.h.",h,",-",h,"))",sep="")
    eval(parse(text=cmd)) # Add in the actual data and align the dates appropriately.
    cmd <- paste("colnames(forecasts.h.",h,") <- c('X',rownames(output[[1]]$forecasts))",sep="")
    eval(parse(text=cmd)) # Give the columns names...
    cmd <- paste("all.forecasts$'h=",h,"' <- forecasts.h.",h,sep="")
    eval(parse(text=cmd)) #... and add it to the list all.forecasts.
  }
  return(list(all.forecasts=all.forecasts,orders=orders))
}
