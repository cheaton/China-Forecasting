library(tseries)
library(forecast)
library(xtable)
library(seasonal)
library(lubridate)
library(dynlm)
library(xts)
library(splus2R)
library(tictoc)
library(zoo)
library(latex2exp)

R <- 100
T <- length
outlier.multiple <- 5
alpha <- 0.05

source("Read_the_data.R")
source("clean.ts.R")

variable.names <- "IP"
for (varname in variable.names)
{
  cmd <- paste("y.all <- ",varname,sep="")
  eval(parse(text=cmd))
  
  T <- length(y.all)
  cv <- qt(1-(alpha/2),df=(R-2))
  coeffs <- ts(rep(NA,length(y.all)),start=start(y.all),frequency=frequency(y.all))
  CI.uppers <- ts(rep(NA,length(y.all)),start=start(y.all),frequency=frequency(y.all))
  CI.lowers <- ts(rep(NA,length(y.all)),start=start(y.all),frequency=frequency(y.all))
  
  for (r in R:T)
  {
    this.end.date <- as.yearmon(time(y.all)[r]); frequency <- frequency(y.all)
    y <- ts(y.all[(r-R+1):r],end=this.end.date,frequency=frequency)
  
    # Deal with outliers
    if (varname=="EP")
    {
      y <- clean.ts(y,multiple=outlier.multiple,interpolate="none",difference=TRUE)$x.clean
    }else
    {
      y <- clean.ts(y,multiple=outlier.multiple,interpolate="cubic",difference=TRUE)$x.clean
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
  
    # Take first differences of y and Y if necessary.
    if (varname == "IP")
    {
      y <- diff(y)
    }
  
    # Estimate the AR(1)
    out <- dynlm(y~lag(y,-1))
    coeff <- summary(out)$coefficients[2,"Estimate"]
    se <- summary(out)$coefficients[2,"Std. Error"]
    CI.upper <- coeff + cv*se
    CI.lower <- coeff - cv*se
    
    # Store the results
    end.year <- end(y)[1]
    end.month <- end(y)[2]
    
    window(coeffs,start=c(end.year,end.month),end=c(end.year,end.month)) <- coeff
    window(CI.uppers,start=c(end.year,end.month),end=c(end.year,end.month)) <- CI.upper
    window(CI.lowers,start=c(end.year,end.month),end=c(end.year,end.month)) <- CI.lower
  }
  if (varname=="IP")
  {
    plot.title <- TeX("$\\Delta$ IP")
  }else
  {
    plot.title <- varname
  }
  pdf(paste("../Output/Plots/AR(1)_Coefficient_",varname,".pdf",sep=""))
#  plot(na.omit(coeffs),ylim=c(min(na.omit(CI.lowers)),max(na.omit(CI.uppers))),ylab="")
  plot(na.omit(coeffs),ylim=c(-0.7,1),ylab="",main=plot.title)
  lines(na.omit(CI.uppers),lty="dotted")
  lines(na.omit(CI.lowers),lty="dotted")
  dev.off()
}
