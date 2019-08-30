## Plots all the variables used for the factor analysis ##

library(tseries)
library(uroot)
library(BMR)
library(forecast)
library(vars)
library(xtable)
library(seasonal)
library(lubridate)
library(dynlm)
library(xts)
library(doParallel)
library(splus2R)
library(tictoc)
library(zoo)

outlier.multiple <- 5 
source("Latex.creation.R") # Some functions that help build a .tex file.
source("clean.ts.R") 
source("transformations.R") 

source("Read_the_data.R")  # Read in the data. See the file for the names of the ts objects created.
source("Clean_up_data.R")

num.var <- dim(FA)[2]
for (i in 35:num.var)
{
  X11()
  par(mfrow=c(4,1))
  plot(FA[,i],ylab=colnames(FA)[i],main=paste(i,". ",colnames(FA)[i],": Levels (0)",sep=""))
  plot(log(FA[,i]),ylab=colnames(FA)[i],main=paste(i,". ",colnames(FA)[i],": Logs (1)",sep=""))
  plot(diff(FA[,i]),ylab=colnames(FA)[i],main=paste(i,". ",colnames(FA)[i],": Diffs (2)",sep=""))
  plot(diff(log(FA[,i])),ylab=colnames(FA)[i],main=paste(i,". ",colnames(FA)[i],": Diffs of Logs (3)",sep=""))
  print(paste(i,". Minimum of ",colnames(FA)[i]," is ",min(FA[,i]),sep=""))
}
