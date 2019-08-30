#########################################################################################
## Reads in the data sets from the directory ../Datasets. If you add any datasets
## you will need to parse the dates and ensure that you create a valid ts object.
## Also if you add or delete any forecasted variables, add their names to variable.names
## since this is used later on.
##########################################################################################

## Variables to be forecast ##

CPI.frame <- read.csv(file="../Datasets/CPI_MOM_datastream_short.csv",header=TRUE)
CPI.frame[,1] <- as.yearmon(as.Date(CPI.frame[,1]))
CPI.zoo <- zoo(CPI.frame[,2],order.by=CPI.frame[,1])
CPI <- as.ts(CPI.zoo)

PPI.frame <- read.csv(file="../Datasets/PPI_Producer_goods_datastream_short.csv",header=TRUE)
PPI.frame[,1] <- as.yearmon(as.Date(PPI.frame[,1]))
PPI.zoo <- zoo(PPI.frame[,2],order.by=PPI.frame[,1])
PPI <- as.ts(PPI.zoo)

IP.frame <- read.csv(file="../Datasets/IP_datastream_short.csv",header=TRUE)
IP.frame[,1] <- as.yearmon(as.Date(IP.frame[,1]))
IP.zoo <- zoo(IP.frame[,2],order.by=IP.frame[,1])
IP <- as.ts(IP.zoo)

EP.frame <- read.csv(file="../Datasets/IP_Electricity_datastream_short.csv",header=TRUE)
EP.frame[,1] <- as.yearmon(as.Date(EP.frame[,1]))
EP.zoo <- zoo(EP.frame[,2],order.by=EP.frame[,1])
EP <- as.ts(EP.zoo)
EP <- (EP - lag(EP,-1))/lag(EP,-1)*100

# EP.frame <- read.csv(file="../Datasets/EP3.csv",header=TRUE)
# EP.frame[,1] <- as.yearmon(as.Date(EP.frame[,1]))
# EP.zoo <- zoo(EP.frame[,2],order.by=EP.frame[,1])
# EP <- as.ts(EP.zoo)


## Extra variable for VAR ##

Rate.frame <- read.csv(file="../Datasets/Prime_lending_rate_datastream_short.csv",header=TRUE)
Rate.frame[,1] <- as.yearmon(as.Date(Rate.frame[,1]))
Rate.zoo <- zoo(Rate.frame[,2],order.by=Rate.frame[,1])
Rate <- as.ts(Rate.zoo)

# ## Variables for factor analysis in the first revision ##

# FA.frame <- read.csv(file="../Datasets/data_for_factor_analysis_short.csv",header=TRUE)
# FA.frame[,1] <- as.yearmon(as.Date(FA.frame[,1]))
# FA.zoo <- zoo(FA.frame[,2:dim(FA.frame)[2]],order.by=FA.frame[,1])
# FA <- as.ts(FA.zoo)

# transformation.codes <- read.csv(file="../Datasets/transformation_codes.csv",header=FALSE) # These codes indicate whether the variables for the factor analysis should be logged and/or differenced. See the tranformations function.

## Variables for factor analysis in the second revision. This includes railway cargo as variable number 17 and the last 10 variables are trade variables from the IMF ##

FA.frame <- read.csv(file="../Datasets/data_for_factor_analysis_short_2nd_rev.csv",header=TRUE)
FA.frame[,1] <- as.yearmon(as.Date(FA.frame[,1]))
FA.zoo <- zoo(FA.frame[,2:dim(FA.frame)[2]],order.by=FA.frame[,1])
FA <- as.ts(FA.zoo)

transformation.codes <- read.csv(file="../Datasets/transformation_codes_2nd_rev.csv",header=FALSE) # These codes indicate whether the variables for the factor analysis should be logged and/or differenced. See the tranformations function.



variable.names <- c("CPI","PPI","IP","EP")



# Subset variables if necessary.
#CPI <- window(CPI,end=c(2008,7))
#PPI <- window(PPI,end=c(2008,7))
#IP <- window(IP,end=c(2008,7))
#EP <- window(EP,end=c(2008,7))
#Rate <- window(Rate,end=c(2008,7))
#FA <- window(FA,end=c(2008,7))

# CPI <- window(CPI,start=c(2001,9))
# PPI <- window(PPI,start=c(2001,9))
# IP <- window(IP,start=c(2001,9))
# EP <- window(EP,start=c(2001,9))
# Rate <- window(Rate,start=c(2001,9))
# FA <- window(FA,start=c(2001,9))
