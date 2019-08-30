library(zoo)
library(forecast)

# First, set some parameters (if they have not already been set).
if (!("horizons" %in% ls()))
{
    horizons <- c(1,3,6,9,12)
}
if (!("variable.names" %in% ls()))
{
    variable.names <- c("CPI","PPI","IP","EP")
}
if (!("forecast.models" %in% ls()))
{
    forecast.models <- c("Mean","Naive","MA","MA-opt","SES","SES-opt","AR(1)","AR(p)","ARd(1)","ARd(p)","ARMA(1,1)","ARMA(p,q)","VAR(1)","VAR(p)","BVAR","Factor(2)","Factor(k)","F(2)VAR(1)","F(2)VAR(p)")
}
type <- "Levels"


# Now read in all the forecasts for all the variables at all the horizons, compute the errors and put them in a list called Errors.
Errors <- list()
for (h in horizons)
{
    for (variable in variable.names)
    {
        # Read in the forecasts from the files.
        if (variable=="IP")
        {
            diff.or.level <- "Differences"
        }else {
           diff.or.level <- "Levels"
        }
        cmd <- paste("fcs.frame <- read.csv(file='../Output/Forecasts/Forecasts_",diff.or.level,"_",variable,"_h_",h,".csv',header=TRUE,stringsAsFactors=FALSE)",sep="")
        eval(parse(text=cmd))
        fcs.frame[,1] <- as.yearmon(as.Date(paste("1",fcs.frame[,1]),format="%d %b %Y"))
        fcs.zoo <- zoo(fcs.frame[,2:dim(fcs.frame)[2]],order.by=fcs.frame[,1])
        fcs <- as.ts(fcs.zoo)
        fcs.names <- forecast.models

        # Compute the errors
        errors <- na.omit(fcs[,2:dim(fcs)[2]] - fcs[,1])
        colnames(errors) <- fcs.names

        # Put in list
        cmd <- paste("Errors$",variable,"$'h=",h,"' <- errors",sep="")
        eval(parse(text=cmd))
    }
}

# Now construct a mts object for each variable with a name like sq.Error.diffs.CPI which contains all the diffferences of the squared errors of each forecasting model and the baseline forecasting model at each horizon. The column means of each of these mts objects is equal to the difference in the mean squared errors of each forecasting model and the baseline at each horizon, and should match the values that can be calculated from the tables that were previously computed.

for (variable in variable.names)
{
    cmd <- paste("sq.Error.diffs.",variable," <- NULL",sep="")
    eval(parse(text=cmd))
    the.names <- NULL
    if (variable=="PPI")
    {
        baseline <- "Naive"
    }else
    {
        baseline <- "Mean"
    }
    for (h in horizons)
    {
        for (model in setdiff(forecast.models,baseline))
        {
            cmd <- paste("b2 <- (Errors$",variable,"$'h=",h,"'[,baseline])^2",sep="")
            eval(parse(text=cmd))
            cmd <- paste("e2 <- (Errors$",variable,"$'h=",h,"'[,-which(colnames(Errors$",variable,"$'h=",h,"')==baseline)])^2",sep="")
            eval(parse(text=cmd))
            dif <- e2 - b2
            colnames(dif) <- paste(variable,".",colnames(e2),".h=",h,sep="")
        }
        the.names <- c(the.names,colnames(dif))
        cmd <- paste("sq.Error.diffs.",variable," <- cbind(sq.Error.diffs.",variable,",dif)",sep="")
        eval(parse(text=cmd))
        cmd <- paste("colnames(sq.Error.diffs.",variable,") <- the.names",sep="")
        eval(parse(text=cmd))
    }
    cmd <- paste("sq.Error.diffs.",variable," <- na.omit(sq.Error.diffs.",variable,")",sep="")
    eval(parse(text=cmd))
}

SPA.table <- matrix(rep(NA,length(variable.names)*3),ncol=3)
rownames(SPA.table) <- variable.names
colnames(SPA.table) <- c("Vbar","cv","pvalue")
for (i in 1:length(variable.names))
{
    cmd <- paste("out <- SPA(sq.Error.diffs.",variable.names[i],")",sep="")
    eval(parse(text=cmd))
    SPA.table[i,1] <- out$Vbar
    SPA.table[i,2] <- out$cv
    SPA.table[i,3] <- out$pvalue
}

write.csv(file="../Output/pvalue_tables/SPA.table.csv",SPA.table)