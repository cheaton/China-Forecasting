library(zoo)
library(strucchange)
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


# Plot the errors and save the plots in the ../Output/Plots directory.
for (h in horizons)
{
    for (variable in variable.names)
    {
        for (model in forecast.models)
        {
            pdf(paste("../Output/Error_Analysis/Error_plot_",variable,"_",model,"_h_",h,".pdf",sep=""))
            cmd <- paste("e <- Errors$",variable,"$'h=",h,"'[,'",model,"']",sep="")
            eval(parse(text=cmd))
            plot(e,ylab='Error')
            dev.off()
        }
    }
}




# Plot the MSPE and save in ../Output/Plots directory.
for (h in horizons)
{
    for (variable in variable.names)
    {
        for (model in forecast.models)
        {
            pdf(paste("../Output/Error_Analysis/MSPE_",variable,"_",model,"_h_",h,".pdf",sep=""))
            cmd <- paste("e <- Errors$",variable,"$'h=",h,"'[,'",model,"']",sep="")
            eval(parse(text=cmd))
            e2 <- e^2
            plot(e2,ylab="MSPE")
            dev.off()
        }
    }
}


# Plot the MSPE in 24 month rolling windows ../Output/Plots directory.
for (h in horizons)
{
    for (variable in variable.names)
    {
        for (model in forecast.models)
        {
            pdf(paste("../Output/Error_Analysis/MSPE_24_",variable,"_",model,"_h_",h,".pdf",sep=""))
            cmd <- paste("e <- Errors$",variable,"$'h=",h,"'[,'",model,"']",sep="")
            eval(parse(text=cmd))
            e2 <- e^2
            cma_e2 <- na.omit(ma(e2, order=24, centre=TRUE))
            plot(cma_e2,ylab="MSPE")
            dev.off()
        }
    }
}

# Plot the MSPE difference from the baseline in 24 month rolling windows ../Output/Plots directory.
for (h in horizons)
{
    for (variable in variable.names)
    {
        if (variable=="PPI")
        {
            baseline <- "Naive"
        }else
        {
            baseline <- "Mean"
        }
        for (model in setdiff(forecast.models,baseline))
        {
            pdf(paste("../Output/Error_Analysis/Diff_MSPE_24_",variable,"_",model,"_h_",h,".pdf",sep=""))
            cmd <- paste("e <- Errors$",variable,"$'h=",h,"'[,'",model,"']",sep="")
            eval(parse(text=cmd))
            cmd <- paste("b <- Errors$",variable,"$'h=",h,"'[,'",baseline,"']",sep="")
            eval(parse(text=cmd))
            diffe2 <- e^2 - b^2
            cma_diffe2 <- na.omit(ma(diffe2, order=24, centre=TRUE))
            y.label <- paste("MSPE(",model,") - MSPE(",baseline,")",sep="")
            plot(cma_diffe2,ylab=y.label)
            dev.off()
        }
    }
}


# Plot the MSPE difference from the baseline and save in ../Output/Plots directory.
for (h in horizons)
{
    for (variable in variable.names)
    {
        if (variable=="PPI")
        {
            baseline <- "Naive"
        }else
        {
            baseline <- "Mean"
        }
        for (model in setdiff(forecast.models,baseline))
        {
            pdf(paste("../Output/Error_Analysis/Diff_MSPE_",variable,"_",model,"_h_",h,".pdf",sep=""))
            cmd <- paste("e <- Errors$",variable,"$'h=",h,"'[,'",model,"']",sep="")
            eval(parse(text=cmd))
            cmd <- paste("b <- Errors$",variable,"$'h=",h,"'[,'",baseline,"']",sep="")
            eval(parse(text=cmd))
            diffe2 <- e^2 - b^2
            y.label <- paste("MSPE(",model,") - MSPE(",baseline,")",sep="")
            plot(diffe2,ylab=y.label)
            dev.off()
        }
    }
}


# Calculate p-values for structural break tests.

supFtests <- list()
GFPtests <- list()
num.models <- length(forecast.models)-1
num.horizons <- length(horizons)
for (variable in variable.names)
{
    if (variable=="PPI")
    {
        baseline <- "Naive"
    }else
    {
        baseline <- "Mean"
    }
    supF.table <- matrix(rep(NA,(num.models*num.horizons)),ncol=num.horizons)
    colnames(supF.table) <- paste("h=",as.character(horizons),sep="")
    rownames(supF.table) <- setdiff(forecast.models,baseline)
    GFP.table <- supF.table 
    for (h in horizons)
    {
        for (model in setdiff(forecast.models,baseline))
        {
            cmd <- paste("e <- Errors$",variable,"$'h=",h,"'[,'",model,"']",sep="")
            eval(parse(text=cmd))
            cmd <- paste("b <- Errors$",variable,"$'h=",h,"'[,'",baseline,"']",sep="")
            eval(parse(text=cmd))
            diffe2 <- e^2 - b^2
            GFP <- gefp(diffe2 ~ 1, fit = lm, vcov=kernHAC)
            supF <- Fstats(diffe2 ~ 1, vcov=vcovHAC)
            cmd <- paste("GFP.table['",model,"','h=",h,"'] <- sctest(GFP)$p.value",sep="")
            eval(parse(text=cmd))
            cmd <- paste("supF.table['",model,"','h=",h,"'] <- sctest(supF)$p.value",sep="")
            eval(parse(text=cmd))
        }
    }
    cmd <- paste("GFPtests$",variable," <- GFP.table",sep="")
    eval(parse(text=cmd))
    cmd <- paste("supFtests$",variable," <- supF.table",sep="")
    eval(parse(text=cmd))
}

library(xtable)
for (variable in variable.names)
{
    table.heading <- paste(variable,": GEFP Test p-values",sep="")
    cmd <- paste("print.xtable(xtable(GFPtests$",variable,",digits=3,caption='",table.heading,"'),file='../Output/Error_Analysis/GEFP_table_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
    eval(parse(text=cmd))

    table.heading <- paste(variable,": supF Test p-values",sep="")
    cmd <- paste("print.xtable(xtable(supFtests$",variable,",digits=3,caption='",table.heading,"'),file='../Output/Error_Analysis/supF_table_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
    eval(parse(text=cmd))
}

# Create a PDF document containing all the error plots.
source("Create_document_with_error_plots.R")
