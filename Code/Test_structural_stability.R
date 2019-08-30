library(lubridate)
library(strucchange)
CPI <- read.csv(file="../Output/Cleaned_data/CPI.cleaned.csv",header=TRUE,stringsAsFactors=FALSE)
starting.date <- as.Date(paste("1 ",CPI[1,1],sep=""),format="%d %b %Y")
CPI <- ts(CPI[,2],start=c(year(starting.date),month(starting.date)),frequency=12)


# ar.out <- ar(CPI,aic=TRUE,order.max=12,method="mle")

max.lag <- 1
CPI.with.lags <- CPI
the.names <- "CPI"
formula <- "CPI~"
for (i in 1:max.lag)
{
    CPI.with.lags <- cbind(CPI.with.lags,lag(CPI,-i))
    the.names <- c(the.names,paste("CPI_",i,sep=""))
    if (i!=1){formula <- paste(formula,"+",sep="")}
    formula <- paste(formula,"CPI_",i,sep="")
}
colnames(CPI.with.lags) <- the.names
formula <- formula(formula)

reg <- lm(formula,data=CPI.with.lags)
# OLS-based CUSUM test with quadratic spectral kernel HAC estimate:
ocus <- gefp(formula, data=CPI.with.lags, fit = lm, vcov = kernHAC)
X11()
plot(ocus, aggregate = FALSE)
sctest(ocus)

# supF test with quadratic spectral kernel HAC estimate:

fs <- Fstats(formula, data=CPI.with.lags, vcov = kernHAC)
X11()
plot(fs)
sctest(fs)

# Breakpoint estimation and confidence intervals with quadratic spectral kernel HAC estimate:

bp <- breakpoints(formula, data=CPI.with.lags)
confint(bp, vcov = kernHAC)
X11()
plot(bp)

# Visualization:

X11()
plot(CPI, ylab = "Forecast errors")
lines(ts(fitted(bp), start = start(CPI), freq = 12), col = 4)
lines(confint(bp, vcov = kernHAC))
