#########################
## User-set parameters ##
#########################

# Set the forecasting models that will be used. Currently, the complete list is forecast.models <- c("Mean","Naive","MA","MA-opt","SES","SES-opt","AR(1)","AR(p)","ARMA(1,1)","ARMA(p,q)","VAR(1)","VAR(p)","BVAR","Factor(2)","Factor(k)","F(2)VAR(1)","F(2)VAR(p)"). You need at least 3. One will be the benchmark and at least 2 others are needed to contruct tables of results.

forecast.models <- c("Mean","Naive","MA","MA-opt","SES","SES-opt","AR(1)","AR(p)","ARd(1)","ARd(p)","ARMA(1,1)","ARMA(p,q)","VAR(1)","VAR(p)","BVAR","Factor(2)","Factor(k)","F(2)VAR(1)","F(2)VAR(p)")
# forecast.models <- c("Mean","Naive","MA","MA-opt","SES","SES-opt","AR(1)","AR(p)","ARd(1)","ARd(p)","ARMA(1,1)","ARMA(p,q)","VAR(1)","VAR(p)","BVAR")
# forecast.models <- c("Mean","Naive","AR(1)","VAR(1)")

# List the models which have parameters that need to be estimated. If any of these models are also in forecast.models, then LaTeX tables of estimated orders will be produced and saved in the directory ../output/order_tables.
models.with.estimated.orders <- c("MA-opt","SES-opt","AR(p)","SAR(p,P)","ARMA(p,q)","VAR(p)","Factor(k)","F(2)VAR(p)") 

# Set the parameters of the models.

model.parameters <- list(order.max.AR=12, order.max.ARIMA.AR=2, order.max.ARIMA.MA=2, order.max.SAR.p=2, order.max.SAR.P=2, order.max.SARMA.p=1, order.max.SARMA.q=1,order.max.SARMA.P=1, order.max.SARMA.Q=1, VAR.lag.max=4, BVAR.lag=4,SES.alpha=0.5, HW.alpha=0.5, HW.beta=0.5, HW.gamma=0.5, max.num.factors = 10, factor.criterion = "PC.1", MA.order = 4, max.MA.order = 12)

# Set some parameters for the simulation.

R <- 100     # The number of observations to use to estimate the model parameters (if necessary) in each simulation.
horizons <- c(1,3,6,9,12)       # The forecast horizons.
benchmark = "Mean"    # One of the forecast model included in forecast.models.
criterion <- "MSE"    # "MSE", "RMSE", "MAE" or "MAPE".
alpha <- 0.05         # The significance level if you want to modify the code to produce critical values.
frequency <- 12       # The frequency of the observations.
parallel.method <- "Fork" # If "Fork" then it will fork processes. If anything else, it will use the snow package for parallelisation. Fork will not work on Windows or with the BVARM function.
num.cores <- 40        # The number of processor cores to use.
outlier.multiple <- 5   # An observation is counted as an outlier if it is outlier.multiple times the IQR, or if the first difference is outlier.multiple times the IQR of the first differences. You can choose which on a variable-by-variable basis by setting the differnce option in the calls to clean.ts in Clean_up_data.R.
CPI.outlier.multiple <- 5
PPI.outlier.multiple <- 5
IP.outlier.multiple <- 5
EP.outlier.multiple <- 5

sarima.method <- "CSS"  # The method used to estimate seasonal ARIMA models. See help(arima) for details. CSS is the fastest.
whereami <- TRUE     # Set to true when debugging. Provides more verbose information on the simulation.



##########################################################################################
### You do not need to set anything below this line. The following are parameters that ###
### are computed from the information provided above.                                  ###
##########################################################################################

num.models <- length(forecast.models)

orders.names <- NULL
if ("MA-opt" %in% forecast.models){orders.names <- c(orders.names,"MA.orders")}
if ("SES-opt" %in% forecast.models){orders.names <- c(orders.names,"SES.alpha")}
if ("AR(p)" %in% forecast.models){orders.names <- c(orders.names,"AR.orders")}
if ("ARMA(p,q)" %in% forecast.models){orders.names <- c(orders.names,"ARMA.AR.orders","ARMA.MA.orders")}
if ("VAR(p)" %in% forecast.models){orders.names <- c(orders.names,"VAR.orders")}
if ("Factor(k)" %in% forecast.models){orders.names <- c(orders.names,"factor.orders")}

all.functions <- c("AR.forecast","ARMA.forecast","Bai.Ng.2002","BVAR.forecast","BVARM","Factor.forecast","FAVAR.forecast","MA.forecast","mean.forecast","naive.forecast","Newey.West","SES.forecast","VAR.forecast","AR.direct")

if (parallel.method=="Fork")
{
  registerDoParallel(cores=num.cores) # This forks processes. It won't work on Windows. If BVAR is included in forecasting.models then this method will freeze.
}else
{
  c1 <- makeCluster(num.cores) # This uses snow. It will work on Windows and Linux and with BVAR.
  registerDoParallel(c1)
}
