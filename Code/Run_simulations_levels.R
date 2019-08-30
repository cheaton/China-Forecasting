source("forecast.simulation.R")

########################
# Forecasts in levels. #
########################
Results.levels <- list()
cat("\nRunning simulation in levels")
for (variable in variable.names)
{
  cat(paste("\nWorking with",variable,"\n"))
  # cmd <- paste("Results.levels$",variable," <- forecast.simulation(",variable,".cleaned$x.clean,",variable,".var,FA.data,R,max(horizons),forecast.models,model.parameters,criterion,orders.names,whereami)",sep="")
  cmd <- paste("outlier.multiple <- ",variable,".outlier.multiple",sep="")
  eval(parse(text=cmd))
  cmd <- paste("Results.levels$",variable," <- forecast.simulation(",variable,",",variable,".var,FA,R,max(horizons),forecast.models,model.parameters,criterion,orders.names,outlier.multiple,variable,whereami,FALSE)",sep="")
  eval(parse(text=cmd))
}

# Results.levels includes the raw data. Since we don't want the forecasts to be compared to this, we need to replace it with the cleaned, seasonally adjusted data that was computed in Clean_up_data.R.

for (variable in variable.names)
{
  for (h in 1:max(horizons))
  {
    cmd <- paste("names <- c('X',colnames(Results.levels$",variable,"$all.forecasts$'h=",h,"'))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("z <- cbind(",variable,".cleaned$x.clean,Results.levels$",variable,"$all.forecasts$'h=",h,"')",sep="")
    eval(parse(text=cmd))
    colnames(z) <- names
    z <- z[,-2]
    cmd <- paste("Results.levels$",variable,"$all.forecasts$'h=",h,"' <- z",sep="")
    eval(parse(text=cmd))
  }
}


# Now save the forecasts.
for (variable in variable.names)
{
  for (h in 1:max(horizons))
  {
    filename <- paste("../Output/Forecasts/Forecasts_Levels_",variable,"_h_",h,".csv",sep="")
    cmd <- paste("forecasts <- Results.levels$",variable,"$all.forecasts$'h=",h,"'",sep="")
    eval(parse(text=cmd))
    write.csv(data.frame(as.yearmon(time(forecasts)),forecasts),file=filename,row.names=FALSE)
   }
   filename <- paste("../Output/order_tables/Orders_Levels_",variable,".csv",sep="")
   cmd <- paste("orders <- Results.levels$",variable,"$orders",sep="")
   eval(parse(text=cmd))
  if (!is.null(orders))
  {
    write.csv(data.frame(as.yearmon(time(orders)),orders),file=filename,row.names=FALSE)
  }
}
