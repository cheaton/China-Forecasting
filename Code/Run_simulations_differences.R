

##############################
# Forecasts in diffferences. #
##############################

# First, run forecast.simulation on the differenced variables.
Results.changes <- list()
cat("\nRunning simulation in differences")
for (variable in variable.names)
{
  cat(paste("\nWorking with",variable,"\n"))
  # cmd <- paste("Results.changes$",variable," <- forecast.simulation(diff(",variable,".cleaned$x.clean),diff(",variable,".var),FA.data,R,max(horizons),forecast.models,model.parameters,criterion,orders.names,whereami)",sep="")
  cmd <- paste("outlier.multiple <- ",variable,".outlier.multiple",sep="")
  eval(parse(text=cmd))
  cmd <- paste("Results.changes$",variable," <- forecast.simulation(",variable,",",variable,".var,FA,R,max(horizons),forecast.models,model.parameters,criterion,orders.names,outlier.multiple,as.character(",variable,"),whereami,TRUE)",sep="")
  eval(parse(text=cmd))
}

# Results.changes includes the raw data. Since we don't want the forecasts to be compared to this, we need to replace it with the cleaned, seaonally adjusted data that was computed in Clean_up_data.R.

for (variable in variable.names)
{
  for (h in 1:max(horizons))
  {
    cmd <- paste("names <- c('X',colnames(Results.changes$",variable,"$all.forecasts$'h=",h,"'))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("z <- cbind(diff(",variable,".cleaned$x.clean),Results.changes$",variable,"$all.forecasts$'h=",h,"')",sep="")
    eval(parse(text=cmd))
    colnames(z) <- names
    z <- z[,-2]
    cmd <- paste("Results.changes$",variable,"$all.forecasts$'h=",h,"' <- z",sep="")
    eval(parse(text=cmd))
  }
}



Results.differences <- Results.changes

# Now, the hard bit. For each variable, for each forecast horizon, we need to replace the differenced 
# variable with the variable in levels. Then, for each variable, for h=1 we need to make all the 
# forecasts equal to the level of the variable in the previous time period plus the forecasted
# change in the variable. Then, for i=2:max(horizon), we need to make the forecasts for h=i equal
# to the forecasted change plus the forecast for the previous period from h=(i-1).

for (variable in variable.names)
{ # Replace the differenced data variable with the same variable in levels in each ts object.
  for (h in 1:max(horizons))
  {
    cmd <- paste("Results.differences$",variable,"$all.forecasts$'h=",h,"' <- cbind(",variable,".cleaned$x.clean,Results.differences$",variable,"$all.forecasts$'h=",h,"')",sep="")
    eval(parse(text=cmd)) # Bind the levels data to each ts object.
   cmd <- paste("Results.differences$",variable,"$all.forecasts$'h=",h,"' <- Results.differences$",variable,"$all.forecasts$'h=",h,"'[,-2]",sep="") 
    eval(parse(text=cmd)) # Remove the differenced data from each ts object.
    cmd <- paste("colnames(Results.differences$",variable,"$all.forecasts$'h=",h,"') <- c('X',forecast.models)",sep="")
    eval(parse(text=cmd)) # Fix the names
  }
}

for (variable in variable.names)
{ # For h=1 make all the forecasts equal to the previous observation plus the forecasted change.
  cmd <- paste("tmp <- Results.differences$",variable,"$all.forecasts$'h=1'",sep="")
  eval(parse(text=cmd))
  tmp <- cbind(tmp,tmp[,-1] + lag(tmp[,1],-1))
  tmp <- tmp[,-seq(from=2,to=(length(forecast.models)+1),by=1)]
  colnames(tmp) <- c('X',forecast.models)
  cmd <- paste("Results.differences$",variable,"$all.forecasts$'h=1' <- tmp",sep="")
  eval(parse(text=cmd))
}

if (max(horizons)>1)
{
for (variable in variable.names)
  { # For i=2:max(horizon), we need to make the forecasts for h=i equal to the forecasted 
    # change plus the forecast for the previous period from h=(i-1).
    for (h in 2:max(horizons))
    {
      cmd <- paste("tmp <- Results.differences$",variable,"$all.forecasts$'h=",h,"'",sep="")
      eval(parse(text=cmd))
      cmd <- paste("tmp_1 <- Results.differences$",variable,"$all.forecasts$'h=",(h-1),"'",sep="")
      eval(parse(text=cmd))
      tmp <- cbind(tmp, tmp[,-1] + lag(tmp_1[,-1],-1))
      tmp <- tmp[,-seq(from=2,to=(length(forecast.models)+1))]
      colnames(tmp) <- c('X',forecast.models)
      cmd <- paste("Results.differences$",variable,"$all.forecasts$'h=",h,"' <- tmp",sep="")
      eval(parse(text=cmd))
    }
  }
}

# Now save the forecasts.
for (variable in variable.names)
{
  for (h in 1:max(horizons))
  {
    filename <- paste("../Output/Forecasts/Forecasts_Changes_",variable,"_h_",h,".csv",sep="")
    cmd <- paste("forecasts <- Results.changes$",variable,"$all.forecasts$'h=",h,"'",sep="")
    eval(parse(text=cmd))
    write.csv(data.frame(as.yearmon(time(forecasts)),forecasts),file=filename,row.names=FALSE)
  }
}

for (variable in variable.names)
{
  for (h in 1:max(horizons))
  {
    filename <- paste("../Output/Forecasts/Forecasts_Differences_",variable,"_h_",h,".csv",sep="")
    cmd <- paste("forecasts <- Results.differences$",variable,"$all.forecasts$'h=",h,"'",sep="")
    eval(parse(text=cmd))
    write.csv(data.frame(as.yearmon(time(forecasts)),forecasts),file=filename,row.names=FALSE)
  }
  filename <- paste("../Output/order_tables/Orders_Differences_",variable,".csv",sep="")
  cmd <- paste("orders <- Results.differences$",variable,"$orders",sep="")
  eval(parse(text=cmd))
  if (!is.null(orders))
  {
    write.csv(data.frame(as.yearmon(time(orders)),orders),file=filename,row.names=FALSE)
  }
}
