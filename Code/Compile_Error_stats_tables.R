error.criteria.tables <- function(Results,horizons)
{

  error.criteria <- list()
  
  for (variable in variable.names)
  {
    ### Initialise some matrices to hold the MSE, RMSE, MAE and MAPE statistics.
    hor.list <- "h = 1"; for (i in 2:max(horizons)){hor.list <- c(hor.list,paste("h = ",i,sep=""))}
    
    MSE.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
    rownames(MSE.table) <- forecast.models
    colnames(MSE.table) <- hor.list
    
    RMSE.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
    rownames(RMSE.table) <- forecast.models
    colnames(RMSE.table) <- hor.list
    
    MAE.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
    rownames(MAE.table) <- forecast.models
    colnames(MAE.table) <- hor.list
    
    MAPE.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
    rownames(MAPE.table) <- forecast.models
    colnames(MAPE.table) <- hor.list
    
    
    # Now compute the statistics.
    for (i in 1:max(horizons))
    {
      cmd <- paste("forecasts <- Results$",variable,"$all.forecasts$'h=",i,"'",sep="")
      eval(parse(text=cmd))
      errors <- forecasts - forecasts[,"X"]
      colnames(errors) <- colnames(forecasts)
      errors <- errors[,2:dim(errors)[2]]
      
      MSE <- colMeans(errors^2,na.rm=TRUE)
      RMSE <- sqrt(MSE)
      MAE <- colMeans(abs(errors),na.rm=TRUE)
      MAPE <- colMeans(abs(errors/forecasts[,"X"]*100),na.rm=TRUE)
      names(MAPE) <- names(MSE)
      
      MSE.table[,i] <- MSE
      RMSE.table[,i] <- RMSE
      MAE.table[,i] <- MAE
      MAPE.table[,i] <- MAPE
    }
    criterion.tables <- list(MSE=MSE.table, RMSE=RMSE.table, MAE=MAE.table, MAPE=MAPE.table)
    cmd <- paste("error.criteria$",variable," <- criterion.tables",sep="")
    eval(parse(text=cmd))
  }
  return(criterion.tables)
}
