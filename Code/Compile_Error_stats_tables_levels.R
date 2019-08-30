# Uses the forecasts in Results.levels to construct tables containing the MSE, RMSE, MAE and MAPE.
# These are put in the list criterion.tables.levels. Also creates the list rel.criterion.tables.levels which
# contains the MSE, RMSE, MAE and MAPE scaled so that the benchmark forecast has a value of 1.
# The benchmark forecast is set in the file Set_simulation_parameters.R. Also saves everything
# in ../Output/Error_tables.

if (whereami){cat("\nConstructing tables of error statistics for levels")}

levels.error.criteria <- list()
levels.error.rel.criteria <- list()

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
    cmd <- paste("forecasts <- Results.levels$",variable,"$all.forecasts$'h=",i,"'",sep="")
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

  # Create tables with values of the criteria relative to the benchmark criterion specified in Set_simulation_parameters.R
  rel.MSE.table <- MSE.table/t(matrix(rep(MSE.table[benchmark,],num.models),ncol=num.models))
  rel.RMSE.table <- RMSE.table/t(matrix(rep(RMSE.table[benchmark,],num.models),ncol=num.models))
  rel.MAE.table <- MAE.table/t(matrix(rep(MAE.table[benchmark,],num.models),ncol=num.models))
  rel.MAPE.table <- MAPE.table/t(matrix(rep(MAPE.table[benchmark,],num.models),ncol=num.models))

  # Create a list containing the tables.
  criterion.tables.levels <- list(MSE=MSE.table, RMSE=RMSE.table, MAE=MAE.table, MAPE=MAPE.table)
  rel.criterion.tables.levels <- list(MSE=rel.MSE.table, RMSE=rel.RMSE.table, MAE=rel.MAE.table, MAPE=rel.MAPE.table)

  # Save the tables as .csv files.
  cmd <- paste("levels.error.criteria$",variable," <- criterion.tables.levels",sep="")
  eval(parse(text=cmd)) # Construct a list cointaining all the tables.
  cmd <- paste("write.csv(levels.error.criteria$",variable,"$MSE,file='../Output/Error_tables/MSE_levels_",variable,".csv')",sep="")
  eval(parse(text=cmd)) # Write the MSEs to a file.
  cmd <- paste("write.csv(levels.error.criteria$",variable,"$RMSE,file='../Output/Error_tables/RMSE_levels_",variable,".csv')",sep="")
  eval(parse(text=cmd)) # Write the RMSEs to a file.
  cmd <- paste("write.csv(levels.error.criteria$",variable,"$MAE,file='../Output/Error_tables/MAE_levels_",variable,".csv')",sep="")
  eval(parse(text=cmd)) # Write the MAEs to a file.
  cmd <- paste("write.csv(levels.error.criteria$",variable,"$MAPE,file='../Output/Error_tables/MAPE_levels_",variable,".csv')",sep="")
  eval(parse(text=cmd)) # Write the MAPEs to a file.
  
  cmd <- paste("levels.error.rel.criteria$",variable," <- rel.criterion.tables.levels",sep="")
  eval(parse(text=cmd)) # Construct a list cointaining all the tables.
  cmd <- paste("write.csv(levels.error.rel.criteria$",variable,"$MSE,file='../Output/Error_tables/rel_MSE_levels_",variable,".csv')",sep="")
  eval(parse(text=cmd)) # Write the MSEs to a file.
  cmd <- paste("write.csv(levels.error.rel.criteria$",variable,"$RMSE,file='../Output/Error_tables/rel_RMSE_levels_",variable,".csv')",sep="")
  eval(parse(text=cmd)) # Write the RMSEs to a file.
  cmd <- paste("write.csv(levels.error.rel.criteria$",variable,"$MAE,file='../Output/Error_tables/rel_MAE_levels_",variable,".csv')",sep="")
  eval(parse(text=cmd)) # Write the MAEs to a file.
  cmd <- paste("write.csv(levels.error.rel.criteria$",variable,"$MAPE,file='../Output/Error_tables/rel_MAPE_levels_",variable,".csv')",sep="")
  eval(parse(text=cmd)) # Write the MAPEs to a file.

  # Save the tables as .tex files.
  table.heading <- paste(variable,": Standardised Mean Squared Forecast Errors by Horizon",sep="")
  cmd <- paste("print.xtable(xtable(MSE.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/Error_tables/MSE_levels_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
  eval(parse(text=cmd))

  table.heading <- paste(variable,": Standardised Mean Squared Forecast Errors by Horizon",sep="")
  cmd <- paste("print.xtable(xtable(RMSE.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/Error_tables/RMSE_levels_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
  eval(parse(text=cmd))

  table.heading <- paste(variable,": Standardised Mean Squared Forecast Errors by Horizon",sep="")
  cmd <- paste("print.xtable(xtable(MAE.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/Error_tables/MAE_levels_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
  eval(parse(text=cmd))

  table.heading <- paste(variable,": Standardised Mean Squared Forecast Errors by Horizon",sep="")
  cmd <- paste("print.xtable(xtable(MAPE.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/Error_tables/MAPE_levels_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
  eval(parse(text=cmd))

  
  table.heading <- paste(variable,": Standardised Mean Squared Forecast Errors by Horizon",sep="")
  cmd <- paste("print.xtable(xtable(rel.MSE.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/Error_tables/rel_MSE_levels_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
  eval(parse(text=cmd))

  table.heading <- paste(variable,": Standardised Mean Squared Forecast Errors by Horizon",sep="")
  cmd <- paste("print.xtable(xtable(rel.RMSE.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/Error_tables/rel_RMSE_levels_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
  eval(parse(text=cmd))

  table.heading <- paste(variable,": Standardised Mean Squared Forecast Errors by Horizon",sep="")
  cmd <- paste("print.xtable(xtable(rel.MAE.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/Error_tables/rel_MAE_levels_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
  eval(parse(text=cmd))

  table.heading <- paste(variable,": Standardised Mean Squared Forecast Errors by Horizon",sep="")
  cmd <- paste("print.xtable(xtable(rel.MAPE.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/Error_tables/rel_MAPE_levels_",variable,".tex',caption.placement='top',booktabs=TRUE)",sep="")
  eval(parse(text=cmd))
}
