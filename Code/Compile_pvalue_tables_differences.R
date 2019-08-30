# Computes the p-values for the conditional and unconditional predictive ability
# test for the forecasts computed from models estimated with the data in differences, 
# and creates a list named p.value.differences.tables.'variable' for each variable
# e.g. p.value.differences.tables.CPI. It also saves tables of p-values in 
# ../Output/pvalue_tables.
#
# This file was copied from Compile_pvalue_tables_levels.R and the word levels
# was replaced by the word changes.

if (whereami){cat("\nConstructing tables of p-values for differences")}

for (variable in variable.names)
{
  ### Initialise some matrices to hold the MSE, RMSE, MAE and MAPE conditional pvalues.
  hor.list <- "h = 1"; for (i in 2:max(horizons)){hor.list <- c(hor.list,paste("h = ",i,sep=""))}
  
  MSE.conditional.pvalue.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
  rownames(MSE.conditional.pvalue.table) <- forecast.models
  colnames(MSE.conditional.pvalue.table) <- hor.list
  MSE.conditional.pvalue.table <- MSE.conditional.pvalue.table[!rownames(MSE.conditional.pvalue.table) %in% benchmark,]   # Remove the benchmark from the table
  
  RMSE.conditional.pvalue.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
  rownames(RMSE.conditional.pvalue.table) <- forecast.models
  colnames(RMSE.conditional.pvalue.table) <- hor.list
  RMSE.conditional.pvalue.table <- RMSE.conditional.pvalue.table[!rownames(RMSE.conditional.pvalue.table) %in% benchmark,]   # Remove the benchmark from the table
  
  MAE.conditional.pvalue.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
  rownames(MAE.conditional.pvalue.table) <- forecast.models
  colnames(MAE.conditional.pvalue.table) <- hor.list
  MAE.conditional.pvalue.table <- MAE.conditional.pvalue.table[!rownames(MAE.conditional.pvalue.table) %in% benchmark,]   # Remove the benchmark from the table
  
  MAPE.conditional.pvalue.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
  rownames(MAPE.conditional.pvalue.table) <- forecast.models
  colnames(MAPE.conditional.pvalue.table) <- hor.list  
  MAPE.conditional.pvalue.table <- MAPE.conditional.pvalue.table[!rownames(MAPE.conditional.pvalue.table) %in% benchmark,]   # Remove the benchmark from the table
  
  ### Initialise some matrices to hold the MSE, RMSE, MAE and MAPE unconditional pvalues.
  MSE.unconditional.pvalue.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
  rownames(MSE.unconditional.pvalue.table) <- forecast.models
  colnames(MSE.unconditional.pvalue.table) <- hor.list
  MSE.unconditional.pvalue.table <- MSE.unconditional.pvalue.table[!rownames(MSE.unconditional.pvalue.table) %in% benchmark,]   # Remove the benchmark from the table
  
  RMSE.unconditional.pvalue.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
  rownames(RMSE.unconditional.pvalue.table) <- forecast.models
  colnames(RMSE.unconditional.pvalue.table) <- hor.list
  RMSE.unconditional.pvalue.table <- RMSE.unconditional.pvalue.table[!rownames(RMSE.unconditional.pvalue.table) %in% benchmark,]   # Remove the benchmark from the table
  
  MAE.unconditional.pvalue.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
  rownames(MAE.unconditional.pvalue.table) <- forecast.models
  colnames(MAE.unconditional.pvalue.table) <- hor.list
  MAE.unconditional.pvalue.table <- MAE.unconditional.pvalue.table[!rownames(MAE.unconditional.pvalue.table) %in% benchmark,]   # Remove the benchmark from the table
  
  MAPE.unconditional.pvalue.table <- matrix(rep(NA,max(horizons)*length(forecast.models)),ncol=max(horizons))
  rownames(MAPE.unconditional.pvalue.table) <- forecast.models
  colnames(MAPE.unconditional.pvalue.table) <- hor.list  
  MAPE.unconditional.pvalue.table <- MAPE.unconditional.pvalue.table[!rownames(MAPE.unconditional.pvalue.table) %in% benchmark,]   # Remove the benchmark from the table
  
  # Now compute the statistics.
  for (i in 1:max(horizons))
  {
    cmd <- paste("forecasts <- Results.differences$",variable,"$all.forecasts$'h=",i,"'",sep="")
    eval(parse(text=cmd))
    errors <- forecasts - forecasts[,"X"]
    colnames(errors) <- colnames(forecasts)
    errors <- errors[,2:dim(errors)[2]]
    
    sq.errors <- errors^2
    abs.errors <- abs(errors)
    abs.pc.errors <- abs(errors/forecasts[,"X"]*100)
    colnames(abs.pc.errors) <- colnames(errors)
    
    for (model in setdiff(forecast.models,benchmark))
    {
      cmd <- paste("MSE.conditional.pvalue.table['",model,"','h = ",i,"'] <- CPAtest(sq.errors[,'",model,"'],sq.errors[,'",benchmark,"'],",i,",alpha,2)$pval",sep="")
      eval(parse(text=cmd))
      cmd <- paste("MSE.unconditional.pvalue.table['",model,"','h = ",i,"'] <- CPAtest(sq.errors[,'",model,"'],sq.errors[,'",benchmark,"'],",i,",alpha,1)$pval",sep="")
      eval(parse(text=cmd))

      cmd <- paste("MAE.conditional.pvalue.table['",model,"','h = ",i,"'] <- CPAtest(abs.errors[,'",model,"'],abs.errors[,'",benchmark,"'],",i,",alpha,2)$pval",sep="")
      eval(parse(text=cmd))
      cmd <- paste("MAE.unconditional.pvalue.table['",model,"','h = ",i,"'] <- CPAtest(abs.errors[,'",model,"'],abs.errors[,'",benchmark,"'],",i,",alpha,1)$pval",sep="")
      eval(parse(text=cmd))

      cmd <- paste("MAPE.conditional.pvalue.table['",model,"','h = ",i,"'] <- CPAtest(abs.pc.errors[,'",model,"'],abs.pc.errors[,'",benchmark,"'],",i,",alpha,2)$pval",sep="")
      eval(parse(text=cmd))
      cmd <- paste("MAPE.unconditional.pvalue.table['",model,"','h = ",i,"'] <- CPAtest(abs.pc.errors[,'",model,"'],abs.pc.errors[,'",benchmark,"'],",i,",alpha,1)$pval",sep="")
      eval(parse(text=cmd))
    }
  }

  # Create a list containing the tables.
  conditional <- list(MSE=MSE.conditional.pvalue.table, MAE=MAE.conditional.pvalue.table, MAPE=MAPE.conditional.pvalue.table) 
  unconditional <- list(MSE=MSE.unconditional.pvalue.table, MAE=MAE.unconditional.pvalue.table, MAPE=MAPE.unconditional.pvalue.table) 
  cmd <- paste("p.value.differences.tables.",variable," <- list(conditional=conditional, unconditional=unconditional)",sep="")
  eval(parse(text=cmd))
  
  # Save the tables as .csv and .tex files.
  crits <- c("MSE","MAE","MAPE")
  for (crit in crits)
  {
    cmd <- paste("write.csv(",crit,".conditional.pvalue.table, '../Output/pvalue_tables/",variable,"_",crit,"_differences_conditional.pvalues.csv')",sep="")
    eval(parse(text=cmd))
    cmd <- paste("write.csv(",crit,".unconditional.pvalue.table, '../Output/pvalue_tables/",variable,"_",crit,"_differences_unconditional.pvalues.csv')",sep="")
    eval(parse(text=cmd))
    

    table.heading <- paste(variable,": Conditional p-values by Horizon")
    cmd <- paste("print.xtable(xtable(",crit,".conditional.pvalue.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/pvalue_tables/",variable,"_",crit,"_differences_conditional.pvalues.tex',caption.placement='top',booktabs=TRUE)",sep="")
    eval(parse(text=cmd))
    table.heading <- paste(variable,": Unconditional p-values by Horizon")
    cmd <- paste("print.xtable(xtable(",crit,".unconditional.pvalue.table[,horizons],digits=3,caption='",table.heading,"'),file='../Output/pvalue_tables/",variable,"_",crit,"_differences_unconditional.pvalues.tex',caption.placement='top',booktabs=TRUE)",sep="")
    eval(parse(text=cmd))
  }
}
