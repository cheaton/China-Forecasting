library(stargazer)

  
if ("AR(1)" %in% forecast.models)
{
  # Levels
  
  the.model.outputs <- NULL
  for (variable in variable.names)
  {
    cmd <- paste("y <- ",variable,".cleaned$x.clean",sep="")
    eval(parse(text=cmd))
    cmd <- paste("AR1.",variable," <- dynlm(y~lag(y,-1))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'AR1.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  column.names <- paste("c(",paste(paste("'$",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("AR1.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='AR(1) Estimates: Levels',covariate.labels='Lagged dependent variable',column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
  eval(parse(text=cmd))
  outfile <- paste("../Output/Model_output_tables/AR1.levels.tex",sep="")
  writeLines(AR1.LaTeX,outfile)

  # Differences

  the.model.outputs <- NULL
  for (variable in variable.names)
  {
    cmd <- paste("y <- diff(",variable,".cleaned$x.clean)",sep="")
    eval(parse(text=cmd))
    cmd <- paste("AR1.",variable," <- dynlm(y~lag(y,-1))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'AR1.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  column.names <- paste("c(",paste(paste("'$\\\\Delta ",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("AR1.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='AR(1) Estimates: Differences',covariate.labels='Lagged dependent variable',column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
  eval(parse(text=cmd))
  outfile <- paste("../Output/Model_output_tables/AR1.differences.tex",sep="")
  writeLines(AR1.LaTeX,outfile)
}
  



if ("AR(p)" %in% forecast.models)
{
  # Levels
  
  the.model.outputs <- NULL
  for (variable in variable.names)
  {
    cmd <- paste("y <- zoo(",variable,".cleaned$x.clean)",sep="")
    eval(parse(text=cmd))
    p <- ar(y,AIC=TRUE,order.max=model.parameters$order.max.AR,method="ols",intercept=TRUE,demean=FALSE)$order
    cmd <- paste("ARp.",variable," <- dynlm(y~lag(y,-1:-",p,"))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("names(ARp.",variable,"$coefficients) <- c('(Intercept)',paste('Lag ',seq(from=1,to=p,by=1),sep=''))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'ARp.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  column.names <- paste("c(",paste(paste("'$",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("ARp.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='AR(p) Estimates: Levels',covariate.labels='Lagged dependent variable',column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE,omit.stat=c('ser','f','adj.rsq'))",sep="")
  eval(parse(text=cmd))
  outfile <- paste("../Output/Model_output_tables/ARp.levels.tex",sep="")
  writeLines(ARp.LaTeX,outfile)

  # Differences

  the.model.outputs <- NULL
  for (variable in variable.names)
  {
    cmd <- paste("y <- zoo(diff(",variable,".cleaned$x.clean))",sep="")
    eval(parse(text=cmd))
    p <- ar(y,AIC=TRUE,order.max=model.parameters$order.max.AR,method="ols",intercept=TRUE,demean=FALSE)$order
    cmd <- paste("ARp.",variable," <- dynlm(y~lag(y,-1:-",p,"))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("names(ARp.",variable,"$coefficients) <- c('(Intercept)',paste('Lag ',seq(from=1,to=p,by=1),sep=''))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'ARp.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  column.names <- paste("c(",paste(paste("'$\\\\Delta ",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("ARp.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='AR(p) Estimates: Differences',covariate.labels='Lagged dependent variable',column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
  eval(parse(text=cmd))
  outfile <- paste("../Output/Model_output_tables/ARp.differences.tex",sep="")
  writeLines(ARp.LaTeX,outfile)
}
  

if ("ARd(1)" %in% forecast.models)
{
  for (h in horizons)
  {
    # Levels
    
    the.model.outputs <- NULL
    for (variable in variable.names)
    {
      cmd <- paste("y <- ",variable,".cleaned$x.clean",sep="")
      eval(parse(text=cmd))
      cmd <- paste("ARd1.",variable," <- dynlm(lag(y,h)~y)",sep="")
      eval(parse(text=cmd))
      cmd <- paste("the.model.outputs <- c(the.model.outputs,'ARd1.",variable,"')",sep="")
      eval(parse(text=cmd))
    }
    
    column.names <- paste("c(",paste(paste("'$",variable.names,"_{t+",h,"}$'",sep=""),collapse=","),")",sep="")
    cmd <- paste("ARd1.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='Direct AR(1) Estimates: Levels: Horizon = ",h,"',covariate.labels='Contemporaneous dependent variable',column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
    eval(parse(text=cmd))
    outfile <- paste("../Output/Model_output_tables/ARd1.levels.h",h,".tex",sep="")
    writeLines(ARd1.LaTeX,outfile)
  
    # Differences
    
    the.model.outputs <- NULL
    for (variable in variable.names)
    {
      cmd <- paste("y <- ",variable,".cleaned$x.clean",sep="")
      eval(parse(text=cmd))
      cmd <- paste("ARd1.",variable," <- dynlm((lag(y,h)-y)~diff(y))",sep="")
      eval(parse(text=cmd))
      cmd <- paste("the.model.outputs <- c(the.model.outputs,'ARd1.",variable,"')",sep="")
      eval(parse(text=cmd))
    }

    column.names <- paste("c(",paste(paste("'$\\\\Delta ",variable.names,"_{t+",h,"}$'",sep=""),collapse=","),")",sep="")
    cmd <- paste("ARd1.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='Direct AR(1) Estimates: Differences: Horizon = ",h,"',covariate.labels='$\\\\Delta$ contemporaneous dependent variable',column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
    eval(parse(text=cmd))
    outfile <- paste("../Output/Model_output_tables/ARd1.differences.h",h,".tex",sep="")
    writeLines(ARd1.LaTeX,outfile)
  }
}
  


if ("ARMA(1,1)" %in% forecast.models)
{
  # Levels
  
  the.model.outputs <- NULL
  for (variable in variable.names)
  {
    cmd <- paste("y <- ",variable,".cleaned$x.clean",sep="")
    eval(parse(text=cmd))
    cmd <- paste("ARMA11.",variable," <- arima(y,order=c(1,0,1),method='ML')",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'ARMA11.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  column.names <- paste("c(",paste(paste("'$",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("ARMA11.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='ARMA(1,1) Estimates: Levels',covariate.labels=c('Lagged dependent variable','$\\\\varepsilon_{t-1}$'),column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
  eval(parse(text=cmd))
  ARMA11.LaTeX <- ARMA11.LaTeX[grep("created by stargazer",ARMA11.LaTeX,fixed=TRUE):length(ARMA11.LaTeX)]
  outfile <- paste("../Output/Model_output_tables/ARMA11.levels.tex",sep="")
  writeLines(ARMA11.LaTeX,outfile)


  # Differences

  the.model.outputs <- NULL
  for (variable in variable.names)
  {
    cmd <- paste("y <- diff(",variable,".cleaned$x.clean)",sep="")
    eval(parse(text=cmd))
    cmd <- paste("ARMA11.",variable," <- arima(y,order=c(1,0,1),method='ML')",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'ARMA11.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  column.names <- paste("c(",paste(paste("'$\\\\Delta ",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("ARMA11.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='ARMA(1,1) Estimates: Differences',covariate.labels=c('$\\\\Delta$ Lagged dependent variable','$\\\\varepsilon_{t-1}$'),column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
  eval(parse(text=cmd))
  ARMA11.LaTeX <- ARMA11.LaTeX[grep("created by stargazer",ARMA11.LaTeX,fixed=TRUE):length(ARMA11.LaTeX)]
  outfile <- paste("../Output/Model_output_tables/ARMA11.differences.tex",sep="")
  writeLines(ARMA11.LaTeX,outfile)
}
  

if ("VAR(1)" %in% forecast.models)
{
  # Levels
  
  the.model.outputs <- NULL
  for (variable in variable.names)
  {
    cmd <- paste("var.data <- var.datasets$",variable,sep="")
    eval(parse(text=cmd))
    var.names <- matrix(unlist(strsplit(colnames(var.data),".",fixed=TRUE)),ncol=3)[1,]
    cmd <- paste("VAR1.",variable," <- dynlm(var.data[,1]~lag(var.data[,1],-1) + lag(var.data[,2],-1) + lag(var.data[,3],-1))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'VAR1.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  row.names <- "c('Lagged dependent variable','Lagged variable','$Rate_{t-1}$')"
  column.names <- paste("c(",paste(paste("'$",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("VAR1.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='VAR(1) Estimates: Levels',covariate.labels=",row.names,",column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
  eval(parse(text=cmd))
  outfile <- paste("../Output/Model_output_tables/VAR1.levels.tex",sep="")
  writeLines(VAR1.LaTeX,outfile)

  # Differences

  the.model.outputs <- NULL
  for (variable in variable.names)
  {
    cmd <- paste("var.data <- diff(var.datasets$",variable,")",sep="")
    eval(parse(text=cmd))
    var.names <- matrix(unlist(strsplit(colnames(var.data),".",fixed=TRUE)),ncol=3)[1,]
    cmd <- paste("VAR1.",variable," <- dynlm(var.data[,1]~lag(var.data[,1],-1) + lag(var.data[,2],-1) + lag(var.data[,3],-1))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'VAR1.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  row.names <- "c('$\\\\Delta$ Lagged dependent variable','$\\\\Delta$ Lagged variable','$\\\\Delta Rate_{t-1}$')"
  column.names <- paste("c(",paste(paste("'$\\\\Delta ",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("VAR1.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='VAR(1) Estimates: Differences',covariate.labels=",row.names,",column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
  eval(parse(text=cmd))
  outfile <- paste("../Output/Model_output_tables/VAR1.differences.tex",sep="")
  writeLines(VAR1.LaTeX,outfile)
}
  

if ("Factor(2)" %in% forecast.models)
{
  for (h in horizons)
  {
   # Levels
    
    the.model.outputs <- NULL
    factor.output <- Bai.Ng.2002(FA.cleaned,2,criterion=NULL)
    F <- ts(factor.output$F.bar,start=as.yearmon(time(FA.cleaned))[1],frequency=frequency(FA.cleaned))/10000 # Get the factors.
    for (variable in variable.names)
    {
      cmd <- paste("y <- ",variable,".cleaned$x.clean",sep="")
      eval(parse(text=cmd))
      cmd <- paste("F2.",variable," <- dynlm(y~lag(F,-h))",sep="")
      eval(parse(text=cmd))
      cmd <- paste("the.model.outputs <- c(the.model.outputs,'F2.",variable,"')",sep="")
      eval(parse(text=cmd))
    }

    column.names <- paste("c(",paste(paste("'$",variable.names,"_{t+",h,"}$'",sep=""),collapse=","),")",sep="")
    cmd <- paste("F2.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='Two Factor Model: Levels: Horizon = ",h,"',covariate.labels=c('$f_{1t}$','$f_{2t}$'),column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
    eval(parse(text=cmd))
    outfile <- paste("../Output/Model_output_tables/F2.levels.h",h,".tex",sep="")
    writeLines(F2.LaTeX,outfile)
  
    # Differences
    
   the.model.outputs <- NULL
   factor.output <- Bai.Ng.2002(diff(FA.cleaned),2,criterion=NULL)
   F <- ts(factor.output$F.bar,start=as.yearmon(time(diff(FA.cleaned)))[1],frequency=frequency(diff(FA.cleaned)))/10000 # Get the factors.
   for (variable in variable.names)
   {
     cmd <- paste("y <- diff(",variable,".cleaned$x.clean)",sep="")
     eval(parse(text=cmd))
     cmd <- paste("F2.",variable," <- dynlm(y~lag(F,-h))",sep="")
     eval(parse(text=cmd))
     cmd <- paste("the.model.outputs <- c(the.model.outputs,'F2.",variable,"')",sep="")
     eval(parse(text=cmd))
   }

   column.names <- paste("c(",paste(paste("'$\\\\Delta ",variable.names,"_{t+",h,"}$'",sep=""),collapse=","),")",sep="")
   cmd <- paste("F2.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='Two Factor Model: Differences: Horizon = ",h,"',covariate.labels=c('$f_{d1t}$','$f_{d2t}$'),column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
   eval(parse(text=cmd))
   outfile <- paste("../Output/Model_output_tables/F2.differences.h",h,".tex",sep="")
   writeLines(F2.LaTeX,outfile)
  }
}
  




if ("F(2)VAR(1)" %in% forecast.models)
{
  # Levels
  
  the.model.outputs <- NULL
  factor.output <- Bai.Ng.2002(FA.cleaned,2,criterion=NULL)
  F <- ts(factor.output$F.bar,start=as.yearmon(time(FA.cleaned))[1],frequency=frequency(FA.cleaned))/10000 # Get the factors.
  for (variable in variable.names)
  {
    cmd <- paste("var.data <- var.datasets$",variable,sep="")
    eval(parse(text=cmd))
    var.names <- matrix(unlist(strsplit(colnames(var.data),".",fixed=TRUE)),ncol=3)[1,]
    cmd <- paste("F2VAR1.",variable," <- dynlm(var.data[,1]~lag(var.data[,1],-1) + lag(var.data[,2],-1) + lag(var.data[,3],-1) + lag(F[,1],-1) + lag(F[,2],-1))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'F2VAR1.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  row.names <- "c('Lagged dependent variable','Lagged variable','$Rate_{t-1}$','$f_{1t}$','$f_{2t}$')"
  column.names <- paste("c(",paste(paste("'$",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("F2VAR1.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='F(2)VAR(1) Estimates: Levels',covariate.labels=",row.names,",column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
  eval(parse(text=cmd))
  outfile <- paste("../Output/Model_output_tables/F2VAR1.levels.tex",sep="")
  writeLines(F2VAR1.LaTeX,outfile)

  # Differences

  the.model.outputs <- NULL
  factor.output <- Bai.Ng.2002(diff(FA.cleaned),2,criterion=NULL)
  F <- ts(factor.output$F.bar,start=as.yearmon(time(FA.cleaned))[1],frequency=frequency(FA.cleaned))/10000 # Get the factors.
  for (variable in variable.names)
  {
    cmd <- paste("var.data <- diff(var.datasets$",variable,")",sep="")
    eval(parse(text=cmd))
    var.names <- matrix(unlist(strsplit(colnames(var.data),".",fixed=TRUE)),ncol=3)[1,]
    cmd <- paste("F2VAR1.",variable," <- dynlm(var.data[,1]~lag(var.data[,1],-1) + lag(var.data[,2],-1) + lag(var.data[,3],-1) + lag(F[,1],-1) + lag(F[,2],-1))",sep="")
    eval(parse(text=cmd))
    cmd <- paste("the.model.outputs <- c(the.model.outputs,'F2VAR1.",variable,"')",sep="")
    eval(parse(text=cmd))
  }
  
  row.names <- "c('$\\\\Delta$ Lagged dependent variable','$\\\\Delta$ Lagged variable','$\\\\Delta Rate_{t-1}$','$f_{d1t}$','$f_{d2t}$')"
  column.names <- paste("c(",paste(paste("'$\\\\Delta  ",variable.names,"_t$'",sep=""),collapse=","),")",sep="")
  cmd <- paste("F2VAR1.LaTeX <- stargazer(",paste(the.model.outputs,collapse=','),",title='F(2)VAR(1) Estimates: Differences',covariate.labels=",row.names,",column.labels=",column.names,",model.numbers=FALSE,dep.var.labels.include=FALSE,no.space=TRUE)",sep="")
  eval(parse(text=cmd))
  outfile <- paste("../Output/Model_output_tables/F2VAR1.differences.tex",sep="")
  writeLines(F2VAR1.LaTeX,outfile)
}
  
