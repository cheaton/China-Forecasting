### Conduct seasonal and non-seasonal unit root tests.

#################### Non-seasonal #######################

##########
# Levels #
##########

# ADF tests

ADF.table <- matrix(rep(NA,length(variable.names)*3),ncol=length(variable.names))
colnames(ADF.table) <- variable.names
rownames(ADF.table) <- c("Lag","Statistic","p-value")

for (variable in variable.names)
{
  cmd <- paste("adf.",variable," <- adf.test(",variable,".cleaned$x.clean,alternative='stationary')",sep="")
  eval(parse(text=cmd))
  cmd <- paste("ADF.table['Lag','",variable,"'] <- adf.",variable,"$parameter",sep="")
  eval(parse(text=cmd))
  cmd <- paste("ADF.table['Statistic','",variable,"'] <- adf.",variable,"$statistic",sep="")
  eval(parse(text=cmd))
  cmd <- paste("ADF.table['p-value','",variable,"'] <- adf.",variable,"$p.value",sep="")
  eval(parse(text=cmd))
}

write.csv(ADF.table,"../Output/Test_statistic_tables/ADF.table.csv")
#table.heading <- "Augmented Dickey-Fuller Test Statistics"
Latex.ADF.table <- xtable(ADF.table,digits=3,caption="Augmented Dickey-Fuller Test Statistics")
print.xtable(Latex.ADF.table,file="../Output/Test_statistic_tables/ADF.table.tex",caption.placement='top',booktabs=TRUE)

# PP tests

PP.table <- matrix(rep(NA,length(variable.names)*3),ncol=length(variable.names))
colnames(PP.table) <- variable.names
rownames(PP.table) <- c("Lag","Statistic","p-value")

for (variable in variable.names)
{
  cmd <- paste("pp.",variable," <- pp.test(",variable,".cleaned$x.clean,alternative='stationary',type='Z(alpha)')",sep="")
  eval(parse(text=cmd))
  cmd <- paste("PP.table['Lag','",variable,"'] <- pp.",variable,"$parameter",sep="")
  eval(parse(text=cmd))
  cmd <- paste("PP.table['Statistic','",variable,"'] <- pp.",variable,"$statistic",sep="")
  eval(parse(text=cmd))
  cmd <- paste("PP.table['p-value','",variable,"'] <- pp.",variable,"$p.value",sep="")
  eval(parse(text=cmd))
}

write.csv(PP.table,"../Output/Test_statistic_tables/PP.table.csv")
#table.heading <- "Phillips-Peron Test Statistics"
Latex.PP.table <- xtable(PP.table,digits=3,caption="Phillips-Peron Test Statistics")
print.xtable(Latex.PP.table,file="../Output/Test_statistic_tables/PP.table.tex",caption.placement='top',booktabs=TRUE)

# KPSS tests

KPSS.table <- matrix(rep(NA,length(variable.names)*3),ncol=length(variable.names))
colnames(KPSS.table) <- variable.names
rownames(KPSS.table) <- c("Lag","Statistic","p-value")

for (variable in variable.names)
{
  cmd <- paste("kpss.",variable," <- kpss.test(",variable,".cleaned$x.clean,null='Level')",sep="")
  eval(parse(text=cmd))
  cmd <- paste("KPSS.table['Lag','",variable,"'] <- kpss.",variable,"$parameter",sep="")
  eval(parse(text=cmd))
  cmd <- paste("KPSS.table['Statistic','",variable,"'] <- kpss.",variable,"$statistic",sep="")
  eval(parse(text=cmd))
  cmd <- paste("KPSS.table['p-value','",variable,"'] <- kpss.",variable,"$p.value",sep="")
  eval(parse(text=cmd))
}

write.csv(KPSS.table,"../Output/Test_statistic_tables/KPSS.table.csv")
#table.heading <- "KPSS Test Statistics"
Latex.KPSS.table <- xtable(KPSS.table,digits=3,caption="KPSS Test Statistics")
print.xtable(Latex.KPSS.table,file="../Output/Test_statistic_tables/KPSS.table.tex",caption.placement='top',booktabs=TRUE)


###############
# Differences #
###############


# ADF tests

ADF.diff.table <- matrix(rep(NA,length(variable.names)*3),ncol=length(variable.names))
colnames(ADF.diff.table) <- variable.names
rownames(ADF.diff.table) <- c("Lag","Statistic","p-value")

for (variable in variable.names)
{
  cmd <- paste("adf.diff.",variable," <- adf.test(diff(",variable,".cleaned$x.clean),alternative='stationary')",sep="")
  eval(parse(text=cmd))
  cmd <- paste("ADF.diff.table['Lag','",variable,"'] <- adf.diff.",variable,"$parameter",sep="")
  eval(parse(text=cmd))
  cmd <- paste("ADF.diff.table['Statistic','",variable,"'] <- adf.diff.",variable,"$statistic",sep="")
  eval(parse(text=cmd))
  cmd <- paste("ADF.diff.table['p-value','",variable,"'] <- adf.diff.",variable,"$p.value",sep="")
  eval(parse(text=cmd))
}
colnames(ADF.diff.table) <- paste("$\\Delta$",variable.names,sep="")

write.csv(ADF.diff.table,"../Output/Test_statistic_tables/ADF.diff.table.csv")
#table.heading <- "Augmented Dickey-Fuller Test Statistics"
Latex.ADF.diff.table <- xtable(ADF.diff.table,digits=3,caption="Augmented Dickey-Fuller Test Statistics for Differenced Variables")
print.xtable(Latex.ADF.diff.table,file="../Output/Test_statistic_tables/ADF.diff.table.tex",caption.placement='top',booktabs=TRUE, table.placement="ht", sanitize.colnames.function = function(x){x})



# PP tests

PP.diff.table <- matrix(rep(NA,length(variable.names)*3),ncol=length(variable.names))
colnames(PP.diff.table) <- variable.names
rownames(PP.diff.table) <- c("Lag","Statistic","p-value")

for (variable in variable.names)
{
  cmd <- paste("pp.diff.",variable," <- pp.test(diff(",variable,".cleaned$x.clean),alternative='stationary',type='Z(alpha)')",sep="")
  eval(parse(text=cmd))
  cmd <- paste("PP.diff.table['Lag','",variable,"'] <- pp.diff.",variable,"$parameter",sep="")
  eval(parse(text=cmd))
  cmd <- paste("PP.diff.table['Statistic','",variable,"'] <- pp.diff.",variable,"$statistic",sep="")
  eval(parse(text=cmd))
  cmd <- paste("PP.diff.table['p-value','",variable,"'] <- pp.diff.",variable,"$p.value",sep="")
  eval(parse(text=cmd))
}
colnames(PP.diff.table) <- paste("$\\Delta$",variable.names,sep="")

write.csv(PP.diff.table,"../Output/Test_statistic_tables/PP.diff.table.csv")
Latex.PP.diff.table <- xtable(PP.diff.table,digits=3,caption="Phillips-Peron Test Statistics for Differenced Variables")
print.xtable(Latex.PP.diff.table,file="../Output/Test_statistic_tables/PP.diff.table.tex",caption.placement='top',booktabs=TRUE, table.placement="ht", sanitize.colnames.function = function(x){x})



# KPSS tests

KPSS.diff.table <- matrix(rep(NA,length(variable.names)*3),ncol=length(variable.names))
colnames(KPSS.diff.table) <- variable.names
rownames(KPSS.diff.table) <- c("Lag","Statistic","p-value")

for (variable in variable.names)
{
  cmd <- paste("kpss.diff.",variable," <- kpss.test(diff(",variable,".cleaned$x.clean),null='Level')",sep="")
  eval(parse(text=cmd))
  cmd <- paste("KPSS.diff.table['Lag','",variable,"'] <- kpss.diff.",variable,"$parameter",sep="")
  eval(parse(text=cmd))
  cmd <- paste("KPSS.diff.table['Statistic','",variable,"'] <- kpss.diff.",variable,"$statistic",sep="")
  eval(parse(text=cmd))
  cmd <- paste("KPSS.diff.table['p-value','",variable,"'] <- kpss.diff.",variable,"$p.value",sep="")
  eval(parse(text=cmd))
}
colnames(KPSS.diff.table) <- paste("$\\Delta$",variable.names,sep="")

write.csv(KPSS.diff.table,"../Output/Test_statistic_tables/KPSS.diff.table.csv")
Latex.KPSS.diff.table <- xtable(KPSS.diff.table,digits=3,caption="KPSS Test Statistics for Differenced Variables")
print.xtable(Latex.KPSS.diff.table,file="../Output/Test_statistic_tables/KPSS.diff.table.tex",caption.placement='top',booktabs=TRUE, table.placement="ht", sanitize.colnames.function = function(x){x})


#################### Seasonal #######################

##########
# Levels #
##########

ch.matrix <- matrix(rep(NA,13*4),ncol=13)
for (i in 1:length(variable.names))
{
  cmd <- paste("ch.",variable.names[i]," <- ch.test(",variable.names[i],".cleaned$x.clean, type='dummy')",sep="")
  eval(parse(text=cmd))
  cmd <- paste("ch.matrix[i,] <- ch.",variable.names[i],"$pvalue",sep="")
  eval(parse(text=cmd))
}
colnames(ch.matrix) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","joint")
rownames(ch.matrix) <- variable.names

write.csv(ch.matrix,"../Output/Test_statistic_tables/CH.table.csv")
Latex.CH.table <- xtable(ch.matrix,digits=3,caption="Canova and Hansen (1995) Test Statistics")
print.xtable(Latex.CH.table,file="../Output/Test_statistic_tables/CH.table.tex",caption.placement='top',booktabs=TRUE, table.placement="ht", sanitize.colnames.function = function(x){x},scalebox=0.8)

###############
# Differences #
###############

ch.diff.matrix <- matrix(rep(NA,13*4),ncol=13)
for (i in 1:length(variable.names))
{
  cmd <- paste("ch.diff.",variable.names[i]," <- ch.test(diff(",variable.names[i],".cleaned$x.clean), type='dummy')",sep="")
  eval(parse(text=cmd))
  cmd <- paste("ch.diff.matrix[i,] <- ch.diff.",variable.names[i],"$pvalue",sep="")
  eval(parse(text=cmd))
}
colnames(ch.diff.matrix) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","joint")
rownames(ch.diff.matrix) <- paste("$\\Delta$",variable.names,sep="")


write.csv(ch.diff.matrix,"../Output/Test_statistic_tables/CH.diff.table.csv")
Latex.CH.diff.table <- xtable(ch.diff.matrix,digits=3,caption="Canova and Hansen (1995) Test Statistics for Differenced Variables")
print.xtable(Latex.CH.diff.table,file="../Output/Test_statistic_tables/CH.diff.table.tex",caption.placement='top',booktabs=TRUE, table.placement="ht", sanitize.rownames.function = function(x){x},scalebox=0.8)
