file.name <- "../Output/Output_dump/Output_dump.tex"
abstract <- "An output dump for the China macro forecasting project. The baseline forecast is the mean. The tables MSEs have been standardised so that the mean has a standardised MSE of 1. The p-values are written in {\\it{italics}} if the p-value is less than 0.05 and the corresponding relative MSE is greater than 1 (i.e. the forecast is significantly worse). If the forecast is significantly better (i.e. the p-value is less than 0.05 and the relative MSE is less than 1) then the p-value is written in {\\bf{bold}} font. PDF bookmarks have been used throughout the document so you can navigate using the index in the left pane."
write.Latex.preamble("Plots and Tables for China Macro-Forecasting Paper","Chris Heaton",abstract,file.name)

## Write a data section.

# A subsection for the raw data.
write.Latex.section("The data",file.name)
write.Latex.subsection("Raw Data",file.name)

for (variable in variable.names)
{
#  write.Latex.subsection(variable,file.name)
  write.Latex.FloatBarrier(file.name)
  cmd <- paste("write.Latex.plot(plot.filename='../../Output/Plots/Plot_of_Raw_",variable,".pdf',caption='Raw ",variable," Data',label='",variable,"_Raw',width=12,height=7,file.name)",sep="")
  eval(parse(text=cmd))
  write.Latex.pdfbookmark(3,paste("Plot of Raw ",variable,sep=""),paste("Raw.plot",variable,sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
  
  cmd <- paste("outliers.exist <- (length(",variable,".cleaned$outlier.dates)>0)",sep="")
  eval(parse(text=cmd))
  
  if (outliers.exist)
  {
    cmd <- paste("the.dates <- paste(",variable,".cleaned$outlier.dates,collapse=', ')",sep="")
    eval(parse(text=cmd))
    write.Latex.text(paste("Defining an outlier to be an observation for which the first difference that is at least ",outlier.multiple," times the interquartile range of the first differences, ",variable," has outliers at the following dates: ",the.dates,". These were removed.\n\n",sep=""),file.name)
  }
 
  cmd <- paste("NAs.exist <- (length(",variable,".cleaned$NA.dates)>0)",sep="")
  eval(parse(text=cmd))
   
  if (NAs.exist)
  { 
    cmd <- paste("the.dates <- paste(",variable,".cleaned$NA.dates,collapse=', ')",sep="")
    eval(parse(text=cmd))
    write.Latex.text(paste(variable," has missing values at the following dates: ",the.dates,". These were replaced using cubic interpolation.\n\n",sep=""),file.name)
  }
}

# A subsection for the cleaned data.
write.Latex.subsection("Cleaned Data",file.name)

write.Latex.text("\n\nHere are plots of the data with outliers removed and missing values interpolated. CPI, PPI and IP do not appear to be seasonal (check whether this is because they are supplied seasonally adjusted). EP is clearly seasonal so it is seasonally adjusted using the X13 model. Plots of the first differences are also provided.\n\n",file.name)

for (variable in variable.names)
{
#  write.Latex.subsection(variable,file.name)
  write.Latex.FloatBarrier(file.name)
  cmd <- paste("write.Latex.plot(plot.filename='../../Output/Plots/Plot_of_Cleaned_",variable,".pdf',caption='Cleaned ",variable," Data',label='",variable,"_Cleaned',width=12,height=7,file.name)",sep="")
  eval(parse(text=cmd))

  write.Latex.pdfbookmark(3,paste("Plot of Cleaned ",variable,sep=""),paste("Clean.plot",variable,sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
  cmd <- paste("write.Latex.plot(plot.filename='../../Output/Plots/Plot_of_Differenced_Cleaned_",variable,".pdf',caption='First Difference of Cleaned ",variable," Data',label='diff_",variable,"_Cleaned',width=12,height=7,file.name)",sep="")
  eval(parse(text=cmd))
  write.Latex.pdfbookmark(3,paste("Plot of Differenced Cleaned ",variable,sep=""),paste("Diff.Clean.plot",variable,sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}

# A subsection for the properties of the variables.
write.Latex.subsection("Time Series Properties",file.name)
write.Latex.subsubsection("ACFs",file.name)

write.Latex.text("\n\nHere are plots of the ACFs of the variables and their first differences.\n\n",file.name)

for (variable in variable.names)
{
#  write.Latex.subsection(variable,file.name)
  write.Latex.FloatBarrier(file.name)
  cmd <- paste("write.Latex.plot(plot.filename='../../Output/Plots/Plot_of_ACFs_Cleaned_",variable,".pdf',caption='Sample ACF of Cleaned ",variable," Data',label='",variable,"_ACF',width=12,height=7,file.name)",sep="")
  eval(parse(text=cmd))
  write.Latex.pdfbookmark(3,paste("Sample ACF of ",variable,sep=""),paste("ACF.plot",variable,sep=""),file.name)

  write.Latex.FloatBarrier(file.name)

  cmd <- paste("write.Latex.plot(plot.filename='../../Output/Plots/Plot_of_ACFs_Differenced_Cleaned_",variable,".pdf',caption='Sample ACF of Differenced Cleaned ",variable," Data',label='",variable,"_diff_ACF',width=12,height=7,file.name)",sep="")
  eval(parse(text=cmd))
  write.Latex.pdfbookmark(3,paste("Sample ACF of differenced ",variable,sep=""),paste("diff.ACF.plot",variable,sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

}

# Unit root tests.
write.Latex.subsubsection("Unit root tests",file.name)
write.Latex.FloatBarrier(file.name)
write.Latex.pdfbookmark(3,"ADF tests","ADF.test",file.name)
write.Latex.table.file("../../Output/Test_statistic_tables/ADF.table.tex",file.name)

write.Latex.FloatBarrier(file.name)
write.Latex.pdfbookmark(3,"ADF diff tests","ADF.diff.test",file.name)
write.Latex.table.file("../../Output/Test_statistic_tables/ADF.diff.table.tex",file.name)


write.Latex.FloatBarrier(file.name)
write.Latex.pdfbookmark(3,"PP tests","PP.test",file.name)
write.Latex.table.file("../../Output/Test_statistic_tables/PP.table.tex",file.name)

write.Latex.FloatBarrier(file.name)
write.Latex.pdfbookmark(3,"PP diff tests","PP.diff.test",file.name)
write.Latex.table.file("../../Output/Test_statistic_tables/PP.diff.table.tex",file.name)


write.Latex.FloatBarrier(file.name)
write.Latex.pdfbookmark(3,"KPSS tests","KPSS.test",file.name)
write.Latex.table.file("../../Output/Test_statistic_tables/KPSS.table.tex",file.name)

write.Latex.FloatBarrier(file.name)
write.Latex.pdfbookmark(3,"KPSS diff tests","KPSS.diff.test",file.name)
write.Latex.table.file("../../Output/Test_statistic_tables/KPSS.diff.table.tex",file.name)


# Seasonal unit root tests.
write.Latex.subsubsection("Seasonal unit root tests",file.name)
write.Latex.FloatBarrier(file.name)
write.Latex.pdfbookmark(3,"CH tests","CH.test",file.name)
write.Latex.table.file("../../Output/Test_statistic_tables/CH.table.tex",file.name)

write.Latex.FloatBarrier(file.name)
write.Latex.pdfbookmark(3,"CH diff tests","CH.diff.test",file.name)
write.Latex.table.file("../../Output/Test_statistic_tables/CH.diff.table.tex",file.name)







write.Latex.section("MSEs and p-values: Levels",file.name)
for (variable in variable.names)
{
  write.Latex.FloatBarrier(file.name)
  write.Latex.subsection(variable,file.name)
  write.Latex.text("\n\nThe following tables were computed by fitting models to the levels of the cleaned data and computing forecasts.\n\n",file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("relMSE for ",variable,":Levels",sep=""),paste("MSE.levels.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/Error_tables/rel_MSE_levels_",variable,".tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("C p-value for ",variable,":Levels",sep=""),paste("Cp.levels.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MSE_levels_conditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("UC p-value for ",variable,":Levels",sep=""),paste("UCp.levels",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MSE_levels_unconditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}


write.Latex.section("MSEs and p-values: Differences",file.name)
for (variable in variable.names)
{
  write.Latex.FloatBarrier(file.name)
  write.Latex.subsection(variable,file.name)
  write.Latex.text("\n\nThe following tables were computed by fitting models to the first differences of the cleaned data, computing forecasts of the first differences at every horizon, and then cumulatively aggregating over horizons to get forecasts of the level of the variable at each horizon..\n\n",file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("relMSE for ",variable,":Differences",sep=""),paste("MSE.differences.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/Error_tables/rel_MSE_differences_",variable,".tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("C p-value for ",variable,":Differences",sep=""),paste("Cp.differences.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MSE_differences_conditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("UC p-value for ",variable,":Differences",sep=""),paste("UCp.differences",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MSE_differences_unconditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}


write.Latex.section("MSEs and p-values: Changes",file.name)
for (variable in variable.names)
{
  write.Latex.FloatBarrier(file.name)
  write.Latex.subsection(variable,file.name)
  write.Latex.text("\n\nThe following tables were computed by fitting models to the first changes of the cleaned data, computing forecasts of the first changes at every horizon, and then cumulatively aggregating over horizons to get forecasts of the level of the variable at each horizon..\n\n",file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("relMSE for ",variable,":Changes",sep=""),paste("MSE.changes.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/Error_tables/rel_MSE_changes_",variable,".tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("C p-value for ",variable,":Changes",sep=""),paste("Cp.changes.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MSE_changes_conditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("UC p-value for ",variable,":Changes",sep=""),paste("UCp.changes",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MSE_changes_unconditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}






write.Latex.section("MAEs and p-values: Levels",file.name)
for (variable in variable.names)
{
  write.Latex.FloatBarrier(file.name)
  write.Latex.subsection(variable,file.name)
  write.Latex.text("\n\nThe following tables were computed by fitting models to the levels of the cleaned data and computing forecasts.\n\n",file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("relMAE for ",variable,":Levels",sep=""),paste("MAE.levels.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/Error_tables/rel_MAE_levels_",variable,".tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("C p-value for ",variable,":Levels",sep=""),paste("Cp.levels.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAE_levels_conditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("UC p-value for ",variable,":Levels",sep=""),paste("UCp.levels",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAE_levels_unconditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}


write.Latex.section("MAEs and p-values: Differences",file.name)
for (variable in variable.names)
{
  write.Latex.FloatBarrier(file.name)
  write.Latex.subsection(variable,file.name)
  write.Latex.text("\n\nThe following tables were computed by fitting models to the first differences of the cleaned data, computing forecasts of the first differences at every horizon, and then cumulatively aggregating over horizons to get forecasts of the level of the variable at each horizon..\n\n",file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("relMAE for ",variable,":Differences",sep=""),paste("MAE.differences.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/Error_tables/rel_MAE_differences_",variable,".tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("C p-value for ",variable,":Differences",sep=""),paste("Cp.differences.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAE_differences_conditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("UC p-value for ",variable,":Differences",sep=""),paste("UCp.differences",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAE_differences_unconditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}


write.Latex.section("MAEs and p-values: Changes",file.name)
for (variable in variable.names)
{
  write.Latex.FloatBarrier(file.name)
  write.Latex.subsection(variable,file.name)
  write.Latex.text("\n\nThe following tables were computed by fitting models to the first changes of the cleaned data, computing forecasts of the first changes at every horizon, and then cumulatively aggregating over horizons to get forecasts of the level of the variable at each horizon..\n\n",file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("relMAE for ",variable,":Changes",sep=""),paste("MAE.changes.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/Error_tables/rel_MAE_changes_",variable,".tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("C p-value for ",variable,":Changes",sep=""),paste("Cp.changes.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAE_changes_conditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("UC p-value for ",variable,":Changes",sep=""),paste("UCp.changes",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAE_changes_unconditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}






write.Latex.section("MAPEs and p-values: Levels",file.name)
for (variable in variable.names)
{
  write.Latex.FloatBarrier(file.name)
  write.Latex.subsection(variable,file.name)
  write.Latex.text("\n\nThe following tables were computed by fitting models to the levels of the cleaned data and computing forecasts.\n\n",file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("relMAPE for ",variable,":Levels",sep=""),paste("MAPE.levels.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/Error_tables/rel_MAPE_levels_",variable,".tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("C p-value for ",variable,":Levels",sep=""),paste("Cp.levels.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAPE_levels_conditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("UC p-value for ",variable,":Levels",sep=""),paste("UCp.levels",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAPE_levels_unconditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}


write.Latex.section("MAPEs and p-values: Differences",file.name)
for (variable in variable.names)
{
  write.Latex.FloatBarrier(file.name)
  write.Latex.subsection(variable,file.name)
  write.Latex.text("\n\nThe following tables were computed by fitting models to the first differences of the cleaned data, computing forecasts of the first differences at every horizon, and then cumulatively aggregating over horizons to get forecasts of the level of the variable at each horizon..\n\n",file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("relMAPE for ",variable,":Differences",sep=""),paste("MAPE.differences.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/Error_tables/rel_MAPE_differences_",variable,".tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("C p-value for ",variable,":Differences",sep=""),paste("Cp.differences.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAPE_differences_conditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("UC p-value for ",variable,":Differences",sep=""),paste("UCp.differences",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAPE_differences_unconditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}


write.Latex.section("MAPEs and p-values: Changes",file.name)
for (variable in variable.names)
{
  write.Latex.FloatBarrier(file.name)
  write.Latex.subsection(variable,file.name)
  write.Latex.text("\n\nThe following tables were computed by fitting models to the first changes of the cleaned data, computing forecasts of the first changes at every horizon, and then cumulatively aggregating over horizons to get forecasts of the level of the variable at each horizon..\n\n",file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("relMAPE for ",variable,":Changes",sep=""),paste("MAPE.changes.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/Error_tables/rel_MAPE_changes_",variable,".tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("C p-value for ",variable,":Changes",sep=""),paste("Cp.changes.",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAPE_changes_conditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)

  write.Latex.pdfbookmark(3,paste("UC p-value for ",variable,":Changes",sep=""),paste("UCp.changes",variable,sep=""),file.name)
  write.Latex.table.file(paste("../../Output/pvalue_tables/",variable,"_MAPE_changes_unconditional.pvalues.tex",sep=""),file.name)
  write.Latex.FloatBarrier(file.name)
}


write.Latex.section("Model Estimation Output",file.name)

# AR(1) levels
write.Latex.pdfbookmark(3,"AR(1) Estimates Levels","AR(1).output.levels",file.name)
write.Latex.table.file("../../Output/Model_output_tables/AR1.levels.tex",file.name)
write.Latex.FloatBarrier(file.name)

# AR(1) differences
write.Latex.pdfbookmark(3,"AR(1) Estimates Differences","AR(1).output.differences",file.name)
write.Latex.table.file("../../Output/Model_output_tables/AR1.differences.tex",file.name)
write.Latex.FloatBarrier(file.name)

## AR(p) levels
#write.Latex.pdfbookmark(3,"AR(1) Estimates Levels","AR(1).output.levels",file.name)
#write.Latex.table.file("../../Output/Model_output_tables/ARp.levels.tex",file.name)
#write.Latex.FloatBarrier(file.name)

## AR(p) differences
#write.Latex.pdfbookmark(3,"AR(1) Estimates Differences","AR(1).output.differences",file.name)
#write.Latex.table.file("../../Output/Model_output_tables/ARp.differences.tex",file.name)
#write.Latex.FloatBarrier(file.name)

# ARd(1) levels
for (h in horizons)
{
  cmd <- paste("write.Latex.pdfbookmark(3,'ARd(1) Estimates Levels h=",h,"','ARd(1).output.levels.h",h,",',file.name)",sep="")
  eval(parse(text=cmd))
  cmd <- paste("write.Latex.table.file('../../Output/Model_output_tables/ARd1.levels.h",h,".tex',file.name)",sep="")
  eval(parse(text=cmd))
  write.Latex.FloatBarrier(file.name)
}

# ARd(1) differences
for (h in horizons)
{
  cmd <- paste("write.Latex.pdfbookmark(3,'ARd(1) Estimates Differences h=",h,"','ARd(1).output.differences.h",h,",',file.name)",sep="")
  eval(parse(text=cmd))
  cmd <- paste("write.Latex.table.file('../../Output/Model_output_tables/ARd1.differences.h",h,".tex',file.name)",sep="")
  eval(parse(text=cmd))
  write.Latex.FloatBarrier(file.name)
}


# ARMA(1,1) levels
write.Latex.pdfbookmark(3,"ARMA(1,1) Estimates Levels","ARMA(1,1).output.levels",file.name)
write.Latex.table.file("../../Output/Model_output_tables/ARMA11.levels.tex",file.name)
write.Latex.FloatBarrier(file.name)

# ARMA(1,1) differences
write.Latex.pdfbookmark(3,"ARMA(1,1) Estimates Differences","ARMA(1,1).output.differences",file.name)
write.Latex.table.file("../../Output/Model_output_tables/ARMA11.differences.tex",file.name)
write.Latex.FloatBarrier(file.name)

# VAR(1) levels
write.Latex.pdfbookmark(3,"VAR(1) Estimates Levels","VAR(1).output.levels",file.name)
write.Latex.table.file("../../Output/Model_output_tables/VAR1.levels.tex",file.name)
write.Latex.FloatBarrier(file.name)

# VAR(1) differences
write.Latex.pdfbookmark(3,"VAR(1) Estimates Differences","VAR(1).output.differences",file.name)
write.Latex.table.file("../../Output/Model_output_tables/VAR1.differences.tex",file.name)
write.Latex.FloatBarrier(file.name)

# Factor(2) levels
for (h in horizons)
{
  cmd <- paste("write.Latex.pdfbookmark(3,'F(2) Estimates Levels h=",h,"','F(2).output.levels.h",h,",',file.name)",sep="")
  eval(parse(text=cmd))
  cmd <- paste("write.Latex.table.file('../../Output/Model_output_tables/F2.levels.h",h,".tex',file.name)",sep="")
  eval(parse(text=cmd))
  write.Latex.FloatBarrier(file.name)
}

# Factor(2) differences
for (h in horizons)
{
  cmd <- paste("write.Latex.pdfbookmark(3,'F(2) Estimates Differences h=",h,"','F(2).output.differences.h",h,",',file.name)",sep="")
  eval(parse(text=cmd))
  cmd <- paste("write.Latex.table.file('../../Output/Model_output_tables/F2.differences.h",h,".tex',file.name)",sep="")
  eval(parse(text=cmd))
  write.Latex.FloatBarrier(file.name)
}


# F(2)VAR(1) levels
write.Latex.pdfbookmark(3,"F(2)VAR(1) Estimates Levels","F(2)VAR(1).output.levels",file.name)
write.Latex.table.file("../../Output/Model_output_tables/F2VAR1.levels.tex",file.name)
write.Latex.FloatBarrier(file.name)

# F(2)VAR(1) differences
write.Latex.pdfbookmark(3,"F(2)VAR(1) Estimates Differences","F(2)VAR(1).output.differences",file.name)
write.Latex.table.file("../../Output/Model_output_tables/F2VAR1.differences.tex",file.name)
write.Latex.FloatBarrier(file.name)


write.end.Latex.document("apa","referencesHPZ.tex",file.name)

setwd("../Output/Output_dump")
system("pdflatex Output_dump")
system("pdflatex Output_dump")
setwd("../../Code")


