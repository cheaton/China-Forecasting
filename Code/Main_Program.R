#####################################
## Load up libraries and functions ##
#####################################

library(tseries)
library(uroot)
library(BMR)
library(forecast)
library(vars)
library(xtable)
library(seasonal)
library(lubridate)
library(dynlm)
library(xts)
library(doParallel)
library(splus2R)
library(tictoc)
library(zoo)

source("Latex.creation.R") # Some functions that help build a .tex file.
source("clean.ts.R")  # A function which removes outliers of a given magnitude and replaces missing values.
source("forecasting.models.R") # A file containing functions that implement each of the forecasting models.
source("Latex.creation.R") # Some functions for writing a LaTeX document that contains all the output.
source("Compile_Error_stats_tables.R") # A function that constructs tables containing MSEs, etc.
source("CPAtest.R") # An R translation of the Giacomini's Matlab code for the Conditional Predictive Ability Test.
source("forecast.simulation.R") # A function which executes the horse race.
source("BVARM.R") # A wrapper for the BVARM function in the BMR package which is used to estimate the Bayesian VAR model.
source("Bai.Ng.2002.R") # A function which uses Bai and Ng's (2002) PC method to estimate a factor model.
source("transformations.R") # A function which applies transformations (e.g. differences and logs) to the data used for the factor analysis.
source("Create_dummy_variables.R") # Functions that create seasonal dummy variables, including a Lunar New Year dummy variable.

#########################
## Now run the scripts ##
#########################

tic()
source("Set_simulation_parameters.R")  # Read in the user-set parameters and set up parallel processors.
source("Read_the_data.R")  # Read in the data. See the file for the names of the ts objects created.
source("Plot_raw_data.R")  # Plot the raw data for the variables being forecast. PDFs of the plots are put in ../Output/Plots.
source("Clean_up_data.R")  # Remove outliers and replaces missing values by cubic interpolation. Seasonally adjust. The cleaned data now appear in lists with the names variable.cleaned (where variable is the variables name (see Read_the_data.R). Each list has 3 elements: $x.clean is the cleaned data; $outlier.dates contains the dates at which outliers were found; $NA.dates contains the dates at which there were missing data (including those dates at which outliers were removed. Note that the variable outlier.multiple in Set_simulation_parameters.R determines the definition of an outlier.
source("Plot_cleaned_data.R") # Plot the data with outliers removed, missing values interpolated seasonal adjustment where appropriate. PDFs of the plots are put in ../Output/Plots.
source("Plot_ACFs_cleaned_data.R") # Plot the sample ACFs. PDFs of the plots are put in ../Output/Plots.
source("Univariate_tests.R") # Conduct unit root tests. Tables (both as .csv and .tex) are put in ../Output/Test_statistic_tables and are available as ADF.table, PP.table, KPSS.table, ADF.diff.table, PP.diff.table, KPSS.diff.table, CH.table and CH.diff.table.
source("Construct_cleaned_datasets.R") # Use the cleaned up data to construct as ts objects the data sets used for the VAR models and the factor models.
source("In_sample_results.R") # Computes within-sample estimates for all the models with estimated parameters.
source("Run_simulations_levels.R") # Run all the forecast simulations specified in Set_simulation_parameters.R for the levels of the variables. The results are in the list Results.levels, listed under the variable names, and are saved in ../Output/Forecasts.
source("Run_simulations_differences.R") # Run all the forecast simulations specified in Set_simulation_parameters.R for the differences of the variables. The results are in the list Results.differences and Results.changes. The former contains forecasts of the levels of variables computed by fitting model to the differences, forecasting the differences and then aggregating. The latter contains the forecast of the change in each time period. The forecasts are also saved in ../Output/Forecasts.
source("Compile_Error_stats_tables_levels.R") # Construct the tables containing MSE, RMSE, MAE, and MAPE for the forecasts made by models fitted to the levels. Also constructs the same tables for the values relative to the benchmark forecasting model (which is set in the file Set_simulation_parameters.R). The results are available in the lists criterion.tables.levels and rel.criterion.tables.levels and are saved in ../Output/Error_tables as .csv and .tex files.
source("Compile_Error_stats_tables_differences.R") # Construct the tables containing MSE, RMSE, MAE, and MAPE for the forecasts of the levels computed by models fitted to the differences. Also constructs the same tables for the values relative to the benchmark forecasting model (which is set in the file Set_simulation_parameters.R). The results are available in the lists criterion.tables.differences and rel.criterion.tables.differences and are saved in ../Output/Error_tables as .csv and .tex files.
source("Compile_Error_stats_tables_changes.R") # Construct the tables containing MSE, RMSE, MAE, and MAPE for the forecasts of the differences at each horizon computed by models fitted to the differences. Also constructs the same tables for the values relative to the benchmark forecasting model (which is set in the file Set_simulation_parameters.R). The results are available in the lists criterion.tables.changes and rel.criterion.tables.chaanges and are saved in ../Output/Error_tables as .csv and .tex files.
source("Compile_pvalue_tables_levels.R") # Conduct the conditional and unconditional predictive ability tests for the forecasts from models estimated using the levels of variables, compile the tables of p-values and save them in ../Output/pvalue_tables. Also creates a list named p.value.levels.tables.'variable' for each variable, e.g. p.value.levels.tables.CPI.
source("Compile_pvalue_tables_differences.R") # Conduct the conditional and unconditional predictive ability tests for the forecasts from models estimated using the differences of variables, compile the tables of p-values and save them in ../Output/pvalue_tables. Also creates a list named p.value.differences.tables.'variable' for each variable, e.g. p.value.differences.tables.CPI.
source("Compile_pvalue_tables_changes.R") # Conduct the conditional and unconditional predictive ability tests for the forecasts of changes of variables, compile the tables of p-values and save them in ../Output/pvalue_tables. Also creates a list named p.value.changes.tables.'variable' for each variable, e.g. p.value.changes.tables.CPI.
source("Mark_sig_results.R") # Go through all the LaTeX p-value tables. If a result indicates that a forecast is statistically significantly worse than the benchmark, then the p-value is changed to italics. If it is significantly better, then the p-value is changed to bold.
source("Create_Latex_docs.R") # Create a LaTeX document containing all the output.
source("Analyse_forecast_errors.R")  # Create a LaTeX document containing plots of all the forecast errors, the MSPEs, the 24-month moving averages of the MSPEs, the differences between the MSPEs and the MSPE of the baseline forecast, and the 24-month moving averages thereof, and GEFP and supF tests of stability for the differences of the MSPEs.
cat("\n")
toc()
