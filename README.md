# Code for the paper "Forecasting models for the Chinese macroeconomy: the simpler the better?"

## Introduction

The code in the `Code` directory of this repository was used to compute all the results in the tables in the paper. The data used are in the `Datasets` directory. These data are all published by the [National Bureau of Statistics (NBS)](http://www.stats.gov.cn/english) and the [People's Bank of China](http://www.pbc.gov.cn/), but we downloaded them from Datastream. The output of the program is all written to files in the subdirectories of the `Output` directory, and includes much more than appears in the paper.

The code is all written in the [R programming language](https://www.r-project.org/). It was run using Version 3.6.0 of R (Planting of a Tree) on a c5.18xlarge EC2 instance at [Amazon Web Services](https://aws.amazon.com/) running Ubuntu Linux Server 18.04.2 LTS (Bionic).

## Prerequisites

To run the code, you will need an installed version of the R programming language. The code has been written to run on parallel processors by forking processes. This will work on Linux machines, but not on Windows. If you want to run the code on a Windows machine, you can do so by opening the file `Set_simulation_parameters.R` changing the variable `parallel.method` from `Fork` to `snow`. Alternatively, you could change the variable `num.cores` to `1`.

There are several packages that must be installed in order for the code to work. The following table lists the packages, the versions that were used to generate the results in the paper, and the source of the package.

| Package | Version | Source |
|---------|---------|--------|
|BMR |	0.11.0 | https://www.kthohr.com/bmr.html |
 |doParallel | 1.0.14 | https://cloud.r-project.org/ |
 |dynlm |	0.3-6 | https://cloud.r-project.org/ |
 |foreach	| 1.4.4 | https://cloud.r-project.org/ |
 |forecast |	8.7 | https://cloud.r-project.org/ |
 |lubridate | 1.7.4 | https://cloud.r-project.org/ |
 |sandwich | 2.5-1 | https://cloud.r-project.org/ |
 |seasonal | 1.7.0 | https://cloud.r-project.org/ |
 |splus2R | 1.2-2 | https://cloud.r-project.org/ |
 |stargazer | 5.2.2 | https://cloud.r-project.org/ |
 |tictoc | 1.0 | https://cloud.r-project.org/ |
 |tseries | 0.10-46 | https://cloud.r-project.org/ |
 |TTR | 0.23-4 | https://cloud.r-project.org/ |
 |uroot | 2.0-10 | https://cloud.r-project.org/ |
 |vars | 1.5-3 | https://cloud.r-project.org/ |
 |xtable | 1.8-4 | https://cloud.r-project.org/ |
 |xts | 0.11-2 | https://cloud.r-project.org/ |
 |zoo | 1.8-6 | https://cloud.r-project.org/ |
 
A working installation of pdflatex is also required. This is used by the program to create the document `Output/Output_dump/Output_dump.pdf` which contains a lot of the output in a readable form.

## Running the code

Start R in the `Code` directory and execute the command `source("Main_Program.R")`. You can change most of the program parameters in the file `Set_simulation_parameters.R`. All the output of the program is sent to files in subdirectories of `Output`. This includes files containing the cleaned data, the forecasts, the estimated model orders, and all the tables, which may be used to verify that the program is doing what it is supposed to.

## Documentation

There is none! If you start by reading through `Main_Program.R` you will see that the code is largely self-documented with comments and you should be able to see how everything works.

## Licence

<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/3.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/3.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/3.0/">Creative Commons Attribution-NonCommercial-NoDerivs 3.0 Unported License</a>.
