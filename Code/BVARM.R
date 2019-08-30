# When the code for this project was first written, the BMR package (which contained the BVARM) function
# was on CRAN. Subsequently, it was removed and is currently (05/03/2019) available from 
# https://www.kthohr.com/bmr.html, However, the UI has changed considerably. This function is a wrapper
# for the new UI, which allows the original code to run with the new version of BMR.

BVARM <- function(bvar_data,prior,p=4,constant=TRUE,keep=10000,burnin=5000,VType=2,HP1=0.5,HP2=0.5,HP3=100)
{
  library(BMR)
  bvar_data <- as.matrix(bvar_data)
  bvar_obj <- new(bvarm)
  bvar_obj$build(bvar_data,constant,p)
  bvar_obj$prior(prior,VType,2,HP1,HP2,HP3,0.5)
  bvar_obj$gibbs(keep)
  return(bvar_obj)
}
