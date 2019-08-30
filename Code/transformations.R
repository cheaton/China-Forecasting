transformations <- function(X,transformation.codes,rm.NA=TRUE)
{
# X is a ts object with T observations and N variables. transformation.codes is a data.frame 
# with a single entry of N numerics, each indicating a transformation that will be applied
# to the corresponding variable in X. The codes are 0 = no transformation, 1 = logs, 
# 2 = first difference, 3 = first difference of logs. NAs are removed unless rm.NA is set
# to FALSE. 
#
#                                                  Chris Heaton, April 2016.

  N <- dim(X)[2]
  T <- dim(X)[1]
  Ntc <- dim(transformation.codes)[1]
  transformation.codes <- transformation.codes[,1]
  
  if (N!=Ntc)
  {
    print(paste("WARNING: The number of transformation codes (",Ntc,") is different to the number of variables (",N,"). No transformations are being applied. FIX THIS!",sep=""))
  }else
  {
    for (i in 1:N)
    {
      if (transformation.codes[i]==1)
      {
        X[,i] <- log(X[,i])
      }else if (transformation.codes[i]==2)
      {
        X[(2:T),i] <- diff(X[,i])
        X[1,i] <- NA
      }else if (transformation.codes[i]==3)
      {
        X[(2:T),i] <- diff(log(X[,i]))
        X[1,i] <- NA
      }else if (transformation.codes[i]!=0)
      {
        print(paste("WARNING: the transformation code for variable",i,"is",transformation.codes[i],"which is invalid. No transformation has been applied to this variable."))
      }
    }
  }
  if (rm.NA)
  {
    return(na.omit(X))
  }else
  {
    return(X)
  }
}
