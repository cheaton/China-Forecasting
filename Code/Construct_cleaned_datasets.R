##############################################################################
## Construct a data set for each VAR model with the variable being forecast ##
## appearing first in the vector. Combine them into a list.                 ##
##############################################################################

# CPI.var <- cbind(CPI.cleaned$x.clean,IP.cleaned$x.clean,Rate.cleaned$x.clean)
# PPI.var <- cbind(PPI.cleaned$x.clean,IP.cleaned$x.clean,Rate.cleaned$x.clean)
# IP.var <- cbind(IP.cleaned$x.clean,CPI.cleaned$x.clean,Rate.cleaned$x.clean)
# EP.var <- cbind(EP.cleaned$x.clean,CPI.cleaned$x.clean,Rate.cleaned$x.clean)

CPI.var <- cbind(CPI,IP,Rate)
PPI.var <- cbind(PPI,IP,Rate)
IP.var <- cbind(IP,CPI,Rate)
EP.var <- cbind(EP,CPI,Rate)

var.datasets <- list(CPI = CPI.var, PPI = PPI.var, IP = IP.var, EP = EP.var)

# Apply transformations (e.g. logs, differences) to the variable used in the factor analysis.

# FA.data = transformations(FA.cleaned,transformation.codes)  # First revision
# FA.data = scale(FA.data, center=TRUE, scale=FALSE)  # First revision
# FA.cleaned = transformations(FA.cleaned,transformation.codes)  # Second revision
# FA.cleaned = scale(FA.cleaned, center=TRUE, scale=TRUE)   # Second revision

# Save datasets so that they are available for other programs.
for (variable in variable.names)
{
  cmd <- paste("write.csv(",variable,".var, file='../Output/Cleaned_data/",variable,".var.cleaned.csv',row.names=as.yearmon(time(",variable,".var)))",sep="")
  eval(parse(text=cmd))
}

# write.csv(FA.cleaned,file='../Output/Cleaned_data/FA.cleaned.csv',row.names=as.yearmon(time(FA.cleaned)))

