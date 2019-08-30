#################################
## Clean up the data
#################################

CPI.cleaned <- clean.ts(CPI,multiple=CPI.outlier.multiple,interpolate="cubic",difference=TRUE)
PPI.cleaned <- clean.ts(PPI,multiple=PPI.outlier.multiple,interpolate="cubic",difference=TRUE)
IP.cleaned <- clean.ts(IP,multiple=IP.outlier.multiple,interpolate="cubic",difference=TRUE)
EP.cleaned <- clean.ts(EP,multiple=EP.outlier.multiple,interpolate="none",difference=TRUE)

# Seasonally adjust variables

CPI.cleaned$x.clean <- final(seas(
x = CPI.cleaned$x.clean,
xreg = genhol(cny, start = 0, end = 6, center = "calendar"),
regression.aictest = "td",
regression.usertype = "holiday",
outlier = ""
))

PPI.cleaned$x.clean <- final(seas(
x = PPI.cleaned$x.clean,
xreg = genhol(cny, start = 0, end = 6, center = "calendar"),
regression.aictest = "td",
regression.usertype = "holiday",
outlier = ""
))

IP.cleaned$x.clean <- final(seas(
x = IP.cleaned$x.clean,
xreg = genhol(cny, start = 0, end = 6, center = "calendar"),
regression.aictest = "td",
regression.usertype = "holiday",
outlier = ""
))

EP.cleaned$x.clean <- final(seas(
x = EP.cleaned$x.clean,
xreg = genhol(cny, start = 0, end = 6, center = "calendar"),
regression.aictest = "td",
regression.usertype = "holiday",
transform.function = "none",
outlier = NULL,
na.action = na.x13
))


# Clean up the interest rate for the VAR models.
Rate.cleaned <- clean.ts(Rate,multiple=outlier.multiple,interpolate="cubic",difference=FALSE)


####################################################################
## Clean up and transform the X variables for the factor analysis ##
####################################################################
multiples <- rep(Inf,50)
multiples[2] <- outlier.multiple
# multiples[18] <- outlier.multiple   # First revision
# multiples[31] <- outlier.multiple   # First revision
multiples[19] <- outlier.multiple     # Second revision with railway cargo
multiples[32] <- outlier.multiple     # Second revision with railway cargo

FA.cleaned <- FA  # The columns are replaced by cleaned versions below.
for (i in 1:dim(FA)[2])   # Remove all NAs and remove outliers from varibles 2, 18 and 31 (or variables 2, 19 and 32 in the second revision with the railway cargo data included).
{
  FA.cleaned[,i] <- clean.ts(FA[,i],multiple=multiples[i],interpolate="cubic",difference=TRUE)$x.clean
  # if (!(i %in% c(26,27,28,32,39))) # These have features that cause problems for X-13 in the first revision.
 if (!(i %in% c(27,28,29,33,40))) # These have features that cause problems for X-13 in the second revision.
 {
   FA.cleaned[,i] <- final(seas(
   x = FA.cleaned[,i],
   xreg = genhol(cny, start = 0, end = 0, center = "calendar"),
   regression.aictest = "td",
   regression.usertype = "holiday",
   outlier = ""
   ))
 }
}

###############################################################################
## Write cleaned series to files so that they can be used by other programs, ##
## The VAR and FA datasets are saved in Construct_cleaned_datasets.R.        ##
###############################################################################

for (variable in variable.names)
{
  cmd <- paste("write.csv(",variable,".cleaned$x.clean, file='../Output/Cleaned_data/",variable,".cleaned.csv',row.names=as.yearmon(time(",variable,".cleaned$x.clean)))",sep="")
  eval(parse(text=cmd))
}

