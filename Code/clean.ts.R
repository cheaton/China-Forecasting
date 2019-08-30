clean.ts <- function(x,multiple=Inf,interpolate="false",difference=FALSE)
{
# x is a ts object and multiple is a scalar. This function does two things. Firstly,
# any observation that has a magnitude greater than or equal to multiple x the interquartile
# range is replaced by NA. If difference=TRUE, then the observation is replaced by NA
# if the first difference has a magnitude greater than or equal to multiple x the interquartile
# range of the first difference. If multiple=Inf, then this step has no effect.
#
# Secondly, if interpolate!=FALSE, all NAs (including those created in the first step,
# are replaced by either linear (if interpolate="linear") or cubic (if interpolate="cubic")
# interploation. The output is a list with 3 items:
#
#         x.clean = the new series;
#         outlier.dates = the sequence of dates at which the outliers were removed.
#         NA.dates = the sequence of dates at which the NAs were replaced.
#
#                                                       Chris Heaton, April 2016.

  library(zoo)
  if (difference)
  {
    dx<-diff(x)
  }else
  {
    dx <- x
  }
  iqr.dx <- IQR(dx,na.rm=TRUE)
  median.dx <- median(dx,na.rm=TRUE) 
  outlier.indexes <- which(abs(dx)>=(median.dx + multiple*iqr.dx))
  outlier.dates <- as.yearmon(time(x))[outlier.indexes]
  x.ol.na <- x
  x.ol.na[outlier.indexes] <- NA

  NA.indexes <- which(is.na(x.ol.na))
  NA.dates <- as.yearmon(time(x))[NA.indexes]
  
  if (interpolate=="linear")
  {
    x.clean <- na.approx(x.ol.na)
  }else if (interpolate=="cubic")
  {
    x.clean <- na.spline(x.ol.na)
  }else
  {
    x.clean <- x.ol.na
  }
  x.clean <- ts(x.clean,start=as.yearmon(time(x)[1]),frequency=frequency(x))
  return(list(x.clean = x.clean, outlier.dates = outlier.dates, NA.dates = NA.dates))
}
