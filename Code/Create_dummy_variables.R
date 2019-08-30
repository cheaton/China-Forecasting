create.cny.dummy <- function(y,horizon=NULL)
{
    # y is a monthly ts object. This function creates a corresponding ts object containing a centred Lunar New Year dummy variable with the same dates as y. 
    # If horizon is a positive integer, then it also creates another ts object with
    # the LNY dummy variable for the following horizon time periods.
    #
    # The genhol function from the seasonal package does the work and assumes
    # that the holiday lasts for 6 days (and possibly spans both months). The function
    # returns a list containing two elements. cny.dummy is the dummy variable
    # as a ts object. future.cny.dummy is NULL if horizon is NULL. If horizon is
    # an integer then future.cny.dummy is a ts object containing the monthly
    # Lunar New Year dummy variable for the subsequent horizon months.
    #
    #                                             Chris Heaton, July 2019.
    library(seasonal)
    all.cny.dummy <- genhol(cny, start=0, end=6, center="calendar")
    first.date.yearmon <- as.yearmon(index(y)[1])
    first.date <- c(year(first.date.yearmon),month(first.date.yearmon))
    last.date.yearmon <- as.yearmon(index(y)[length(y)])
    last.date <- c(year(last.date.yearmon),month(last.date.yearmon))
    cny.dummy <- window(all.cny.dummy,first.date,last.date)
    if (!is.null(horizon))
    {
        first.extra.date.yearmon <- last.date.yearmon + 1/frequency
        last.extra.date.yearmon <- first.extra.date.yearmon + (horizon-1)/frequency
        first.extra.date <- c(year(first.extra.date.yearmon),month(first.extra.date.yearmon))
        last.extra.date <- c(year(last.extra.date.yearmon),month(last.extra.date.yearmon))
        cny.dummy.extra <- window(all.cny.dummy,first.extra.date,last.extra.date)
    }else{
       cny.dummy.extra <- NULL
    }
    return(list(cny.dummy = cny.dummy, future.cny.dummy = cny.dummy.extra))
}


create.seasonal.dummies <- function(x) 
{
    # Modified version of seasonaldummy from the forecast package. This version
    # doesn't drop a dummy and there is no h. Also, the output is a ts object.
    # x is a ts object. The function returns a set of monthly seasonal dummies 
    # as a mts object.
    #
    #                                          Chris Heaton, July 2019.
    if (!is.ts(x)) {
        stop("Not a time series")
    }
    else {
        fr.x <- frequency(x)
    }
    if (fr.x == 1) {
        stop("Non-seasonal time series")
    }
    dummy <- as.factor(cycle(x))
    dummy.mat <- matrix(0, ncol = frequency(x), nrow = length(x))
    nrow <- 1:length(x)
    for (i in 1:(frequency(x))) dummy.mat[dummy == paste(i), 
        i] <- 1
    colnames(dummy.mat) <- if (fr.x == 12) {
        month.abb[1:12]
    }
    else if (fr.x == 4) {
        c("Q1", "Q2", "Q3", "Q4")
    }
    else {
        paste("S", 1:(fr.x), sep = "")
    }
    dummy.mat <- ts(dummy.mat,start=start(x),frequency=frequency(x))
    return(dummy.mat)
}

