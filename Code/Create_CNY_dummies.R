library(lubridate)
library(zoo)
cny.dates.df <- read.csv(file="../Datasets/Chinese_New_Year_Dates.csv",header=TRUE,stringsAsFactors=TRUE)
cny.dates.df[,2] <- as.Date(cny.dates.df[,2],format="%Y/%m/%d")
num.years <- length(cny.dates.df[,1])
cny.dates.matrix <- as.matrix(cny.dates.df)
rownames(cny.dates.matrix) <- cny.dates.df[,1]
cny.dates.matrix <- cny.dates.matrix[,2]

CNY <- ts(rep(0,num.years*12),start=c(cny.dates.df[1,1],1),frequency=12)
for (year in 1:num.years)
{
    if (month(cny.dates.df[year,2])==1)
    {
        window(CNY,start=c(cny.dates.df[year,1],1),end=c(cny.dates.df[year,1],1)) <- 1
    }else {
       window(CNY,start=c(cny.dates.df[year,1],2),end=c(cny.dates.df[year,1],2)) <- 1
    }
}

CNY <- window(CNY, start=c(1996,10), end=c(2018,12))
CNY.df <- data.frame(CNY)
rownames(CNY.df) <- as.yearmon(time(CNY))
write.csv(CNY.df,file="../Datasets/CNY_dummies.csv")