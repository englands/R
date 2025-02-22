## line chart for the Amtrak data
Amtrak.df <- read.csv("Amtrak.csv")

# use time series analysis
library(forecast)
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
plot(ridership.ts, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))

## Boston housing data
housing.df <- read.csv("BostonHousing.csv")

## scatter plot with axes names using ggplot
library(ggplot2)
ggplot(housing.df) + geom_point(aes(x = LSTAT, y = MEDV), colour = "navy", alpha = 0.7)

## bar chart of CHAS vs. mean MEDV using ggplot
# compute mean MEDV per CHAS = (0, 1)
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$CHAS), FUN = mean)
names(data.for.plot)<- c("CHAS", "MeanMEDV")
# bar chart plot with ggplot
ggplot(data.for.plot) + geom_bar(aes(x = CHAS, y = MeanMEDV), stat = "identity")

## bar chart of CHAS vs. % CAT..MEDV
data.for.plot <- aggregate(housing.df$CAT..MEDV, by = list(housing.df$CHAS), FUN = mean)
names(data.for.plot) <- c("CHAS", "MeanCATMEDV")
barplot(data.for.plot$MeanCATMEDV * 100, names.arg = data.for.plot$CHAS,
        xlab = "CHAS", ylab = "% of CAT.MEDV")
