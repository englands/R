library(forecast)
Amtrak.df <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), 
                   end = c(2004, 3), freq = 12)

## fit curve / alternative plot with ggplot
library(ggplot2)
ggplot(Amtrak.df, aes(y = Ridership, 
                       x = Month, 
                       group = 12)) + 
  geom_line() + 
  geom_smooth(formula = y ~ poly(x, 2), 
              method = "lm", colour = "navy",
              se = FALSE)

## zoom in, monthly, and annual plots
ridership.2yrs <- window(ridership.ts, start = c(1991,1), 
                         end = c(1992,12))
plot(ridership.2yrs, xlab = "Year", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 2300))
monthly.ridership.ts <- tapply(ridership.ts, cycle(ridership.ts), mean)
plot(monthly.ridership.ts, xlab = "Month", ylab = "Average Ridership",
     ylim = c(1300, 2300), type = "1", xaxt = 'n')
## set x labels
axis(1, at = c(1:12), labels = c("Jan", "Feb", "Mar",
                                 "Apr", "May", "Jun",
                                 "Jul", "Aug", "Sep",
                                 "Oct", "Nov", "Dec"))

annual.ridership.ts <- aggregate(ridership.ts, FUN = mean)
plot(annual.ridership.ts, xlab = "Year", ylab = "Average Ridership",
     ylim = c(1300, 2300))