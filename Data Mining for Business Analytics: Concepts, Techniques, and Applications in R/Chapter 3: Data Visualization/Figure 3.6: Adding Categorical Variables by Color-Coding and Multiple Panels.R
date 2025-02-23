## Boston Housing Data
housing.df <- read.csv("BostonHousing.csv")

## color plot using ggplot2
library(ggplot2)
ggplot(housing.df, aes(y = NOX, x = LSTAT, colour = CAT..MEDV)) +
  geom_point(alpha = 0.6)

## panel plots
# compute mean MEDV per RAD and CHAS
# In aggregate() use argument drop = FALSE to include all combinations
# (exiting and missing) of RAD X CHAS.
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$RAD, 
                                                      housing.df$CHAS),
                           FUN = mean, drop = FALSE)
names(data.for.plot) <- c("RAD", "CHAS", "meanMEDV")

par(mfcol = c(2,1))

# plot the data with ggplot
ggplot(data.for.plot) + 
  geom_bar(aes(x = as.factor(RAD), y = 'meanMEDV'), stat = "identity") +
  xlab("RAD") + facet_grid(CHAS ~ .)
