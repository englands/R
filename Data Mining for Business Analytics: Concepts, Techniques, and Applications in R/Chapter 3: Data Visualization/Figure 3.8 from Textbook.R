## Boston Housing Data
housing.df <- read.csv("BostonHousing.csv")

options(scipen=999) # avoid scientific notation

## scatter plot: regular and log scale using ggplot2
ggplot(housing.df) + geom_point(aes(x = CRIM, y = MEDV)) + 
  scale_x_log10(breaks = 10^(-2:2),
                labels = format(10^(-2:2), scientific = FALSE, drop0trailing = TRUE)) + 
  scale_y_log10(breaks = c(5, 10, 20, 40))

## boxplot: regular and log scale
# no ggplot2 alternate
