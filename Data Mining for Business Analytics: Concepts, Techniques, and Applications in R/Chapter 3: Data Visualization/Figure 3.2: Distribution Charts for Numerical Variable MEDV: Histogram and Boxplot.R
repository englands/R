## Boston Housing Data
housing.df <- read.csv("BostonHousing.csv")

## histogram of MEDV with ggplot
library(ggplot2)
ggplot(housing.df) + geom_histogram(aes(x = MEDV), binwidth = 5)

## boxplot of MEDV for different values of CHAS using ggplot\
ggplot(housing.df) + geom_boxplot(aes(x = as.factor(CHAS), y = MEDV)) + xlab("CHAS")
