housing.df <- read.csv("BostonHousing.csv")

library(MASS)
par(mfcol = c(2,1))
parcoord(housing.df[housing.df$CAT..MEDV == 0, -14], 
         main = "CAT..MEDV = 0")
parcoord(housing.df[housing.df$CAT..MEDV == 1, -14], 
         main = "CAT..MEDV = 1")