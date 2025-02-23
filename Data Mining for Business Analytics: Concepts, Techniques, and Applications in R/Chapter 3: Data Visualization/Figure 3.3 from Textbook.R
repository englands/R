## Boston Housing Data
housing.df <- read.csv("BostonHousing.csv")

## side-by-side boxplots
# use par() to split the plots into panels
par(mfcol = c(1,4))
boxplot(housing.df$NOX ~ housing.df$CAT..MEDV, xlab = "CAT..MEDV", ylab = "NOX")
boxplot(housing.df$LSTAT ~ housing.df$CAT..MEDV, xlab = "CAT..MEDV", ylab = 
          "LSTAT")
boxplot(housing.df$PTRATIO ~ housing.df$CAT..MEDV, xlab = "CAT..MEDV", ylab = 
          "PTRATIO")
boxplot(housing.df$INDUS ~ housing.df$CAT..MEDV, xlab = "CAT..MEDV", ylab = 
          "INDUS")
