## Boston Housing Data
housing.df <- read.csv("BostonHousing.csv")

# is.na returns a Boolean (TRUE/FALSE) output
# indicating location of missing values
# multiplying the Boolean value by 1 converts the output 
# into binary
heatmap(1 * is.na(housing.df), Rowv = NA, Colv = NA)

# error due to no missing data