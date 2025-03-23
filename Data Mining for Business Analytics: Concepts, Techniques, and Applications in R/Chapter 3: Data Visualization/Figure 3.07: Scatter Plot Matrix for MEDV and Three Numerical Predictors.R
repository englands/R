## Boston Housing Data
housing.df <- read.csv("BostonHousing.csv")

## simple plot
# use plot() to generate a matrix of 4x4 panels with 
# variable name on the diagonal
# and scatter plots in the remaining panels.
plot(housing.df[, c(1, 3, 12, 13)])

# did not use alternate ggally package since it is not used
# in class
