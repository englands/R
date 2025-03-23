## Boston Housing Data
housing.df <- read.csv("BostonHousing.csv")

## simple heatmap of correlations (without values)
heatmap(cor(housing.df), Rowv = NA, Colv = NA)

## heatmap with values, ggplot2
library(ggplot2)
install.packages("reshape")
library(reshape) # to generate input for the plot
cor.mat <- round(cor(housing.df), 2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))
