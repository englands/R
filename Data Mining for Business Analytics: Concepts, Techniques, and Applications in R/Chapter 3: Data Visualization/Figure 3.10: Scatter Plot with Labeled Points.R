utilities.df <- read.csv("Utilities.csv")

# alternate with ggplot
library(ggplot2)
ggplot(utilities.df, aes(y = Fuel_Cost,
                         x = Sales)) + 
  geom_point() + 
  geom_text(aes(label = paste(" ", Company)),
            size = 4, hjust = 0.0, angle = 15) + 
  ylim(0.25, 2.25) + xlim(3000, 18000)
