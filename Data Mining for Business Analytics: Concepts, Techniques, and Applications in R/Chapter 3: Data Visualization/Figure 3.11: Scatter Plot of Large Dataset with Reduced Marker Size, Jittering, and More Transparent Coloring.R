universal.df <- read.csv("UniversalBank.csv")

# adding transparent colors in ggplot
library(ggplot2)
ggplot(universal.df) + 
  geom_jitter(aes(x = Income, y = CCAvg, 
                  colour = Securities.Account)) + 
  scale_x_log10(breaks = c(10, 20, 40, 60, 100, 200)) + 
  scale_y_log10(breaks = c(0.1, 0.2, 0.4, 0.6, 1.0, 2.0, 4.0, 6.0))
