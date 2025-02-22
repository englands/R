housing.df <- read.csv("WestRoxbury.csv", header = TRUE) # load data
names(housing.df) # print a list of variables to the screen
colnames(housing.df) [1] <- c("TOTAL_VALUE") # change the first column's name

train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
train.data <- housing.df[train.rows, ]

reg <- lm(TOTAL_VALUE ~ ., data = housing.df, subset = train.rows)
tr.res <- data.frame(train.data$TOTAL_VALUE, reg$fitted.values, reg$residuals)
head(tr.res)

# code for fitting a regression model to training data (West Roxbury)