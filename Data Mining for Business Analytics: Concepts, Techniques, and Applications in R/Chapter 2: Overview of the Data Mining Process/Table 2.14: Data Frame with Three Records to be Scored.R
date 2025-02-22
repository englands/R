housing.df <- read.csv("WestRoxbury.csv", header = TRUE) # load data
names(housing.df) # print a list of variables to the screen

colnames(housing.df)[1] <- c("TOTAL_VALUE") # change the first column's name

## from table 2.9, partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as 
# validation
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- housing.df[train.rows, ]
# assign row IDs that are not already in the training set, into validation
valid.rows <- setdiff(rownames(housing.df), train.rows)
valid.data <- housing.df[valid.rows, ]

## partitioning into training (50%), validation (30%), test (20%)
# not already in the training set
# use setdiff() to find records not already in training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), 
                     dim(housing.df)[1]*0.3)

# assign the remaining 20% row IDs serve as test
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]

# code from Table 2.12
pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL_VALUE, pred, residuals = 
                       valid.data$TOTAL_VALUE - pred)
head(vl.res)

# beginning of Table 2.13 code
library(forecast)
# compute accuracy on training set
accuracy(reg$fitted.values, train.data$TOTAL_VALUE)

# compute accuracy on prediction set
pred <- predict(reg, newdata = valid.data)
accuracy(pred, valid.data$TOTAL_VALUE)

# beginning of Table 2.14 code
# new.data can be read from a csv file, or defined directly in R
# new.data not showing in code

pred <- predict(reg, newdata = new.data) # code not working













# beginning of code for Table 2.13
