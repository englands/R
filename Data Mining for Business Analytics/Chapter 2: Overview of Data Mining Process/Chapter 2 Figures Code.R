#### Chapter 2 Coding Exercises ####


#### Table 2.3: Working with Files in R ####

# load data
housing.df <- read.csv("WestRoxbury.csv", header = TRUE)
# find the dimension of data frame
dim(housing.df)
# show the first six rows
head(housing.df)
# show all the data in a new tab
View(housing.df)

# Practice showing different subsets of the data
# show the first 10 rows of the first column only
housing.df[1:10, 1]
# show the first 10 rows of each of the columns
housing.df[1:10, ]
# show the fifth row of the first 10 columns
housing.df[5, 1:10]
# show the fifth row of some columns
housing.df[5, c(1:2, 4, 8:10)]
# show the whole first column
housing.df[, 1]
# a different way to show the whole first column
housing.df$TOTAL.VALUE
# show the first 10 rows of the first column
housing.df$TOTAL.VALUE[1:10]
# find the length of the first column
length(housing.df$TOTAL.VALUE)
# find the mean of the first column
mean(housing.df$TOTAL.VALUE)
# find summary statistics for each column
summary(housing.df)


#### Table 2.4: Sampling in R ####

# random sample of 5 observations
s <- sample(row.names(housing.df), 5)
housing.df[s,]

# oversample houses with over 10 rooms
s <- sample(row.names(housing.df), 5, prob = ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s,]


#### Table 2.5: Reviewing Variables in R ####

# print a list of variables to the screen
names(housing.df)
# print the list in a useful column format
t(t(names(housing.df)))
# change the first column's name
colnames(housing.df)[1]<- c("TOTAL_VALUE")
# REMODEL is a factor variable
class(housing.df$REMODEL)
# Same.
class(housing.df[ , 14])
# It can take one of three levels
levels(housing.df[, 14])
# BEDROOMS is an integer variable
class(housing.df$BEDROOMS)
# Total_Value is a numeric variable
class(housing.df[, 1])


#### Table 2.6: Creating Dummy Variables in R ####

# use model.matrix() to convert all categorical variables in the data frame
# into a set of dummy variables. We must then turn the resulting data 
# matrix back into a data frame for further work.
xtotal <- model.matrix(~ 0 + BEDROOMS + REMODEL, data = housing.df)
# xtotal$BEDROOMS[1:5] # will not work because xtotal is a matrix, commented out
xtotal <- as.data.frame(xtotal)
# check the names of the dummy variables
t(t(names(xtotal)))
names(xtotal)
head(xtotal)
# drop one of the dummy variables.
# In this case, drop REMODELRecent.
xtotal <- xtotal[, -4]
xtotal <- xtotal[, !names(xtotal) %in% "REMODELRecent"]


#### Table 2.7: Missing Data ####

# To illustrate missing data procedures, we first convert a few entries for
# bedrooms to NA's. Then we impute these missing values using the median of the 
# remaining values.
rows.to.missing <- sample(row.names(housing.df), 10)
housing.df[rows.to.missing,]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)
# Now we have 10 NA's and the median of the remaining values is 3.

# replace the missing values using the median of the remaining values.
# use median() with na.rm = TRUE to ignore missing values when computing the median.
housing.df[rows.to.missing,]$BEDROOMS <- median(housing.df$BEDROOMS, na.rm = TRUE)

summary(housing.df$BEDROOMS)


#### Table 2.9: Data Partitioning in R ####

# use set.seed() to get the same partitions when re-running the R code.
set.seed(1)

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as 
# validation
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- housing.df[train.rows, ]
# assign row IDS that are not already in the training set, into validation
valid.rows <- setdiff(rownames(housing.df), train.rows)
valid.data <- housing.df[valid.rows, ]

# alternative code for validation (works only when row names are numeric):
# collect all the columns without training row ID into validation set
# valid.data <- housing.df[-train.rows, ] # does not work in this case, commented out

## partitioning into training (50%), validation (30%), test (20%)
# randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5)

# sample 30% of the row IDs into the validation set, drawing only from records
# not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows),
                     dim(housing.df)[1]*0.3)

# assign the remaining 20% row IDs serve as test
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from the appropriate rows
train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]


#### Table 2.11: Predictions (Fitted Values) for a Sample of Training Data ####

# code for fitting a regression model to training data (West Roxbury)
reg <- lm(TOTAL_VALUE ~ ., data = housing.df, subset = train.rows)
tr.res <- data.frame(train.data$TOTAL_VALUE, reg$fitted.values, reg$residuals)
head(tr.res)


#### Table 2.12: Predictions for a Sample of Validation Data ####

# code for applying the regression model to predict validation set (West Roxbury)

pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL_VALUE, pred, residuals = 
                       valid.data$TOTAL_VALUE - pred)
head(vl.res)


#### Table 2.13: Prediction Error Metrics for Training and Validation Data ####

library(forecast)
# compute accuracy on training set
accuracy(reg$fitted.values, train.data$TOTAL_VALUE)

# compute accuracy on prediction set
pred <- predict(reg, newdata = valid.data)
accuracy(pred, valid.data$TOTAL_VALUE)


#### Table 2.14: Data Frame with Three Records to be Scored ####

# new.data can be read from a csv file, or defined directly in R
# must manually add in new.data
new.data <- data.frame(
  TAX = c(100, 101, 102),
  LOT.SQFT = c(3818, 3791, 4275),
  YR.BUILT = c(1960, 1940, 1925),
  GROSS.AREA = c(2670, 2886, 3264),
  LIVING.AREA = c(1710, 1474, 1523),
  FLOORS = c(2.0, 1.5, 1.0),
  ROOMS = c(10, 6, 6),
  BEDROOMS = c(4, 3, 2),
  FULL.BATH = c(1, 1, 1),
  HALF.BATH = c(1, 1, 0),
  KITCHEN = c(1, 1, 1),
  FIREPLACE = c(1, 1, 0),
  REMODEL = factor(c("None", "None", "Recent"), 
                   levels = c("None", "Recent"))
)
pred <- predict(reg, newdata = new.data)
pred
