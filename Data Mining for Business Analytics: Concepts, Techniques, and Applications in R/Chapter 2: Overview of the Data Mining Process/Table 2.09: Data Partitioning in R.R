housing.df <- read.csv("WestRoxbury.csv", header = TRUE) # load data

# use set.seed() to get the same partitions when re-running the R code.
set.seed(1)

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the 
# remaining 40% serve as validation
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- housing.df[train.rows, ]
# assign row IDs that are not already in the training set, 
# into validation
valid.rows <- setdiff(rownames(housing.df), train.rows)
valid.data <- housing.df[valid.rows, ]

# alternative code for validation (works only when row names are numeric):
# collect all the columns without training row ID into
# validation set
valid.data <- housing.df[-train.rows, ] # does not work in this case

## partitioning into training (50%), validation (30%), test (20%)
# randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5)

# sample 30% of the row IDs into the validation set, drawing only
# from records not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows),
                     dim(housing.df)[1]*0.3)

# assign the following 20% row IDs serve as test
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

# create the 3 data frames by collecting all columns from
# from the appropriate rows
train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]
