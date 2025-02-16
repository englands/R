# use a model.matrix() to convert all 
# categorical variables in the data frame into
# a set of dummy variables. We must then turn 
# the resulting data matrix back into a data
# frame for further work.
housing.df <- read.csv("WestRoxbury.csv", header = TRUE) # load data
xtotal <- model.matrix(~ BEDROOMS + REMODEL, data = housing.df)
xtotal$BEDROOMS[1:5] # will not work because xtotal is a matrix
xtotal <- as.data.frame(xtotal)
t(t(names(xtotal))) # check the names of the dummy variables
head(xtotal)
xtotal <- xtotal[, -4] # drop one of the dummy variables
# In this case, drop REMODELRecent
