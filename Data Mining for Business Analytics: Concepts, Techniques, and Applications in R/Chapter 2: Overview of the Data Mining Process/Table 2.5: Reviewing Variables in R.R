names(housing.df) # print a list of variables to the screen
t(t(names(housing.df))) # print the list in a useful column format
colnames(housing.df)[1] <- c("TOTAL_VALUE") # change the first column's name
class(housing.df$REMODEL) # REMODEL is a factor variable
class(housing.df[ ,14]) # Same.
levels(housing.df[, 14]) # It can take one of three levels
class(housing.df$BEDROOMS) # BEDROOMS is an integer variable
class(housing.df[, 1]) # Total_Value is a numeric variable
