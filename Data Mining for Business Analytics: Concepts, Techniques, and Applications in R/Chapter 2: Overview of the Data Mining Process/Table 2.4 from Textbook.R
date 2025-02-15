# random sample of 5 observations
s <- sample(row.names(housing.df), 5)

# oversample houses with over 10 rooms
s <- sample(row.names(housing.df), 5, prob = ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s, ]