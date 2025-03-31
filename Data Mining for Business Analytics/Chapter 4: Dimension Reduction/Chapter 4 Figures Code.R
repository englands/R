#### Chapter 4 Coding Exercises ####

#### Table 4.3: Summary Statistics for the Boston Housing Data ####

boston.housing.df <- read.csv("BostonHousing.csv", header = TRUE)
head(boston.housing.df, 9)
summary(boston.housing.df)

# compute mean, standard dev., min, max, median, length, and missing values 
# of CRIM
mean(boston.housing.df$CRIM) # mean
sd(boston.housing.df$CRIM) # standard deviation
min(boston.housing.df$CRIM) # minimum
max(boston.housing.df$CRIM) # maximum
median(boston.housing.df$CRIM) # median
length(boston.housing.df$CRIM)

# find the number of missing values of variable CRIM
sum(is.na(boston.housing.df$CRIM))

# compute mean, standard dev., min, max, median, length, and missing values for 
# all variables.
data.frame(mean = sapply(boston.housing.df, mean),  
             sd = sapply(boston.housing.df, sd), 
             min = sapply(boston.housing.df, min), 
             max = sapply(boston.housing.df, max),  
             median = sapply(boston.housing.df, median),  
             length = sapply(boston.housing.df, length),  
             miss.val = sapply(boston.housing.df, function(x)
               sum(length(which(is.na(x))))) )

# needed to remove all '+' to have the code run


#### Table 4.4: Correlation Table for Boston Housing Data ####

round(cor(boston.housing.df),2)


#### Table 4.5: Number of Neighborhoods that Bound the Charles River vs. Not ####

boston.housing.df <- read.csv("BostonHousing.csv")
table(boston.housing.df$CHAS)


#### Table 4.6: Average MEDV by CHAS and RM ####

# create bins of size 1
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))

# compute the average of MEDV by (binned) RM and CHAS
# in aggregate() use the argument by= to define the list of aggregating variables
# and FUN= as an aggregating function.
aggregate(boston.housing.df$MEDV,
          by=list(RM=boston.housing.df$RM.bin,
                  CHAS=boston.housing.df$CHAS),
          FUN=mean)


#### Table 4.7: Pivot Tables in R ####

# use install.packages("reshape") the first time the package is used
library(reshape)
boston.housing.df <- read.csv("BostonHousing.csv")
# create bins of size 1
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))

# use melt() to stack a set of columns into a single column of data.
# stack MEDV values for each combination of (binned) RM and CHAS
mlt <- melt(boston.housing.df, id=c("RM.bin", "CHAS"),
            measure=c("MEDV"))
head(mlt, 5)

# use cast() to reshape data and generate pivot table
cast(mlt, RM.bin ~ CHAS, subset=variable=="MEDV",
     margins=c("grand_row", "grand_col"), mean)


#### Figure 4.1: Distribution of CAT.MEDV ####

library(ggmap)
boston.housing.df <- read.csv("BostonHousing.csv")

tbl <- table(boston.housing.df$CAT..MEDV, boston.housing.df$ZN)
prop.tbl <- prop.table(tbl, margin=2)
barplot(prop.tbl, xlab="ZN", ylab="", yaxt="n", 
        main="Distribution of CAT.MEDV by ZN")
axis(2, at=(seq(0,1, 0.2)), paste(seq(0,100,20), "%"))


#### Table 4.10: PCA on the Two Variables Calories and Rating ####

cereals.df <- read.csv("Cereals.csv")
# compute PCs on two dimensions
pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating))
summary(pcs)
pcs$rot
scores <- pcs$x
head(scores, 5)


#### Table 4.11: PCA Output Using All 13 Numerical Variables in Breakfast Cereals Dataset ####

pcs <- prcomp(na.omit(cereals.df[,-c(1:3)]))
summary(pcs)


#### Table 4.12: PCA Output Using All Normalized 13 Numerical Variables in the Breakfast Cereals Dataset ####

pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)