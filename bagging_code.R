# The following code goes through the entire process, from
# data cleaning to creating predictions, of our submission to
# the following Kaggle competition: kaggle.com/c/stats101c-lec4-final-competition

# Created by Darren Tsang and Yonatan Khalil



### Load in necessary libraries
library(tidyverse) # for cleaning data
library(randomForest) # for creating our model


### find_pred() function
find_pred <- function(df, check, correlation){
  # include all initially
  include <- rep(T, length(check))
  for (i in seq(1, length(check))){
    cur_col = check[i]
    if(include[i]){
      # check which columns have higher correlation than cur_col
      a <- (abs(unname(df[, cur_col])) > correlation)
      # check which columns in a also are in check
      in_both <- intersect(check, names(df[a, cur_col]))
      if(!is.null(in_both)){
        # get check's position that contains each element of in_both
        pos <- match(in_both, check)
        for (j in pos){
          if(i < j){
            # set to false (not including)
            include[j] = F
          }
        }
      }
    }
  }
  
  return(check[include])
}


### Load in datasets and look at how many observations in each

training <- read.csv("stats101c-lec4-final-competition/training.csv")
dim(training)

testing <- read.csv("stats101c-lec4-final-competition/test.csv")
dim(testing)

### Data Cleaning

# remove id variable, as they are assigned arbitrarily
new_training <- training[, -1]
new_testing <- testing[, -1]


# separate the Date column into the month, day, year, hour, minute
new_training <- separate(new_training, 1, c("month", "day", "year", "hour", "minute"))
new_testing <- separate(new_testing, 1, c("month", "day", "year", "hour", "minute"))


# converts the month, day, year, hour, minute to numeric
new_training$month <- as.numeric(new_training$month)
new_training$day <- as.numeric(new_training$day)
new_training$year <- as.numeric(new_training$year)
new_training$hour <- as.numeric(new_training$hour)
new_training$minute <- as.numeric(new_training$minute)

# same as above, but this time for new_testing
new_testing$month <- as.numeric(new_testing$month)
new_testing$day <- as.numeric(new_testing$day)
new_testing$year <- as.numeric(new_testing$year)
new_testing$hour <- as.numeric(new_testing$hour)
new_testing$minute <- as.numeric(new_testing$minute)



# calculate min_of_day
new_training <- cbind("min_of_day" = new_training$hour*60 + new_training$minute,
                      new_training)
new_testing <- cbind("min_of_day" = new_testing$hour*60 + new_testing$minute, 
                     new_testing)


# remove hour and minute columns because we created min_of_day that incorporates both of them
# remove year column because they are all 2020
new_training <- new_training[, !(names(new_training) %in% c("hour", "minute", "year"))]
new_testing <- new_testing[, !(names(new_testing) %in% c("hour", "minute", "year"))]



# find columns that are all 0 in training dataset
all_zero <- (which(colSums(new_training) == 0))

# from both training and testing datset, remove columns that are all 0
new_training <- new_training[, -all_zero]
new_testing <- new_testing[, -all_zero]


dim(new_training)
dim(new_testing)

### Choosing which predictors to use by removing multicollinearity


potential_predictors <- names(sort(abs(cor(new_training)[, "growth_2_6"]), decreasing = T)[-1])
length(potential_predictors)


final_predictors <- find_pred(cor(new_training), potential_predictors, .7)
length(final_predictors)
print(final_predictors)


### Take only the predictors we selected above

reduced_new_training <- new_training[, c(final_predictors, 'growth_2_6')]

### Training our model

set.seed(999) # change seed to 12 for random forest
bagged.tree <- randomForest(growth_2_6 ~ ., 
                            data = reduced_new_training, 
                            mtry = dim(reduced_new_training)[2] - 1, # change to 165 for random forest
                            ntree = 500,
                            importance = T)
print(bagged.tree)



bagged.tree.predictions <- predict(bagged.tree, reduced_new_training)
bagged.tree.rmse <- sqrt(mean((bagged.tree.predictions - reduced_new_training$growth_2_6)^2))
bagged.tree.rmse

varImpPlot(bagged.tree, n.var = 10, type = 1)


### Making predictions on test data

# create dataframe with two columns, id and predictions
submit <- data.frame(read.csv("stats101c-lec4-final-competition/test.csv")$id, 
                     predict(bagged.tree, new_testing))

# name columns to comply with rules
colnames(submit) <- c("id", "growth_2_6")

# write to csv file, to be submitted to Kaggle
write.csv(submit, "Final_submit_this.csv", row.names=FALSE)
