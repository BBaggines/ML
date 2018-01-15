rm(list = ls())
library(magrittr)
library(dplyr)
library(VIM)
library(caret)
library(randomForest)
library(verification)


# normalized gini function taked from:
# https://www.kaggle.com/c/ClaimPredictionChallenge/discussion/703

normalizedGini <- function(aa, pp) {
  Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
    accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
    gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
    sum(gini.sum) / length(a)
  }
  Gini(aa,pp) / Gini(aa,aa)
}


setwd("C:\\Users\\shrad\\Desktop\\AML\\Kaggle")
file_name <- "train.csv"
train <- read.csv(file_name)
train[train == -1] <- NA 


#removing all the columns of calc as there is hardly any variation in target values
drops <- names(train)[grepl('_calc', names(train))]

################################### handling missing values ###########################################

# 79% data have atleast one NA value
(nrow(train) - sum(complete.cases(train)))/nrow(train)

# substituting with mean value for -1 in numeric data columns wit missing values
train$ps_car_12[train$ps_car_12 == -1] <- mean(train$ps_car_12[train$ps_car_12 != -1])  # number of -1 value = 1
train$ps_car_11[train$ps_car_11 == -1] <- mean(train$ps_car_11[train$ps_car_11 != -1])  # number of -1 value = 5
train$ps_car_14[train$ps_car_14 == -1] <- mean(train$ps_car_14[train$ps_car_14 != -1])  # number of -1 value = 42620
train$ps_reg_03[train$ps_reg_03 == -1] <- mean(train$ps_reg_03[train$ps_reg_03 != -1])  # number of -1 value = 107772

# Adding ps_car_05_cat to the list as 25% values in the column are NA and teh distribution of 
# target values for the remaining 75% values of ps_car_05_cat is equally distributed
drops <- c(drops, "ps_car_05_cat")

# ps_car_03_cat : has maximum number of NAs.
# However, the prediction follows a linearly increasing graph.
# hence, we keep the column as it is with -1 values for NA in the column

# ps_ind_04_cat : Has 40% of rows with NA values has target = 1. However, only 3% of both ps_ind_04_cat =0 
# and 1 is target = 1. Hence, right now keeping the columns as it is with an exra factor for -1.

# ps_car_01_cat : similiar case as above with 38% for NA factor as target = 1 and almost 5% for 
# all the other values

# ps_car_07_cat : similiar to ps_car_03_cat logic. Here, it follows a linearly decreasing graph.

# ps_ind_02_cat : similar case as above with 17 % for target = 1. Dropping the column.
drops <- c(drops, "ps_ind_02_cat")

# ps_car_09_cat : in this case replacing all the values with -1 as 1 as the percentage of target = 1 is almost 9%
# ps_car_09_cat = 1 has the most number of target = 1 after NAs which is 5%
train$ps_car_09_cat[train$ps_car_09_cat == -1] <- 1

# ps_ind_05_cat : with similiar logic to above, replacing the values with 2.
train$ps_ind_05_cat[train$ps_ind_05_cat == -1] <- 2

# ps_car_02_cat : similiar logic as above
train$ps_car_02_cat[train$ps_car_02_cat == -1] <- 0


dtrain <- train[,!(names(train) %in% drops)]

sort(colSums(dtrain == -1))

######################## one hot encoding for factor variables ##############################

# collect the categorical variable names
cat_vars <- names(dtrain)[grepl('_cat$', names(dtrain))]

# removing ps_car_11_cat as the number of levels is huge and it might not be a categorical variable
cat_vars <- cat_vars[1:11]

# convert categorical features to factors
dtrain <- dtrain %>%
  mutate_at(.vars = cat_vars, .funs = as.factor)

str(dtrain)

# One hot encode the factor variables
dtrain <- as.data.frame(model.matrix(~ . - 1, data = dtrain))
str(dtrain)

#make target variable into factors
dtrain$target <- as.factor(dtrain$target)

# set seed for reproducibility
set.seed(123)

# making a train index
train_index <- sample(c(TRUE, FALSE), replace = TRUE, size = nrow(dtrain), prob = c(0.1,0.9))

names(dtrain)[5] <- "ps_ind_04_cat_1"

# split the data according to the train index
training <- as.data.frame(dtrain[train_index, ])
testing <- as.data.frame(dtrain[!train_index, ])

########################### Random Forest ##########################################################

library(randomForest)
str(training)

drops <- c("id")
training <- training[,!(names(training) %in% drops)]

fit <- randomForest(target ~ ., training,ntree=500)


predicted <- predict(fit,testing, type = "prob")
normalizedGini(as.integer(testing$target), predicted[,2])  # gini coefficient on train data is 0.2157639

