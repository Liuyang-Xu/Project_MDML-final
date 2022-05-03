######################################################################################
# main
######################################################################################

######################################################################################
# load library
library(tidyverse)
library(lubridate)
library(rvest)
######################################################################################

######################################################################################
# load data
load("../data/punk_gender.RData")
load("../data/punk_image_RGBA.RData")
punk_attributes <- read_csv("../data/punk_attributes.csv")
sale_history <- read_csv("../data/Sale_History_Data_Cleaning_Final_result.csv")
######################################################################################

######################################################################################
# unify the data
punk_attributes <- punk_attributes %>% 
  mutate(punk_name = punk_gender$punk_name[id]) %>% 
  select(-attributes) %>% 
  relocate(punk_name)

sale_history <- sale_history %>% 
  mutate(punk_name = punk_gender$punk_name[punk]) %>% 
  select(-`...1`,-punk) %>% 
  relocate(punk_name)

# join the data
sale_data <- sale_history %>% 
  left_join(punk_gender, by = "punk_name") %>% 
  left_join(punk_image_RGBA, by = "punk_name") %>% 
  left_join(punk_attributes, by = "punk_name")
######################################################################################

######################################################################################
# split the data set

# randomize the order
sale_data <- sale_data %>% 
  slice(sample(1:n()))

# 50% train
train <- sale_data %>% 
  slice(1:round(n()/2))

# 25% validate
validate <- sale_data %>% 
  slice((round(n()/2) + 1):(round(3*n()/4)))

# 25% test
test <- sale_data %>% 
  slice((round(3*n()/4) + 1):n())
######################################################################################

######################################################################################

# use nnet model
fit.nnet <- nnetnnet(..., data = train, size = 2)
test <- test %>% 
  mutate(predicted.probability.nnet = predict(fit.nnet, test, type='raw'))

test.pred - prediction(test$predicted.probability.nnet, test$outcome)
test.perf - performance(test.pred, auc)
cat('the auc score is ', test.perf@y.values[[1]], n)


# use ... model

# predicting outcome as a function of cuisine, borough, month, weekday, and the four historical features 
formula <- "outcome ~ cuisine + borough + month + weekday + 
                      num_previous_low_inspections + num_previous_med_inspections + 
                      num_previous_high_inspections + num_previous_closings"

# Fit a random forest model on train
# Use 1000 trees, and make sure that both respect.unordered.factors and probability are TRUE
# other settings default values
fit.rf <- ranger(formula, data = train, num.trees = 1000,
                 respect.unordered.factors = T, probability = T)

# compute the AUC of this model on test.
test <- as_tibble(test) %>% 
  mutate(predicted.probability.rf = predict(fit.rf, test, type='response')$predictions[,2])

test.pred <- prediction(test$predicted.probability.rf, test$outcome)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', test.perf@y.values[[1]], "\n")
######################################################################################