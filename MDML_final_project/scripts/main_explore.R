######################################################################################
# main_explore
######################################################################################

######################################################################################
# load library
library(tidyverse)
library(ROCR)
library(lubridate)
library(rvest)

# library(ranger)
library(randomForest)

library(nnet)
library(MASS)
library(class)
library(ggplot2)
######################################################################################

######################################################################################
# load data
load("../data/sale_data.RData")
######################################################################################

######################################################################################
# check the sale distribution by year
table(sale_data$year)

p <- ggplot(data = sale_data, aes(x = year, y = sold_price))+
  geom_point()
p

sale_data <- sale_data %>% 
  filter(year >= 2021) %>% 
  mutate(gender = as.factor(gender),
         year = as.factor(year),
         month = as.factor(month)) %>% 
  dplyr:: select(-over_ave_ratio, -id)
######################################################################################

######################################################################################
# split the data set

# randomize the order
sale_data <- sale_data %>% 
  slice(sample(1:n()))

# 75% train
train <- sale_data %>% 
  slice(1:(round(3*n()/4)))

# 25% validate
validate <- sale_data %>% 
  slice((round(3*n()/4) + 1):n())

######################################################################################

######################################################################################
# model selection
######################################################################################

######################################################################################
# use nnet model
# fit.nnet <- nnet(over_ave_ratio ~ ., data = train, size = 2, maxit = 1000)
fit.nnet <- nnet(sold_price ~ ., data = train, size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=1000)
validate <- validate %>% 
  mutate(predicted.probability.nnet = predict(fit.nnet, validate, type='raw'))
######################################################################################

######################################################################################
# use random forest model

# Fit a random forest model on train
fit.rf <- randomForest(sold_price ~ ., data = train, ntree=1000, 
                         importance = TRUE, na.action = na.omit)

# compute the AUC of this model on test.
validate <- validate %>% 
  mutate(predicted.probability.rf = predict(fit.rf, validate, type='response'))
######################################################################################

######################################################################################  
# use linear regression  model
# Fit a linear regression model on train
fit.lm.o <- lm(formula = sold_price ~ ., data = train)
# summary(fit.lm)
# compute the AUC of this model on test.

fit.lm <- stepAIC(fit.lm.o, trace = FALSE)

validate$predicted.probability.lm = predict(fit.lm, validate, type = 'response')
######################################################################################

######################################################################################
plot.data <- tibble( 
  Real_Sold_Price = validate$sold_price, 
  Prediction = validate$predicted.probability.nnet) %>% 
  arrange(Real_Sold_Price) %>% 
  mutate(Index = c(1:nrow(validate))) %>% 
  filter(Prediction<4000000 & Real_Sold_Price>0 & Real_Sold_Price<1500000) %>% 
  slice(1:(n()-1))
p <- ggplot(data = plot.data, aes(x = Index))+
  geom_line(aes(y = Real_Sold_Price, color = "Real_Sold_Price")) +
  geom_line(aes(y = Prediction, color = "Prediction"))
p
######################################################################################