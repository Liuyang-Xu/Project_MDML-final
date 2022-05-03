######################################################################################
# main
######################################################################################

######################################################################################
# load library
library(tidyverse)
library(ROCR)
library(lubridate)
library(rvest)
library(nnet)
######################################################################################

######################################################################################
# load data
load("../data/sale_data.RData")
######################################################################################

######################################################################################
# standardize the data
sale_data <- sale_data %>% 
  mutate(over_ave_ratio = as.factor(over_ave_ratio),
         gender = as.factor(gender),
         year = as.factor(year),
         month = as.factor(month)) %>% 
  select(-sold_price)

# standardize real-valued attributes
# record mean
## last sold price
last_sold_price_mean <- mean(train$last_sold_price)

## RGBA color
green_mean <- mean(train$green)
red_mean <- mean(train$red)
blue_mean <- mean(train$blue)
alpha_mean <- mean(train$alpha)

## sale information
bid_mean <- mean(train$bid)
bid_withdrawn_mean <- mean(train$bid)
offer_mean <- mean(train$bid)
offer_withdrawn_mean <- mean(train$offer_withdrawn)
transfer_mean <- mean(train$transfer)

# record sd
sold_price_sd <- sd(train$sold_price)
last_sold_price_sd <- sd(train$last_sold_price)

green_sd <- sd(train$green)
red_sd <- sd(train$red)
blue_sd <- sd(train$blue)
alpha_sd <- sd(train$alpha)

bid_sd <- sd(train$bid)
bid_withdrawn_sd <- sd(train$bid)
offer_sd <- sd(train$bid)
offer_withdrawn_sd <- sd(train$offer_withdrawn)
transfer_sd <- sd(train$transfer)

# standardize the real-value attribute
train <- train %>% 
  mutate(sold_price = (sold_price - sold_price_mean)/sold_price_sd,
         last_sold_price = (last_sold_price - last_sold_price_mean)/last_sold_price_sd,
         
         red = (red - red_mean)/red_sd,
         blue = (blue - blue_mean)/blue_sd,
         green = (green - green_mean)/green_sd,
         alpha = (alpha - alpha_mean)/alpha_sd,
         
         bid = (bid - bid_mean)/bid_sd,
         bid_withdrawn = (bid_withdrawn - bid_withdrawn_mean)/bid_withdrawn_sd,
         offer = (offer - offer_mean)/offer_sd,
         offer_withdrawn = (offer_withdrawn - offer_withdrawn_mean)/offer_withdrawn_sd,
         transfer = (transfer - transfer_mean)/transfer_sd)
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
# model selection
######################################################################################

######################################################################################
# use nnet model
fit.nnet <- nnet(over_ave_ratio ~ ., data = train, size = 2)
validate <- validate %>% 
  mutate(predicted.probability.nnet = predict(fit.nnet, validate, type='raw'))

validate.pred <- prediction(validate$predicted.probability.nnet, validate$over_ave_ratio)
validate.perf <- performance(validate.pred, "auc")
cat('the auc score is ', validate.perf@y.values[[1]], "\n")
######################################################################################

######################################################################################
# use random forest model

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




######################################################################################
# use logistic model

#fit the logistic model 

#train a logistic stops in sqf from 2008
train <- sale_data_2 %>% filter(year==2017)
# standardize real-valued attributes
###mean of training set 
sold_price_mean <- mean(train$sold_price)
last_sold_price_mean <- mean(train$last_sold_price)
#largest_bid_mean <- mean(train$largest_bid)
green_mean <- mean(train$green)
red_mean <- mean(train$red)
blue_mean <- mean(train$blue)
alpha_mean <- mean(train$alpha)
bid_mean <- mean(train$bid)
bid_withdrawn_mean <- mean(train$bid)
offer_mean <- mean(train$bid)
offer_withdrawn_mean <- mean(train$offer_withdrawn)
transfer_mean <- mean(train$transfer)
#unwrap_mean <- mean(train$unwrap)
#wrap_mean <- mean(train$wrap)
###sd of training set 
sold_price_sd <- sd(train$sold_price)
last_sold_price_sd <- sd(train$last_sold_price)
#largest_bid_sd <- sd(train$largest_bid)
green_sd <- sd(train$green)
red_sd <- sd(train$red)
blue_sd <- sd(train$blue)
alpha_sd <- sd(train$alpha)
bid_sd <- sd(train$bid)
bid_withdrawn_sd <- sd(train$bid)
offer_sd <- sd(train$bid)
offer_withdrawn_sd <- sd(train$offer_withdrawn)
transfer_sd <- sd(train$transfer)
#unwrap_sd <- sd(train$unwrap)
#wrap_sd <- sd(train$wrap)
#standarize the real-value attribute
train <- train %>% 
  mutate(sold_price = (sold_price - sold_price_mean)/sold_price_sd,
         last_sold_price = (last_sold_price - last_sold_price_mean)/last_sold_price_sd,
         #largest_bid    = (largest_bid  - largest_bid_mean )/largest_bid_sd,
         red = (red - red_mean)/red_sd,
         blue = (blue - blue_mean)/blue_sd,
         green = (green - green_mean)/green_sd,
         alpha = (alpha - alpha_mean)/alpha_sd,
         
         bid = (bid - bid_mean)/bid_sd,
         bid_withdrawn = (bid_withdrawn - bid_withdrawn_mean)/bid_withdrawn_sd,
         offer = (offer - offer_mean)/offer_sd,
         offer_withdrawn = (offer_withdrawn - offer_withdrawn_mean)/offer_withdrawn_sd,
         transfer = (transfer - transfer_mean)/transfer_sd,
         #unwrap = (unwrap - unwrap_mean)/unwrap_sd,
         #wrap = (wrap - wrap_mean)/wrap_sd,
  )


train <- train %>% select(-year,-month)
#construct the regression formula
formula <- over_ave_ratio ~ bid + bid_withdrawn + offer + offer_withdrawn + 
  transfer  +sold_price  + last_sold_price + #+largest_bid + unwrap + wrap
  gender+red+green + blue + alpha   +
  Bandana   +  Beanie + Choker  +  Pilot_Helmet +  Tiara +  Orange_Side + Buck_Teeth+          
  Welding_Goggles+ Pigtails +  Pink_With_Hat + Top_Hat  +  Spots +         
  Rosy_Cheeks +  Blonde_Short  +Wild_White_Hair + Cowboy_Hat+  Wild_Blonde+       
  Straight_Hair_Blonde+ Big_Beard +  Red_Mohawk + Half_Shaved  + Blonde_Bob +       
  Vampire_Hair+Clown_Hair_Green+Straight_Hair_Dark+Straight_Hair+Silver_Chain  +      
  Dark_Hair +  Purple_Hair+Gold_Chain +  Medical_Mask +Tassle_Hat +     
  Fedora +  Police_Cap  + Clown_Nose  +    Cap_Forward  +   Hoodie +    
  Front_Beard_Dark + Frown +  Purple_Eye_Shadow + Handlebars +  Blue_Eye_Shadow+   
  Green_Eye_Shadow +Vape  + Front_Beard  +  Chinstrap + D_Glasses  +  
  Luxurious_Beard+Mustache  + Normal_Beard_Black  + Normal_Beard+ Eye_Mask +
  Goat+  Do_rag +Shaved_Head+   Muttonchops  +  Peak_Spike +
  Pipe + VR + Cap+ Small_Shades+ Clown_Eyes_Green+
  Clown_Eyes_Blue + Headband +  Crazy_Hair   +  Knitted_Cap+      Mohawk_Dark +
  Mohawk +  Mohawk_Thin   + Frumpy_Hair   + Wild_Hair  + Messy_Hair+
  Eye_Patch+ Stringy_Hair+Classic_Shades +  Shadow_Beard+Regular_Shades+
  Horned_Rim_Glasses +  Big_Shades +  Nerd_Glasses+Black_Lipstick + Mole +
  Purple_Lipstick+ Hot_Lipstick + Cigarette+  Earring                   

fit_1 <- glm(formula , data = train, family = 'binomial')
summary(fit_1)

#fit the test model
sale_data_2 <- sale_data %>% 
  mutate(gender = as.factor(gender),
         year = as.factor(year),
         month = as.factor(month))

test <- sale_data_2 %>% filter(year!=2017)
#standardize
sold_price_mean <- mean(test$sold_price)
last_sold_price_mean <- mean(test$last_sold_price)
#largest_bid_mean <- mean(test$largest_bid)
green_mean <- mean(test$green)
red_mean <- mean(test$red)
blue_mean <- mean(test$blue)
alpha_mean <- mean(test$alpha)
###sd of testing set 
sold_price_sd <- sd(test$sold_price)
last_sold_price_sd <- sd(test$last_sold_price)
#largest_bid_sd <- sd(test$largest_bid)
green_sd <- sd(test$green)
red_sd <- sd(test$red)
blue_sd <- sd(test$blue)
alpha_sd <- sd(test$alpha)
#standarize the real-value attribute
test <- test %>% 
  mutate(sold_price = (sold_price - sold_price_mean)/sold_price_sd,
         last_sold_price = (last_sold_price - last_sold_price_mean)/last_sold_price_sd,
         #largest_bid    = (largest_bid  - largest_bid_mean )/largest_bid_sd,
         red = (red - red_mean)/red_sd,
         blue = (blue - blue_mean)/blue_sd,
         green = (green - green_mean)/green_sd,
         alpha = (alpha - alpha_mean)/alpha_sd)

test <- test %>% mutate(predicted.probability =  
                          predict(fit_1, test, type='response'))

######################################################################################  

######################################################################################  
#fit the K-NN model  


######################################################################################
