######################################################################################
# main
######################################################################################

######################################################################################
# load library
library(tidyverse)
library(ROCR)
library(lubridate)
library(rvest)
library(ranger)
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
# 
sale_data <- sale_data %>% 
  mutate(over_ave_ratio = as.factor(over_ave_ratio),
         gender = as.factor(gender),
         year = as.factor(year),
         month = as.factor(month)) %>% 
  select(-sold_price, -id)
######################################################################################

######################################################################################
# model selection
######################################################################################

######################################################################################
Iter <- 1 # iteration time

AUC_performance <- data.frame(Iteration = c(1:Iter),
                              nnet = rep(NA, Iter),
                              rf = rep(NA, Iter),
                              logi = rep(NA, Iter),
                              lm = rep(NA, Iter))
start.time <- Sys.time() 

for (i in 1:Iter){
  ###################################################
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
  ###################################################
  
  ###################################################
  # use random forest model
  
  # Fit a random forest model on train
  # Use 1000 trees, and make sure that both respect.unordered.factors and probability are TRUE
  # other settings default values
  fit.rf <- ranger(over_ave_ratio ~ ., data = train, num.trees = 1000,
                   respect.unordered.factors = T, probability = T)
  
  # compute the AUC of this model on test.
  validate <- validate %>% 
    mutate(predicted.probability.rf = predict(fit.rf, validate, type='response')$predictions[,2])
  
  validate.pred <- prediction(validate$predicted.probability.rf, validate$over_ave_ratio)
  validate.perf <- performance(validate.pred, "auc")
  AUC_performance$rf[i] <- validate.perf@y.values[[1]]
  ###################################################
  
  ###################################################
  # use nnet model
  fit.nnet <- nnet(over_ave_ratio ~ ., data = train, size=10, MaxNWts=10000, maxit=1000)
  validate <- validate %>% 
    mutate(predicted.probability.nnet = predict(fit.nnet, validate, type='raw'))
  
  validate.pred <- prediction(validate$predicted.probability.nnet, validate$over_ave_ratio)
  validate.perf <- performance(validate.pred, "auc")
  AUC_performance$nnet[i] <- validate.perf@y.values[[1]]
  ###################################################
  
  ###################################################
  # use logistic model
  
  # standardize real-valued attributes
  
  ## mean of training set 
  last_sold_price_mean <- mean(train$last_sold_price)
  green_mean <- mean(train$green)
  red_mean <- mean(train$red)
  blue_mean <- mean(train$blue)
  alpha_mean <- mean(train$alpha)
  bid_mean <- mean(train$bid)
  bid_withdrawn_mean <- mean(train$bid)
  offer_mean <- mean(train$bid)
  offer_withdrawn_mean <- mean(train$offer_withdrawn)
  transfer_mean <- mean(train$transfer)
  
  
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
  
  # standarize the real-value attribute
  train <- train %>% 
    mutate(
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
  
  # construct the regression formula
  formula <- over_ave_ratio ~ bid + bid_withdrawn + offer + offer_withdrawn +
    transfer + last_sold_price + #+largest_bid + unwrap + wrap
    gender+red+green + blue + alpha +
    Bandana   +  Beanie + Choker  +  Pilot_Helmet +  Tiara +  Orange_Side + #Buck_Teeth+          
    Welding_Goggles+ Pigtails +  Pink_With_Hat + Top_Hat  +  Spots +        
    Rosy_Cheeks +  Blonde_Short  +Wild_White_Hair + Cowboy_Hat+  Wild_Blonde+      
    Straight_Hair_Blonde+ Big_Beard +  Red_Mohawk + Half_Shaved  + Blonde_Bob +      
    Vampire_Hair+Clown_Hair_Green+Straight_Hair_Dark+Straight_Hair+Silver_Chain  +      
    Dark_Hair +  Purple_Hair+Gold_Chain +  Tassle_Hat +     #Medical_Mask +
    Fedora +    + Clown_Nose  +    Cap_Forward  +   Hoodie +    #Police_Cap +
    Front_Beard_Dark + Frown +  Purple_Eye_Shadow + Handlebars +  Blue_Eye_Shadow+  
    Green_Eye_Shadow +Vape  + Front_Beard  +  Chinstrap + D_Glasses  +  
    Luxurious_Beard+ Normal_Beard+ Eye_Mask + #Mustache  + #Normal_Beard_Black  +
    Goat+  Do_rag +Shaved_Head+     Peak_Spike + #Muttonchops  +
    Pipe + VR + Cap+ Small_Shades+ Clown_Eyes_Green+
    Clown_Eyes_Blue + Headband +  Crazy_Hair   +  Knitted_Cap+      Mohawk_Dark +
    Mohawk +  Mohawk_Thin   + Frumpy_Hair   + Wild_Hair  + Messy_Hair+
    Stringy_Hair+#Classic_Shades +  
    Shadow_Beard+Regular_Shades+  #Eye_Patch+
    Horned_Rim_Glasses +  Big_Shades +  Nerd_Glasses+ Mole + #Black_Lipstick +
    Cigarette+  Earring      #Hot_Lipstick +  #Purple_Lipstick+                  
  
  fit.logi <- glm(formula , data = train, family = 'binomial')
  # summary(fit_1)
  
  # fit.logi <- stepAIC(fit.logi.o, trace = FALSE)
  
  #standarize the real-value attribute
  validate.logi <- validate %>% 
    mutate(last_sold_price = (last_sold_price - last_sold_price_mean)/last_sold_price_sd,
           red = (red - red_mean)/red_sd,
           blue = (blue - blue_mean)/blue_sd,
           green = (green - green_mean)/green_sd,
           alpha = (alpha - alpha_mean)/alpha_sd)
  
  validate <- validate %>% mutate(predicted.probability.logi =  predict(fit.logi, validate.logi, type='response'))
  
  # compute the AUC of this model on test.
  
  validate.pred <- prediction(validate$predicted.probability.logi, validate$over_ave_ratio)
  validate.perf <- performance(validate.pred, "auc")
  AUC_performance$logi[i] <- validate.perf@y.values[[1]]
  ###################################################
  
  ###################################################
  # use linear regression  model
  
  # Fit a linear regression model on train
  train.lm <- train %>% 
    mutate(over_ave_ratio = as.numeric(over_ave_ratio))
  
  validate.lm <- validate %>%
    mutate(over_ave_ratio = as.numeric(over_ave_ratio))
  
  fit.lm <- lm(formula = over_ave_ratio ~ ., data = train.lm)
  # summary(fit.lm)
  
  # fit.lm <- stepAIC(fit.lm.o, trace = FALSE)
  
  # compute the AUC of this model on test.
  validate$predicted.probability.lm = predict(fit.lm, validate.lm, type = 'response')
  validate.pred <- prediction(validate$predicted.probability.lm, validate$over_ave_ratio)
  validate.perf <- performance(validate.pred, "auc")
  AUC_performance$lm[i] <- validate.perf@y.values[[1]]
  ###################################################
}

######################################################################################
# plot the AUCs
p <- ggplot(data = AUC_performance, aes(x = Iteration)) +
  geom_line(aes(y = nnet, color = "nnet")) +
  geom_line(aes(y = rf, color = "rf")) +
  geom_line(aes(y = logi, color = "logi")) +
  geom_line(aes(y = lm, color = "lm")) +
  xlab("Iteration") + ylab("AUC")
p
######################################################################################

end.time <- Sys.time() 

end.time - start.time
