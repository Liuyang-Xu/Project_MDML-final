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

library(ggpubr)
######################################################################################

######################################################################################
# load data
load("../data/sale_data.RData")
######################################################################################

######################################################################################
# check the sale distribution by year
table(sale_data$year)

sale_data <- sale_data %>% 
  filter(sold_price < 1500000 & sold_price >0 )
p <- ggplot(data = sale_data, aes(x = year, y = sold_price))+
  geom_jitter(width = 0.2, height = 0.01,
              size = 0.6, col = "gray28") +
  geom_point(shape = 16, size = 5, col = "brown4", alpha = 0.1, stroke = 0.1) +
  ylab("sold price")
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
  Prediction.nnet = validate$predicted.probability.nnet,
  Prediction.rf = validate$predicted.probability.rf,
  Prediction.lm = validate$predicted.probability.lm) %>% 
  arrange(Real_Sold_Price) %>% 
  mutate(Index = c(1:nrow(validate))) %>% 
  filter(Prediction.nnet <4000000 & 
           Prediction.rf <4000000 &
           Prediction.lm <4000000 &
           Real_Sold_Price>0 & Real_Sold_Price<1500000)

p1 <- ggplot(data = plot.data, aes(x = Index))+
  geom_line(aes(y = Prediction.nnet, color = "Prediction by nnet"), alpha = 0.5) + 
  geom_line(aes(y = Real_Sold_Price, color = "Real Sold Price")) +
  scale_colour_manual("", 
                    breaks = c("Prediction by nnet", "Real Sold Price"),
                    values = c("brown4", "blue"))
p2 <- ggplot(data = plot.data, aes(x = Index))+
  geom_line(aes(y = Prediction.rf, color = "Prediction by rf"), alpha = 0.5) + 
  geom_line(aes(y = Real_Sold_Price, color = "Real Sold Price")) +
  scale_colour_manual("", 
                      breaks = c("Prediction by rf", "Real Sold Price"),
                      values = c("brown4", "blue"))
p3 <- ggplot(data = plot.data, aes(x = Index))+
  geom_line(aes(y = Prediction.lm, color = "Prediction by lm"), alpha = 0.5) + 
  geom_line(aes(y = Real_Sold_Price, color = "Real Sold Price")) +
  scale_colour_manual("", 
                      breaks = c("Prediction by lm", "Real Sold Price"),
                      values = c("brown4", "blue"))
ggarrange(p1,p2,p3,ncol = 1, nrow = 3, labels = c("nnet", "rf", "lm"))
######################################################################################

######################################################################################
plot.data <- tibble(
  Real_Sold_Price = validate$sold_price, 
  Prediction.nnet = validate$predicted.probability.nnet - validate$sold_price,
  Prediction.rf = validate$predicted.probability.rf - validate$sold_price,
  Prediction.lm = validate$predicted.probability.lm - validate$sold_price,
  Prediction.nnet2 = Prediction.nnet^2,
  Prediction.rf2 = Prediction.rf^2,
  Prediction.lm2 = Prediction.lm^2) %>% 
  arrange(Real_Sold_Price) %>% 
  mutate(Index = c(1:nrow(validate))) %>% 
  filter(Real_Sold_Price>0 & Real_Sold_Price<1500000,
         Prediction.nnet > -500000 & Prediction.nnet < 1000000)

p1 <- ggplot(data = plot.data, aes(x = Index, y = Prediction.nnet))+
  geom_point(col = "burlywood", alpha = 0.1)+
  ylab("Residuals")
p2 <- ggplot(data = plot.data, aes(x = Index, y = Prediction.rf))+
  geom_point(col = "skyblue4", alpha = 0.1)+
  ylab("Residuals")
p3 <- ggplot(data = plot.data, aes(x = Index, y = Prediction.lm))+
  geom_point(col = "chartreuse4", alpha = 0.1)+
  ylab("Residuals")
ggarrange(p1,p2,p3,ncol = 1, nrow = 3, labels = c("nnet", "rf", "lm"))
######################################################################################

rss <- tibble(
  Real_Sold_Price = validate$sold_price, 
  Prediction.nnet = validate$predicted.probability.nnet - validate$sold_price,
  Prediction.rf = validate$predicted.probability.rf - validate$sold_price,
  Prediction.lm = validate$predicted.probability.lm - validate$sold_price,
  Prediction.nnet2 = Prediction.nnet^2,
  Prediction.rf2 = Prediction.rf^2,
  Prediction.lm2 = Prediction.lm^2) %>% 
  summarise(nnet2 = sum(Prediction.nnet2)/n(),
            rf2 = sum(Prediction.rf2)/n(),
            lm2 = sum(Prediction.lm2)/n(),)

######################################################################################
# Eth
plot.data <- tibble(
  Real_Sold_Price = validate$sold_price/2,933.20, 
  Prediction.nnet = validate$predicted.probability.nnet/2,933.20,
  Prediction.rf = validate$predicted.probability.rf/2,933.20,
  Prediction.lm = validate$predicted.probability.lm/2,933.20) %>% 
  arrange(Real_Sold_Price) %>% 
  mutate(Index = c(1:nrow(validate))) %>% 

p1 <- ggplot(data = plot.data, aes(x = Index))+
  geom_line(aes(y = Prediction.nnet, color = "Prediction by nnet"), alpha = 0.5) + 
  geom_line(aes(y = Real_Sold_Price, color = "Real Sold Price")) +
  scale_colour_manual("", 
                      breaks = c("Prediction by nnet", "Real Sold Price"),
                      values = c("brown4", "blue"))
p2 <- ggplot(data = plot.data, aes(x = Index))+
  geom_line(aes(y = Prediction.rf, color = "Prediction by rf"), alpha = 0.5) + 
  geom_line(aes(y = Real_Sold_Price, color = "Real Sold Price")) +
  scale_colour_manual("", 
                      breaks = c("Prediction by rf", "Real Sold Price"),
                      values = c("brown4", "blue"))
p3 <- ggplot(data = plot.data, aes(x = Index))+
  geom_line(aes(y = Prediction.lm, color = "Prediction by lm"), alpha = 0.5) + 
  geom_line(aes(y = Real_Sold_Price, color = "Real Sold Price")) +
  scale_colour_manual("", 
                      breaks = c("Prediction by lm", "Real Sold Price"),
                      values = c("brown4", "blue"))
ggarrange(p1,p2,p3,ncol = 1, nrow = 3, labels = c("nnet", "rf", "lm"))
######################################################################################

######################################################################################
plot.data <- tibble(
  Real_Sold_Price = validate$sold_price/2,933.20, 
  Prediction.nnet = (validate$predicted.probability.nnet - validate$sold_price)/2,933.20,
  Prediction.rf = (validate$predicted.probability.rf - validate$sold_price)/2,933.20,
  Prediction.lm = (validate$predicted.probability.lm - validate$sold_price)/2,933.20,
  Prediction.nnet2 = Prediction.nnet^2,
  Prediction.rf2 = Prediction.rf^2,
  Prediction.lm2 = Prediction.lm^2) %>% 
  arrange(Real_Sold_Price) %>% 
  mutate(Index = c(1:nrow(validate)))

p1 <- ggplot(data = plot.data, aes(x = Index, y = Prediction.nnet))+
  geom_point(col = "burlywood", alpha = 0.1)+
  ylab("Residuals")
p2 <- ggplot(data = plot.data, aes(x = Index, y = Prediction.rf))+
  geom_point(col = "skyblue4", alpha = 0.1)+
  ylab("Residuals")
p3 <- ggplot(data = plot.data, aes(x = Index, y = Prediction.lm))+
  geom_point(col = "chartreuse4", alpha = 0.1)+
  ylab("Residuals")
ggarrange(p1,p2,p3,ncol = 1, nrow = 3, labels = c("nnet", "rf", "lm"))
######################################################################################

rss <- tibble(
  Real_Sold_Price = validate$sold_price/2,933.20, 
  Prediction.nnet = (validate$predicted.probability.nnet - validate$sold_price)/2,933.20,
  Prediction.rf = (validate$predicted.probability.rf - validate$sold_price)/2,933.20,
  Prediction.lm = (validate$predicted.probability.lm - validate$sold_price)/2,933.20,
  Prediction.nnet2 = Prediction.nnet^2,
  Prediction.rf2 = Prediction.rf^2,
  Prediction.lm2 = Prediction.lm^2) %>% 
  summarise(nnet2 = sum(Prediction.nnet2)/n(),
            rf2 = sum(Prediction.rf2)/n(),
            lm2 = sum(Prediction.lm2)/n(),)