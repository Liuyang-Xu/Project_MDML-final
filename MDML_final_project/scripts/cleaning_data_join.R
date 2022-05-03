######################################################################################
# load library
library(tidyverse)
library(lubridate)
library(rvest)
library(haven)
######################################################################################

######################################################################################
# load data
load("../data/punk_gender.RData")
load("../data/punk_image_RGBA.RData")
punk_attributes <- read_csv("../data/punk_attributes.csv")
Sale_History_Data_Cleaning_Final_result <- read_csv("../data/Sale_History_Data_Cleaning_Final_result.csv")
######################################################################################

######################################################################################
#replace NAs in data_sale by knowing the generating cost of each NFT first transaction date
Sale_History_Data_Cleaning_Final_result_test <- Sale_History_Data_Cleaning_Final_result %>%
  group_by(punk) %>%
  mutate(year_month = format(as.Date(sold_date),"%Y-%m")) %>%  ## extract the year
  ungroup()

#replacing NAs 
Sale_History_Data_Cleaning_Final_result_test <- Sale_History_Data_Cleaning_Final_result_test%>%
  mutate(last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-01", 23.01),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-02", 23.58),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-03", 22.42),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-04", 22.54),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-05", 38.21),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-06", 27.22),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-07", 22.31),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-08", 36.20),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-09", 26.23),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-10", 13.69),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-11", 16.35),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2017-12", 24.71),
         
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-01", 29.40),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-02", 20.19),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-03", 11.48),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-04", 16.26),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-05", 26.02),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-06", 34.21),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-07", 11.02),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-08", 14.42),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-09", 16.56),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-10", 14.90),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-11", 22.65),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2018-12", 13.84),
         
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-01", 14.21),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-02", 18.83),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-03", 11.48),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-04", 16.26),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-05", 26.02),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-06", 34.21),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-07", 11.02),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-08", 14.42),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-09", 16.56),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-10", 14.90),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-11", 22.65),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2019-12", 13.84),
         
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-01", 8.97),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-02", 10.00),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-03", 13.07),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-04", 23.83),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-05", 28.39),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-06", 52.29),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-07", 78.90),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-08", 236.12),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-09", 119.86),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-10", 40.82),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-11", 78.98),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2020-12", 99.48),
         
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-01", 128.79),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-02", 117.85),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-03", 210.89),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-04", 56.78),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-05", 32.28),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-06", 32.00),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-07", 44.29),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-08", 136.21),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-09", 107.08),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-10", 157.06),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-11", 142.42),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2021-12", 97.14),
         
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2022-01", 122.79),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2022-02", 74.18),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2022-03", 55.28),
         last_sold_price = replace(last_sold_price, is.na(last_sold_price) & year_month == "2022-04", (68.58+62.41+55.07+68.72+62.79)/5))
######################################################################################

######################################################################################
# unify the data
punk_attributes <- punk_attributes %>% 
  mutate(punk_name = punk_gender$punk_name[id]) %>% 
  select(-attributes) %>% 
  relocate(punk_name)

Sale_History_Data_Cleaning_Final_result_test <- Sale_History_Data_Cleaning_Final_result_test %>% 
  mutate(punk_name = punk_gender$punk_name[punk]) %>% 
  select(-`...1`,-punk) %>% 
  relocate(punk_name)
######################################################################################

######################################################################################
# join the data
sale_data <- Sale_History_Data_Cleaning_Final_result_test %>% 
  left_join(punk_gender, by = "punk_name") %>% 
  left_join(punk_image_RGBA, by = "punk_name") %>% 
  left_join(punk_attributes, by = "punk_name") %>% 
  select(-largest_bid) #drop largest bid col because too many NAs
######################################################################################

######################################################################################         
# find the average increasing rate for each transaction by finding difference between
# current sold price and last sold price except for first transaction 
sale_data_1 <- subset(sale_data , sale_data$last_sold_price != "NA") #delete the fist transaction record in each punk
sale_data_1 <- subset(sale_data , sale_data$last_sold_price != 0) #delete the last_sold_price transaction contains 0
ave_increasing_ratio <- 0.001*(mean(sale_data_1$sold_price/sale_data_1$last_sold_price))#average ratio between current sale and last sale ==  3143.927

# set the index 
sale_data <- sale_data %>%
  mutate(over_ave_ratio = ifelse((sold_price/last_sold_price) >  ave_increasing_ratio,1,0))

# drop the row that column over_ave_ratio == 0
sale_data <- sale_data %>% filter(over_ave_ratio != "NA")

# drop the year_month column 
sale_data <- sale_data %>% select(-year_month)%>%
  mutate(year = year(sold_date),
         month = month(sold_date))%>%select(-sold_date,-punk_name, -id,-wrap,-unwrap)
sale_data <- na.omit((sale_data))
######################################################################################

######################################################################################
# save the data
save(sale_data, file = "../data/sale_data.RData")
######################################################################################