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
sale_history <- read_csv("../data/sale_history_account.csv")
######################################################################################

######################################################################################
# unify the data
sale_history <- sale_history %>% 
  mutate(punk_name = punk_gender$punk_name[punk]) %>% 
  select(-`...1`,-punk) %>% 
  relocate(punk_name)

# join the data
sale_data <- sale_history %>% 
  left_join(punk_gender, by = "punk_name") %>% 
  left_join(punk_image_RGBA, by = "punk_name")
######################################################################################
