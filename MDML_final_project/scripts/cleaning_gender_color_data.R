######################################################################################
# load library
library(tidyverse)
library(tidytext)
library(rvest)
######################################################################################

######################################################################################
# load the data
load("../data/sales_scraped_information.RData")
######################################################################################

######################################################################################
# clean the gender data
punk_gender <- as_tibble(unlist(punk_gender)) %>% 
  unnest_tokens(word, value) %>% 
  slice(seq(from=4, to=n(), by=5)) %>% 
  mutate(punk_name = Punk_name) %>% 
  dplyr::rename("gender" = "word")
######################################################################################

######################################################################################
# clean the image data
punk_image_RGBA <- as_tibble(matrix(unlist(punk_image), nrow = punk_count*4, byrow=TRUE)) %>% 
  rowSums() %>% 
  matrix(nrow = punk_count, ncol = 4, byrow = TRUE) %>% as_tibble() %>% 
  mutate(punk_name = Punk_name) %>% 
  dplyr::rename("red" = "V1", "green" = "V2", "blue" = "V3", "alpha" = "V4")
######################################################################################

######################################################################################
# save the data
save(punk_gender, file = "../data/punk_gender.RData")
save(punk_image_RGBA, file = "../data/punk_image_RGBA.RData")
######################################################################################
