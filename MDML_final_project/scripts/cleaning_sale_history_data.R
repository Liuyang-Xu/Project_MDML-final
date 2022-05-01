######################################################################################
# load library
library(tidyverse)
library(lubridate)
library(rvest)
library(png)
library(str2str)

library(plyr)
library(dplyr)
library(tidytext)
library(ROCR)

######################################################################################
#

#first, clean the data like dollars and data
sale_bitcoin_dollar <- c()
sale_history_sub <- c()
for(i in 1:length(sale_history)){
  sale_history_sub[[i]] <- as.data.frame(sale_history[[i]])
  sale_history_sub[[i]] <- sale_history_sub[[i]] %>% mutate(
    dollar = str_split_fixed(sale_history_sub[[i]]$V4, "??", 2))
  sale_history_sub[[i]]$dollar[,2]<- gsub('[$,(),<]',"", sale_history_sub[[i]]$dollar[,2])# change all number with symbol to numerics form
   sale_history_sub[[i]]$dollar[,2]<- gsub("M" ,"e6", sale_history_sub[[i]]$dollar[,2])
   sale_history_sub[[i]]$dollar[,2]<- gsub("B" ,"e8", sale_history_sub[[i]]$dollar[,2])
   
    
    sale_history_sub[[i]]$dollar[,2]<- as.numeric(sale_history_sub[[i]]$dollar[,2])
    #View all punk's "sold","bid","offered" which smaller than 0.01 as 1 dollar
}


#clean the date
for(i in 1:length(sale_history)){
  sale_history_sub[[i]] <- as.data.frame(sale_history_sub[[i]])
  sale_history_sub[[i]] <- sale_history_sub[[i]]%>%
    mutate(V5 = mdy(sale_history_sub[[i]]$V5))}
#clean the "Bid * " and "Sold * " and seperate the dollar and bitcoin
for(i in 1:length(sale_history)){
  sale_history_sub[[i]]$V1 <- gsub("Bid *","Bid " , sale_history_sub[[i]]$V1)
  sale_history_sub[[i]]$V1 <- gsub("Sold *","Sold " , sale_history_sub[[i]]$V1)
  sale_history_sub[[i]] <- sale_history_sub[[i]]%>%
    mutate(dollar_2 = dollar[,2])
  }




#combine 6674 dataset

#break 6674 punk pictures to a lots of transaction
result <- data.frame()
test <- list()
punk <- vector()
for(i in 1:length(sale_history_sub)){
  test[[i]] <- sale_history_sub[[i]]%>%
    mutate(index = ifelse(V1 == "Sold ", 1, 0))%>%
    mutate(index2 = cumsum(index == 1))%>%
    filter(index2 != "0" )%>%
    dplyr::group_by(index2) %>%
    dplyr::summarize(bid = sum(V1 == "Bid "),
                     bid_withdrawn = sum(V1 == "Bid Withdrawn"),
                     offer = sum(V1 == "Offered"),
                     offer_withdrawn = sum(V1 == "Offer Withdrawn"),
                     transfer = sum(V1 == "Transfer"),
                     unwrap = sum(V1 == "(Unwrap)"),
                     wrap = sum(V1 == "(Wrap)"),
                     largest_offer = max(dollar_2[V1 == "Offered"]), #largest offered in this transaction
                     largest_bid = max(dollar_2[V1 == "Bid "]), #largest bid in this transaction
                     sold_price = max(dollar_2[V1 == "Sold "]),#sold_price
                     sold_date = V5[V1 == "Sold "])%>%mutate(
                       punk = i
                     )
   
}

#bind all transactions to one data_frame
result <- do.call(rbind,test)
#connect each sell to their own punk
result <- result %>% select(-index2,-largest_offer)%>% 
  relocate(punk, .before = bid)

#add last sold_price corresponde to the sold price
last_sold_price <- result$sold_price[result$sold_price !=177275]
Final_result <- cbind_fill(result,last_sold_price)
write.csv(result, "result.csv")

#find the position of first  transaction and delete it
for(i in 1:length(Final_result$punk)){
  Final_result$position_last_sold[[i]] <- ifelse(Final_result$punk[[i]] + 1 != Final_result$punk[[i+1]],0,1)
}
Final_result$X2[Final_result$position_last_sold == "1"] <- "NA" 
#drop the index colmn and rename the X2
Final_result <- Final_result %>% select(-position_last_sold)%>%mutate(
  last_sold_price = X2)%>%select(-X2)
write.csv(Final_result, "Final_result.csv")
