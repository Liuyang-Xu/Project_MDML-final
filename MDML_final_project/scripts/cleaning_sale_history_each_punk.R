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

######################################################################################
# load the data
load("../data/sales_scraped_information.RData")
######################################################################################

######################################################################################
#find the offer number, sold numer, bid number, transform number, and offer withdrawn 
#number for each sales 
occurance_list <- c()
for(i in 1:length(sale_history)){
  occurance_list[[i]] <- as.data.frame(table(sale_history[[i]]$V1))
  occurance_list[[i]] <- spread(occurance_list[[i]],key = Var1,value = Freq)
}
#bind 6674 sales history
sale_history_account <- as.data.frame(occurance_list[0])
for(i in 1:length(occurance_list)){
  sale_history_account <- rbind.fill(sale_history_account, occurance_list[[i]])
  #test <- test %>% full_join(occurance_list[i], 'copy' = TRUE)
}
#bind 6674 sale history with it's own sales
sale_history_account_test <- sale_history_account %>% mutate(
  punk_attributes = row_number(Claimed)) 
  
sale_history_account_test <- sale_history_account_test%>%
  relocate(punk_attributes)
#why there is the Bid* and Sold* in the column? for example(View(sale_history[[705]])),
#and what dose it means on Rapped and Unrapped? for example(View(sale_history[[20]]))

###############################################################################

#First, we seperate the price to bitcoin and dollars in different column


sale_bitcoin_dollar <- c()
sale_history_sub <- c()
for(i in 1:length(sale_history)){
  sale_history_sub[[i]] <- as.data.frame(sale_history[[i]])
  sale_history_sub[[i]] <- sale_history_sub[[i]] %>% mutate(
    dollar = str_split_fixed(sale_history_sub[[i]]$V4, "??", 2))
  
  sale_history_sub[[i]]$dollar[,2]<- gsub('[M]',"00000", sale_history_sub[[i]]$dollar[,2])# covert "M" to "00000"
  sale_history_sub[[i]]$dollar[,2]<- gsub('[B]',"00000000", sale_history_sub[[i]]$dollar[,2])# covert "B" to "000000000"
  sale_history_sub[[i]]$dollar[,2]<- as.numeric(gsub('[$,().<Y]',"", sale_history_sub[[i]]$dollar[,2]))# change all number with symbol to numerics form
                                                                                                      #View all punk's "sold","bid","offered" which smaller than 0.01 as 1 dollar
    #sale_history[[i]]$dollar[,2]<- as.numeric(gsub('M',"00000", sale_history[[i]]$dollar[,2]))
    
     }
#Find the Largest Sold, smallest Sold, Average Sold, largest offer, smallest offer, average offer,
#Largest Bid, smallest Bid, and Average Bid in each punk attribute.
claim <- c()

largest_sold <- c()
smallest_sold <- c()
average_sold <- c()

largest_offer <- c()
smallest_offer <- c()
average_offer <- c()

largest_bid <- c()
smallest_bid <- c()
average_bid <- c()

for(i in 1:length(sale_history_sub)){

  largest_sold[[i]] <- max(as.numeric(subset(sale_history_sub[[i]]$dollar[,2],
                                             sale_history_sub[[i]]$V1 == "Sold " | sale_history_sub[[i]]$V1 == "Sold * ")))
  smallest_sold[[i]] <- min(as.numeric(subset(sale_history_sub[[i]]$dollar[,2],
                                              sale_history_sub[[i]]$V1 == "Sold " | sale_history_sub[[i]]$V1 == "Sold * ")))
  average_sold[[i]] <- mean(as.numeric(subset(sale_history_sub[[i]]$dollar[,2],
                                              sale_history_sub[[i]]$V1 == "Sold " | sale_history_sub[[i]]$V1 == "Sold * ")))
  
  largest_offer[[i]] <- max(as.numeric(subset(sale_history_sub[[i]]$dollar[,2],
                                              sale_history_sub[[i]]$V1 == "Offered" )))
  smallest_offer[[i]] <- min(as.numeric(subset(sale_history_sub[[i]]$dollar[,2],
                                               sale_history_sub[[i]]$V1 == "Offered" )))
  average_offer[[i]] <- mean(as.numeric(subset(sale_history_sub[[i]]$dollar[,2],sale_history_sub[[i]]$V1 == "Offered")), na.rm=TRUE)#Note: there is a really strange offered in sale_history[[291]], I think it is type-error or someone did it intentional. I think we can
                                                                                                                        #ignore this unresonable offer, or erase all of the row?? 231 NA
                                                                                                                        # sale_history[[335]]
  
  largest_bid[[i]] <- max(as.numeric(subset(sale_history_sub[[i]]$dollar[,2],sale_history_sub[[i]]$V1 == "Bid " | sale_history_sub[[i]]$V1 == "Bid * ")))
  smallest_bid[[i]] <- min(as.numeric(subset(sale_history_sub[[i]]$dollar[,2],
                                             sale_history_sub[[i]]$V1 == "Bid " | sale_history_sub[[i]]$V1 == "Bid * ")))
  average_bid[[i]] <- mean(as.numeric(subset(sale_history_sub[[i]]$dollar[,2],sale_history_sub[[i]]$V1 == "Bid " | sale_history_sub[[i]]$V1 == "Sold * ")), na.rm=TRUE) #some of the punk pictures do not have bid, there is some 983 NA
  
}

#Add column to the sale_history_account_test dataframe
sale_history_account_test$largest_sold <- largest_sold
sale_history_account_test$smallest_sold <- smallest_sold
sale_history_account_test$average_sold <-  average_sold
sale_history_account_test$largest_offer <- largest_offer
sale_history_account_test$smallest_offer <- smallest_offer
sale_history_account_test$average_offer <- average_offer
sale_history_account_test$largest_bid <- largest_bid
sale_history_account_test$smallest_bid <- smallest_bid
sale_history_account_test$average_bid <- average_bid

################################################################################

#how long dose it take to made the first sold, second sold, and Last sold

#data of claimed 
#test
for(i in 1:length(sale_history)){
  df1[i] <- mdy(subset(sale_history_sub[[i]]$V5,sale_history_sub[[i]]$V1 == "Claimed"))#claimed date
  df2[i] <- mdy(unique(subset(sale_history_sub[[i]]$V5,
                       sale_history_sub[[i]]$dollar[,2] == min(
                         as.numeric(subset(sale_history_sub[[i]]$dollar[,2],
                                           sale_history_sub[[i]]$V1 == "Sold " | sale_history_sub[[i]]$V1 == "Sold * "))))))#min sold date
 df3[i] <- mdy(unique(subset(sale_history_sub[[i]]$V5,
                         sale_history_sub[[i]]$dollar[,2] == max(
                            as.numeric(subset(sale_history_sub[[i]]$dollar[,2],
                                          sale_history_sub[[i]]$V1 == "Sold " | sale_history_sub[[i]]$V1 == "Sold * "))))))#max sold date
  #calculate the difference 
  Time_reached_min_cold[i] <- difftime(df2[i],df1[i])
  time_reached_max_cold[i] <-  difftime(df3[i],df1[i])
  
}
#add to our sale_history_account_test data set
sale_history_account_test$claimed_date <- df1
sale_history_account_test$min_sold_date <- df1
sale_history_account_test$max_sold_date <- df1
sale_history_account_test$Time_reached_min_cold <- Time_reached_min_cold
sale_history_account_test$Time_reached_max_cold <- time_reached_max_cold

#change the time_reached_min_cold and time_reached_max_cold to hour units.
sale_history_account_test_1 <- sale_history_account_test %>% mutate(
  Time_reached_min_cold = Time_reached_min_cold/(60*60),
  Time_reached_max_cold = Time_reached_max_cold/(60*60)) %>% mutate(
    Time_reached_min_cold = gsub("[secs]","",Time_reached_min_cold),
    Time_reached_max_cold = gsub("[secs]","",Time_reached_max_cold)
     )

#export the csv file

#convert all inf tO NA
write.csv(sale_history_account_test_1, "../data/sale_history_account_test.csv", row.names = FALSE)
######################################################################################
