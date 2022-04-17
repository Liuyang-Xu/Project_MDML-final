######################################################################################
# Part B
######################################################################################

######################################################################################
# load library
library(tidyverse)
library(lubridate)
library(rvest)
# library(readr)
######################################################################################

######################################################################################
### 2.

# download website data
main_page_url <- 'https://www.larvalabs.com/cryptopunks'
main_page_response <- read_html(main_page_url)

# extract sales page information
sales_page <- html_elements(x = main_page_response, 
                              xpath = '//a[@href = "/cryptopunks/sales"]')
sales_page_url <- html_attr(x = sales_page, 'href')
sales_count <- parse_number(html_text(sales_page)) 

# put every sale in one page(easier to scrape)
sales_page_url <- paste0('https://www.larvalabs.com', sales_page_url, '?perPage=', sales_count, "&page=1")
sales_page_response <- read_html(sales_page_url)

# extract each sale page information
sales_page.each <- html_elements(x = sales_page_response, 
                            xpath = '//div[@class= "text-center"]//a')

sales_page_url.each <- html_attr(x = sales_page.each, 'href')
Punk_name <- unique(parse_number(html_attr(x = sales_page.each, 'title')))

sales_page_url.each <- unique(paste0('https://www.larvalabs.com', sales_page_url.each))


sales_page_url.each_response <- rep(NA, length(sales_page_url.each))
# for (i in 1:length(sales_page_url.each)){
for (i in 1){
  sales_page_url.each_response[i] <- read_html(sales_page_url.each[i])
  
  
  
  # extract sales page information
  sale <- html_elements(x = sales_page_url.each_response, 
                        xpath = '//a[@href = "/cryptopunks/sales"]')
  sales_page_url <- html_attr(x = sales_page, 'href')
  sales_count <- parse_number(html_text(sales_page)) 
  
}




