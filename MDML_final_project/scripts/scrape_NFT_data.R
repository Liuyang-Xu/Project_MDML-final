##################################################################################
# load library
library(tidyverse)
library(lubridate)
library(rvest)
######################################################################################

######################################################################################
# website main page
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


# sales_page_url.each_response <- vector(mode = "list", length = 1)

# for (i in 1:length(sales_page_url.each)){
for (i in 1){
  sales_page_url.each_response <- append(sales_page_url.each_response, read_html(sales_page_url.each[i]))
  
  # wait for 3 second to avoid HTTP error 429
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<3){} #dummy while loop
  print(i)
}

# save(sales_page_url.each_response, file = "sales_page_url_each_response.RData")
# save(main_page_response, file = "test.RData")

# sales_page_url.each_response  %>% toString %>% saveRDS(., "sales_page_url_each_response.RDS")
# newobject <- readRDS("someobject.RDS") %>% read_html

# saveRDS(as.character(sales_page_url.each_response), "sales_page_url_each_response0.RDS")

# saveRDS(sales_page_url.each_response, "sales_page_url_each_response1.RDS")

i <- 1
sales_page_url.each_response <- read_html(sales_page_url.each[i])

# extract sales page information
punk_gender <- html_elements(x = sales_page_url.each_response, 
                              xpath = '//div[@id="punkDetails"]//h4') %>% 
  html_text()

punk_attributes <- html_elements(x = sales_page_url.each_response, 
                              xpath = '//div[@id="punkDetails"]//div[@class="row detail-row"]//a') %>% 
  html_text()
punk_attributes <- punk_attributes[2: (2+parse_number(punk_attributes[1])-1)]

sale_history <- html_elements(x = sales_page_url.each_response, 
                      xpath = '//div[@id="punkHistory"]//td') %>% 
  html_text()
sale_history <- as_tibble(matrix(sale_history, 
                                 nrow = length(sale_history)/5, ncol = 5, 
                                 byrow=TRUE))
