######################################################################################
# load library
library(tidyverse)
library(lubridate)
library(rvest)
library(png)
######################################################################################

######################################################################################
# website main page
main_page_url <- 'https://www.larvalabs.com/cryptopunks'
main_page_response <- read_html(main_page_url)

# extract sales page information
sales_page <- html_elements(x = main_page_response, 
                              xpath = '//a[@href = "/cryptopunks/sales"]')
sales_page_url <- html_attr(x = sales_page, 'href')
sales_count <- parse_number(html_text(sales_page))# count sales amount

# put every sale in one page(easier to scrape)
sales_page_url <- paste0('https://www.larvalabs.com', sales_page_url, '?perPage=', sales_count, "&page=1")
sales_page_response <- read_html(sales_page_url)

# extract each sale page information
sales_page.each <- html_elements(x = sales_page_response, 
                            xpath = '//div[@class= "text-center"]//a')

# get each sale page url 
sales_page_url.each <- html_attr(x = sales_page.each, 'href')
## get each unique punk's name
Punk_name <- unique(parse_number(html_attr(x = sales_page.each, 'title'))) 
## make the sale page url can be directly read
sales_page_url.each <- unique(paste0('https://www.larvalabs.com', sales_page_url.each)) 

# extract the image url
punk_image_url.each <- html_elements(x = sales_page_response, 
                                 xpath = '//div[@class= "text-center"]//img')
punk_image_url.each <- html_attr(x = punk_image_url.each, 'src')
punk_image_url.each <- unique(paste0('https://www.larvalabs.com', punk_image_url.each))
######################################################################################

######################################################################################
start <- Sys.time() # record time

# get the image, gender, attributes, and sale history information 

# count total punk amount appeared in the sale history
punk_count <- length(sales_page_url.each) 

# create empty list
punk_image <- vector(mode = "list", length = punk_count)
punk_gender <- vector(mode = "list", length = punk_count)
punk_attributes <- vector(mode = "list", length = punk_count)
sale_history <- vector(mode = "list", length = punk_count)

# use for loop to scrape the information
# (which can avoid HTTP error 429)
for (i in 1:length(sales_page_url.each)){
# for (i in 1:10){
  
  # extract the image of each punk
  punk_image_url.each.temp <- punk_image_url.each[i]
  tempimage <- tempfile() # to avoid save the image to the disk
  download.file(punk_image_url.each.temp, tempimage, mode="wb")
  punk_image[[i]] <- readPNG(tempimage)
  file.remove(tempimage) # cleanup
  
  # extract sales page information
  sales_page_url.each_response <- read_html(sales_page_url.each[i])
  
  ## punk gender
  punk_gender[[i]] <- html_elements(x = sales_page_url.each_response, 
                               xpath = '//div[@id="punkDetails"]//h4') %>% 
    html_text()
  
  ## punk attributes
  punk_attributes.temp <- html_elements(x = sales_page_url.each_response, 
                                   xpath = '//div[@id="punkDetails"]//div[@class="row detail-row"]//a') %>% 
    html_text()
  punk_attributes[[i]] <- punk_attributes.temp[2: (2+parse_number(punk_attributes.temp[1])-1)]
  
  ## punk sale history
  sale_history.temp <- html_elements(x = sales_page_url.each_response, 
                                xpath = '//div[@id="punkHistory"]//td') %>% 
    html_text()
  sale_history[[i]] <- as_tibble(matrix(sale_history.temp, 
                                   nrow = length(sale_history.temp)/5, ncol = 5, 
                                   byrow=TRUE))
  
  # wait for 3 second to avoid HTTP error 429
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<3){} #dummy while loop
  
  print(i) # show the progress
}

end <- Sys.time()  # record time

end - start  # record time
######################################################################################

######################################################################################
# save the data
save(# in case we need
     sales_count,
     sales_page_url.each,
     punk_image_url.each,
     # core data
     Punk_name,
     punk_count,
     punk_image, 
     punk_gender, 
     punk_attributes, 
     sale_history,
     file = "../data/sales_scraped_information.RData")
######################################################################################
