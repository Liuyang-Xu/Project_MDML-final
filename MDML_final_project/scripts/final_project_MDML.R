######################################################################################
# Part B
######################################################################################

######################################################################################
# load library
library(tidyverse)
library(lubridate)
library(rvest)
######################################################################################

######################################################################################
### 2.

# download website data
url <- 'https://www.universalhub.com/crime/home.html'
response <- read_html(url)

# extract Crime by neighborhood select menu
neighborhood <- html_elements(x = response, 
                              xpath = '//select[@id = "edit-jump"]//option')

# get every neighborhood name from the "Crime by neighborhood" selection box
neighborhood_names <- html_text(neighborhood)
neighborhood_names <- neighborhood_names[2:length(neighborhood)]

# get every neighborhood url from the "Crime by neighborhood" selection box
neighborhood_url <- html_attr(x = neighborhood, 'value')
neighborhood_url <- neighborhood_url[2:length(neighborhood)]
neighborhood_url <- paste0('https://www.universalhub.com', neighborhood_url)

# some neighborhood have multiple pages
# add these extra pages to the 'neighborhood_url' as well
for (i in 1:length(neighborhood_names)){
  temp_response <- read_html(neighborhood_url[i])
  page_2_exist <- html_element(x = temp_response,
                               xpath = '//li[@class = "pager-item"]')
  if (is.na(page_2_exist) != TRUE) {# means there are extra pages
    hrefs <- html_attr(x = html_children(page_2_exist), 'href')
    neighborhood_url_add <- paste0('https://www.universalhub.com', hrefs)
    neighborhood_url <- c(neighborhood_url, neighborhood_url_add)
  }
}

crime_data <- tibble(crime = NA, hour = NA, nbhd = NA)

for (i in 1:length(neighborhood_url)){
  # get each url's response
  neighborhood_response <- read_html(neighborhood_url[i])
  
  # crime
  Type <- html_elements(x = neighborhood_response, 
                        xpath = '//td[@class = "views-field views-field-name"]')
  crime <- html_text(x = Type, trim = T)
  
  # hour
  Date <- html_elements(x = neighborhood_response, 
                        xpath = '//span[@class = "date-display-single"]')
  hour <- html_text(x = Date)
  hour <- lubridate::parse_date_time(hour, 'm/d/y - H:M p')
  hour <- hour(hour)
  
  # nbhd
  Street <- html_elements(x = neighborhood_response, 
                          xpath = '//td[@class = "views-field views-field-street"]')
  nbhd <- html_text(x = Street, trim = T)
  
  crime_data.this_url <- tibble(crime, hour, nbhd)
  
  crime_data <- bind_rows(crime_data, crime_data.this_url)
}

crime_data <- crime_data[2:nrow(crime_data),]
crime_data[crime_data == ""] <-  NA

# Make sure to combine crime type names that obviously refer to the same crime
# table(crime_data$crime)
# change the writing mistakes
crime_data[crime_data == "Cruelty to animals"] <- "Animal cruelty"
crime_data[crime_data == "Assault with a dangeous weapon"] <- "Assault with a dangerous weapon"
crime_data[crime_data == "Shoting"] <- "Shooting"
crime_data[crime_data == "Shotting"] <- "Shooting"
######################################################################################

######################################################################################
### 3.
crime_data_by_hour <- crime_data %>% 
  group_by(hour) %>% 
  count()

p <- ggplot(data = crime_data_by_hour, aes(x=hour, y=n))
p <- p + geom_line()
p <- p + labs(y = "number of crimes")
p
ggsave(plot=p, file='../figures/crime_by_hour.png', height=5, width=5)
######################################################################################

######################################################################################
### 4.

# six most common types
six_common_types <- crime_data %>% 
  count(crime) %>% 
  arrange(desc(n))

six_common_types <- six_common_types[1:6,]

# filter the data to just these 6 types
crime_data_six_common_types <- crime_data %>% 
  filter(crime == six_common_types$crime[1] |
           crime == six_common_types$crime[2] |
           crime == six_common_types$crime[3] |
           crime == six_common_types$crime[4] |
           crime == six_common_types$crime[5] |
           crime == six_common_types$crime[6])

# count the number of each crime in each hour
crime_type_data_by_hour <- crime_data_six_common_types %>% 
  group_by(crime, hour) %>% 
  count()

p <- ggplot(data = crime_type_data_by_hour, aes(x=hour, y=n))
p <- p + geom_line()
p <- p + facet_wrap(~crime)
p <- p + labs(y = "number of crimes")
p
ggsave(plot=p, file='../figures/crime_type_by_hour.png', height=5, width=5)
######################################################################################
