# library
library("tidyverse")
library("readxl") # process the .xlsx files

# read data

# Top Calibers Recovered and Traced in the United States and Territories
calibers <- read_excel("../data/Firearms Trace Data/modified/2012/calibers-by-state.xlsx", 
                       range = "B2:BE33")
colnames(calibers)[1] <- "Top Calibers"

# Categories Associated with Firearms Recovered and Traced in the United States and Territories
categories <- read_excel("../data/Firearms Trace Data/modified/2012/categories-by-state.xlsx", 
                         range = "B2:BE68")
colnames(categories)[1] <- "Categories"

# Age of Possessor - Firearms Recovered and Traced in the United States and Territories
possessor_age <- read_excel("../data/Firearms Trace Data/modified/2012/possessor-age.xlsx", 
                            range = "A4:I58")

# Number of Firearms Sourced and Recovered in the United States and Territories
source_recovery <- read_excel("../data/Firearms Trace Data/modified/2012/source-recovery-by-state.xlsx", 
                              range = "B2:BF57")
colnames(source_recovery)[1] <- "Recovery/Source"

# Time-to-Crime - Firearms Recovered and Traced in the United States and Territories
ttc_RT <- read_excel("../data/Firearms Trace Data/modified/2012/ttc.xlsx", 
                     range = "A4:H58")

# Time-to-Crime - Firearms Sourced and Recovered in the United States and Territories 
ttc_SR <- read_excel("../data/Firearms Trace Data/modified/2012/final_ttc_source_recovery_by_state-cy.xlsx", 
                     range = "B2:BF380")
colnames(ttc_SR)[1] <- "Source State" # every col is recovery state
colnames(ttc_SR)[2] <- "Time Range"

State_Names <- na.omit(unique(ttc_SR$`Source State`))
for (i in 1:length(State_Names)){
  start_row <- (i-1)*7+2
  end_row <- i*7
  ttc_SR$`Source State`[start_row:end_row] <- ttc_SR$`Source State`[((i-1)*7+1)]
}

# Firearm Types Recovered and Traced in the United States and Territories
types_by_state <- read_excel("../data/Firearms Trace Data/modified/2012/types-by-state.xlsx",
                             range = "B2:BE17")
colnames(types_by_state)[1] <- "Firearm Types"

# FFL Theft/Loss Reports Matching Firearms Subsequently Recovered and Traced
FFL <- read_excel("../data/Firearms Trace Data/modified/2012/thefthitsontracesbyrecstn.xlsx",
                  range = "B1:C53")
