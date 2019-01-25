library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Total Households by County
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Get state data
geography=geo.make(state=09)
yearlist=c(2010:2017)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

state_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                         table.number="B10063", col.names=col.names, key=key)
  Sys.sleep(3)
  year <- data@endyear
  print(paste("Processing: ", year))
  year <- paste(year-4, year, sep="-")
  geo <- data@geography
  households <- acsSum(data, 1, "Total Households")
  numbers <- data.table(
             geo,
             estimate(households),
             year,
             `Measure Type` = rep_len("Number", nrow(data)),
             Variable = rep_len("Total Households", nrow(data)))
  numbers.moe <- data.table(
                 geo, 
                 standard.error(households) * 1.645,
                 year,
                 `Measure Type` = rep_len("Number", nrow(data)),
                 Variable = rep_len("Margins of Error", nrow(data)))
  names <- c("County", 
              "FIPS",            
              "Value", 
              "Year", 
              "Measure Type", 
              "Variable")
  setnames(numbers, names)
  setnames(numbers.moe, names)
  
  state_data <- rbind(state_data, numbers, numbers.moe)
}

#Get county data
geography=geo.make(state=09, county="*")

county_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                         table.number="B10063", col.names=col.names, key=key)
  Sys.sleep(3)
  year <- data@endyear
  print(paste("Processing: ", year))
  year <- paste(year-4, year, sep="-")
  geo <- data@geography
  geo$NAME <- gsub(", Connecticut", "", geo$NAME)
  geo$county <- gsub("^", "09", geo$county)
  geo$state <- NULL  
  households <- acsSum(data, 1, "Total Households")
  numbers <- data.table(
             geo,
             estimate(households),
             year,
             `Measure Type` = rep_len("Number", nrow(data)),
             Variable = rep_len("Total Households", nrow(data)))
  numbers.moe <- data.table(
                 geo, 
                 standard.error(households) * 1.645,
                 year,
                 `Measure Type` = rep_len("Number", nrow(data)),
                 Variable = rep_len("Margins of Error", nrow(data)))
  names <- c("County", 
             "FIPS",            
             "Value", 
             "Year", 
             "Measure Type", 
             "Variable")
  setnames(numbers, names)
  setnames(numbers.moe, names)

  county_data <- rbind(county_data, numbers, numbers.moe)
}

households <- rbind(state_data, county_data)

households <- households %>% 
  select(County, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(County, Year, desc(Variable))

write.table (
  households,
  file.path(getwd(), "data", "total-households-county-2017.csv"),
  sep = ",",
  row.names = F,
  na = "-6666"
)



