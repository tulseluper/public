#####################################################################################
#                                                                                   #
#                          Created by Robert Riedl                                  #
#                               Maintained at:                                      #
#.  https://github.com/tulseluper/public/tree/main/R_scripts/covid_wastewater       #
#                                                                                   #
#####################################################################################

#This script loads wastewater treatment data that has been downloaded and computes the average
# wastewater values for the trailing 14 days from the last sample date.
# From this data, an activity phase is computed based on script requester's mapping

#############
# LIBRARIES #
#############
library(readxl)
library(janitor)
library(dplyr)


#############
# FUNCTIONS #
#############
# Given a measurement of COVID wastewater samples, determines phase
compute_phase <- function(x){
  ifelse(x > 350000000, "0", 
         ifelse(x > 300000000,"1",
                ifelse(x > 200000000, "2", 
                       ifelse(x > 150000000,"3",
                              ifelse(x > 100000000, "4", 
                                     ifelse(x > 50000000,"5",
                                            ifelse(x > 25000000, "6", 
                                                   ifelse(x > 15000000,"7",
                                                          ifelse(x > 7000000,"8",
                                                                 ifelse(x > 4000000,"9",
                                                                        ifelse(x > 2000000,"10",
                                                                               
                                                                               ifelse("11"))))))))))))
}

#### CURRENTLY NOT WORKING #####
# url <- "https://doh.wa.gov/sites/default/files/legacy/Documents/1600/coronavirus/data-tables/Downloadable_Wastewater.xlsx"
# Data1 <- read_excel(url)
################################

#read data from local drive
Data1 <- read_excel("/Users/boberts/Downloads/Downloadable_Wastewater.xlsx")

#clean data
Data1 <- clean_names(Data1) %>% na.omit()
Data1$sample_collection_date<- as.Date(Data1$sample_collection_date) #converts dttm to date

#compute last sample date by county
last_date <- Data1 %>%
  group_by(county) %>%
  summarise(last_sample_date = max(sample_collection_date))

#create new df with wastewater data and last sample date for each county
Data3 <- left_join(Data1, last_date, by = "county")

#filter data by last 2 weeks from last sample date for each county
Data3 <- Data3 %>%
  filter(sample_collection_date >= last_sample_date - 14)

#create new df with the average for each county over the last 14 days since the last sample date
mean_weight <- Data3 %>%
  group_by(county,last_sample_date) %>%
  summarise(mean_COVID = mean(normalized_sars_co_v_2_concentration_viral_copies_person_day))

# add columns for phase and age of the last sample
mean_weight <- mean_weight %>% mutate(phase = compute_phase(mean_COVID), age = today()-last_sample_date)
cat("\014") #clears the screen
print(mean_weight,n=100)
