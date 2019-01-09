#Federal HUBZone Program Analysis
#loading libraries 
library (tidyverse)
library (reshape)
library(knitr)
library(rmarkdown)
library(lubridate)


# loading data
HUBZoneRaw <- read_csv("HUBZone_Raw.csv", 
                       col_types = cols(action_date = col_date(format = "%m/%d/%Y")))
                       ptions(stringsAsFactors = FALSE)
#analyze wards
ward_amount <- group_by(HUBZoneRaw, WARD) %>%
  summarise(total_contracts = sum(federal_action_obligation )) %>%
  arrange(desc(total_contracts))

# percentage 
ward_amount$total_contracts / sum(ward_amount$total_contracts)

# parse year                       
HUBZoneRaw$HUB_Year <- format(as.Date(HUBZoneRaw$action_date), "%Y")     

firms_total <- group_by(HUBZoneRaw, recipient_parent_duns) %>%
  summarise(total_contracts = sum(federal_action_obligation)) %>%
  arrange(desc(total_contracts))

top11 <- head(firms_total, 11)

sum(top11$total_contracts)

sum(top11$total_contracts) / sum(firms_total$total_contracts)

firms_2018 <- group_by(HUBZoneRaw, HUB_Year) %>%
  filter(HUB_Year == 2018) %>%
  summarise(fy2018_contracts = sum(federal_action_obligation))

firms_2018
