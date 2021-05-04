library(tidyverse)
library(readr)
library(lubridate)

#@#___________Import Data___________#@# 
shelter_2017 <- read_csv("datasets/FY_2017_Dallas_Animal_Shelter_Data.csv")
shelter_2018 <- read_csv("datasets/FY_2018_Dallas_Animal_Shelter_Data.csv")
shelter_2019 <- read_csv("datasets/FY_2019_Dallas_Animal_Shelter_Data.csv")
shelter_2020 <- read_csv("datasets/FY2020_Dallas_Animal_Shelter_Data.csv")
shelter_2021 <- read_csv("datasets/FY2021_Dallas_Animal_Shelter_Data.csv")

#@#___________Bind together 2018-2021 Data___________#@#

#make column names uniform
names(shelter_2020) <- names(shelter_2019)
names(shelter_2021) <- names(shelter_2019)

#combine 2018-2021 and select relevant columns
shelter_2018_2021 <- rbind(shelter_2018,
                      shelter_2019,
                      shelter_2020,
                      shelter_2021
                     ) %>% 
                select(Animal_Id,
                       Animal_Type,
                       Animal_Breed,
                       Kennel_Status,
                       Activity_Sequence,
                       Census_Tract,
                       Council_District,
                       Intake_Type,
                       Intake_Subtype,
                       Reason,
                       Intake_Date,
                       Intake_Time,
                       Intake_Condition,
                       Hold_Request,
                       Outcome_Type,
                       Outcome_Subtype,
                       Outcome_Date,
                       Outcome_Time,
                       Outcome_Condition,
                       Chip_Status,
                       Animal_Origin,
                       Month,
                       Year
                      )


#@#___________Edit 2017 Data to be uniform with other datasets___________#@# 

#select relevant columns
shelter_2017_clean <- shelter_2017 %>%
                  select("Animal ID",
                         "Animal Type",
                         "Animal Breed",
                         "Kennel Status",
                         "Activity Sequence",
                         "Census Tract",
                         "Council District",
                         "Intake Type",
                         "Intake Subtype",
                         "Reason",
                         "Intake Date",
                         "Intake Time",
                         "Intake Condition",
                         "Hold Request",
                         "Outcome Type",
                         "Outcome Date",
                         "Outcome Time",
                         "Outcome Condition",
                         "Chip Status",
                         "Animal Origin",
                         Month,
                         Year
                        ) %>% 
                  add_column(Outcome_Subtype = rep(NA, nrow(shelter_2017)),
                            .before = 16) # Add missing column Outcome_Subtype
#make names uniform
names(shelter_2017_clean) <- names(shelter_2018_2021)

#@#___________Combine All Shelter Years___________#@# 
shelter_2017_2021 <- rbind(shelter_2017_clean,
                            shelter_2018_2021)

#@#___________Clean environment___________#@# 
rm(list = c("shelter_2017",
            "shelter_2018",
            "shelter_2019",
            "shelter_2020",
            "shelter_2021",
            "shelter_2018_2021",
            "shelter_2017_clean"
            )
  )

#@#___________Clean Shelter Data___________#@# 
shelter_full_clean <- shelter_2017_2021 %>% 
                          mutate_at(vars(Year), ~str_remove(.x, "FY")
                                 ) %>% 
                          rename(Fiscal_Year = Year
                                 ) %>% 
                          mutate_at(vars(Month),
                                  ~str_replace(.x, "\\.", " ")
                                 ) %>% 
                          mutate_if(is.character, 
                                  str_to_title
                                 ) %>% 
                          mutate_at(vars(Intake_Date, Outcome_Date),
                                  ~str_remove(.x,"12:00:00 Am")
                                 )

rm(shelter_2017_2021)

#save final data file
write.csv(shelter_full_clean, "shelter_2017_2021.csv")


