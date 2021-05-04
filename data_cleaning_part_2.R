library(tidyverse)
library(readr)
library(lubridate)

#@#___________Import shelter data and filter for dogs and cats___________#@# 
dgct <- read_csv("shelter_full_clean.csv") %>%
          filter(Animal_Type %in% c("Dog", "Cat")
                 )

#@#___________intake reclassification___________#@# 
#Dead - Dead on arrival intakes
#Stray
#Medical Treatment - Animal intakes for medical treatments
#Transfers = Transfers In
#Relinquished by Owner - relinquished by owner or foster 
#Owner requested euthanasia
#Other - Other intakes

intakes2 <- case_when(str_detect(dgct$Intake_Subtype, "Dead On Arrival") == TRUE ~ "Dead",
                      str_detect(dgct$Intake_Subtype, " Dead on Arrival") == TRUE ~ "Dead",
                      dgct$Intake_Subtype == "Died"  ~ "Dead",
                      dgct$Intake_Type == "Stray"  ~ "Stray",
                      dgct$Intake_Type == "Treatment" ~ "Medical Treatment",
                      dgct$Intake_Type == "Owner Surrender" & dgct$Intake_Subtype != "Euthanasia Requested" ~ "Relinquished by Owner",
                      dgct$Intake_Type == "Foster" & dgct$Intake_Subtype == "Return" ~ "Relinquished by Owner",
                      dgct$Intake_Type == "Owner Surrender" & dgct$Intake_Subtype == "Euthanasia Requested" ~ "Owner Requested Euthanasia",
                      dgct$Intake_Type == "Transfer" ~ "Transfer In",
                      TRUE ~ "Other"
                     )

#@#___________Outcome reclassification___________#@# 
#Owner Intended Euthanasia
#Adoption
#Returned to Owner
#Foster
#Euthanized
#Medical Treatment
#Transfer - transfer out
#Died in Care
#Losting in Care
#Other Live outcome - Other
#Other - lost report, found report, found exp, lost exp

outcomes2 <- case_when( dgct$Intake_Subtype == "Euthanasia Requested" & dgct$Outcome_Type == "Euthanized" ~ 'Owner Intended Euthanasia',
                        dgct$Outcome_Type == "Adoption" ~ "Adoption",
                        dgct$Outcome_Type == "Returned To Owner" ~ "Returned to Owner",
                        dgct$Outcome_Type == "Foster" ~ "Foster",
                        dgct$Outcome_Type == "Euthanized" ~ "Euthanized",
                        dgct$Outcome_Type == "Treatment" ~ "Medical Treatment",
                        dgct$Outcome_Type == "Transfer" ~ "Transfer",
                        dgct$Outcome_Type == "Dead On Arrival" ~ "Dead on Arrival",
                        dgct$Outcome_Type == "Died" ~ "Died in Care",
                        dgct$Outcome_Type == "Missing" ~ "Lost in Care",
                        dgct$Outcome_Type == "Disposal" ~ "Died in Care",
                        dgct$Outcome_Type == "Other" ~ "Other Live Outcome",
                        TRUE ~ "Other" # lost report, found report, found exp, lost exp
                       )
#@#___________Add new classifications and stay duration___________#@# 

dogcat <- dgct %>%
            mutate(Outcome_reclass = outcomes2,
                   Intake_reclass = intakes2
                  ) %>% 
            mutate(duration_days = mdy(Outcome_Date) - mdy(Intake_Date)
                   )
#@#___________Filter for live intakes only___________#@# 
dogcat_live_intakes <- dogcat %>% 
                        filter(Intake_reclass != "Dead"
                               ) %>% 
                        filter(Outcome_reclass != "Dead on Arrival"
                               ) %>% 
                        filter(Outcome_Type != "Dispos Req")

#@#___________Save live intakes and clear environment___________#@# 
write.csv(dogcat_live_intakes, "dog_cat_live_intakes.csv",
          row.names = FALSE
          )

rm(intakes2,
   outcomes2,
   dgct,
   dogcat)
