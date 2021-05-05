library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(magick)


#@#___________Import Data___________#@# 
data <- read_csv("dog_cat_live_intakes.csv") %>% 
  filter(Outcome_reclass != "Owner Intended Euthanasia")

fy_months <- month.abb[c(10:12, 1:9)]


#@#___________Visualize live/other outcomes using donut chart___________#@# 
donut_data_total <- data %>% 
                group_by(Outcome_Class) %>% 
                summarize(n = n()) %>% 
                mutate(frac = n / sum(n),
                       ymax = cumsum(frac),
                       ymin = c(0, head(ymax, n=-1)) ) %>%
                arrange(desc(n))

donut_data_dog <- data %>% 
                    filter(Animal_Type == "Dog") %>% 
                    group_by(Outcome_Class) %>% 
                    summarize(n = n()) %>% 
                    mutate(frac = n / sum(n),
                           ymax = cumsum(frac),
                           ymin = c(0, head(ymax, n=-1)) ) %>%
                    arrange(desc(n))

donut_data_cat <- data %>% 
                    filter(Animal_Type == "Cat") %>% 
                    group_by(Outcome_Class) %>% 
                    summarize(n = n()) %>% 
                    mutate(frac = n / sum(n),
                           ymax = cumsum(frac),
                           ymin = c(0, head(ymax, n=-1)) ) %>%
                    arrange(desc(n))

donut_all <- donut_data_total %>% 
  ggplot(aes(fill=Outcome_Class,
              ymax=ymax,
              ymin=ymin,
              xmax=4,
              xmin=2.5)
         ) +
          geom_rect() +
          coord_polar(theta="y") +
          xlim(c(0, 4)) +
          labs(fill = "Type of Intake") +
          theme(panel.grid=element_blank()) +
          geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                        x = 3.25,
                        y = (ymin + ymax) / 2)) +
          theme(axis.text=element_blank()) +
          theme(axis.ticks=element_blank()) +
          scale_fill_manual(values = c("#3DBD61", "#D95959") )+
          theme_void() +
          theme(legend.position = "none")

cat_donut_all <- donut_data_cat %>% 
  ggplot(aes(fill=Outcome_Class,
             ymax=ymax,
             ymin=ymin,
             xmax=4,
             xmin=2.5)
  ) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(fill = "Type of Intake") +
  theme(panel.grid=element_blank()) +
  geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                x = 3.25,
                y = (ymin + ymax) / 2)) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  scale_fill_manual(values = c("#3DBD61", "#D95959") )+
  theme_void() +
  theme(legend.position = "none")

dog_donut_all <- donut_data_dog %>% 
  ggplot(aes(fill=Outcome_Class,
             ymax=ymax,
             ymin=ymin,
             xmax=4,
             xmin=2.5)
  ) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(fill = "Type of Intake") +
  theme(panel.grid=element_blank()) +
  geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                x = 3.25,
                y = (ymin + ymax) / 2)) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  scale_fill_manual(values = c("#3DBD61", "#D95959") )+
  theme_void() +
  theme(legend.position = "none")

dog_donut_all_image <- ggdraw() +
  draw_image("images/dog-images-free-clipart.png", scale = .3) +
  draw_plot(dog_donut_all)

cat_donut_all_image <- ggdraw() +
  draw_image("images/cat-removebg-preview.png", scale = .3) +
  draw_plot(cat_donut_all)

donut_all_image <- ggdraw() +
  draw_image("images/cat-removebg-preview.png", scale = .2, x = -.05)+
  draw_image("images/dog-images-free-clipart.png", scale = .2, x = .05) +
  draw_plot(donut_all)
  

#@#___________Outcome Type By Year_All___________#@# 

#2017
donut_data_2017 <- data %>% 
                      filter(Fiscal_Year == 2017) %>% 
                      group_by(Outcome_Class) %>% 
                      summarize(n = n()) %>% 
                      mutate(frac = n / sum(n),
                             ymax = cumsum(frac),
                             ymin = c(0, head(ymax, n=-1)) ) %>%
                      arrange(desc(n))

donut_all_2017 <- donut_data_2017 %>% 
  ggplot(aes(fill=Outcome_Class,
             ymax=ymax,
             ymin=ymin,
             xmax=4,
             xmin=2.5)
  ) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(fill = "Type of Intake") +
  theme(panel.grid=element_blank()) +
  geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                x = 3.25,
                y = (ymin + ymax) / 2)) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  scale_fill_manual(values = c("#3DBD61", "#D95959") )+
  theme_void() +
  theme(legend.position = "none")

donut_all_2017_img <- ggdraw() +
  draw_image("images/cat-removebg-preview.png", scale = .2, x = -.05)+
  draw_image("images/dog-images-free-clipart.png", scale = .2, x = .05) +
  draw_plot(donut_all_2017)
#2018
donut_data_2018 <- data %>% 
  filter(Fiscal_Year == 2018) %>% 
  group_by(Outcome_Class) %>% 
  summarize(n = n()) %>% 
  mutate(frac = n / sum(n),
         ymax = cumsum(frac),
         ymin = c(0, head(ymax, n=-1)) ) %>%
  arrange(desc(n))

donut_all_2018 <- donut_data_2018 %>% 
  ggplot(aes(fill=Outcome_Class,
             ymax=ymax,
             ymin=ymin,
             xmax=4,
             xmin=2.5)
  ) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(fill = "Type of Intake") +
  theme(panel.grid=element_blank()) +
  geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                x = 3.25,
                y = (ymin + ymax) / 2)) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  scale_fill_manual(values = c("#3DBD61", "#D95959") )+
  theme_void() +
  theme(legend.position = "none")

donut_all_2018_img <- ggdraw() +
  draw_image("images/cat-removebg-preview.png", scale = .2, x = -.05)+
  draw_image("images/dog-images-free-clipart.png", scale = .2, x = .05) +
  draw_plot(donut_all_2018)

#2019
donut_data_2019 <- data %>% 
  filter(Fiscal_Year == 2019) %>% 
  group_by(Outcome_Class) %>% 
  summarize(n = n()) %>% 
  mutate(frac = n / sum(n),
         ymax = cumsum(frac),
         ymin = c(0, head(ymax, n=-1)) ) %>%
  arrange(desc(n))

donut_all_2019 <- donut_data_2019 %>% 
  ggplot(aes(fill=Outcome_Class,
             ymax=ymax,
             ymin=ymin,
             xmax=4,
             xmin=2.5)
  ) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(fill = "Type of Intake") +
  theme(panel.grid=element_blank()) +
  geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                x = 3.25,
                y = (ymin + ymax) / 2)) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  scale_fill_manual(values = c("#3DBD61", "#D95959") )+
  theme_void() +
  theme(legend.position = "none")

donut_all_2019_img <- ggdraw() +
  draw_image("images/cat-removebg-preview.png", scale = .2, x = -.05)+
  draw_image("images/dog-images-free-clipart.png", scale = .2, x = .05) +
  draw_plot(donut_all_2019)
#2020
donut_data_2020 <- data %>% 
  filter(Fiscal_Year == 2020) %>% 
  group_by(Outcome_Class) %>% 
  summarize(n = n()) %>% 
  mutate(frac = n / sum(n),
         ymax = cumsum(frac),
         ymin = c(0, head(ymax, n=-1)) ) %>%
  arrange(desc(n))

donut_all_2020 <- donut_data_2020 %>% 
  ggplot(aes(fill=Outcome_Class,
             ymax=ymax,
             ymin=ymin,
             xmax=4,
             xmin=2.5)
  ) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(fill = "Type of Intake") +
  theme(panel.grid=element_blank()) +
  geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                x = 3.25,
                y = (ymin + ymax) / 2)) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  scale_fill_manual(values = c("#3DBD61", "#D95959") )+
  theme_void() +
  theme(legend.position = "none")

donut_all_2020_img <- ggdraw() +
  draw_image("images/cat-removebg-preview.png", scale = .2, x = -.05)+
  draw_image("images/dog-images-free-clipart.png", scale = .2, x = .05) +
  draw_plot(donut_all_2020)

#2021
donut_data_2021 <- data %>% 
  filter(Fiscal_Year == 2021) %>% 
  group_by(Outcome_Class) %>% 
  summarize(n = n()) %>% 
  mutate(frac = n / sum(n),
         ymax = cumsum(frac),
         ymin = c(0, head(ymax, n=-1)) ) %>%
  arrange(desc(n))

donut_all_2021 <- donut_data_2021 %>% 
  ggplot(aes(fill=Outcome_Class,
             ymax=ymax,
             ymin=ymin,
             xmax=4,
             xmin=2.5)
  ) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(fill = "Type of Intake") +
  theme(panel.grid=element_blank()) +
  geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                x = 3.25,
                y = (ymin + ymax) / 2)) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  scale_fill_manual(values = c("#3DBD61", "#D95959") )+
  theme_void() +
  theme(legend.position = "none")

donut_all_2021_img <- ggdraw() +
  draw_image("images/cat-removebg-preview.png", scale = .2, x = -.05)+
  draw_image("images/dog-images-free-clipart.png", scale = .2, x = .05) +
  draw_plot(donut_all_2021)

#@#___________Outcome Linegraph type by fiscal year/month___________#@# 
monthly_data <- data %>% 
  group_by(Month) %>% 
  summarize(n = n())

monthly_live_outcomes <- data %>% 
  filter(Outcome_Class == "Live Outcome") %>% 
  group_by(Month) %>% 
  summarize(live = n())

proportion_live_outcomes <- inner_join(monthly_data,
                                     monthly_live_outcomes,
                                     by = "Month" ) %>% 
                           mutate(proportion_live = live / n)

porp_live_all <- proportion_live_outcomes %>%
  ggplot(aes(x = my(Month), y = proportion_live)) +
  geom_line(col = "blue", size = 1.2) +
  geom_hline(yintercept = 0.9, linetype = 2)+
  theme_minimal() +
  labs(y = "Proportion of Live Outcomes",
       x = "Month") +
  scale_x_date(date_breaks = "6 months", date_labels = c("July 2016",
                                                         "Jan 2017",
                                                         "July 2017",
                                                         "Jan 2018",
                                                         "July 2018",
                                                         "Jan 2019",
                                                         "July 2019",
                                                         "Jan 2020",
                                                         "July 2020",
                                                         "Jan 2021",
                                                         "July 2021"
                                                          )) +
  theme(axis.text.x = element_text(angle = 45))

#@#___________2017 line chart___________#@# 
outcomes_17 <- data %>% 
              filter(Fiscal_Year == 2017) %>% 
              mutate_at(vars(Month), my) %>% 
              group_by(Month) %>% 
              summarize(n = n())

live_outcomes_17 <-  data %>% 
                      filter(Outcome_Class == "Live Outcome") %>% 
                      filter(Fiscal_Year == 2017) %>% 
                      mutate_at(vars(Month), my) %>% 
                      group_by(Month) %>% 
                      summarize(live = n())

prop_live_17 <- inner_join(outcomes_17,
                           live_outcomes_17,
                            by = "Month"
                          ) %>% 
                mutate(proportion_live = live / n)

prop_live_all_2017 <- prop_live_17 %>% 
                        ggplot(aes(x = Month, y = proportion_live)) +
                          geom_line(col = "blue", size = 1.2) + 
                          geom_hline(yintercept = 0.9, linetype = 2)+
                          theme_minimal() +
                          labs(y = "Proportion of Live Outcomes",
                               x = "Month") +
                          scale_x_date(breaks = unique(prop_live_17$Month),
                                       labels = fy_months)
              
#@#___________linechart 2018___________#@# 
outcomes_18 <- data %>% 
  filter(Fiscal_Year == 2018) %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(n = n())

live_outcomes_18 <-  data %>% 
  filter(Outcome_Class == "Live Outcome") %>% 
  filter(Fiscal_Year == 2018) %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(live = n())

prop_live_18 <- inner_join(outcomes_18,
                           live_outcomes_18,
                           by = "Month"
) %>% 
  mutate(proportion_live = live / n)

prop_live_all_2018 <- prop_live_18 %>% 
  ggplot(aes(x = Month, y = proportion_live)) +
  geom_line(col = "blue", size = 1.2) + 
  geom_hline(yintercept = 0.9, linetype = 2)+
  theme_minimal() +
  labs(y = "Proportion of Live Outcomes",
       x = "Month") +
  scale_x_date(breaks = unique(prop_live_18$Month),
               labels = fy_months)

#@#___________linechart 2019___________#@# 
outcomes_19 <- data %>% 
  filter(Fiscal_Year == 2019) %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(n = n())

live_outcomes_19 <-  data %>% 
  filter(Outcome_Class == "Live Outcome") %>% 
  filter(Fiscal_Year == 2019) %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(live = n())

prop_live_19 <- inner_join(outcomes_19,
                           live_outcomes_19,
                           by = "Month"
) %>% 
  mutate(proportion_live = live / n)

prop_live_all_2019 <- prop_live_19 %>% 
  ggplot(aes(x = Month, y = proportion_live)) +
  geom_line(col = "blue", size = 1.2) + 
  geom_hline(yintercept = 0.9, linetype = 2)+
  theme_minimal() +
  labs(y = "Proportion of Live Outcomes",
       x = "Month") +
  scale_x_date(breaks = unique(prop_live_19$Month),
               labels = fy_months)
#@#___________linechart 2020___________#@# 
outcomes_20 <- data %>% 
  filter(Fiscal_Year == 2020) %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(n = n())

live_outcomes_20 <-  data %>% 
  filter(Outcome_Class == "Live Outcome") %>% 
  filter(Fiscal_Year == 2020) %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(live = n())

prop_live_20 <- inner_join(outcomes_20,
                           live_outcomes_20,
                           by = "Month"
) %>% 
  mutate(proportion_live = live / n)

prop_live_all_2020 <- prop_live_20 %>% 
  ggplot(aes(x = Month, y = proportion_live)) +
  geom_line(col = "blue", size = 1.2) + 
  geom_hline(yintercept = 0.9, linetype = 2)+
  theme_minimal() +
  labs(y = "Proportion of Live Outcomes",
       x = "Month") +
  scale_x_date(breaks = unique(prop_live_20$Month),
               labels = fy_months)
#@#___________linechart 2021___________#@# 
outcomes_21 <- data %>% 
  filter(Fiscal_Year == 2021) %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(n = n())

live_outcomes_21 <-  data %>% 
  filter(Outcome_Class == "Live Outcome") %>% 
  filter(Fiscal_Year == 2021) %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(live = n())

prop_live_21 <- inner_join(outcomes_21,
                           live_outcomes_21,
                           by = "Month"
) %>% 
  mutate(proportion_live = live / n)

prop_live_all_2021 <- prop_live_21 %>% 
  ggplot(aes(x = Month, y = proportion_live)) +
  geom_line(col = "blue", size = 1.2) + 
  geom_hline(yintercept = 0.9, linetype = 2)+
  theme_minimal() +
  labs(y = "Proportion of Live Outcomes",
       x = "Month") +
  scale_x_date(breaks = unique(prop_live_21$Month),
               labels = fy_months[1:8])

#@#___________Cat Donuts Yearly___________#@# 

cat_donut_creator <- function(fy){
donut_data_cat <- data %>% 
  filter(Animal_Type == "Cat") %>% 
  filter(Fiscal_Year == fy) %>% 
  group_by(Outcome_Class) %>% 
  summarize(n = n()) %>% 
  mutate(frac = n / sum(n),
         ymax = cumsum(frac),
         ymin = c(0, head(ymax, n=-1)) ) %>%
  arrange(desc(n))

cat_donut_all <- donut_data_cat %>% 
  ggplot(aes(fill=Outcome_Class,
             ymax=ymax,
             ymin=ymin,
             xmax=4,
             xmin=2.5)
  ) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  labs(fill = "Type of Intake") +
  theme(panel.grid=element_blank()) +
  geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                x = 3.25,
                y = (ymin + ymax) / 2)) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  scale_fill_manual(values = c("#3DBD61", "#D95959") )+
  theme_void() +
  theme(legend.position = "none")

cat_donut_all_image <- ggdraw() +
  draw_image("images/cat-removebg-preview.png", scale = .3) +
  draw_plot(cat_donut_all)
cat_donut_all_image
}


cat_donut_17 <- cat_donut_creator(2017)
cat_donut_18 <- cat_donut_creator(2018)
cat_donut_19 <- cat_donut_creator(2019)
cat_donut_20 <- cat_donut_creator(2020)
cat_donut_21 <- cat_donut_creator(2021)

#@#___________Dog Donuts Yearly___________#@# 

dog_donut_creator <- function(fy){
  donut_data_dog <- data %>% 
    filter(Animal_Type == "Dog") %>% 
    filter(Fiscal_Year == fy) %>% 
    group_by(Outcome_Class) %>% 
    summarize(n = n()) %>% 
    mutate(frac = n / sum(n),
           ymax = cumsum(frac),
           ymin = c(0, head(ymax, n=-1)) ) %>%
    arrange(desc(n))
  
  dog_donut_all <- donut_data_dog %>% 
    ggplot(aes(fill=Outcome_Class,
               ymax=ymax,
               ymin=ymin,
               xmax=4,
               xmin=2.5)
    ) +
    geom_rect() +
    coord_polar(theta="y") +
    xlim(c(0, 4)) +
    labs(fill = "Type of Intake") +
    theme(panel.grid=element_blank()) +
    geom_text(aes(label = (frac*100 )%>% round(2) %>% paste0("%"),
                  x = 3.25,
                  y = (ymin + ymax) / 2)) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    scale_fill_manual(values = c("#3DBD61", "#D95959") )+
    theme_void() +
    theme(legend.position = "none")
  
  dog_donut_all_image <- ggdraw() +
    draw_image("images/dog-images-free-clipart.png", scale = .3) +
    draw_plot(dog_donut_all)
  dog_donut_all_image
}


dog_donut_17 <- dog_donut_creator(2017)
dog_donut_18 <- dog_donut_creator(2018)
dog_donut_19 <- dog_donut_creator(2019)
dog_donut_20 <- dog_donut_creator(2020)
dog_donut_21 <- dog_donut_creator(2021)

#@#___________Dog Line Graph___________#@# 
monthly_data <- data %>% 
  group_by(Month) %>% 
  summarize(n = n())

monthly_live_outcomes <- data %>% 
  filter(Outcome_Class == "Live Outcome") %>% 
  group_by(Month) %>% 
  summarize(live = n())

proportion_live_outcomes <- inner_join(monthly_data,
                                       monthly_live_outcomes,
                                       by = "Month" ) %>% 
  mutate(proportion_live = live / n)

porp_live_all <- proportion_live_outcomes %>%
  ggplot(aes(x = my(Month), y = proportion_live)) +
  geom_line(col = "blue", size = 1.2) +
  geom_hline(yintercept = 0.9, linetype = 2)+
  theme_minimal() +
  labs(y = "Proportion of Live Outcomes",
       x = "Month") +
  scale_x_date(date_breaks = "6 months", date_labels = c("July 2016",
                                                         "Jan 2017",
                                                         "July 2017",
                                                         "Jan 2018",
                                                         "July 2018",
                                                         "Jan 2019",
                                                         "July 2019",
                                                         "Jan 2020",
                                                         "July 2020",
                                                         "Jan 2021",
                                                         "July 2021"
  )) +
  theme(axis.text.x = element_text(angle = 45))

#@#___________2017 line chart___________#@# 

## All Years
outcomes_dog <- data %>% 
  filter(Animal_Type == "Dog") %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(n = n())

live_outcomes_dog <-  data %>% 
  filter(Outcome_Class == "Live Outcome") %>% 
  filter(Animal_Type == "Dog") %>% 
  mutate_at(vars(Month), my) %>% 
  group_by(Month) %>% 
  summarize(live = n())

prop_live_dog <- inner_join(outcomes_dog,
                           live_outcomes_dog,
                           by = "Month"
) %>% 
  mutate(proportion_live = live / n)

prop_live_dog_g <- prop_live_dog %>% 
  ggplot(aes(x = Month, y = proportion_live)) +
  geom_line(col = "blue", size = 1.2) + 
  geom_hline(yintercept = 0.9, linetype = 2)+
  theme_minimal() +
  labs(y = "Proportion of Live Outcomes",
       x = "Month")

### Yearly
dog_line <- function(fy){
  outcomes_dog <- data %>% 
    filter(Animal_Type == "Dog") %>% 
    filter(Fiscal_Year == fy) %>% 
    mutate_at(vars(Month), my) %>% 
    group_by(Month) %>% 
    summarize(n = n())
  
  live_outcomes_dog <-  data %>% 
    filter(Outcome_Class == "Live Outcome") %>% 
    filter(Fiscal_Year == fy) %>% 
    filter(Animal_Type == "Dog") %>% 
    mutate_at(vars(Month), my) %>% 
    group_by(Month) %>% 
    summarize(live = n())
  
  prop_live_dog <- inner_join(outcomes_dog,
                              live_outcomes_dog,
                              by = "Month"
  ) %>% 
    mutate(proportion_live = live / n)
  
  prop_live_dog_g <- prop_live_dog %>% 
    ggplot(aes(x = Month, y = proportion_live)) +
    geom_line(col = "blue", size = 1.2) + 
    geom_hline(yintercept = 0.9, linetype = 2)+
    theme_minimal() +
    labs(y = "Proportion of Live Outcomes",
         x = "Month") +
    scale_x_date(breaks = unique(prop_live_dog$Month), date_labels = fy_months)
    
  prop_live_dog_g
}

dog_line_17 <- dog_line(2017)
dog_line_18 <- dog_line(2018)
dog_line_19 <- dog_line(2019)
dog_line_20 <- dog_line(2020)

  outcomes_dog_21 <- data %>% 
    filter(Animal_Type == "Dog") %>% 
    filter(Fiscal_Year == 2021) %>% 
    mutate_at(vars(Month), my) %>% 
    group_by(Month) %>% 
    summarize(n = n())
  
  live_outcomes_dog_21 <-  data %>% 
    filter(Outcome_Class == "Live Outcome") %>% 
    filter(Fiscal_Year == 2021) %>% 
    filter(Animal_Type == "Dog") %>% 
    mutate_at(vars(Month), my) %>% 
    group_by(Month) %>% 
    summarize(live = n())
  
  prop_live_dog <- inner_join(outcomes_dog_21,
                              live_outcomes_dog_21,
                              by = "Month"
  ) %>% 
    mutate(proportion_live = live / n)
  
  dog_line_21 <- prop_live_dog %>% 
    ggplot(aes(x = Month, y = proportion_live)) +
    geom_line(col = "blue", size = 1.2) + 
    geom_hline(yintercept = 0.9, linetype = 2)+
    theme_minimal() +
    labs(y = "Proportion of Live Outcomes",
         x = "Month") +
    scale_x_date(breaks = unique(prop_live_dog$Month), date_labels = fy_months[1:8])
  
  
  ##
  
  #@#___________2017 line chart cat___________#@# 
  
  ## All Years
  outcomes_cat <- data %>% 
    filter(Animal_Type == "Cat") %>% 
    mutate_at(vars(Month), my) %>% 
    group_by(Month) %>% 
    summarize(n = n())
  
  live_outcomes_cat <-  data %>% 
    filter(Outcome_Class == "Live Outcome") %>% 
    filter(Animal_Type == "Cat") %>% 
    mutate_at(vars(Month), my) %>% 
    group_by(Month) %>% 
    summarize(live = n())
  
  prop_live_cat <- inner_join(outcomes_cat,
                              live_outcomes_cat,
                              by = "Month"
  ) %>% 
    mutate(proportion_live = live / n)
  
  prop_live_cat_g <- prop_live_cat %>% 
    ggplot(aes(x = Month, y = proportion_live)) +
    geom_line(col = "blue", size = 1.2) + 
    geom_hline(yintercept = 0.9, linetype = 2)+
    theme_minimal() +
    labs(y = "Proportion of Live Outcomes",
         x = "Month")
  
  ### Yearly
  cat_line <- function(fy){
    outcomes_cat <- data %>% 
      filter(Animal_Type == "Cat") %>% 
      filter(Fiscal_Year == fy) %>% 
      mutate_at(vars(Month), my) %>% 
      group_by(Month) %>% 
      summarize(n = n())
    
    live_outcomes_cat <-  data %>% 
      filter(Outcome_Class == "Live Outcome") %>% 
      filter(Fiscal_Year == fy) %>% 
      filter(Animal_Type == "Cat") %>% 
      mutate_at(vars(Month), my) %>% 
      group_by(Month) %>% 
      summarize(live = n())
    
    prop_live_cat <- inner_join(outcomes_cat,
                                live_outcomes_cat,
                                by = "Month"
    ) %>% 
      mutate(proportion_live = live / n)
    
    prop_live_cat_g <- prop_live_cat %>% 
      ggplot(aes(x = Month, y = proportion_live)) +
      geom_line(col = "blue", size = 1.2) + 
      geom_hline(yintercept = 0.9, linetype = 2)+
      theme_minimal() +
      labs(y = "Proportion of Live Outcomes",
           x = "Month") +
      scale_x_date(breaks = unique(prop_live_cat$Month), date_labels = fy_months)
    
    prop_live_cat_g
  }
  
  cat_line_17 <- cat_line(2017)
  cat_line_18 <- cat_line(2018)
  cat_line_19 <- cat_line(2019)
  cat_line_20 <- cat_line(2020)
  
  outcomes_cat_21 <- data %>% 
    filter(Animal_Type == "Cat") %>% 
    filter(Fiscal_Year == 2021) %>% 
    mutate_at(vars(Month), my) %>% 
    group_by(Month) %>% 
    summarize(n = n())
  
  live_outcomes_cat_21 <-  data %>% 
    filter(Outcome_Class == "Live Outcome") %>% 
    filter(Fiscal_Year == 2021) %>% 
    filter(Animal_Type == "Cat") %>% 
    mutate_at(vars(Month), my) %>% 
    group_by(Month) %>% 
    summarize(live = n())
  
  prop_live_cat <- inner_join(outcomes_cat_21,
                              live_outcomes_cat_21,
                              by = "Month"
  ) %>% 
    mutate(proportion_live = live / n)
  
  cat_line_21 <- prop_live_cat %>% 
    ggplot(aes(x = Month, y = proportion_live)) +
    geom_line(col = "blue", size = 1.2) + 
    geom_hline(yintercept = 0.9, linetype = 2)+
    theme_minimal() +
    labs(y = "Proportion of Live Outcomes",
         x = "Month") +
    scale_x_date(breaks = unique(prop_live_cat$Month), date_labels = fy_months[1:8])
  
  
  