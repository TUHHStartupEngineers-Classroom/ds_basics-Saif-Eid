library(tidyverse)
library(scales)
library(ggplot2)
library(dplyr)

covid_data_tbl_1 <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

tbl_1 <- covid_data_tbl_1 %>% select(date, location, total_deaths, population)
tbl_2 <- tbl_1 %>% filter(date == as.Date("2021-05-04"))
covid_2 <-  tbl_2 %>%
  mutate(mortality = (total_deaths/population)) %>% 
  rename(region = location) %>% 
  mutate(region = case_when(
    
    region == "United Kingdom" ~ "UK",
    region == "United States" ~ "USA",
    region == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ region
    
  )) %>%
  distinct()

world_map <- map_data("world")

combined <- world_map[covid_2$region %in% covid_2$region, ]
combined$mort <- covid_2$mortality[match(combined$region, covid_2$region)]
countries <- unique(combined$region)
CDF <- data.frame(label1=countries)

for(i in CDF){
  combined$mort <- ifelse(combined$region %in% CDF$label1[i], (covid_2$mortality), combined$mort)
}


 ggplot(combined, aes(x=long, y=lat, group=group, fill=mort)) +
  geom_polygon(colour = "white") +
   scale_fill_gradient(low = "red", high = "red4", limits = c(-0.0005, 0.0025), labels = percent) +
  theme_minimal() +
  labs(fill="Mortality Rate (%)", title = "Confirmed COVID-19 deaths relative to the size of the population",
       subtitle = "Around 3 Million confirmed COVID-19 deaths worldwide", x="Challenge 2", y="") +
  scale_y_continuous(breaks = c()) +
  scale_x_continuous(breaks = c()) +
  theme(panel.border = element_blank())