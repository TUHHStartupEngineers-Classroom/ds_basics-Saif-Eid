library(tidyverse)
library(scales)
library(ggplot2)
library(dplyr)
library(data.table)
library(rvest) 
library(magrittr)
library(readr)
library(lubridate)
library (ggmap)

covid_data_tbl_1 <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

main_tbl <- covid_data_tbl_1 %>% select(date, location, total_cases) 
sub_tbl <- main_tbl %>% filter(location == "Germany" | location == "France" | location == "Spain" | location == "United Kingdom" | location == "United States")

plt <- ggplot(sub_tbl, aes(x=date, y = total_cases, group = location, colour = location)) + 
  geom_line() +
  theme_minimal() + 
  labs(
    title = "Covid-19 confirmed cases worldwide",
    subtitle = "As of 04/05/2021",
    x = "Challenge 1",
    y = "Cumulative Cases") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, prefix = "", suffix = "M")) +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%B'%y")) +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_colour_manual(values = c("darkorange1", "blueviolet", "deeppink", "lawngreen", "darkgoldenrod1")) +
  geom_label(label =  "32,686,256",
             vjust = 1, 
             hjust = 0.5,
             size  = 4,
             fill  = "darkgoldenrod1",
             color = "white",
             fontface = "italic",
             data = sub_tbl %>%
               filter(location == "United States" & date >= as.Date("2021-05-08"))) 
plt + theme(
  legend.position = "bottom",
  legend.box = "vertical"
) 