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
setDT(covid_data_tbl_1)
total_cases_in_germany <- covid_data_tbl_1[location == "Germany"] %>% 
  mutate(year_month = floor_date(date, "days") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(cases = total_cases) %>% 
  ungroup()
names(total_cases_in_germany) [2] <- "cases_germany"

total_cases_in_france <- covid_data_tbl_1[location == "France"] %>% 
  mutate(year_month = floor_date(date, "days") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(cases = total_cases) %>% 
  ungroup()
names(total_cases_in_france) [2] <- "cases_france"
  
total_cases_in_spain <- covid_data_tbl_1[location == "Spain"] %>% 
  mutate(year_month = floor_date(date, "days") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(cases = total_cases) %>% 
  ungroup()
names(total_cases_in_spain) [2] <- "cases_spain"

total_cases_in_uk <- covid_data_tbl_1[location == "United Kingdom"] %>% 
  mutate(year_month = floor_date(date, "days") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(cases = total_cases) %>% 
  ungroup()
names(total_cases_in_uk) [2] <- "cases_uk"

total_cases_in_us <- covid_data_tbl_1[location == "United States"] %>% 
  mutate(year_month = floor_date(date, "days") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(cases = total_cases) %>% 
  ungroup() 
  names(total_cases_in_us) [2] <- "cases_us"

  cases_tbl <- left_join(total_cases_in_france, total_cases_in_germany, by = "year_month") %>%
    left_join(total_cases_in_spain, by = "year_month") %>%
    left_join(total_cases_in_uk, by = "year_month") %>% 
    left_join(total_cases_in_us, by = "year_month")

  cols <- c("France", 
            "Germany", 
            "Spain", 
            "United Kingdom", 
            "United States")
  
  plot <- cases_tbl %>%
    ggplot(aes()) +
    geom_line(aes(x = year_month, y = cases_france), color = 'darkorange1') + 
    geom_line(aes(x = year_month, y = cases_germany), color = 'blueviolet') + 
    geom_line(aes(x = year_month, y = cases_spain), color = 'deeppink') + 
    geom_line(aes(x = year_month, y = cases_uk), color = 'lawngreen') + 
    geom_line(aes(x = year_month, y = cases_us), color = 'darkgoldenrod1') +
    theme_minimal() + 
    labs(
      title = "Covid-19 confirmed cases worldwide",
      subtitle = "As of 04/05/2021",
      x = "Challenge 1",
      y = "Cumulative Cases",
      color = cols) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, prefix = "", suffix = "M")) +
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%B'%y")) +
    theme(axis.text.x = element_text(angle = 45)) + 
    ####
    
    scale_color_continuous(France = "darkorange1", Germany = "blueviolet", Spain = "deeppink", United_Kingdom = "lawngreen", United_States = "darkgoldenrod1")
    geom_label(data = cases_tbl, 
              select(cases_tbl$year_month >=2021-05-01),
               vjust = -0.5, 
               size  = 5,
               fill  = "#1f78b4",
               color = "black",
               fontface = "italic"
     )
    legend("topright", c("France", "Germany", "Spain", "United Kingdom", "United States"), fill = c("darkorange1", "blueviolet", "deeppink", "lawngreen", "darkgoldenrod1"))
  
  geom_label(label =  "Major Demand This Year",
             vjust = -0.5, 
             size  = 5,
             fill  = "#1f78b4",
             color = "black",
             fontface = "italic",
             data = cases_tbl %>%  
             filter(cases_us == 32512934))
  
  par(mar = c(5,4,4,8),
      xpd = TRUE)
  
  legend("bottom",
         inset = 0.5,
         title = "Continent/Country",
         cols,
         lwd = 2,
         col = c("darkorange1", "blueviolet", "deeppink", "lawngreen", "darkgoldenrod1"),
         lty = c(2,3)
    )
  
  legend("bottom",
         legend = c("France", "Germany", "Spain", "United Kingdom", "United States"),
         pch = 1,
         col = c("darkorange1", "blueviolet", "deeppink", "lawngreen", "darkgoldenrod1")
         )
  
  legend("bottom",  
         inset = c(0, -3),
         legend = c("France", "Germany", "Spain", "United Kingdom", "United States"),
         col = c("darkorange1", "blueviolet", "deeppink", "lawngreen", "darkgoldenrod1"),
         pch = 1:5,
         title = "Continent/Country" )
