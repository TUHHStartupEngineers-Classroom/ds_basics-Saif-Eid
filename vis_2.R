library(tidyverse)
library(scales)
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
setDT(covid_data_tbl)
sac <- covid_data_tbl[location == "Germany"]


covid_europe <- covid_data_tbl[location == "Germany"] %>% 
  mutate(year_month = floor_date(date, "months") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(cases = total_cases) %>% 
  ungroup()

covid_europe %>%
  
  ggplot(aes(year_month, cases)) +
  geom_smooth(method = "loess", span = 0.1)

######

covid_europe_1 <- covid_data_tbl[location == "France"] %>% 
  mutate(year_month = floor_date(date, "months") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(covid = total_cases) %>% 
  ungroup()

covid_europe_1 %>%  
  
  ggplot(aes(year_month, covid)) +
  geom_smooth(method = "loess", span = 0.1)

######

covid_europe_2 <- covid_data_tbl[location == "Spain"] %>% 
  mutate(year_month = floor_date(date, "months") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(covid = total_cases) %>% 
  ungroup()

covid_europe_2 %>%  
  
  ggplot(aes(year_month, covid)) +
  geom_smooth(method = "loess", span = 0.1)

#####

covid_europe_3 <- covid_data_tbl[location == "United Kingdom"] %>% 
  mutate(year_month = floor_date(date, "months") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(covid = total_cases) %>% 
  ungroup()



#####

covid_europe_4 <- covid_data_tbl[location == "United States"] %>% 
  mutate(year_month = floor_date(date, "months") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(covid = total_cases) %>% 
  ungroup()

covid_europe_4 <- covid_data_tbl[location == "United States"] %>% 
  mutate(year_month = floor_date(date, "months") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(covid = total_cases) %>% 
  ungroup()

 covid_europe_4 %>%  
  
  ggplot(aes(year_month, covid)) +
  geom_smooth(method = "loess", span = 0.1)

covid_europe_4 %>% 
  left_join(covid_europe_3) %>% 
  left_join(covid_europe_2) %>%
  ggplot(aes(year_month, covid)) +
  geom_smooth(method = "loess", span = 0.1)

p2 <- ggplot(covid_data_tbl[covid_data_tbl$location == "Germany",], aes(x=date, y=total_cases)) + geom_line(color = "Blue",size = 1) + theme_dark() 