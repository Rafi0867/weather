# load library and install packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(haven)
library(dplyr)
  install.packages("labelled")
library(labelled)

# loading weather data
monthly_weather <- read_excel("raw_data/master.xlsx") %>%
  select(-DateTime) %>%
  rename_all(tolower) %>%
  group_by(division, year, month) %>%
    summarize(rainfall = mean(rainfall),
              acc_rain = mean(`accumulated rainfall`),
              mean_temp = mean(`mean temperature`),
              soil_moist = mean(`soil moisture`),
              low_temp = mean(`min temperature`),
              high_temp = mean(`max temperature`),
              spi = mean(spi),
              evap = mean(evaporation)) %>%
  ungroup()


# visualize the data 
monthly_weather %>% filter(month == 1) %>%
  ggplot()+
  geom_line( aes(x = year, y = rainfall, color = division), linewidth = 1)+
  theme_minimal()

# load monthly ndvi data
monthly_ndvi <- read_dta("raw_data/monthly_ndvi_01to22.dta") %>%
  rename(division = div,
         ndvi = mean) %>%
  mutate(division = as.character(as_factor(division))) %>%
  filter(division != "Mymensingh")


# visualize the data
monthly_ndvi %>% filter(month == 2)%>%
  ggplot()+
  geom_line(aes(x = year , y = ndvi, color = division), linewidth = 1)+
  theme_minimal()


# make a single data set for monthly weather and ndvi
env_ndvi <- inner_join(monthly_weather, monthly_ndvi, by = c("division", "year", "month"))

# calculating the correlation coefficient between two variables
cor(env_ndvi$ndvi, env_ndvi$spi)



ggplot()+
  geom_point(data = env_ndvi%>% filter(division == "Rajshahi"),
             aes(x = ndvi, y = mean_temp, color = year),size = 3)


# creating plots with more inclusive summarizing options and group by functions
env_ndvi %>%
  group_by(division, year) %>%
    summarise(ndvi = min(ndvi)) %>%
    filter(division != "Chittagong", year >= 2010) %>%
      ggplot()+
        geom_line(aes(x = year, y = ndvi, color = division), linewidth = 1)+
        theme_minimal()
