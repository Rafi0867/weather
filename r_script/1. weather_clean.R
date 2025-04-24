# load library and install packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(haven)
library(dplyr)


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
              evap = mean(evaporation))
