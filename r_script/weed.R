# Load packages
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(tibble)
library(lubridate)
library(patchwork)  # For combining plots

# Read data
data <- read_csv("raw_data/plot1_ndvi.csv") %>%
  mutate(month = month(date),
         year = year(date),
         id = as.factor(id),
         nr_ratio = ndvi/rgvi) %>%
  filter(month >= 4 & month <= 10)



# creating a summary data set
summary <- data %>%
  group_by(id, year, month) %>%
  summarise(avg_ndvi = mean(ndvi, na.rm= TRUE),
            avg_msi = mean(msi),
            avg_ndwi = mean(ndwi),
            avg_rgvi = mean(rgvi),
            avg_nr_ratio = mean(nr_ratio),
            .groups = 'drop')


# # Reshape to long format
# data_long <- pivot_longer(data, cols = c("NDVI", "NDWI", "RGVI", "MSI"),
#                           names_to = "Index", values_to = "Value")


# NDVI only (green line, no legend)
ggplot(data = data, aes(x = date, y = ndvi, color = id)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "NDVI Time-Series for 3 Plots",
       x = "Month", y = "NDVI Value", color = "Plot ID") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


ggplot(data = data%>%filter(year == 2023), aes(x = ndvi, y = rgvi, color = id))+
  geom_point(size = 2)+
  labs(
    title = "2024"
  )+
  theme_minimal()


# Assume you have these four plots defined already:
# p1 = NDVI plot
# p2 = MSI plot
# p3 = RGVI plot
# p4 = NDWI plot

# creating combined graph for all parameters
p1 <- ggplot(summary%>%filter(year == 2023), aes(x = month, y = avg_ndvi, color = id)) +
  geom_line(size = 1) + geom_point() +
  labs(title = "NDVI in 2023", y = "", x = "Month")+
  theme_minimal()

p2 <- ggplot(summary%>%filter(year == 2023), aes(x = month, y = avg_msi, color = id)) +
  geom_line(size = 1) + geom_point() +
  labs(title = "MSI in 2023", y = "", x = "Month")+
  theme_minimal()

p3 <- ggplot(summary%>%filter(year == 2023), aes(x = month, y = avg_rgvi, color = id)) +
  geom_line(size = 1) + geom_point() +
  labs(title = "RGVI in 2023", y = "", x = "Month")+
  theme_minimal()

p4 <- ggplot(summary%>%filter(year == 2023), aes(x = month, y = avg_ndwi, color = id)) +
  geom_line(size = 1) + geom_point() +
  labs(title = "NDWI in 2023", y = "", x = "Month")+
  theme_minimal()

# Combine all four plots in a 2x2 grid layout
combined_plot <- (p1 | p2) / (p3 | p4)

# Print combined plot
print(combined_plot)



# making time series plots
p11 <- ggplot(data = data, aes(x = date, y = ndvi, color = id)) +
          geom_line(size = 0.5) +
          geom_point(size = 1) +
          labs(title = "NDVI Time-Series for 3 Plots",
               x = "Month", y = "", color = "Plot ID") +
          theme_minimal() +
          scale_color_brewer(palette = "Set1")


p12 <- ggplot(data = data, aes(x = date, y = msi, color = id)) +
          geom_line(size = 0.5) +
          geom_point(size = 1) +
          labs(title = "MSI Time-Series for 3 Plots",
               x = "Month", y = "", color = "Plot ID") +
          theme_minimal() +
          scale_color_brewer(palette = "Set1")


p13 <- ggplot(data = data, aes(x = date, y = rgvi, color = id)) +
          geom_line(size = 0.5) +
          geom_point(size = 1) +
          labs(title = "RGVI Time-Series for 3 Plots",
               x = "Month", y = "", color = "Plot ID") +
          theme_minimal() +
          scale_color_brewer(palette = "Set1")


p14 <- ggplot(data = data, aes(x = date, y = ndwi, color = id)) +
          geom_line(size = 0.5) +
          geom_point(size = 1) +
          labs(title = "NDWI Time-Series for 3 Plots",
               x = "Month", y = "", color = "Plot ID") +
          theme_minimal() +
          scale_color_brewer(palette = "Set1")



# Combine all four plots in a 2x2 grid layout
combined_plot2 <- (p11 | p12) / (p13 | p14)

# Print combined plot
print(combined_plot2)




# creating combined graph for all parameters 2022
p21 <- ggplot(summary%>%filter(year == 2022), aes(x = month, y = avg_ndvi, color = id)) +
  geom_line(size = 1) + geom_point() +
  labs(title = "NDVI in 2022", y = "", x = "Month")+
  theme_minimal()

p22 <- ggplot(summary%>%filter(year == 2022), aes(x = month, y = avg_msi, color = id)) +
  geom_line(size = 1) + geom_point() +
  labs(title = "MSI in 2022", y = "", x = "Month")+
  theme_minimal()

p23 <- ggplot(summary%>%filter(year == 2022), aes(x = month, y = avg_rgvi, color = id)) +
  geom_line(size = 1) + geom_point() +
  labs(title = "RGVI in 2022", y = "", x = "Month")+
  theme_minimal()

p24 <- ggplot(summary%>%filter(year == 2022), aes(x = month, y = avg_ndwi, color = id)) +
  geom_line(size = 1) + geom_point() +
  labs(title = "NDWI in 2022", y = "", x = "Month")+
  theme_minimal()

# Combine all four plots in a 2x2 grid layout
combined_plot22 <- (p21 | p22) / (p23 | p24)

# Print combined plot
print(combined_plot22)






















