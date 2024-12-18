# HOBO_data lake Okaro
# Explanation of this script ------------------------------------------------

# 
# 
#
#


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working directory
getwd()
#setwd("~/PhD/Data......")

# Define the list of packages
packages <- c("readr", "readxl", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
DO_Logger_14_reef_122_291024 <- read_csv("Data_raw/DO_Loggers/DO_Logger_14_reef_122_291024.csv", skip = 1)
DO_Logger_14_reef_122_131224 <- read_csv("Data_raw/DO_Loggers/DO_Logger_14_reef_122_131224.csv", skip = 1)

DO_Logger_11_reef_123_291024 <- read_csv("Data_raw/DO_Loggers/DO_Logger_11_reef_123_291024.csv", skip = 1)
DO_Logger_11_reef_123_131224 <- read_csv("Data_raw/DO_Loggers/DO_Logger_11_reef_123_131224.csv", skip = 1)

DO_Logger_3_control_124_291024 <- read_csv("Data_raw/DO_Loggers/DO_Logger_3_control_124_291024.csv", skip = 1)
DO_Logger_3_control_124_131224 <- read_csv("Data_raw/DO_Loggers/DO_Logger_3_control_124_131224.csv", skip = 1)

# Combine and Rename the columns -----------------------------------------------------------
DO_Logger_14_reef_122_291024 <- DO_Logger_14_reef_122_291024 %>% select(ID = 1,DateTime = 2,DO_mg_L = 3,Temp_C = 4) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Pacific/Auckland"),Site = "122 Reef", )
DO_Logger_14_reef_122_131224 <- DO_Logger_14_reef_122_131224 %>% select(ID = 1,DateTime = 2,DO_mg_L = 3,Temp_C = 4) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Pacific/Auckland"),Site = "122 Reef")
Site_122 <- bind_rows(DO_Logger_14_reef_122_291024, DO_Logger_14_reef_122_131224)


DO_Logger_11_reef_123_291024 <- DO_Logger_11_reef_123_291024 %>% select(ID = 1,DateTime = 2,DO_mg_L = 3,Temp_C = 4) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Pacific/Auckland"),Site = "123 Reef")
DO_Logger_11_reef_123_131224 <- DO_Logger_11_reef_123_131224 %>% select(ID = 1,DateTime = 2,DO_mg_L = 3,Temp_C = 4) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Pacific/Auckland"),Site = "123 Reef")
Site_123 <- bind_rows(DO_Logger_11_reef_123_291024, DO_Logger_11_reef_123_131224)

DO_Logger_3_control_124_291024 <- DO_Logger_3_control_124_291024 %>% select(ID = 1,DateTime = 2,DO_mg_L = 3,Temp_C = 4) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Pacific/Auckland"),Site = "124 Control")
DO_Logger_3_control_124_131224 <- DO_Logger_3_control_124_131224 %>% select(ID = 1,DateTime = 2,DO_mg_L = 3,Temp_C = 4) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Pacific/Auckland"),Site = "124 Control")
Site_124 <- bind_rows(DO_Logger_3_control_124_291024, DO_Logger_3_control_124_131224)


# Summarise the data -----------------------------------------------------------
Hobo_combined_data <- bind_rows(Site_122, Site_123, Site_124)

ggplot(Hobo_combined_data, aes(x = DateTime , y = DO_mg_L, color = Site)) +
  geom_line() 

ggplot(Hobo_combined_data, aes(x = DateTime, y = Temp_C, color = Site)) +
  geom_line() 

# Function to calculate daily mean for each dataset
daily_mean <- function(data) {
  data %>%
    mutate(Date = as.Date(DateTime)) %>%  # Extract the date from DateTime
    group_by(Date, Site) %>%              # Group by Date and Site
    summarise(
      DO_mean = mean(DO_mg_L, na.rm = TRUE),
      Temp_mean = mean(Temp_C, na.rm = TRUE),
      .groups = 'drop'
    )
}

# Calculate daily means for each site
daily_means <- daily_mean(Hobo_combined_data)


ggplot(daily_means, aes(x = Date, y = DO_mean, color = Site)) +
  geom_line() 

ggplot(daily_means, aes(x = Date, y = Temp_mean, color = Site)) +
  geom_line() 


# Combine the plots 
Hobo_combined_data$DateTime <- as.POSIXct(Hobo_combined_data$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
daily_means$Date <- as.POSIXct(daily_means$Date, format = "%Y-%m-%d", tz = "UTC")


ggplot() +
  geom_line(data = Hobo_combined_data, aes(x = DateTime, y = DO_mg_L, color = Site), alpha = 0.6) +
  geom_line(data = daily_means, aes(x = Date, y = DO_mean, color = Site), linewidth = 1.2) +
  labs(title = "Dissolved Oxygen (DO) Levels",x = "Date",y = "DO (mg/L)",color = "Site") +
  theme_bw()

ggplot() +
  geom_line(data = Hobo_combined_data, aes(x = DateTime, y = Temp_C, color = Site), alpha = 0.6) +
  geom_line(data = daily_means, aes(x = Date, y = Temp_mean, color = Site), linewidth = 1.2) +
  labs(title = "Water temperature",x = "Date",y = "Temp_C",color = "Site") +
  theme_bw()


# 2. Data_cleaning

# 3. Data_analysis

# 4. Visualization
