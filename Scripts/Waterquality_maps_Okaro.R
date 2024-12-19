# Waterquality maps data Okaro
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
packages <- c("rLakeAnalyzer","readr", "readxl", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# 1. Import the data sets ------------------------------------------------------
Bathymetry_Okaro2_0 <- read_csv("Data_raw/Bathymetry_Okaro2.0.csv")

Dosings_Okaro_BOPRC <- read_csv("Data_raw/Dosings_Okaro_BOPRC.csv")

# Okro water qulity from all waq data bop
Okaro_wq <- read_excel("Data_raw/Okaro_wq.xlsx")

# Okro water qulity from all ctd data bop
Okaro_CTD <- read_excel("Data_raw/Okaro_CTD.xlsx")

Okaro_CTD_bouy <- list.files(path = "Data_raw/Okaro_bouy_profiles", pattern = "\\.csv$", full.names = TRUE)
Okaro_CTD_bouy <- bind_rows(lapply(Okaro_CTD_bouy, read.csv)) # make it into a DF

Hobo_combined_data <- read_csv("Data_mod/Hobo_combined_data.csv")
Hobo_combined_data <- Hobo_combined_data %>% rename(DOconc = DO_mg_L,TmpWtr = Temp_C)

HOBO_Info <- read_excel("Data_raw/Data_Reefs_Okaro.xlsx", sheet = "HOBO_Info")

# 2. Data cleaning -------------------------------------------------------------
# Combine the castings from the bouy and the council in one df
names(Okaro_CTD_bouy)
names(Okaro_CTD)

Okaro_CTD_bouy$DateTime <- as_datetime(Okaro_CTD_bouy$DateTime)
Okaro_CTD$Time <- as_datetime(Okaro_CTD$Time)

# Rename columns in Okaro_CTD_bouy
Okaro_CTD <- Okaro_CTD %>%
  rename("DateTime" = "Time",
         "DptSns" = "Depth (m)",
         "TmpWtr" = "Water Temp (degC)",
         "DOconc" = "DO (g/m^3)",
         "DOpsat" = "DO sat (%)",
         "Cond"   = "SpC (uS/cm)",
         "SpCond" = "Sp Cond (uS/cm)",
         "TurbRT" = "Turbidity NTU (_NTU)")

# Merge the two datasets
Okaro_CTD_all <- bind_rows(Okaro_CTD_bouy, Okaro_CTD) %>%
  arrange(DateTime)

Okaro_CTD_all <- Okaro_CTD_all %>%
  mutate(Date = as.Date(DateTime),
         Time = format(as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"),
         Day = day(Date),
         Month = month(Date, label = TRUE, abbr = TRUE),
         Year = year(Date),
         Season = case_when(
           Month %in% c("Dec", "Jan", "Feb") ~ "Summer",
           Month %in% c("Mar", "Apr", "May") ~ "Autumn",
           Month %in% c("Jun", "Jul", "Aug") ~ "Winter",
           Month %in% c("Sep", "Oct", "Nov") ~ "Spring"))


# Remove rows without depth and temperature values
Okaro_CTD_all <- Okaro_CTD_all %>%
  filter(!is.na(DptSns))%>%
  filter(!is.na(TmpWtr))

# Make cast ID for each cast based on the reset of the depth
Okaro_CTD_all <- Okaro_CTD_all %>%
  arrange(DateTime) %>%  # Ensure the data is ordered by Date and Time
  mutate(cast_id = cumsum(DptSns < lag(DptSns, default = first(DptSns)))+ 1)%>%
  mutate(cast_id = as.factor(cast_id))

# calcualte Schmitt sability
calculate_schmidt <- function(data, bathymetry) {
  schmidt_values <- data %>%
    group_by(cast_id) %>%
    summarise(schmidt_value = schmidt.stability(
      wtr = TmpWtr,
      depths = DptSns,
      bthA = bathymetry$areas,
      bthD = bathymetry$depths
    ), .groups = 'drop') # Added `.groups = 'drop'` to avoid issues with grouping
  return(schmidt_values)
}

# Apply the function to your dataset
schmidt_results <- calculate_schmidt(Okaro_CTD_all, Bathymetry_Okaro2_0)

# Merge the Schmidt Stability values back into the original data frame
Okaro_CTD_all <- Okaro_CTD_all %>%
  left_join(schmidt_results, by = "cast_id")

plot(Okaro_CTD_all$DateTime, Okaro_CTD_all$schmidt_value)



# calculate thermocline
# Step 1: Calculate the thermocline depth for each cast_id
Thermo_Okaro <- Okaro_CTD_all %>%
  arrange(cast_id, DptSns) %>%
  distinct(cast_id, DptSns, TmpWtr, .keep_all = TRUE) %>%
  group_by(cast_id) %>%
  summarise(thermo_depth = thermo.depth(wtr = TmpWtr, depths = DptSns, seasonal = FALSE))

# Step 2: Join the thermo_depth back to the original data and separate into above and below thermocline
Okaro_CTD_all <- Okaro_CTD_all %>%
  left_join(Thermo_Okaro, by = "cast_id") %>%
  mutate(Position = ifelse(DptSns <= thermo_depth, "Above", "Below"))

# Step 3: Calculate means and SEs above and below the thermocline
meanvalues_Okaro <- Okaro_CTD_all %>%
  group_by(cast_id, Position) %>%
  summarise(
    mean_TmpWtr = mean(TmpWtr, na.rm = TRUE),
    mean_DOconc = mean(DOconc, na.rm = TRUE),
    mean_DOpsat = mean(DOpsat, na.rm = TRUE),
    mean_Cond = mean(Cond, na.rm = TRUE),
    mean_SpCond = mean(SpCond, na.rm = TRUE),
    mean_pH = mean(pH, na.rm = TRUE),
    mean_ORP = mean(ORP, na.rm = TRUE),
    mean_TurbRT = mean(TurbRT, na.rm = TRUE),
    mean_FlChlr = mean(FlChlr, na.rm = TRUE),
    mean_FlPhyc = mean(FlPhyc, na.rm = TRUE),
    mean_TmpChg = mean(TmpChg, na.rm = TRUE),
    mean_StrsTt = mean(StrsTt, na.rm = TRUE),
    se_TmpWtr = sd(TmpWtr, na.rm = TRUE) / sqrt(n()),
    se_DOconc = sd(DOconc, na.rm = TRUE) / sqrt(n()),
    se_DOpsat = sd(DOpsat, na.rm = TRUE) / sqrt(n()),
    se_Cond = sd(Cond, na.rm = TRUE) / sqrt(n()),
    se_SpCond = sd(SpCond, na.rm = TRUE) / sqrt(n()),
    se_pH = sd(pH, na.rm = TRUE) / sqrt(n()),
    se_ORP = sd(ORP, na.rm = TRUE) / sqrt(n()),
    se_TurbRT = sd(TurbRT, na.rm = TRUE) / sqrt(n()),
    se_FlChlr = sd(FlChlr, na.rm = TRUE) / sqrt(n()),
    se_FlPhyc = sd(FlPhyc, na.rm = TRUE) / sqrt(n()),
    se_TmpChg = sd(TmpChg, na.rm = TRUE) / sqrt(n()),
    se_StrsTt = sd(StrsTt, na.rm = TRUE) / sqrt(n()),) %>%
  left_join(Okaro_CTD_all %>% select(cast_id, Date, Year, Month, Season, thermo_depth) %>% distinct(), by = "cast_id")

summary(meanvalues_Okaro)



# 3. Data analysis -------------------------------------------------------------
ggplot(meanvalues_Okaro, aes(Date,thermo_depth))+
  geom_point()


ggplot(meanvalues_Okaro, aes(x = Date, y = mean_TmpWtr,col = Position)) +
  geom_point(aes()) +
  geom_errorbar(aes(ymin = mean_TmpWtr - se_TmpWtr, ymax = mean_TmpWtr + se_TmpWtr), width = 0.2)
geom_line(col="black")


Okaro_CTD_all <- Okaro_CTD_all %>%
  mutate(Habitat = case_when(TmpWtr >= 10 & TmpWtr <= 21 & DOconc > 5.0 ~ "Optimal", TRUE ~ "Unsuitable"))

ggplot(Okaro_CTD_all, aes(Habitat, DptSns))+
  geom_point()

cast1 <- Okaro_CTD_all %>% filter(cast_id==1)
Bathymetry_Okaro2_0

# Habitat classification
cast1 <- cast1 %>%
  mutate(Habitat = case_when(TmpWtr >= 10 & TmpWtr <= 21 & DOconc > 5.0 ~ "Optimal", TRUE ~ "Unsuitable"))

# Map depths to the bathymetry data
habitat_with_areas <- habitat_data %>%
  mutate(
    depth_bin = case_when(
      DptSns >= 0 & DptSns < 5 ~ 0,
      DptSns >= 5 & DptSns < 10 ~ -5,
      DptSns >= 10 & DptSns < 15 ~ -10,
      DptSns >= 15 & DptSns <= 18 ~ -15,
      TRUE ~ NA_real_
    )
  ) %>%
  left_join(bathymetry, by = c("depth_bin" = "depths"))

# Summarize total area for each habitat type
area_by_habitat <- habitat_with_areas %>%
  group_by(Habitat) %>%
  summarize(total_area = sum(areas, na.rm = TRUE))

# Print the results
print(area_by_habitat)



# 4. Daily mean  -------------------------------------------------------------
# Function to calculate daily mean for each dataset
daily_mean <- function(data) {
  data %>%
    mutate(Date = as.Date(DateTime)) %>%  
    group_by(Date, Site) %>%              
    summarise(
      mean_DOconc  = mean(DOconc , na.rm = TRUE),
      mean_TmpWtr = mean(TmpWtr , na.rm = TRUE),
      .groups = 'drop')}

# Calculate daily means for each site
daily_means_Hobo <- daily_mean(Hobo_combined_data)
daily_means_CTD <- daily_mean(Okaro_CTD_all)

Hobo_combined_data$DateTime <- as.POSIXct(Hobo_combined_data$DateTime)
daily_means_Hobo$Date <- as.POSIXct(daily_means_Hobo$Date)
daily_means_CTD$Date <- as.POSIXct(daily_means_CTD$Date)

ggplot() +
  geom_line(data = Hobo_combined_data, aes(x = DateTime, y = DOconc, color = Site), alpha = 0.6) +
  geom_line(data = daily_means_Hobo, aes(x = Date, y = mean_DOconc, color = Site), linewidth = 1.2) +
  geom_line(data = Hobo_combined_data, aes(x = DateTime, y = TmpWtr, color = Site), alpha = 0.6) +
  geom_line(data = daily_means_Hobo, aes(x = Date, y = mean_TmpWtr, color = Site), linewidth = 1.2) +
  geom_point(data= HOBO_Info, aes(Date_in, DO_mgl))+
  geom_point(data= HOBO_Info, aes(Date_in, Temperature ))+
  labs(title = "DO Levels & Water temperature",x = "Date",y = "Value",color = "Site") +
  theme_bw()

ggplot() +
  geom_line(data = daily_means_CTD, aes(x = Date, y = mean_DOconc), linewidth = 1.2) +
  geom_line(data = daily_means_CTD, aes(x = Date, y = mean_TmpWtr), linewidth = 1.2) +
  labs(title = "DO Levels & Water temperature",x = "Date",y = "Value")+
  theme_bw()

  

