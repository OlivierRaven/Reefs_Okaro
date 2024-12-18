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
packages <- c("readr", "readxl", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# 1. Import the data sets ------------------------------------------------------
Bathymetry_Okaro2_0 <- read_csv("Data_raw/Bathymetry_Okaro2.0.csv")

Okaro_CTD <- read_excel("Data_raw/Okaro_CTD.xlsx")

Okaro_wq <- read_excel("Data_raw/Okaro_wq.xlsx")

# 2. Data cleaning -------------------------------------------------------------

# 3. Data analysis -------------------------------------------------------------

# 4. Visualization -------------------------------------------------------------