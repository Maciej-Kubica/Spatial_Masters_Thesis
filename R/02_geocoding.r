


library(tidygeocoder)
library(dplyr)

setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Processed")

df_schools_combined <- read.csv("schools_combined_data.csv")

# STEP 1: GEOCODE SCHOOLS USING ARCGIS

# Geocode using ArcGIS and adding "Poland" to the search
df_schools_combined_geo <- df_schools_combined %>%
  mutate(
    # Combine everything into a clean search string
    full_address = paste(ulica, miejscowosc, "Poland", sep = ", ")
  ) %>%
  geocode(
    address = full_address, 
    method = 'arcgis',   # Switching from OSM to ArcGIS
    lat = latitude,
    long = longitude
  )

# Save the dataframe to a CSV file in your current working directory
write.csv(df_schools_combined_geo, "geocoded_schools.csv", row.names = FALSE)

