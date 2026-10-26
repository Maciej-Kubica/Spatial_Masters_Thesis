library(dplyr)
library(readxl)
library(sf)
library(ggplot2)

## Loading the data
setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Processed")
Main_dataset <- read.csv("schools_with_travel_time_neighbors.csv")

setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Dictionary")
dict <- read.csv("powiats_dictionary.csv")
dict$powiat_cke <- ifelse(substr(dict$powiat_sio, 1, 2) == "m.", substr(dict$powiat_sio, 3, 100), dict$powiat_sio) 

setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Raw_18_10_2026")
Powiaty_Geometry <- readRDS("Geometria_Powiaty.rds")
names(Powiaty_Geometry)[names(Powiaty_Geometry) == 'Kod teryt powiatu'] <- 'kod_teryt'
Powiaty_Geometry$kod_teryt <- as.numeric(Powiaty_Geometry$kod_teryt) * 1000


powiat_df <- Main_dataset %>%
  select(kod_teryt_powiat, bdl_mean_wage_2024, bdl_unemployment_2024, smup_free_lunch_pct,
  smup_commuting_students_pct, smup_mean_class_size, smup_social_service_per_1k, smup_student_teacher_ratio,
  smup_tenured_teachers_pct) %>% 
  distinct()

powiat_df <- powiat_df %>%
  left_join(Powiaty_Geometry[,c(2, 4)], by = c("kod_teryt_powiat" = "kod_teryt"))

powiat_df <- st_as_sf(powiat_df, sf_column_name = "geometry")


sum(duplicated(powiat_df$kod_teryt_powiat)) # Check for duplicates
setdiff(dict$teryt_powiat, powiat_df$kod_teryt_powiat) # Check for missing powiats in the dictionary
#There is no data for Powiat Siedlecki

## Visualization of the powiaty map with the data

# Create the spatial heatmaps

ggplot(powiat_df) +
  geom_sf(aes(fill = smup_free_lunch_pct), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = "Free lunch (%)") +
  theme_void()+
  labs(
    title = "Free Lunch (%)",
    subtitle = "Powiaty in Poland"
  )

  ggplot(powiat_df) +
  geom_sf(aes(fill = bdl_unemployment_2024), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = "Unemployment (%)") +
  theme_void()+
  labs(
    title = "Unemployment (%)",
    subtitle = "Powiaty in Poland"
  )

  