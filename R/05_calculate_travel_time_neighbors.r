# ==============================================================================
# Script: 05_calculate_travel_time_neighbors.r
# Author: Maciej Kubica / Spatial Masters Thesis
# Purpose: Calculate the number of neighboring schools reachable within < 20 minutes
#          driving time for every school in the main dataset using OSRM travel times.
#          Saves the enriched dataset to CSV and RDS.
# ==============================================================================

library(dplyr)
library(readr)

setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Processed")

cat(">>> Step 1: Loading main school dataset and travel time pairs...\n")

# Paths
main_schools_path <- "geocoded_schools.csv"
pairs_path        <- "school_travel_times_pairs.csv"
out_csv_root      <- "schools_with_travel_time_neighbors.csv"
proc_dir          <- file.path("Data", "Processed")
out_csv_proc      <- file.path(proc_dir, "schools_with_travel_time_neighbors.csv")
out_rds_proc      <- file.path(proc_dir, "schools_with_travel_time_neighbors.rds")


df_schools <- read.csv(main_schools_path, stringsAsFactors = FALSE)
df_pairs   <- read.csv(pairs_path, stringsAsFactors = FALSE)


# ------------------------------------------------------------------------------
# Step 2: Calculate the number of neighbors reachable within < 20 minutes
# ------------------------------------------------------------------------------

# A. Count neighbors reachable within strictly < 20 minutes
pairs_under_20 <- df_pairs %>% 
  filter(!is.na(travel_time_mins) & travel_time_mins < 20)

neighbors_20_count <- pairs_under_20 %>%
  group_by(origin_id) %>%
  summarise(
    neighbors_under_20min = n(),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Step 3: Merge back into main school dataset
# ------------------------------------------------------------------------------

df_schools_enriched <- df_schools %>%
  left_join(neighbors_20_count, by = c("rspo" = "origin_id")) %>%
  mutate(
    neighbors_under_20min = coalesce(neighbors_under_20min, 0L)
  )



# ------------------------------------------------------------------------------
# Step 4: Save enriched dataset to CSV and RDS
# ------------------------------------------------------------------------------

write.csv(df_schools_enriched, out_csv_root, row.names = FALSE)
cat("-> Saved root CSV to:", out_csv_root, "\n")


