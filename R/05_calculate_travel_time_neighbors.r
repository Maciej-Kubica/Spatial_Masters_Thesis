# ==============================================================================
# Script: 05_calculate_travel_time_neighbors.r
# Author: Maciej Kubica / Spatial Masters Thesis
# Purpose: Calculate the number of neighboring schools reachable within < 20 minutes
#          driving time for every school in the main dataset using OSRM travel times.
#          Saves the enriched dataset to CSV and RDS.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

cat(">>> Step 1: Loading main school dataset and travel time pairs...\n")

# Paths
main_schools_path <- "geocoded_schools.csv"
pairs_path        <- "school_travel_times_pairs.csv"
out_csv_root      <- "schools_with_travel_time_neighbors.csv"
proc_dir          <- file.path("Data", "Processed")
out_csv_proc      <- file.path(proc_dir, "schools_with_travel_time_neighbors.csv")
out_rds_proc      <- file.path(proc_dir, "schools_with_travel_time_neighbors.rds")

if (!file.exists(main_schools_path)) {
  stop("Main schools file not found: ", main_schools_path)
}
if (!file.exists(pairs_path)) {
  stop("Travel times pairs file not found: ", pairs_path)
}

df_schools <- read.csv(main_schools_path, stringsAsFactors = FALSE)
df_pairs   <- read.csv(pairs_path, stringsAsFactors = FALSE)

cat(sprintf("Loaded %d schools from '%s'.\n", nrow(df_schools), main_schools_path))
cat(sprintf("Loaded %d travel time pairs from '%s'.\n", nrow(df_pairs), pairs_path))

# ------------------------------------------------------------------------------
# Step 2: Calculate the number of neighbors reachable within < 20 minutes
# ------------------------------------------------------------------------------
cat("\n>>> Step 2: Aggregating travel time metrics per school...\n")

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
cat(">>> Step 3: Merging metrics into main school dataset...\n")

df_schools_enriched <- df_schools %>%
  left_join(neighbors_20_count, by = c("rspo" = "origin_id")) %>%
  mutate(
    neighbors_under_20min = coalesce(neighbors_under_20min, 0L)
  )



# ------------------------------------------------------------------------------
# Step 4: Save enriched dataset to CSV and RDS
# ------------------------------------------------------------------------------
cat("\n>>> Step 4: Saving enriched dataset...\n")

# Save in root directory
write.csv(df_schools_enriched, out_csv_root, row.names = FALSE)
cat("-> Saved root CSV to:", out_csv_root, "\n")

# Save in Data/Processed/
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
write.csv(df_schools_enriched, out_csv_proc, row.names = FALSE)
saveRDS(df_schools_enriched, out_rds_proc)
cat("-> Saved processed CSV to:", out_csv_proc, "\n")
cat("-> Saved processed RDS to:", out_rds_proc, "\n")

cat("\n>>> Script 05 completed successfully!\n")
