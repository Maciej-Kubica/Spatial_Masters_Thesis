library(sf)
library(osrm)
library(dplyr)


# 1 PRE-FILTER USING STRAIGHT-LINE DISTANCE (30km)
df_schools_combined_geo <- read.csv("geocoded_schools.csv")

# Convert to spatial object and transform to Polish CRS (EPSG:2180)
schools_sf <- st_as_sf(df_schools_combined_geo, coords = c("longitude", "latitude"), crs = 4326)
schools_proj <- st_transform(schools_sf, crs = 2180)

# Extract ONLY pairs within 30km (30,000 meters)
# NOTE: This returns a list of row indices, not the actual IDs
neighbors_list <- st_is_within_distance(schools_proj, dist = 30000)

# Convert the row indices back to your actual 'rspo' values
pairs_df <- data.frame(
  origin_id = rep(df_schools_combined_geo$rspo, lengths(neighbors_list)),
  dest_id = df_schools_combined_geo$rspo[unlist(neighbors_list)]
)

# Remove self-matches (a school doesn't compete with itself)
pairs_df <- pairs_df %>% filter(origin_id != dest_id)

# 2 FINDING EXACT NEIGHBOURS WITHIN 20 MINS CAR TRAVEL TIME

# Prepare empty travel_time_mins column if not present
pairs_df$travel_time_mins <- NA

# Extract unique list of schools needed for the lookup
coords <- df_schools_combined_geo %>% 
  select(rspo, longitude, latitude)

# Process in chunks of pairs (e.g., 100 pairs at a time)
batch_size <- 100
total_pairs <- nrow(pairs_df)

cat("Starting batch processing for", total_pairs, "pairs...\n")

for(i in seq(1, total_pairs, by = batch_size)) {
  
  end_idx <- min(i + batch_size - 1, total_pairs)
  batch <- pairs_df[i:end_idx, ]
  
  # Get unique points involved in this batch
  unique_rspos <- unique(c(batch$origin_id, batch$dest_id))
  batch_coords <- coords %>% filter(rspo %in% unique_rspos)
  
  # Request matrix for these points from OSRM
  tryCatch({
    res <- osrmTable(loc = batch_coords[, c("rspo", "longitude", "latitude")])
    
    # Extract times from matrix and map back to batch pairs
    for(k in 1:nrow(batch)) {
      orig_str <- as.character(batch$origin_id[k])
      dest_str <- as.character(batch$dest_id[k])
      
      if(orig_str %in% rownames(res$durations) && dest_str %in% colnames(res$durations)) {
        pairs_df$travel_time_mins[i + k - 1] <- res$durations[orig_str, dest_str]
      }
    }
  }, error = function(e) {
    # If batch fails, leave as NA or retry individually
  })
  
  if(i %% 1000 < batch_size) {
    cat(sprintf("Processed %d / %d pairs (%.1f%%)\n", end_idx, total_pairs, (end_idx/total_pairs)*100))
  }
  
  Sys.sleep(1) # A modest pause between large batches is sufficient
}


# 3 COUNT NEIGHBOURS & JOIN TO MAIN DATAFRAME

# Filter to pairs within 20 minutes driving time and count competitors per school
competitors_summary <- pairs_df %>%
  # Convert travel_time_mins (stored as seconds by osrmTable) to minutes (/60)
  mutate(travel_time_mins = travel_time_mins / 60) %>%
  filter(!is.na(travel_time_mins) & travel_time_mins <= 20) %>%
  group_by(origin_id) %>%
  summarise(competitors_20min = n(), .groups = "drop")

# Join competitor counts back to your main dataframe
df_schools_combined_geo <- df_schools_combined_geo %>%
  left_join(competitors_summary, by = c("rspo" = "origin_id")) %>%
  # Replace NA with 0 for schools with zero competitors within 20 mins
  mutate(competitors_20min = ifelse(is.na(competitors_20min), 0, competitors_20min))

# Save the final dataset with competitor counts
write.csv(df_schools_combined_geo, "df_schools_with_competitors.csv", row.names = FALSE)

# Inspect the summary distribution of competitor counts
summary(df_schools_combined_geo$competitors_20min)


#COUNT NEIGHBOURS & JOIN TO MAIN DATAFRAME

# Filter to pairs within 20 minutes driving time and count competitors per school
competitors_summary <- pairs_df %>%
  # Convert travel_time_mins (stored as seconds by osrmTable) to minutes (/60)
  mutate(travel_time_mins = travel_time_mins / 60) %>%
  filter(!is.na(travel_time_mins) & travel_time_mins <= 20) %>%
  group_by(origin_id) %>%
  summarise(competitors_20min = n(), .groups = "drop")

# Join competitor counts back to your main dataframe
df_schools_combined_geo <- df_schools_combined_geo %>%
  left_join(competitors_summary, by = c("rspo" = "origin_id")) %>%
  # Replace NA with 0 for schools with zero competitors within 20 mins
  mutate(competitors_20min = ifelse(is.na(competitors_20min), 0, competitors_20min))

# Save the final dataset with competitor counts
write.csv(df_schools_combined_geo, "df_schools_with_competitors.csv", row.names = FALSE)

# Inspect the summary distribution of competitor counts
summary(df_schools_combined_geo$competitors_20min)