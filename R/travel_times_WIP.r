library(sf)
library(osrm)
library(dplyr)

# 1. LOAD AND PREP SUBSET 
df_schools_combined_geo <- read.csv("geocoded_schools.csv")

schools_sf <- st_as_sf(df_schools_combined_geo, coords = c("longitude", "latitude"), crs = 4326)
schools_proj <- st_transform(schools_sf, crs = 2180)

# 2. CREATE A SMALL TEST SUBSET (first 5 schools)
test_schools <- schools_proj[1:5, ]

neighbors_list <- st_is_within_distance(test_schools, dist = 30000)
pairs_df <- data.frame(
  origin_id = rep(test_schools$rspo, lengths(neighbors_list)),
  dest_id = test_schools$rspo[unlist(neighbors_list)]
) %>% filter(origin_id != dest_id)

pairs_df$travel_time_mins <- NA

# Ensure coords are numeric just in case
coords <- df_schools_combined_geo %>% 
  select(rspo, longitude, latitude) %>%
  mutate(
    longitude = as.numeric(gsub(",", ".", longitude)),
    latitude = as.numeric(gsub(",", ".", latitude))
  ) %>%
  as.data.frame()

# 3. PROCESS WITH SMALLER BATCH SIZE
batch_size <- 30  
total_pairs <- nrow(pairs_df)

cat("Starting test for", total_pairs, "pairs...\n")

for(i in seq(1, total_pairs, by = batch_size)) {
  
  end_idx <- min(i + batch_size - 1, total_pairs)
  batch <- pairs_df[i:end_idx, ]
  
  unique_rspos <- unique(c(batch$origin_id, batch$dest_id))
  batch_coords <- coords %>% filter(rspo %in% unique_rspos)
  
  cat(sprintf("Batch %d: Requesting matrix for %d unique locations...\n", i, nrow(batch_coords)))
  
  # Pass ONLY longitude and latitude to OSRM
  res <- osrmTable(loc = batch_coords[, c("longitude", "latitude")])
  
  # Manually assign the rspo IDs to the row and column names of the durations matrix
  rownames(res$durations) <- as.character(batch_coords$rspo)
  colnames(res$durations) <- as.character(batch_coords$rspo)
  
  for(k in 1:nrow(batch)) {
    orig_str <- as.character(batch$origin_id[k])
    dest_str <- as.character(batch$dest_id[k])
    
    # Lookup now works properly with the renamed matrix
    if(orig_str %in% rownames(res$durations) && dest_str %in% colnames(res$durations)) {
      pairs_df$travel_time_mins[i + k - 1] <- res$durations[orig_str, dest_str]
    }
  }
  
  Sys.sleep(1)
}

print(head(pairs_df))