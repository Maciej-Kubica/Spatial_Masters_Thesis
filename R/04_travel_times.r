library(sf)
library(osrm)
library(dplyr)

setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Processed")
# 1. LOAD AND PREP FULL DATASET 
df_schools_combined_geo <- read.csv("geocoded_schools.csv")

schools_sf <- st_as_sf(df_schools_combined_geo, coords = c("longitude", "latitude"), crs = 4326)
schools_proj <- st_transform(schools_sf, crs = 2180)

# 2. FIND NEIGHBORS WITHIN 30KM FOR THE ENTIRE DATASET
neighbors_list <- st_is_within_distance(schools_proj, dist = 30000)

pairs_df <- data.frame(
  origin_id = rep(df_schools_combined_geo$rspo, lengths(neighbors_list)),
  dest_id = df_schools_combined_geo$rspo[unlist(neighbors_list)]
) %>% filter(origin_id != dest_id)

pairs_df$travel_time_mins <- NA

# 3. PROCESS WITH SMALLER BATCH SIZE
batch_size <- 60
total_pairs <- nrow(pairs_df)

cat("Starting processing for", total_pairs, "pairs...\n")

for(i in seq(1, total_pairs, by = batch_size)) {
  
  end_idx <- min(i + batch_size - 1, total_pairs)
  batch <- pairs_df[i:end_idx, ]
  
  unique_rspos <- unique(c(batch$origin_id, batch$dest_id))
  batch_coords <- coords %>% filter(rspo %in% unique_rspos)
  
  # Wrapping the API call in a tryCatch prevents a multi-hour run from crashing 
  # due to a single dropped internet connection or API timeout
  tryCatch({
    # Pass ONLY longitude and latitude to 
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
  }, error = function(e) {
    cat(sprintf("\nError in batch %d to %d: %s (moving to next batch...)\n", i, end_idx, e$message))
  })
  

  cat(sprintf("Processed %d / %d pairs (%.1f%%)\n", end_idx, total_pairs, (end_idx/total_pairs)*100))

  
  Sys.sleep(1) # Crucial to prevent API rate limits/bans
}

# 4. SAVE MAPPED TRAVEL TIMES TO CSV
cat("Saving results to CSV...\n")
write.csv(pairs_df, "school_travel_times_pairs.csv", row.names = FALSE)
cat("Done! Output saved as 'school_travel_times_pairs.csv'\n")
