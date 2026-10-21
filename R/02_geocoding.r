


library(tidygeocoder)
library(dplyr)

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



library(sf)
library(osrm)


# STEP 2: PRE-FILTER USING STRAIGHT-LINE DISTANCE (20km)


# Convert to spatial object and transform to Polish CRS (EPSG:2180)
schools_sf <- st_as_sf(df_schools_combined_geo, coords = c("longitude", "latitude"), crs = 4326)
schools_proj <- st_transform(schools_sf, crs = 2180)

# Extract ONLY pairs within 20km (20,000 meters)
# NOTE: This returns a list of row indices, not the actual IDs
neighbors_list <- st_is_within_distance(schools_proj, dist = 20000)

# Convert the row indices back to your actual 'id_oke' values
pairs_df <- data.frame(
  origin_id = rep(df_schools_combined_geo$id_oke, lengths(neighbors_list)),
  dest_id = df_schools_combined_geo$id_oke[unlist(neighbors_list)]
)

# Remove self-matches (a school doesn't compete with itself)
pairs_df <- pairs_df %>% filter(origin_id != dest_id)