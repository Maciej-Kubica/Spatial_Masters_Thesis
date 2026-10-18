# ==============================================================================
# Script: 01_add_teryt_to_smup.R
# Author: Maciej Kubica / Spatial Masters Thesis
# Purpose: Add official 7-digit GUS TERYT codes, 4-digit codes, and Voivodeship
#          names to all 6 SMUP CSV files, deterministically resolving the 10
#          duplicate county name pairs using the verified ordering rule.
#          Outputs clean individual CSVs and a consolidated wide dataset.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
})

cat(">>> Enriching SMUP datasets with official TERYT codes and Voivodeships...\n")

# Paths
raw_dir    <- file.path("Data", "Raw_18_10_2026")
dict_file  <- file.path("Data", "Dictionary", "powiats_dictionary.rds")
out_dir    <- file.path("Data", "SMUP_with_TERYT")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Load Master Dictionary
# ------------------------------------------------------------------------------
if (!file.exists(dict_file)) {
  cat("Master dictionary not found. Generating now via 00_create_powiats_dictionary.R...\n")
  source(file.path("R", "00_create_powiats_dictionary.R"))
}
dict_df <- readRDS(dict_file)

duplicate_names <- dict_df %>% 
  filter(is_duplicate_name) %>% 
  pull(nazwa_smup) %>% 
  unique()

cat(sprintf("Loaded dictionary with %d poviats. Found %d duplicate name pairs to disambiguate.\n",
            nrow(dict_df), length(duplicate_names)))

# ------------------------------------------------------------------------------
# 2. List the 6 Raw SMUP CSV files
# ------------------------------------------------------------------------------
smup_files <- list.files(raw_dir, pattern = "Powiat\\.csv$", full.names = TRUE)
stopifnot("Expected 6 SMUP files" = length(smup_files) == 6)

# Friendly indicator variable names for consolidated dataset
indicator_name_map <- c(
  "Commuting_Students_2024_Powiat.csv"    = "commuting_students_pct",
  "Free_Lunch_2024_Powiat.csv"            = "free_lunch_pct",
  "Mean_Class_Size_2024_Powiat.csv"       = "mean_class_size",
  "Social_Service_2024_Powiat.csv"        = "social_service_per_1k",
  "Student_Teacher_Ratio_2024_Powiat.csv" = "student_teacher_ratio",
  "Teacher_Experience_2024_Powiat.csv"    = "tenured_teachers_pct"
)

# ------------------------------------------------------------------------------
# 3. Process each SMUP file
# ------------------------------------------------------------------------------
wide_list <- list()

for (sf in smup_files) {
  file_base <- basename(sf)
  var_clean_name <- indicator_name_map[file_base]
  cat(sprintf("\nProcessing: %s ...\n", file_base))
  
  # Read raw file (UTF-8, semicolon delimiter)
  raw_df <- read_delim(sf, delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  
  if (nrow(raw_df) != 380) {
    stop(sprintf("File %s has %d rows instead of expected 380!", file_base, nrow(raw_df)))
  }
  
  # Deterministic tie-breaking for duplicate names
  enriched_df <- raw_df %>%
    mutate(
      nazwa_smup = `Jednostka terytorialna`,
      # Explicitly treat "-", ".", and empty strings as NA (e.g. poviats with 0 licea like łomżyński, suwalski, kaliski)
      clean_val_str = if_else(as.character(Wartość) %in% c("-", ".", "bd.", "brak"), NA_character_, as.character(Wartość)),
      wartosc_num   = as.numeric(str_replace(clean_val_str, ",", "."))
    ) %>%
    group_by(nazwa_smup) %>%
    mutate(
      duplicate_order = if_else(nazwa_smup %in% duplicate_names, as.integer(row_number()), NA_integer_)
    ) %>%
    ungroup() %>%
    left_join(
      dict_df %>% select(teryt_powiat, teryt_powiat_4, teryt_wojewodztwo, wojewodztwo, nazwa_smup, duplicate_order),
      by = c("nazwa_smup", "duplicate_order")
    )
  
  # Quality & integrity assertions
  stopifnot("Row count must remain 380" = nrow(enriched_df) == 380)
  stopifnot("All TERYT codes must be non-NA" = sum(is.na(enriched_df$teryt_powiat)) == 0)
  stopifnot("All 380 TERYT codes must be distinct" = n_distinct(enriched_df$teryt_powiat) == 380)
  
  # Verify duplicate disambiguation against our proven reference
  bielski_check <- enriched_df %>% filter(nazwa_smup == "Powiat bielski")
  stopifnot("Powiat bielski row 1 must be Podlaskie (2003000)" = 
              bielski_check$teryt_powiat[1] == "2003000" && bielski_check$wojewodztwo[1] == "Podlaskie")
  stopifnot("Powiat bielski row 2 must be Śląskie (2402000)" = 
              bielski_check$teryt_powiat[2] == "2402000" && bielski_check$wojewodztwo[2] == "Śląskie")
  
  # Organize column order
  out_df <- enriched_df %>%
    select(
      kod_teryt = teryt_powiat,
      teryt_powiat_4,
      teryt_wojewodztwo,
      wojewodztwo,
      jednostka_terytorialna = `Jednostka terytorialna`,
      wskaznik = `Wskaźnik`,
      wymiar = `Wymiar`,
      pozycja_wymiaru = `Pozycja wymiaru`,
      rok = `Rok`,
      wartosc_oryginalna = `Wartość`,
      wartosc_num
    ) %>%
    arrange(kod_teryt)
  
  # Save enriched individual CSV
  out_csv <- file.path(out_dir, file_base)
  write_csv(out_df, out_csv)
  cat(sprintf("  -> Saved enriched file to: %s [380 rows, 11 cols]\n", out_csv))
  
  # Store clean metric for the consolidated wide table
  wide_list[[var_clean_name]] <- out_df %>%
    select(kod_teryt, !!sym(var_clean_name) := wartosc_num)
}

# ------------------------------------------------------------------------------
# 4. Generate Consolidated Wide SMUP Table (1 row per powiat, 6 metrics)
# ------------------------------------------------------------------------------
cat("\n>>> Building consolidated wide SMUP dataset...\n")

smup_wide <- dict_df %>%
  select(kod_teryt = teryt_powiat, teryt_powiat_4, teryt_wojewodztwo, wojewodztwo, powiat_nazwa = nazwa_smup)

for (var_name in names(wide_list)) {
  smup_wide <- smup_wide %>% left_join(wide_list[[var_name]], by = "kod_teryt")
}

wide_csv <- file.path(out_dir, "SMUP_2024_Powiat_All_Indicators.csv")
wide_rds <- file.path(out_dir, "SMUP_2024_Powiat_All_Indicators.rds")

write_csv(smup_wide, wide_csv)
saveRDS(smup_wide, wide_rds)

cat(">>> Saved consolidated wide SMUP CSV to:", wide_csv, "\n")
cat(">>> Saved consolidated wide SMUP RDS to:", wide_rds, "\n")

cat("\nSample preview of consolidated SMUP data (first 5 poviats):\n")
print(head(smup_wide, 5))

cat("\nSummary of the 10 disambiguated duplicate poviats across indicators:\n")
print(
  smup_wide %>% 
    filter(powiat_nazwa %in% duplicate_names) %>% 
    select(kod_teryt, wojewodztwo, powiat_nazwa, free_lunch_pct, student_teacher_ratio, tenured_teachers_pct) %>%
    arrange(powiat_nazwa, kod_teryt)
)

cat("\n>>> All 6 SMUP files successfully enriched with TERYT codes!\n")
