# ==============================================================================
# Script: 00_create_powiats_dictionary.R
# Author: Maciej Kubica / Spatial Masters Thesis
# Purpose: Build a master reference dictionary for all 380 poviats in Poland.
#          Includes 7-digit TERYT codes, 4-digit codes, official BDL names,
#          SMUP names, Voivodeship names, and flags/ranks for the 10 pairs
#          of duplicate powiat names across different voivodeships.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(stringi)
})

cat(">>> Building Master Powiats Dictionary (380 units)...\n")

# Paths
project_root <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()
raw_dir      <- file.path("Data", "Raw_18_10_2026")
dict_dir     <- file.path("Data", "Dictionary")

if (!dir.exists(dict_dir)) dir.create(dict_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Authoritative 380 poviats from GUS BDL
# ------------------------------------------------------------------------------
bdl_file <- file.path(raw_dir, "Mean_Wage_2024_Powiat.xlsx")
wage_raw <- read_excel(bdl_file, sheet = "TABLICA", skip = 2)
names(wage_raw) <- c("teryt_powiat", "nazwa_bdl", "wage_2024")

# ------------------------------------------------------------------------------
# 2. Extract Voivodeships & Powiat names from SIO registry
# ------------------------------------------------------------------------------
sio_file <- file.path(raw_dir, "List_of_Schools_2025_School.csv")
sio_raw <- read_csv(sio_file, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

proper_woj_map <- c(
  "DOLNOŚLĄSKIE"        = "Dolnośląskie",
  "KUJAWSKO-POMORSKIE"  = "Kujawsko-Pomorskie",
  "LUBELSKIE"           = "Lubelskie",
  "LUBUSKIE"            = "Lubuskie",
  "ŁÓDZKIE"             = "Łódzkie",
  "MAŁOPOLSKIE"         = "Małopolskie",
  "MAZOWIECKIE"         = "Mazowieckie",
  "OPOLSKIE"            = "Opolskie",
  "PODKARPACKIE"        = "Podkarpackie",
  "PODLASKIE"           = "Podlaskie",
  "POMORSKIE"           = "Pomorskie",
  "ŚLĄSKIE"             = "Śląskie",
  "ŚWIĘTOKRZYSKIE"      = "Świętokrzyskie",
  "WARMIŃSKO-MAZURSKIE" = "Warmińsko-Mazurskie",
  "WIELKOPOLSKIE"       = "Wielkopolskie",
  "ZACHODNIOPOMORSKIE"  = "Zachodniopomorskie"
)

sio_pows <- sio_raw %>%
  mutate(
    teryt_powiat = paste0(sprintf("%04d", as.integer(idTerytPowiat)), "000"),
    teryt_wojewodztwo = sprintf("%02d", as.integer(idTerytWojewodztwo)),
    wojewodztwo = proper_woj_map[Wojewodztwo]
  ) %>%
  distinct(teryt_powiat, teryt_wojewodztwo, wojewodztwo, powiat_sio = Powiat)

# ------------------------------------------------------------------------------
# 3. Combine and Standardize Names
# ------------------------------------------------------------------------------
dict_df <- wage_raw %>%
  select(teryt_powiat, nazwa_bdl) %>%
  left_join(sio_pows, by = "teryt_powiat") %>%
  mutate(
    teryt_powiat_4 = substr(teryt_powiat, 1, 4),
    # BDL uses "Powiat m. Wałbrzych od 2013", SMUP uses "Powiat m. Wałbrzych"
    nazwa_smup = str_replace(nazwa_bdl, " od 2013", "")
  )

# ------------------------------------------------------------------------------
# 4. Identify & Disambiguate the 10 Duplicate Names
#    In SMUP, alphabetical order breaks ties via ascending TERYT code.
# ------------------------------------------------------------------------------
dict_df <- dict_df %>%
  group_by(nazwa_smup) %>%
  mutate(
    is_duplicate_name = n() > 1,
    duplicate_order   = if_else(is_duplicate_name, as.integer(rank(teryt_powiat)), NA_integer_)
  ) %>%
  ungroup() %>%
  select(
    teryt_powiat,
    teryt_powiat_4,
    teryt_wojewodztwo,
    wojewodztwo,
    nazwa_bdl,
    nazwa_smup,
    powiat_sio,
    is_duplicate_name,
    duplicate_order
  ) %>%
  arrange(teryt_powiat)

# ------------------------------------------------------------------------------
# 5. Validation Assertions
# ------------------------------------------------------------------------------
stopifnot("Dictionary must have exactly 380 rows" = nrow(dict_df) == 380)
stopifnot("All TERYT codes must be unique" = n_distinct(dict_df$teryt_powiat) == 380)
stopifnot("No NA in TERYT codes" = sum(is.na(dict_df$teryt_powiat)) == 0)
stopifnot("No NA in Voivodeship" = sum(is.na(dict_df$wojewodztwo)) == 0)
stopifnot("Exactly 10 duplicate pairs (20 poviats)" = sum(dict_df$is_duplicate_name) == 20)


# ------------------------------------------------------------------------------
# 6. Save outputs
# ------------------------------------------------------------------------------
csv_out <- file.path(dict_dir, "powiats_dictionary.csv")
rds_out <- file.path(dict_dir, "powiats_dictionary.rds")

write_csv(dict_df, csv_out)
saveRDS(dict_df, rds_out)
