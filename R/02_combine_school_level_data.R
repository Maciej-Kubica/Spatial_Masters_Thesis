# ==============================================================================
# Script: 02_combine_school_level_data.R
# Author: Maciej Kubica / Spatial Masters Thesis
# Purpose: Load all datasets (CKE, SIO, BDL, SMUP, and Dictionary) into distinct
#          data frames, then combine them into ONE master dataset where each
#          observation is a single school, enriched with both school-level
#          covariates (from SIO) and poviat-level context (from BDL and SMUP).
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(stringr)
})

cat(">>> Step 1: Loading all datasets into distinct data frames...\n")

# Paths
raw_dir      <- file.path("Data", "Raw_18_10_2026")
dict_file    <- file.path("Data", "Dictionary", "powiats_dictionary.rds")
smup_file    <- file.path("Data", "SMUP_with_TERYT", "SMUP_2024_Powiat_All_Indicators.rds")
proc_dir     <- file.path("Data", "Processed")


# ------------------------------------------------------------------------------
# 1. Load Master Dictionary
# ------------------------------------------------------------------------------
cat("Loading Dictionary (380 poviats reference)...\n")
df_dict <- readRDS(dict_file)

# ------------------------------------------------------------------------------
# 2. Load CKE Matura Exam Results (School-level outcomes)
# ------------------------------------------------------------------------------
cat("Loading CKE Matura 2025 Basic Math data...\n")
cke_raw <- read_excel(file.path(raw_dir, "Basic_Math_2025_School.xlsx"), skip = 2)

df_cke_math <- cke_raw %>%
  select(
    id_oke                    = 1,
    wojewodztwo               = 2,
    powiat                    = 3,
    gmina                     = 4,
    typ_gminy                 = 5,
    kod_teryt_gmina           = 6,
    rspo                      = 7,
    nazwa_szkoly              = 8,
    miejscowosc               = 9,
    ulica                     = 10,
    typ_placowki              = 11,
    rodzaj_placowki           = 12,
    czy_publiczna             = 13,
    matura_zdajacy_ogolem     = 14,
    matura_przystapili_ogolem = 15,
    math_liczba_zdajacych     = 87,
    math_zdawalnosc_pct       = 89,
    math_sredni_wynik_pct     = 90,
    math_odchylenie_std       = 91,
    math_mediana              = 92,
    math_modalna              = 93
  ) %>%
  mutate(
    rspo             = as.character(rspo),
    kod_teryt_gmina  = sprintf("%07d", as.integer(kod_teryt_gmina)),
    # Extract 7-digit Poviat TERYT code (first 4 digits of Gmina TERYT + '000')
    kod_teryt_powiat = paste0(substr(kod_teryt_gmina, 1, 4), "000"),
    across(
      c(matura_zdajacy_ogolem, matura_przystapili_ogolem, math_liczba_zdajacych,
        math_zdawalnosc_pct, math_sredni_wynik_pct, 
        math_odchylenie_std, math_mediana, math_modalna),
      ~ as.numeric(str_replace(as.character(.x), ",", "."))
    )
  )

# ------------------------------------------------------------------------------
# 3. Load SIO School Registry (School-level covariates)
# ------------------------------------------------------------------------------
cat("Loading SIO School Registry covariates...\n")
sio_raw <- read_csv(file.path(raw_dir, "List_of_Schools_2025_School.csv"), 
                    locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

df_sio_schools <- sio_raw %>%
  select(
    rspo                        = RSPO,
    sio_ucz_ogolem              = ucz_ogolem,
    sio_ucz_dziewczeta          = `w tym_ucz_dziewczeta`,
    sio_oddzialy                = lb_oddz,
    sio_etat_umowa              = etatUmowa,
    sio_etat_niepelnozatr       = `w tym_etat_niepełnozatr`,
    sio_naucz_osoby             = lb_naucz_osoby,
    sio_naucz_pelnozatr         = `w tym_lb_naucz_pelnozatr`,
    sio_naucz_niepelnozatr      = `w tym_lb_naucz_niepelnozatr`,
    sio_typ_organu_prowadzacego = `Typ organu prowadzącego`,
    sio_organ_prowadzacy        = `Organ prowadzący`
  ) %>%
  mutate(
    rspo = as.character(rspo),
    across(
      c(sio_ucz_ogolem, sio_ucz_dziewczeta, sio_oddzialy, sio_etat_umowa,
        sio_etat_niepelnozatr, sio_naucz_osoby, sio_naucz_pelnozatr, sio_naucz_niepelnozatr),
      ~ as.numeric(as.character(.x))
    )
  )

# ------------------------------------------------------------------------------
# 4. Load GUS BDL Indicators (Poviat-level socioeconomics)
# ------------------------------------------------------------------------------
cat("Loading GUS BDL Wages & Unemployment...\n")
wage_raw <- read_excel(file.path(raw_dir, "Mean_Wage_2024_Powiat.xlsx"), sheet = "TABLICA", skip = 2)
df_bdl_wage <- wage_raw %>%
  select(kod_teryt_powiat = 1, bdl_mean_wage_2024 = 3) %>%
  mutate(
    kod_teryt_powiat   = as.character(kod_teryt_powiat),
    bdl_mean_wage_2024 = as.numeric(bdl_mean_wage_2024)
  )

unemp_raw <- read_excel(file.path(raw_dir, "Unemployment_2024_Powiat.xlsx"), sheet = "TABLICA", skip = 2)
df_bdl_unemployment <- unemp_raw %>%
  select(kod_teryt_powiat = 1, bdl_unemployment_2024 = 3) %>%
  mutate(
    kod_teryt_powiat      = as.character(kod_teryt_powiat),
    bdl_unemployment_2024 = as.numeric(bdl_unemployment_2024)
  )

# ------------------------------------------------------------------------------
# 5. Load SMUP Enriched Indicators (Poviat-level education & social welfare)
# ------------------------------------------------------------------------------
cat("Loading enriched SMUP indicators...\n")
df_smup_all <- readRDS(smup_file) %>%
  select(
    kod_teryt_powiat            = kod_teryt,
    smup_commuting_students_pct = commuting_students_pct,
    smup_free_lunch_pct         = free_lunch_pct,
    smup_mean_class_size        = mean_class_size,
    smup_social_service_per_1k  = social_service_per_1k,
    smup_student_teacher_ratio  = student_teacher_ratio,
    smup_tenured_teachers_pct   = tenured_teachers_pct
  )

# ==============================================================================
# Step 2: Combine all data into ONE single school-level data frame
# ==============================================================================
cat("\n>>> Step 2: Combining all data at single school observation level...\n")

# Base table: CKE schools taking Matura exams
# Left join 1: School-level SIO registry variables (by rspo)
# Left join 2: Poviat-level BDL wages (by kod_teryt_powiat)
# Left join 3: Poviat-level BDL unemployment (by kod_teryt_powiat)
# Left join 4: Poviat-level SMUP educational/social indicators (by kod_teryt_powiat)

df_schools_combined <- df_cke_math %>%
  left_join(df_sio_schools, by = "rspo") %>%
  left_join(df_bdl_wage, by = "kod_teryt_powiat") %>%
  left_join(df_bdl_unemployment, by = "kod_teryt_powiat") %>%
  left_join(df_smup_all, by = "kod_teryt_powiat") %>%
  # Crucial for HSAR: Order strictly by higher-level spatial unit (kod_teryt_powiat)
  arrange(kod_teryt_powiat, rspo)

# Let's keep only high-schools and technical schools for youth that had a least 5 students taking the Matura exam in 2025
df_schools_combined <- df_schools_combined[df_schools_combined$typ_placowki %in% c("liceum", "technikum"), ]
df_schools_combined <- df_schools_combined[df_schools_combined$rodzaj_placowki %in% "dla młodzieży", ]
df_schools_combined <- df_schools_combined[df_schools_combined$math_liczba_zdajacych >= 5, ]



# ------------------------------------------------------------------------------
# Quality & Consistency Assertions
# ------------------------------------------------------------------------------

stopifnot("All schools must have a valid 7-digit Poviat TERYT" = sum(is.na(df_schools_combined$kod_teryt_powiat)) == 0)
stopifnot("All schools must match BDL wages" = sum(is.na(df_schools_combined$bdl_mean_wage_2024)) == 0)
stopifnot("All schools must match BDL unemployment" = sum(is.na(df_schools_combined$bdl_unemployment_2024)) == 0)

cat(sprintf(">>> Combined dataset successfully built: %d schools across %d poviats.\n",
            nrow(df_schools_combined), n_distinct(df_schools_combined$kod_teryt_powiat)))

# ------------------------------------------------------------------------------
# Save Outputs
# ------------------------------------------------------------------------------
out_csv <- file.path(proc_dir, "schools_combined_data.csv")

write_csv(df_schools_combined, out_csv)

cat(">>> Saved CSV to:", out_csv, "\n")

cat("\nPreview of first 3 schools in combined dataset:\n")
print(
  df_schools_combined %>% 
    select(rspo, nazwa_szkoly, typ_placowki, math_sredni_wynik_pct, 
           kod_teryt_powiat, bdl_mean_wage_2024, smup_free_lunch_pct) %>% 
    head(3)
)
