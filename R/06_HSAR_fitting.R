library(dplyr)
library(readxl)
library(sf)
library(ggplot2)
library(spdep)
library(Matrix)
library(HSAR)


setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Processed")
Main_dataset <- read.csv("schools_with_travel_time_neighbors.csv")
setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Raw_18_10_2026")
Powiaty_Geometry <- readRDS("Geometria_Powiaty.rds")

#Working subset of the main dataset
Data <- Main_dataset[, c("rspo", "kod_teryt_powiat", "math_sredni_wynik_pct", "bdl_mean_wage_2024", "bdl_unemployment_2024", "smup_free_lunch_pct", "smup_student_teacher_ratio", "smup_commuting_students_pct", "smup_mean_class_size", "smup_social_service_per_1k", "smup_tenured_teachers_pct", "neighbors_under_20min", "typ_placowki", "latitude", "longitude")]

for(i in 1:ncol(Data)) {
    print(sum(is.na(Data[,i])))
}

for(i in 1:ncol(Data)) {
    Data[,i][is.na(Data[,i])] <- mean(Data[,i], na.rm = TRUE)
}

#Sort by powiat_teryt
Data <- arrange(Data, kod_teryt_powiat)

Powiaty_Geometry$`Kod teryt powiatu` <- as.numeric(Powiaty_Geometry$`Kod teryt powiatu`) * 1000
Powiaty_Geometry <- arrange(Powiaty_Geometry, `Kod teryt powiatu`)
Powiaty_Geometry <- Powiaty_Geometry[-which(Powiaty_Geometry$`Kod teryt powiatu` == 1426000),] #Remove Powiat Siedlecki, which is missing in the dataset

# ==============================================================================
# Implementation of the 3 matrices: Delta, M, and W
# ==============================================================================

# 1. Random effect design matrix: Delta (N x J)
# Links lower-level units (N schools) to higher-level units (J powiats)
unique_powiaty <- Powiaty_Geometry$`Kod teryt powiatu`
powiat_idx <- match(Data$kod_teryt_powiat, unique_powiaty)

Delta <- sparseMatrix(
  i = seq_len(nrow(Data)),
  j = powiat_idx,
  x = 1,
  dims = c(nrow(Data), length(unique_powiaty))
)

# 2. Higher-level spatial weights matrix: M (J x J)
# Powiat-level contiguity matrix, row-standardized and sparse
sf_use_s2(FALSE)
nb_pow <- poly2nb(Powiaty_Geometry)

# Handle isolated powiat (m. Siedlce) resulting from removing Powiat Siedlecki
empty_idx <- which(card(nb_pow) == 0)
if (length(empty_idx) > 0) {
  centroids <- suppressWarnings(st_coordinates(st_centroid(st_geometry(Powiaty_Geometry))))
  for (i in empty_idx) {
    dists <- (centroids[, 1] - centroids[i, 1])^2 + (centroids[, 2] - centroids[i, 2])^2
    dists[i] <- Inf
    nb_pow[[i]] <- as.integer(which.min(dists))
  }
}

mat_pow <- nb2mat(nb_pow, style = "W", zero.policy = FALSE)
M <- as(mat_pow, "dgCMatrix")

# 3. Lower-level spatial weights matrix: W (N x N)
# School-level spatial weights matrix (k-nearest neighbors based on coordinates)
k_schools <- 5
coords <- as.matrix(Data[, c("longitude", "latitude")])

# 1. Find 5 nearest neighbors
knn <- knearneigh(coords, k = k_schools)
nb_schools <- knn2nb(knn)

# 2. Extract actual distances between neighbors
dists <- nbdists(nb_schools, coords)

# 3. Apply inverse distance decay (e.g., 1 / max(d, 0.001))
inv_dists <- lapply(dists, function(d) 1 / pmax(d, 0.001))

# 4. Row-standardize and convert to dgCMatrix
mat_schools <- nb2mat(nb_schools, glist = inv_dists, style = "W")
W <- as(mat_schools, "dgCMatrix")

# ==============================================================================
# 4. Fitting the HSAR model
# ==============================================================================

# Model specification:
# Explained variable: math_sredni_wynik_pct
# Regressors: bdl_mean_wage_2024, bdl_unemployment_2024, smup_free_lunch_pct, smup_student_teacher_ratio
fmla <- math_sredni_wynik_pct ~ bdl_mean_wage_2024 + bdl_unemployment_2024 + 
                                smup_free_lunch_pct + smup_student_teacher_ratio +
                                smup_commuting_students_pct + smup_mean_class_size +
                                smup_social_service_per_1k + smup_tenured_teachers_pct +
                                neighbors_under_20min

# Baseline OLS regression to provide starting beta coefficients
ols_fit <- lm(fmla, data = Data)
summary(ols_fit)

# Initial parameter values for MCMC estimation
pars <- list(
  rho     = 0.5,
  lambda  = 0.5,
  sigma2e = 2.0,
  sigma2u = 2.0,
  betas   = coef(ols_fit)
)

# Run HSAR MCMC estimation
set.seed(123)
hsar_fit <- hsar(
  formula          = fmla,
  data             = Data,
  W                = W,
  M                = M,
  Delta            = Delta,
  burnin           = 1000,
  Nsim             = 2000,
  thinning         = 1,
  parameters.start = pars
)

# Display model summary (posterior means, SDs, spatial parameters rho and lambda, impacts, DIC)
summary(hsar_fit)
