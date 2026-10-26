library(dplyr)
library(readxl)
library(sf)
library(ggplot2)
library(spdep)
library(Matrix)


setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Processed")
Main_dataset <- read.csv("schools_with_travel_time_neighbors.csv")
setwd("C:/Users/Maciej/Desktop/Magisterka/Spatial_Masters_Thesis\\Data\\Raw_18_10_2026")
Powiaty_Geometry <- readRDS("Geometria_Powiaty.rds")

#Working subset of the main dataset
Data <- Main_dataset[, c("rspo", "kod_teryt_powiat", "math_sredni_wynik_pct", "bdl_mean_wage_2024", "bdl_unemployment_2024", "smup_free_lunch_pct", "smup_student_teacher_ratio", "latitude", "longitude")]

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

mat_pow <- nb2mat(nb_pow, style = "W", zero.policy = TRUE)
M <- as(mat_pow, "dgCMatrix")

# 3. Lower-level spatial weights matrix: W (N x N)
# School-level spatial weights matrix (k-nearest neighbors based on coordinates)
coords <- as.matrix(Data[, c("longitude", "latitude")])
k_schools <- 5
nb_schools <- knn2nb(knearneigh(coords, k = k_schools))
mat_schools <- nb2mat(nb_schools, style = "W")
W <- as(mat_schools, "dgCMatrix")
