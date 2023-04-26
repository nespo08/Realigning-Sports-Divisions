## PURPOSE: Preprocess all data

# Load libraries ----------------------------------------------------------

library(tidyverse)

# Load stadiums.csv from Kaggle and create functions to preprocess data --------

# Contains long/lat of all pro sports stadiums, as well as division information
stadium_data <- data.frame(read.csv("Data/stadiums.csv"))
n_rows <- length(stadium_data$Team)
n_cols <- length(stadium_data)
colnames(stadium_data)

# Convert from degrees to radians
deg2rad <- function(deg) {
  (deg * pi) / (180)
}

# Convert from long/lat to Cartesian coordinates - easier to utilize in calculations
to_cartesian <- function(lat, long){
  # Average radius from https://en.wikipedia.org/wiki/Earth_radius
  earth_radius <- 3959 
  cartesian_coords <- matrix(NA, n_rows, 2)
  
  # Formulas from https://sciencing.com/calculate-antipode-6170267.html
  cartesian_coords[,1] <- earth_radius * cos(lat) * cos(long)
  cartesian_coords[,2] <- earth_radius * cos(lat) * sin(long)
  
  return(cartesian_coords)
}

# Convert from lat/long to Cartesian coordinates --------------------------

# First, convert from degrees to radians for lat/long
stadium_data <- stadium_data %>%
  mutate(Lat_Rad = deg2rad(Lat)) %>%
  mutate(Long_Rad = deg2rad(Long))

# Then, convert from lat/long to Cartesian coordinates and append to dataframe
cartesian <- to_cartesian(stadium_data$Lat_Rad, stadium_data$Long_Rad)

stadium_data <- stadium_data %>%
  mutate(Cartesian_X = cartesian[,1]) %>%
  mutate(Cartesian_Y = cartesian[,2])


# Other preprocessing ------------------------------------------------------

# Create empty variable for new division alignments
stadium_data <- stadium_data %>%
  mutate(New_Division = NA)

stadium_data <- stadium_data %>%
  mutate(New_Division_Alt = NA)

# Remove MLS - Split only by conference not by regional divsion
stadium_data <- stadium_data %>%
  filter(League != "MLS")

# Write dataframe as an RDS file ------------------------------------------
write_rds(stadium_data, "Data/stadium_data.rds", compress = "gz")
