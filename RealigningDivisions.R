## PURPOSE: Use clustering techniques to realign divisions, minimizing the travel distance for teams

# THIS ALGORTIHM GROUPS TEAMS BY HAVING RANKING THEIR DISTANCES TO DIFFERENT CENTERS

# Load libraries and data -------------------------------------------------

library(usmap)
library(maptools)
library(ggplot2)
library(rgdal)
library(tidyverse)
library(kableExtra)

set.seed(4)

new_stadium_data <- read_rds("Data/stadium_data.rds")


# Haversine formula for distance (function) -----------------------------------------

haversine <- function(long1, long2, lat1, lat2){
  # Haversine formula
  dlon <- long2 - long1
  dlat <- lat2 - lat1
  a <- sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
  
  c <- 2 * asin(sqrt(a))
  
  # Radius of Earth in miles
  r <- 3956
  
  # Calculate the result
  return(c * r)
}

# Display current structure of divisions for each league ------------------

# Want to include current average travel distance per team within their division

average_div_travel <- function(league){
  # Filter by league
  league_data <- new_stadium_data %>%
    filter(League == league)
  
  teams <- league_data$Team
  team_itr <- 1
  avg_travel <- c()
  
  # Iterate over each team and find their average travel distance within their division
  for(currTeam in teams){
    
    # Find division of current team, as well as their divisional opponents
    currTeam_info <- league_data[which(teams == currTeam),]
    div <- currTeam_info$Division
    
    div_opp <- league_data %>%
      filter(Division == div, Team != currTeam)
    div_opp_names <- div_opp$Team
    
    # Find location of current team and compare to each divisional opponent
    currTeam_long <- currTeam_info$Long_Rad
    currTeam_lat <- currTeam_info$Lat_Rad
    temp_dist <- c() # Store travel distances for divisional teams
    
    # Debugging
    # print(currTeam_info$Team)
    
    opp_itr <- 1
    
    for(oppTeam in div_opp$Team){
      
      # Get div opponent's info
      oppTeam_info <- league_data[which(teams == oppTeam),]
      oppTeam_long <- oppTeam_info$Long_Rad
      oppTeam_lat <- oppTeam_info$Lat_Rad
      
      # Find distance (Pythagorean dist) and add to temp storage
      haversine_dist <- haversine(currTeam_long, oppTeam_long, currTeam_lat, oppTeam_lat)
      temp_dist[opp_itr] <- haversine_dist
      
      # Debugging
      # print(paste(oppTeam_info$Team, as.character(haversine_dist)))
      
      opp_itr <- opp_itr + 1
    }
    
    avg_travel[team_itr] <- mean(temp_dist)
    team_itr <- team_itr + 1
  }
  
  # Return for each league, then append to vector of length 124, then add to dataframe in order
  return(avg_travel)
}

# Find current average travel distance for each team
leagues <- unique(new_stadium_data$League)
current_travel_dist <- c()
itr <- 1

for(league in leagues){
  current_travel_dist <- append(current_travel_dist, average_div_travel(league))
  itr <- itr + 1
}

# Append to original dataframe
new_stadium_data <- new_stadium_data %>%
  mutate(Current_Travel = current_travel_dist)

# Kable table to show current divisional alignment and distances




# K-means clustering to minimize dist. ------------------------------------

realign_div <- function(league, num_div){
  realign_data <- new_stadium_data %>%
    filter(League == league)
  num_divisions <- num_div
  
  teams <- realign_data$Team
  teams_per_div <- length(teams) / num_divisions
  
  # We have to use a modified kmeans algorithm to ensure we maintain the current divisional structure
  
  # 1. Find k centers using kmeans (or kmeans++), where k is the number of divisions
  init_kmeans <- kmeans(dplyr::select(realign_data, Lat_Rad, Long_Rad),
                        algorithm = "Lloyd", 
                        centers = num_divisions, 
                        nstart = 30)
  centers <- init_kmeans$centers
  
  # Store numbers of each cluster
  cluster_counts <- table(as.data.frame(init_kmeans$cluster))
  
  # First, find the closest center for each team and it's distance (ignore div. size for now)
  closest_center <- c()
  closest_center_dist <- c()
  dist_pairings <- vector(mode = 'list', length = length(teams))
  team_itr <- 1
  
  for(currTeam in teams){
    # Team information
    currTeam_info <- realign_data[which(teams == currTeam),]
    
    # Debugging 
    # print(currTeam)
    
    # Find location of current team and compare to each divisional opponent
    currTeam_long <- currTeam_info$Long_Rad
    currTeam_lat <- currTeam_info$Lat_Rad
    
    # Get first center's info
    cen_lat <- centers[1,1]
    cen_long <- centers[1,2]
    min_cen <- 1
    min_dist <- haversine(currTeam_long, cen_long, currTeam_lat, cen_lat)
    
    # Store list of 6 (center, center distance) for each team, ordered from min dist to max
    cen_dist_pairs <- vector(mode = 'list', length = num_divisions)
    
    # Fill list for this team
    cen_dist_pairs[[1]] <- c(min_cen, min_dist)
    
    # Check each kmeans center, excluding the first
    for(c in 2:num_divisions){
      
      # Debugging 
      # print(paste("c:", as.character(c)))
      
      # Get current center's long and lat
      cen_lat <- centers[c,1]
      cen_long <- centers[c,2]
      
      # Find distance and check if less than existing min.
      haversine_dist <- haversine(currTeam_long, cen_long, currTeam_lat, cen_lat)
      
      #if(haversine_dist < min_dist){
      #min_dist <- haversine_dist
      #min_cen <- c
      #}
      
      # Debugging 
      # print(min_dist)
      # print(min_cen)
      
      # Fill list for this team
      cen_dist_pairs[[c]] <- c(c, haversine_dist)
    }
    
    # Store min center and min dist for current team
    # closest_center[team_itr] <- min_cen
    # closest_center_dist[team_itr] <- min_dist
    
    # Sort and store list of pairings
    cen_dist_pairs <- cen_dist_pairs[order(sapply(cen_dist_pairs,'[[',2))]
    dist_pairings[[team_itr]] <- cen_dist_pairs
    team_itr <- team_itr + 1
  }
  
  # Add closest center distance and closest center as columns in existing dataset
  #mlb_data <- mlb_data %>%
  # mutate(Closest_Center = closest_center,
  #     Closest_Center_Dist = closest_center_dist,
  #   Dist_Pairings = dist_pairings)
  realign_data <- realign_data %>%
    mutate(Dist_Pairings = dist_pairings)
  
  
  # Find six subsets of five teams by iterating over each center value and finding the 5 min. distances. Add teams to vector to "check" them off
  center_pairs <- realign_data$Dist_Pairings
  checked_teams <- c()
  order_of_centers <- order(cluster_counts,
                            decreasing = FALSE)
  
  for(c in order_of_centers){
    # Store pairings for comparison
    store_pairings <- vector(mode = 'list', length = length(teams))
    
    for(i in 1:length(teams)){
      temp_pairings <- center_pairs[[i]]
      store_pairings[i] <- temp_pairings[which(sapply(temp_pairings,'[[',1) == c)]
    }
    
    # Find num_divisions min at current c in store_pairings and add teams to checked list
    teams_in_div <- 0
    pair_itr <- 1
    teams_nums <- order(sapply(store_pairings,'[[',2))
    
    while(teams_in_div < teams_per_div){
      if(!(realign_data[teams_nums[pair_itr],]$Team %in% checked_teams)){
        
        # Add team to current division
        checked_teams <- append(checked_teams, realign_data[teams_nums[pair_itr],]$Team)
        realign_data[teams_nums[pair_itr],]$New_Division <- paste(league, as.character(c))
        teams_in_div <- teams_in_div + 1
      }
      else{
        pair_itr <- pair_itr + 1
      }
    }
  }
  
  return(realign_data$New_Division)
  
}

# Find new divisions for each league

mlb_div <- realign_div("MLB", 6)
nfl_div <- realign_div("NFL", 8)
nba_div <- realign_div("NBA", 2)
nhl_div <- realign_div("NHL", 4)

new_divisions <- c()
new_divisions <- append(new_divisions, nba_div)
new_divisions <- append(new_divisions, nfl_div)
new_divisions <- append(new_divisions, nhl_div)
new_divisions <- append(new_divisions, mlb_div)

# Append to original dataframe
new_stadium_data <- new_stadium_data %>%
  mutate(New_Division = new_divisions)

realign_div_2 <- function(league, num_div){
  realign_data <- new_stadium_data %>%
    filter(League == league)
  num_divisions <- num_div
  
  teams <- realign_data$Team
  teams_per_div <- length(teams) / num_divisions
  
  # We have to use a modified kmeans algorithm to ensure we maintain the current divisional structure
  
  # 1. Find k centers using kmeans (or kmeans++), where k is the number of divisions
  init_kmeans <- kmeans(dplyr::select(realign_data, Lat_Rad, Long_Rad),
                        algorithm = "Lloyd", 
                        centers = num_divisions, 
                        nstart = 30)
  centers <- init_kmeans$centers
  
  # Store numbers of each cluster
  cluster_counts <- table(as.data.frame(init_kmeans$cluster))
  
  # First, find the closest center for each team and it's distance (ignore div. size for now)
  closest_center <- c()
  closest_center_dist <- c()
  dist_pairings <- vector(mode = 'list', length = length(teams))
  team_itr <- 1
  
  for(currTeam in teams){
    # Team information
    currTeam_info <- realign_data[which(teams == currTeam),]
    
    # Debugging 
    # print(currTeam)
    
    # Find location of current team and compare to each divisional opponent
    currTeam_long <- currTeam_info$Long_Rad
    currTeam_lat <- currTeam_info$Lat_Rad
    
    # Get first center's info
    cen_lat <- centers[1,1]
    cen_long <- centers[1,2]
    min_cen <- 1
    min_dist <- haversine(currTeam_long, cen_long, currTeam_lat, cen_lat)
    
    # Store list of 6 (center, center distance) for each team, ordered from min dist to max
    cen_dist_pairs <- vector(mode = 'list', length = num_divisions)
    
    # Fill list for this team
    cen_dist_pairs[[1]] <- c(min_cen, min_dist)
    
    # Check each kmeans center, excluding the first
    for(c in 2:num_divisions){
      
      # Debugging 
      # print(paste("c:", as.character(c)))
      
      # Get current center's long and lat
      cen_lat <- centers[c,1]
      cen_long <- centers[c,2]
      
      # Find distance and check if less than existing min.
      haversine_dist <- haversine(currTeam_long, cen_long, currTeam_lat, cen_lat)
      
      #if(haversine_dist < min_dist){
      #min_dist <- haversine_dist
      #min_cen <- c
      #}
      
      # Debugging 
      # print(min_dist)
      # print(min_cen)
      
      # Fill list for this team
      cen_dist_pairs[[c]] <- c(c, haversine_dist)
    }
    
    # Store min center and min dist for current team
    # closest_center[team_itr] <- min_cen
    # closest_center_dist[team_itr] <- min_dist
    
    # Sort and store list of pairings
    cen_dist_pairs <- cen_dist_pairs[order(sapply(cen_dist_pairs,'[[',2))]
    dist_pairings[[team_itr]] <- cen_dist_pairs
    team_itr <- team_itr + 1
  }
  
  # Add closest center distance and closest center as columns in existing dataset
  #mlb_data <- mlb_data %>%
  # mutate(Closest_Center = closest_center,
  #     Closest_Center_Dist = closest_center_dist,
  #   Dist_Pairings = dist_pairings)
  realign_data <- realign_data %>%
    mutate(Dist_Pairings = dist_pairings)
  
  
  # Find six subsets of five teams by iterating over each center value and finding the 5 min. distances. Add teams to vector to "check" them off
  center_pairs <- realign_data$Dist_Pairings
  checked_teams <- c()
  order_of_centers <- order(cluster_counts,
                            decreasing = FALSE)
  
  for(c in order_of_centers){
    # Store pairings for comparison
    store_pairings <- vector(mode = 'list', length = length(teams))
    
    for(i in 1:length(teams)){
      temp_pairings <- center_pairs[[i]]
      store_pairings[i] <- temp_pairings[which(sapply(temp_pairings,'[[',1) == c)]
    }
    
    # Find 5 min at current c in store_pairings and add teams to checked list
    teams_in_div <- 0
    pair_itr <- 1
    teams_nums <- order(sapply(store_pairings,'[[',2))
    
    while(teams_in_div < teams_per_div){
      if(!(realign_data[teams_nums[pair_itr],]$Team %in% checked_teams)){
        
        # Add team to current division
        checked_teams <- append(checked_teams, realign_data[teams_nums[pair_itr],]$Team)
        realign_data[teams_nums[pair_itr],]$New_Division_Alt <- paste(league, as.character(c))
        teams_in_div <- teams_in_div + 1
      }
      else{
        pair_itr <- pair_itr + 1
      }
    }
  }
  return(realign_data$New_Division_Alt)
}

# Find new divisions for each league

mlb_div_2 <- realign_div_2("MLB", 6)
nfl_div_2 <- realign_div_2("NFL", 8)
nba_div_2 <- realign_div_2("NBA", 2)
nhl_div_2 <- realign_div_2("NHL", 4)

new_divisions_2 <- c()
new_divisions_2 <- append(new_divisions_2, nba_div_2)
new_divisions_2 <- append(new_divisions_2, nfl_div_2)
new_divisions_2 <- append(new_divisions_2, nhl_div_2)
new_divisions_2 <- append(new_divisions_2, mlb_div_2)

# Append to original dataframe
new_stadium_data <- new_stadium_data %>%
  mutate(New_Division_Alt = new_divisions_2)

new_stadium_data %>%
  filter(New_Division_Alt == "NHL 4")

new_stadium_data %>%
  filter(New_Division == "NFL 2")


# Create tables to display current and new divisions ----------------------

# Current divisions
current_div_table <- new_stadium_data %>%
  group_by(League, Division) %>%
  arrange(League, Division) %>%
  select(League, Team, Division)

kable(current_div_table[1:30,])

write_rds(current_div_table, "Data/current_div_table.rds", compress = "gz")



# Read in info from python script -----------------------------------------

python_travel_data <- read_csv("Data/Python_Travel.csv")

# Update new division values to fit R indexing
python_travel_data <- python_travel_data %>%
  mutate(New_Divisions = New_Divisions + 1)

python_average_div_travel <- function(league){
  # Filter by league
  league_data <- python_travel_data %>%
    filter(League == league)
  
  teams <- league_data$Team
  team_itr <- 1
  avg_travel <- c()
  
  # Iterate over each team and find their average travel distance within their division
  for(currTeam in teams){
    
    # Find division of current team, as well as their divisional opponents
    currTeam_info <- league_data[which(teams == currTeam),]
    div <- c()
    div_opp <- c()
    div_opp_names <- c()
    
    div <- currTeam_info$New_Divisions
    div_opp <- league_data %>%
      filter(New_Divisions == div, Team != currTeam)
    div_opp_names <- div_opp$Team
    
    # Find location of current team and compare to each divisional opponent
    currTeam_long <- currTeam_info$Long_Rad
    currTeam_lat <- currTeam_info$Lat_Rad
    temp_dist <- c() # Store travel distances for divisional teams
    
    # Debugging
    # print(currTeam_info$Team)
    
    opp_itr <- 1
    
    for(oppTeam in div_opp$Team){
      
      # Get div opponent's info
      oppTeam_info <- league_data[which(teams == oppTeam),]
      oppTeam_long <- oppTeam_info$Long_Rad
      oppTeam_lat <- oppTeam_info$Lat_Rad
      
      # Find distance and add to temp storage
      haversine_dist <- haversine(currTeam_long, oppTeam_long, currTeam_lat, oppTeam_lat)
      temp_dist[opp_itr] <- haversine_dist
      
      # Debugging
      # print(paste(oppTeam_info$Team, as.character(haversine_dist)))
      
      opp_itr <- opp_itr + 1
    }
    
    avg_travel[team_itr] <- mean(temp_dist)
    team_itr <- team_itr + 1
  }
  
  # Return for each league, then append to vector of length 124, then add to dataframe in order
  return(avg_travel)
}

# Find current average travel distance for each team
leagues <- unique(python_travel_data$League)
new_travel_dist_python <- c()
itr <- 1

# Python. method
for(league in leagues){
  new_travel_dist_python <- append(new_travel_dist_python, python_average_div_travel(league))
  itr <- itr + 1
}

new_stadium_data <- new_stadium_data %>%
  mutate(Python_Divisions = python_travel_data$New_Divisions)

write_rds(new_stadium_data, "Data/division_information.rds", compress = "gz")


# Calculate changes in average travel for each team -----------------------


new_average_div_travel <- function(league, alt){
  # Filter by league
  league_data <- new_stadium_data %>%
    filter(League == league)
  
  teams <- league_data$Team
  team_itr <- 1
  avg_travel <- c()
  
  # Iterate over each team and find their average travel distance within their division
  for(currTeam in teams){
    
    # Find division of current team, as well as their divisional opponents
    currTeam_info <- league_data[which(teams == currTeam),]
    div <- c()
    div_opp <- c()
    div_opp_names <- c()
    
    if(alt){
      div <- currTeam_info$New_Division_Alt
      div_opp <- league_data %>%
        filter(New_Division_Alt == div, Team != currTeam)
    }
    else{
      div <- currTeam_info$New_Division
      div_opp <- league_data %>%
        filter(New_Division == div, Team != currTeam)
    }
    
    div_opp_names <- div_opp$Team
    
    
    # Find location of current team and compare to each divisional opponent
    currTeam_long <- currTeam_info$Long_Rad
    currTeam_lat <- currTeam_info$Lat_Rad
    temp_dist <- c() # Store travel distances for divisional teams
    
    # Debugging
    # print(currTeam_info$Team)
    
    opp_itr <- 1
    
    for(oppTeam in div_opp$Team){
      
      # Get div opponent's info
      oppTeam_info <- league_data[which(teams == oppTeam),]
      oppTeam_long <- oppTeam_info$Long_Rad
      oppTeam_lat <- oppTeam_info$Lat_Rad
      
      # Find distance and add to temp storage
      haversine_dist <- haversine(currTeam_long, oppTeam_long, currTeam_lat, oppTeam_lat)
      temp_dist[opp_itr] <- haversine_dist
      
      # Debugging
      # print(paste(oppTeam_info$Team, as.character(haversine_dist)))
      
      opp_itr <- opp_itr + 1
    }
    
    avg_travel[team_itr] <- mean(temp_dist)
    team_itr <- team_itr + 1
  }
  
  # Return for each league, then append to vector of length 124, then add to dataframe in order
  return(avg_travel)
}

# Find current average travel distance for each team
leagues <- unique(new_stadium_data$League)
new_travel_dist_alt <- c()
new_travel_dist <- c()
itr <- 1

# Alt. method
for(league in leagues){
  new_travel_dist_alt <- append(new_travel_dist_alt, new_average_div_travel(league, TRUE))
  itr <- itr + 1
}

itr <- 1

# Original method
for(league in leagues){
  new_travel_dist <- append(new_travel_dist, new_average_div_travel(league, FALSE))
  itr <- itr + 1
}

# Append to original dataframe
new_stadium_data <- new_stadium_data %>%
  mutate(New_Travel_Alt = new_travel_dist_alt) %>%
  mutate(New_Travel = new_travel_dist) %>%
  mutate(New_Travel_Python = new_travel_dist_python)

# Find percent decrease in travel for each league with each method

# MLB

mlb_travel <- new_stadium_data %>%
  filter(League == "MLB") %>%
  select(Current_Travel, New_Travel, New_Travel_Alt, New_Travel_Python)

# Calculate average for MLB - before and after
mlb_current_avg <- mean(mlb_travel$Current_Travel)
mlb_new_avg <- mean(mlb_travel$New_Travel)
mlb_new_alt_avg <- mean(mlb_travel$New_Travel_Alt)
mlb_new_python_avg <- mean(mlb_travel$New_Travel_Python)

# NFL

nfl_travel <- new_stadium_data %>%
  filter(League == "NFL") %>%
  select(Current_Travel, New_Travel, New_Travel_Alt, New_Travel_Python)

# Calculate average for NFL - before and after
nfl_current_avg <- mean(nfl_travel$Current_Travel)
nfl_new_avg <- mean(nfl_travel$New_Travel)
nfl_new_alt_avg <- mean(nfl_travel$New_Travel_Alt)
nfl_new_python_avg <- mean(nfl_travel$New_Travel_Python)

# NHL

nhl_travel <- new_stadium_data %>%
  filter(League == "NHL") %>%
  select(Current_Travel, New_Travel, New_Travel_Alt, New_Travel_Python)

# Calculate average for NHL - before and after
nhl_current_avg <- mean(nhl_travel$Current_Travel)
nhl_new_avg <- mean(nhl_travel$New_Travel)
nhl_new_alt_avg <- mean(nhl_travel$New_Travel_Alt)
nhl_new_python_avg <- mean(nhl_travel$New_Travel_Python)

# NBA

nba_travel <- new_stadium_data %>%
  filter(League == "NBA") %>%
  select(Current_Travel, New_Travel, New_Travel_Alt, New_Travel_Python)

# Calculate averages - before and after
nba_current_avg <- mean(nba_travel$Current_Travel)
nba_new_avg <- mean(nba_travel$New_Travel)
nba_new_alt_avg <- mean(nba_travel$New_Travel_Alt)
nba_new_python_avg <- mean(nba_travel$New_Travel_Python)



# Print Travel
league_current_avg <- t(cbind(nba_current_avg, nfl_current_avg, nhl_current_avg, mlb_current_avg))
league_new_avg <- t(cbind(nba_new_avg, nfl_new_avg, nhl_new_avg, mlb_new_avg))
league_new_alt_avg <- t(cbind(nba_new_alt_avg, nfl_new_alt_avg, nhl_new_alt_avg, mlb_new_alt_avg))
league_new_python_avg <- t(cbind(nba_new_python_avg, nfl_new_python_avg, nhl_new_python_avg, mlb_new_python_avg))
leagues_out <- unique(new_stadium_data$League)

output_df <- data.frame(League_Names = leagues_out,
                        League_Current_Avg = league_current_avg,
                        League_New_Avg = league_new_avg,
                        League_New_Diff = league_new_avg - league_current_avg,
                        League_New_Alt_Avg = league_new_alt_avg,
                        League_New_Alt_Diff = league_new_alt_avg - league_current_avg,
                        League_New_Python_Avg = league_new_python_avg,
                        League_New_Python_Diff = league_new_python_avg - league_current_avg)

colnames(output_df) <- c("League", "Current Average", "Method 1", "M1 Change", "Method 2","M2 Change", "Constrained K-Means", "Constrained K-Means Change")
kable(output_df, digits = 4, align = "l")

write_rds(output_df, "Data/travel_numbers.rds", compress = "gz")




# Other distance calculations ---------------------------------------------

# AL East current divisions
al_east_data <- new_stadium_data %>%
  filter(Division == "AL East")

al_east_avg_travel <- mean(al_east_data$Current_Travel)

# AL East Python divisions
al_east_python_data <- new_stadium_data %>%
  filter(League == "MLB",
         Python_Divisions == 3)

al_east_python_avg_travel <- mean(al_east_python_data$New_Travel_Python)

# AL East Rank Method divisions
al_east_rank_method_data <- new_stadium_data %>%
  filter(New_Division == "MLB 3")

al_east_rank_method_avg_travel <- mean(al_east_rank_method_data$New_Travel)

# AL East Alt Method divisions
al_east_alt_method_data <- new_stadium_data %>%
  filter(New_Division_Alt == "MLB 1")

al_east_alt_method_avg_travel <- mean(al_east_alt_method_data$New_Travel_Alt)

