# This python script will use the constrained kmeans algorithm

import numpy as np
import pandas as pd
from k_means_constrained import KMeansConstrained #by Joshua Levy-Kramer
import plotly.graph_objects as go
from pathlib import Path  

stadium_data = pd.read_csv("Data/stadiums.csv")

# Filter out MLS
stadium_data = stadium_data[stadium_data.League != "MLS"]

# Add columns for long and lat in radians
  stadium_data["Lat_Rad"] = np.deg2rad(stadium_data["Lat"])
  stadium_data["Long_Rad"] = np.deg2rad(stadium_data["Long"])

def realign_div(league):
  league_data = stadium_data[stadium_data.League == league]

  num_divisions = len(pd.unique(league_data.Division))
  num_teams = len(pd.unique(league_data.Team))
  teams_per_div = num_teams / num_divisions

  # Group long and lat measures (both are in radians)
  dist = np.array(league_data[["Lat_Rad", "Long_Rad"]])

  int_kmeans_constrained = KMeansConstrained(n_clusters = num_divisions, 
                            size_min = teams_per_div, 
                            size_max = teams_per_div, 
                            n_init = 30, 
                            random_state = 42)
  int_kmeans_constrained.fit_predict(dist)

  new_div = int_kmeans_constrained.labels_

  return new_div

# Attached new divisions to existing dataframe
mlb_new_div = realign_div("MLB")
nba_new_div = realign_div("NBA")
nfl_new_div = realign_div("NFL")
nhl_new_div = realign_div("NHL")

new_divisions = np.append(nba_new_div, nfl_new_div)
new_divisions = np.append(new_divisions, nhl_new_div)
new_divisions = np.append(new_divisions, mlb_new_div)

stadium_data["New_Divisions"] = new_divisions

# Create tables to visualize new divisions
stadium_data = stadium_data.groupby("League", "New_Divisions")

# MLB
mlb_data = stadium_data[stadium_data.League == "MLB"]
mlb_data = mlb_data.sort_values(['New_Divisions'], ascending=[1])

mlb_table = go.Figure(data=[go.Table(
    header=dict(values=["Team"],
                line_color='darkslategray',
                fill_color='lightskyblue',
                align='left'),
    cells=dict(values=[mlb_data.Team],
               line_color='darkslategray',
               fill_color='lightcyan',
               align='left'))
])

mlb_table.update_layout(width=10000, height=10000)
mlb_table.show()

# NFL
nfl_data = stadium_data[stadium_data.League == "NFL"]
nfl_data = nfl_data.sort_values(['New_Divisions'], ascending=[1])

nfl_table = go.Figure(data=[go.Table(
    header=dict(values=["Team"],
                line_color='darkslategray',
                fill_color='lightskyblue',
                align='left'),
    cells=dict(values=[nfl_data.Team],
               line_color='darkslategray',
               fill_color='lightcyan',
               align='left'))
])

nfl_table.update_layout(width=10000, height=10000)
nfl_table.show()

# NHL
nhl_data = stadium_data[stadium_data.League == "NHL"]
nhl_data = nhl_data.sort_values(['New_Divisions'], ascending=[1])

nhl_table = go.Figure(data=[go.Table(
    header=dict(values=["Team"],
                line_color='darkslategray',
                fill_color='lightskyblue',
                align='left'),
    cells=dict(values=[nhl_data.Team],
               line_color='darkslategray',
               fill_color='lightcyan',
               align='left'))
])

nhl_table.update_layout(width=10000, height=10000)
nhl_table.show()

# NBA
nba_data = stadium_data[stadium_data.League == "NBA"]
nba_data = nba_data.sort_values(['New_Divisions'], ascending=[1])

nba_table = go.Figure(data=[go.Table(
    header=dict(values=["Team"],
                line_color='darkslategray',
                fill_color='lightskyblue',
                align='left'),
    cells=dict(values=[nfl_data.Team],
               line_color='darkslategray',
               fill_color='lightcyan',
               align='left'))
])

nba_table.update_layout(width=10000, height=10000)
nba_table.show()


# Find reduction in travel rates
  
def haversine(long1, long2, lat1, lat2):
  # Haversine formula
  dlon = long2 - long1
  dlat = lat2 - lat1
  a = np.sin(dlat / 2)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2)**2
  
  c = 2 * np.arcsin(np.sqrt(a))
  
  # Radius of Earth in miles
  r = 3956
  
  # Calculate the result
  return(c * r)

# Original travel averages

#def average_div_travel(league):
  
  # Return for each league, then append to vector of length 124, then add to dataframe in order
  #return(avg_travel)

filepath = Path('Data/Python_Travel.csv')  
filepath.parent.mkdir(parents=True, exist_ok=True)  
stadium_data.to_csv(filepath) 
