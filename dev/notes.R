# examine shot fidelity version

match1 <- fromJSON('data/matches/11/1.json')
match2 <- fromJSON('data/matches/11/2.json')
season_files = list.files(path = 'data/matches/11')

rowcount = 0
for (i in season_files) {
  season <- fromJSON(paste0('data/matches/11/',i))
  low_fidelity <- season %>% filter(metadata$shot_fidelity_version != 2)
  rowcount = rowcount + nrow(low_fidelity)
  print(low_fidelity)
}
rowcount

################################### NOTES #######################################

# all Barcelona matches have high shot fidelity!


################################### IDEAS #######################################

# Build a model (binary probabilistic classifier) of scoring probability based on freeze frame and several attributes
# that outputs 'Goal' or 'Not goal'

# Attributes:

# Goalkeeper positioning
# Goalkeeper rating (FIFA?)
# Angle of the shot
# Shooting distance
# Number of players between shooter and goal
# Opposition players proximity to shooter
# Shooter under pressure or not (similar to previous one)
# Shooter's rating (FIFA?)
# Home or away
# Strong foot or not
# First time shot or not
# Ball rebound from goalkeeper or not (then the keeper is probably lying on the ground)

# Compare this to xG from StatsBomb
# Train, validate and test on 30% and evaluate if classifier is correct (with confusion matrix, AUC, etc.)
# Compare Barca players based on whether they under or overperform (score less or more than they should)
# Maybe we should train separate models for players to get more accurate predictions
# Plot some shots that should have been scored and some that had little chance but still went in
# Try out different ML models for this and compare
# Deal with freekicks and penalties