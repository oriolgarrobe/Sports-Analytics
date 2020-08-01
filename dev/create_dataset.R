library(jsonlite)
library(tidyverse)

### Script that creates the dataset for analysis from Statsbomb data

## merge all Barca events and filter to shots
competitions <- fromJSON('data/competitions.json')
shots = data.frame()
counter = 0

# get all season ids for La Liga
season_ids = competitions %>% filter(competition_id==11) %>% select(season_id)

# loop through all seasons
for (season in season_ids$season_id) {
  current_season <- fromJSON(paste0('data/matches/11/',season,'.json'))
  match_ids <- current_season %>% select(match_id)
  
  # loop through all matches in current season
  for (match in match_ids$match_id) {
    
    # prints for following progress (it takes a few minutes)
    print(match)
    print(counter)
    
    # read events of current match
    current_match <- fromJSON(paste0('data/events/',match,'.json'), flatten = T)
    filtered = dplyr::filter(current_match, team.name=='Barcelona' & type.name == 'Shot') %>% select(id,
                                                                                                     related_events,
                                                                                                     player.id,
                                                                                                     player.name,
                                                                                                     position.id,
                                                                                                     position.name,
                                                                                                     location,
                                                                                                     under_pressure,
                                                                                                     starts_with('shot')
                                                                                                     # shot.first_time,
                                                                                                     # shot.aerial_won,
                                                                                                     # shot.one_on_one,
                                                                                                     # shot.technique.id,
                                                                                                     # shot.technique.name,
                                                                                                     # shot.type.id,
                                                                                                     # shot.type.name,
                                                                                                     # shot.body_part.id,
                                                                                                     # shot.body_part.name,
                                                                                                     # shot.outcome.id,
                                                                                                     # shot.outcome.name,
                                                                                                     # shot.statsbomb_xg,
                                                                                                     # shot.end_location,
                                                                                                     # shot.freeze_frame,
                                                                                                     # shot.type.name,
                                                                                                     )
    # set logical columns' values to FALSE if NA
    for (df_col in colnames(filtered)) {
      if (class(filtered[[df_col]])=='logical') {
        filtered[[df_col]] = filtered[[df_col]] %>% replace_na(FALSE)
      }
    }
    
    # add missing columns if missing (https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist)
    cols <- c(
              # necessary columns (but not all matches have them, in that case it's a missing value = NA)
              shot.one_on_one = NA, shot.first_time = NA, shot.open_goal = NA,
              # unnecesary columns (need to add them, because some matches have them and they have to have the same structure)
              shot.deflected = NA, shot.aerial_won = NA, shot.saved_off_target = NA, shot.saved_to_post = NA, shot.redirect = NA, shot.follows_dribble = NA)
    filtered = filtered %>% add_column(!!!cols[!names(cols) %in% names(.)])
    
    # add match_id
    filtered = filtered %>% add_column(match_id = match)
    
    # order columns alphabetically (to ensure that matches have the same order of columns before merging)
    filtered = filtered %>% select(order(colnames(.)))
    
    # first iteration overwrites shots, after that only appends to it
    if (counter == 0) {
      shots = filtered
    } else {
      shots = rbind(shots, filtered)
    }
    counter = counter + 1
  }
}
# 7225 rows x 30 columns


## filter only open play shots
shots <- filter(shots, shot.type.name == 'Open Play')
# 6485 rows x 30 columns

## create goal variable (1 if goal, 0 if not goal)
shots <- mutate(shots, goal = ifelse(shot.outcome.name == 'Goal', 1, 0))
# 6485 rows x 31 columns

## remove unnecessary columns
shots <- select(shots,-c(shot.redirect,
                         shot.follows_dribble,
                         shot.saved_off_target,
                         shot.saved_to_post,
                         shot.deflected,
                         shot.aerial_won))
# 6485 rows x 25 columns



## create table of all matches with home - away column
# merge all matches (not at event level)
season_ids = competitions %>% filter(competition_id==11) %>% select(season_id)
all_matches = data.frame()
counter = 0

# loop through all seasons
for (season in season_ids$season_id) {
  
  # read in current season matches
  current_season <- fromJSON(paste0('data/matches/11/',season,'.json'), flatten = T)
  current_season <- select(current_season, c(match_id, 
                                             home_team.home_team_name, 
                                             away_team.away_team_name,
                                             home_score,
                                             away_score))
  
  # first iteration overwrites all_matches, after that only appends to it
  if (counter == 0) {
    all_matches = current_season
  } else {
    all_matches = rbind(all_matches, current_season)
  }
  counter = counter + 1
}
# 452 rows

# create home variable (TRUE if home, FALSE if away)
all_matches <- mutate(all_matches, home = ifelse(home_team.home_team_name == 'Barcelona', TRUE, FALSE))

# merge with shots and add home column to shots
shots <- shots %>% left_join(all_matches, by = 'match_id') %>% select(-c(home_team.home_team_name, 
                                                                         away_team.away_team_name,
                                                                         home_score,
                                                                         away_score))
# 6485 rows x 26 columns


# remove shot type columns
shots <- select(shots,-c(shot.type.id,
                         shot.type.name))
# 6485 rows x 24 columns


### save shots dataframe for future use
save(shots,file="shots.Rda")


### load data
load("shots.Rda")
