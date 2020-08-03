library(jsonlite)
library(tidyverse)

############################## Script that creates the dataset for analysis from Statsbomb data ##############################

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
    
    # add season_id
    filtered = filtered %>% add_column(season_id = season)
    
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
                                             match_date,
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

# create opposition team name variable
all_matches <- mutate(all_matches, opponent_team = ifelse(home, away_team.away_team_name, home_team.home_team_name))

### save all_matches dataframe for future use
save(all_matches,file="dev/all_matches.Rda")

## load all_matches
load("dev/all_matches.Rda")


# merge with shots and add home, opponent_team columns to shots
shots <- shots %>% left_join(all_matches, by = 'match_id') %>% select(-c(home_team.home_team_name, 
                                                                         away_team.away_team_name,
                                                                         home_score,
                                                                         away_score,
                                                                         match_date))
# 6485 rows x 27 columns


# remove shot type columns
shots <- select(shots,-c(shot.type.id,
                         shot.type.name))
# 6485 rows x 25 columns


## add goalkeeper name in a new column
shots$gk_name = NA

get_gk_name <- function(freeze_frame){
  freeze_frame = as.data.frame(freeze_frame)
  gk_index = which(freeze_frame$position.name=='Goalkeeper' & !freeze_frame$teammate)
  if (isTRUE(gk_index>0)) {
    return(freeze_frame$player.name[gk_index])
  } else {
    return(NA)
  }
}

for (i in 1:nrow(shots)) {
  print(i)
  shots[i,]$gk_name = get_gk_name(shots[i,]$shot.freeze_frame)
}

# 6485 rows x 26 columns


## add geometric features to shots
shots$dist = NA
shots$angle = NA
shots$obstacles = NA
shots$pressure_prox = NA 
shots$pressure_block = NA 
shots$gk_obstacle = NA 
shots$gk_pos = NA 
shots$gk_pos_adjusted = NA
shots$split_angle = NA 
shots$gk_dist_from_player = NA 
shots$gk_dist_from_goal = NA 

# correct database mistake
shots[[6467, 'shot.freeze_frame']]$teammate[9] = FALSE
shots[[6467, 'shot.freeze_frame']]$player.name[9] = 'Idriss Carlos Kameni'
shots$gk_name[6467] = 'Idriss Carlos Kameni'

for (i in 6467:nrow(shots)) {
  print(i)
  geom = geom_features(shots[i,]$location, shots[i,]$shot.freeze_frame)
  shots[i,]$dist = geom$dist
  shots[i,]$angle = geom$angle
  shots[i,]$obstacles = geom$obstacles
  shots[i,]$pressure_prox = geom$pressure_prox
  shots[i,]$pressure_block = geom$pressure_block
  shots[i,]$gk_obstacle = geom$gk_obstacle
  shots[i,]$gk_pos = geom$gk_pos
  shots[i,]$gk_pos_adjusted = geom$gk_pos_adjusted
  shots[i,]$split_angle = geom$split_angle
  shots[i,]$gk_dist_from_player = geom$gk_dist_from_player
  shots[i,]$gk_dist_from_goal = geom$gk_dist_from_goal
}

# 6485 rows x 37 columns


# fill in goalkeeper name, where missing
matches = filter(shots, is.na(gk_name)) %>% select(match_id)

shots[1351,]$gk_name = 'Salvatore Sirigu'
shots[2549,]$gk_name = 'Orestis Karnezis'
shots[4363,]$gk_name = 'Iker Casillas Fernández'
shots[6241,]$gk_name = 	'Miguel Ángel Moyà Rumbo'

# fill in first_time = FALSE, where missing (only 9 rows)
shots[['shot.first_time']] = shots[['shot.first_time']] %>% replace_na(FALSE)

# fill in angle where missing (because the shooter is standing on the goalline, so the angle is 180)
shots[1614,]$angle = 180

# Add preferred foot
pref_foot_df <- read_xlsx('data/preferred_foot.xlsx', col_names = TRUE)
shots <- merge(shots, pref_foot_df, by = "player.id", all.x = T)

# Add player ratings
player_ratings_df <- read_xlsx('data/player_ratings.xlsx', col_names = TRUE)
player_ratings_df$jersey_number <- NULL
player_ratings_df$position.id <- NULL
player_ratings_df$position.name <- NULL
player_ratings_df$player.name <- NULL

shots <- merge(shots, player_ratings_df, by = c("player.id", "season_id"), all.x = T)


### save shots dataframe for future use
save(shots,file="dev/shots.Rda")


### load data
load("dev/shots.Rda")


#### select dataset for analysis

anal <- shots %>% select(id,
                         #strong_foot, #(Boolean)
                         #player_rating,
                         #gk_rating,
                         shot.first_time,
                         under_pressure,
                         home,
                         dist,
                         angle,
                         obstacles,
                         pressure_prox,
                         pressure_block,
                         gk_obstacle,
                         gk_pos_adjusted,
                         gk_dist_from_player,
                         gk_dist_from_goal,
                         goal)

