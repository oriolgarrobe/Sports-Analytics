library(jsonlite)
library(tidyverse)

 competitions <- fromJSON('data/competitions.json')

##############################################################################################
# Barcelona - Real Madrid
# 3-0
# 2017.12.23
# https://www.whoscored.com/Matches/1222139/Live/Spain-LaLiga-2017-2018-Real-Madrid-Barcelona
# https://www.youtube.com/watch?v=RTKb97wltyo&t=94s

match_df <- fromJSON('data/events/9736.json')
match_df_flat <- fromJSON('data/events/9736.json', flatten = T)

goals = match_df %>% filter(match_df$shot$outcome$name == 'Goal')
goals_flat = match_df_flat %>% filter(match_df_flat$shot.outcome.name == 'Goal')
cards = match_df %>% filter(match_df$foul_committed$card$name %in% c('Yellow Card', 'Red Card') |
                              match_df$bad_behaviour$card$name %in% c('Yellow Card', 'Red Card'))


# plot pitch for goals
for (i in rownames(goals)) {
  active_player = goals_flat[[i,'location']]
  passive_players = goals_flat[[i, 'shot.freeze_frame']]
  teammate = goals_flat[[i, 'shot.freeze_frame']]$teammate
  plot_pitch(active_player, passive_players, main = i)
}


shots_df <- match_df %>% filter(match_df$type$id == 16)
shots_df_flat <- match_df_flat %>% filter(match_df_flat$type.id == 16)



##############################################################################################

# all Barcelona matches (= all La Liga matches, competition_id = 11)
shots = 0
season_ids = competitions %>% filter(competition_id==11) %>% select(season_id)
for (season in season_ids$season_id) {
  current_season <- fromJSON(paste0('data/matches/11/',season,'.json'))
  match_ids <- current_season %>% select(match_id)
  for (match in match_ids$match_id) {
    current_match <- fromJSON(paste0('data/events/',match,'.json'), flatten = T)
    shots = shots + nrow(filter(current_match, type.name == 'Shot' & team.name == 'Barcelona'))
    print(shots)
  }
}
#shots = 7225

# check if all non-penalty shots have freeze frame

# combine all matches into one and filter shots

match_df2_flat <- fromJSON('data/events/9827.json', flatten = T) #1 FK, 1 penalty goal
laspalmas = match_df2_flat %>% dplyr::filter(team.name=='Barcelona') %>% dplyr::filter(type.name == 'Shot') %>% select(shot.outcome.name,
                                                                                                                       shot.freeze_frame,
                                                                                                                       shot.type.name,
                                                                                                                       player.name,
                                                                                                                       location,
                                                                                                                       under_pressure,
                                                                                                                       shot.statsbomb_xg,
                                                                                                                       shot.body_part.name)
     #%>% select(shot.freeze_frame, shot.outcome.name, shot.type.name)

realmadrid = match_df_flat %>% dplyr::filter(team.name=='Barcelona') %>% dplyr::filter(type.name == 'Shot') %>% select(shot.outcome.name,
                                                                                                                       shot.freeze_frame,
                                                                                                                       shot.type.name,
                                                                                                                       shot.statsbomb_xg,
                                                                                                                       team.name,
                                                                                                                       player.name,
                                                                                                                       location,
                                                                                                                       minute)

#merge 2 matches
merged = rbind(laspalmas, realmadrid) # NICE

# now merge all matches
shots = data.frame()
counter = 0
season_ids = competitions %>% filter(competition_id==11) %>% select(season_id)
for (season in season_ids$season_id) {
  current_season <- fromJSON(paste0('data/matches/11/',season,'.json'))
  match_ids <- current_season %>% select(match_id)
  for (match in match_ids$match_id) {
    current_match <- fromJSON(paste0('data/events/',match,'.json'), flatten = T)
    filtered = current_match %>% dplyr::filter(team.name=='Barcelona') %>% dplyr::filter(type.name == 'Shot') %>% select(location,
                                                                                                               shot.outcome.name,
                                                                                                               shot.freeze_frame,
                                                                                                               shot.type.name)
    if (counter == 0) {
      shots = filtered
    } else {
      shots = rbind(shots, filtered)
    }
    counter = counter + 1
  }
}


non_penalty_shots = filter(shots, shot.type.name != 'Penalty') #7126
open_play_shots = filter(shots, shot.type.name == 'Open Play') #6485


nrow(filter(open_play_shots, shot.outcome.name == 'Goal')) # 1056 goals

# SHOTS
# check if this runs -> freeze frame is filled in
counter = 0
for (i in 1:nrow(shots)) {
  if (!is.numeric(shots$shot.freeze_frame[[i]]$location[[1]])) {
    counter = counter + 1
    print(paste('freeze frame missing: ',i))
  } 
}
counter # 80 missing, but there are 99 penalties

# NON PENALTY SHOTS
# check if this runs -> freeze frame is filled in
counter = 0
for (i in 1:nrow(non_penalty_shots)) {
  if (!is.numeric(non_penalty_shots$shot.freeze_frame[[i]]$location[[1]])) {
    counter = counter + 1
    print(paste('freeze frame missing: ',i))
  } 
}
counter # 0 missing, GOOD NEWS!!
                            



############## PLOT SOME INTERESTING SHOTS #################

# corner

active_player = shots[[7026,'location']]
passive_players = shots[[7026, 'shot.freeze_frame']]
teammate = shots[[7026, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = 'corner')

# penalty

active_player = shots[[6652,'location']]
passive_players = shots[[6652, 'shot.freeze_frame']]
teammate = shots[[6652, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = 'penalty')

# free kick

active_player = shots[[1,'location']]
passive_players = shots[[1, 'shot.freeze_frame']]
teammate = shots[[1, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = 'free kick')

# random open play

active_player = shots[[4,'location']]
passive_players = shots[[4, 'shot.freeze_frame']]
teammate = shots[[4, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = 'open play')



################################# CHECK HOW OWN GOALS ARE RECORDED #####################################x

# own goals by barcelona

# Mathieu, 2016.03.20. vs Villareal, competition id = 11, season id = 27, match id = 266106

villareal <- fromJSON('data/events/266106.json', flatten = T)
villareal_shots = villareal %>% dplyr::filter(type.name == 'Shot') %>% select(shot.outcome.name,
                                                                        shot.freeze_frame,
                                                                        shot.type.name,
                                                                        team.name,
                                                                        player.name,
                                                                        minute)

# Mathieu's own goal is not recorded as a shot, but as an Own Goal event:

owngoal = villareal %>% dplyr::filter(type.name=='Own Goal Against') %>% select(shot.outcome.name,
                                                                           shot.freeze_frame,
                                                                           shot.type.name,
                                                                           team.name,
                                                                           player.name,
                                                                           minute)


################################# Plot shots to figure out pitch orientation ######################################

####### AWAY
## Real Madrid - Barcelona (2017.12.23)

realmadrid = match_df_flat %>% dplyr::filter(team.name=='Barcelona') %>% dplyr::filter(type.name == 'Shot') %>% select(shot.outcome.name,
                                                                                                                       shot.freeze_frame,
                                                                                                                       shot.type.name,
                                                                                                                       shot.statsbomb_xg,
                                                                                                                       team.name,
                                                                                                                       player.name,
                                                                                                                       location,
                                                                                                                       minute)
# attacking to the left (1st half)
id = 1

active_player = realmadrid[[id,'location']]
passive_players = realmadrid[[id, 'shot.freeze_frame']]
teammate = realmadrid[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right

id = 3
  
active_player = realmadrid[[id,'location']]
passive_players = realmadrid[[id, 'shot.freeze_frame']]
teammate = realmadrid[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right



# attacking to the right (2nd half)

id = 6

active_player = realmadrid[[id,'location']]
passive_players = realmadrid[[id, 'shot.freeze_frame']]
teammate = realmadrid[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right


id = 18

active_player = realmadrid[[id,'location']]
passive_players = realmadrid[[id, 'shot.freeze_frame']]
teammate = realmadrid[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right


### HOME
## Barcelona - Real Madrid 5-0 (2010.11.29)
#https://www.youtube.com/watch?v=Jeiu7y-a220

realmadrid_home <- fromJSON('data/events/69299.json', flatten = T)

realmadrid_home = realmadrid_home %>% dplyr::filter(team.name=='Barcelona') %>% dplyr::filter(type.name == 'Shot') %>% select(shot.outcome.name,
                                                                                                                       shot.freeze_frame,
                                                                                                                       shot.type.name,
                                                                                                                       shot.statsbomb_xg,
                                                                                                                       team.name,
                                                                                                                       player.name,
                                                                                                                       location,
                                                                                                                       minute)


# attacking to the right (1st half)
id = 4

active_player = realmadrid_home[[id,'location']]
passive_players = realmadrid_home[[id, 'shot.freeze_frame']]
teammate = realmadrid_home[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid_home[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right

id = 5

active_player = realmadrid_home[[id,'location']]
passive_players = realmadrid_home[[id, 'shot.freeze_frame']]
teammate = realmadrid_home[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid_home[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right



# attacking to the left (2nd half)
id = 10

active_player = realmadrid_home[[id,'location']]
passive_players = realmadrid_home[[id, 'shot.freeze_frame']]
teammate = realmadrid_home[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid_home[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right


id = 11

active_player = realmadrid_home[[id,'location']]
passive_players = realmadrid_home[[id, 'shot.freeze_frame']]
teammate = realmadrid_home[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid_home[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right

id = 14

active_player = realmadrid_home[[id,'location']]
passive_players = realmadrid_home[[id, 'shot.freeze_frame']]
teammate = realmadrid_home[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid_home[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right


### random match 1
## Barcelona - Betis 5-1 2006.02.18. (2 Betis own goals)
#https://www.youtube.com/watch?v=UhTt1DPlTgc
#https://int.soccerway.com/matches/2006/02/18/spain/primera-division/futbol-club-barcelona/real-betis/280858/

betis <- fromJSON('data/events/69172.json', flatten = T)

betis = betis %>% dplyr::filter(team.name=='Barcelona') %>% dplyr::filter(type.name == 'Shot') %>% select(shot.outcome.name,
                                                                                                                              shot.freeze_frame,
                                                                                                                              shot.type.name,
                                                                                                                              shot.statsbomb_xg,
                                                                                                                              team.name,
                                                                                                                              player.name,
                                                                                                                              location,
                                                                                                                              minute)


# attacking to the right (1st half)
id = 2

active_player = betis[[id,'location']]
passive_players = betis[[id, 'shot.freeze_frame']]
teammate = betis[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', betis[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right

# attacking to the left (2nd half)
id = 11

active_player = betis[[id,'location']]
passive_players = betis[[id, 'shot.freeze_frame']]
teammate = betis[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', betis[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right






### random match 2
# Sevilla - Barcelona 1-4, 2014-02-09
# https://www.youtube.com/watch?v=jrBWJAD9Kc0
#https://www.fcbarcelona.com/en/matches/34477/sevilla-fc-barcelona-la-liga-2013-2014

sevilla <- fromJSON('data/events/267675.json', flatten = T)

sevilla = sevilla %>% dplyr::filter(team.name=='Barcelona') %>% dplyr::filter(type.name == 'Shot') %>% select(shot.outcome.name,
                                                                                                          shot.freeze_frame,
                                                                                                          shot.type.name,
                                                                                                          shot.statsbomb_xg,
                                                                                                          team.name,
                                                                                                          player.name,
                                                                                                          location,
                                                                                                          minute)

# attacking to the right (1st half)
id = 2

active_player = sevilla[[id,'location']]
passive_players = sevilla[[id, 'shot.freeze_frame']]
teammate = sevilla[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', sevilla[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right

# attacking to the left (2nd half)
id = 5

active_player = sevilla[[id,'location']]
passive_players = sevilla[[id, 'shot.freeze_frame']]
teammate = sevilla[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', sevilla[[id, 'shot.statsbomb_xg']]))
# attack is plotted to the right

##############################################################################################


############### NOTES ###############

# if you use fromJSON(..., flatten = T), then you can reference the fields with a dot (.)
# if you use fromJSON(..., flatten = F), then you can reference the fields with a $

# every shot has a related event that describes the goalkeeper's action
# pitch coordinates are from the perspective of the attacking team -> it can change depending on which team the event belongs to

# Total shots by Barcelona (all seasons) = 7225
# Total non-penalty shots by Barcelona (all seasons) = 7126
# Total open play shots by Barcelona (all seasons) = 6485
# Total goals from open play by Barcelona (all seasons) = 1056

# Every non-penalty shot has a freeze frame
# Some (19) penalty shots also have a freeze frame (out of 99), but that only contains the keeper's position

# Barcelona always seem to attack to right regardsless of home - away or 1st - 2nd half

# Variables

# Used

# shot.outcome.name,
# shot.freeze_frame,
# shot.type.name,
# player.name,
# location,
# under_pressure,
# shot.statsbomb_xg,
# shot.body_part.name

# Could be relevant
# Goalkeeper positioning
# Goalkeeper rating (from FIFA game?)
# Angle of the shot
# Shooting distance
# Number of players between shooter and goal
# Opposition players proximity to shooter
# Shooter under pressure or not (similar to previous one)
# Shooter's rating (from FIFA game?)
# Home or away
# Strong foot or not
# First time shot or not
