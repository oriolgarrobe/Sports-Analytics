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
                                                                                                                       shot.type.name)
     #%>% select(shot.freeze_frame, shot.outcome.name, shot.type.name)

realmadrid = match_df_flat %>% dplyr::filter(team.name=='Barcelona') %>% dplyr::filter(type.name == 'Shot') %>% select(shot.outcome.name,
                                                                                                                       shot.freeze_frame,
                                                                                                                       shot.type.name)

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
    filtered = current_match %>% dplyr::filter(team.name=='Barcelona') %>% dplyr::filter(type.name == 'Shot') %>% select(shot.outcome.name,
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



View(match_df_flat
     %>% dplyr::filter(team.name=='Barcelona')
     %>% dplyr::filter(length(unlist(shot.freeze_frame)>0))
     %>% select(shot.freeze_frame, shot.outcome.name, shot.type.name))

                            
############### NOTES ###############

# if you use fromJSON(..., flatten = T), then you can reference the fields with a dot (.)
# if you use fromJSON(..., flatten = F), then you can reference the fields with a $

# every shot has a related event that describes the goalkeeper's action
# pitch coordinates are from the perspective of the attacking team -> it can change depending on which team the event belongs to

# Total shots by Barcelona (all seasons) = 7225