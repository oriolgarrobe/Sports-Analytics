library(jsonlite)
library(dplyr)

competitions <- jsonlite::fromJSON('data/competitions.json')

# Barcelona vs Real Madrid (2017.12.23.)
match_df <- jsonlite::fromJSON('data/events/9736.json')
match_df_flat <- jsonlite::fromJSON('data/events/9736.json', flatten = T)

goals = match_df %>% filter(match_df$shot$outcome$name == 'Goal')
goals_flat = match_df_flat %>% filter(match_df_flat$shot.outcome.name == 'Goal')
cards = match_df %>% filter(match_df$foul_committed$card$name %in% c('Yellow Card', 'Red Card') |
                              match_df$bad_behaviour$card$name %in% c('Yellow Card', 'Red Card'))



# all Barcelona matches (= all La Liga matches, competition_id = 11)
shots = 0
season_ids = competitions %>% filter(competition_id==11) %>% select(season_id)
for (season in season_ids$season_id) {
  current_season <- jsonlite::fromJSON(paste0('data/matches/11/',season,'.json'))
  match_ids <- current_season %>% select(match_id)
  for (match in match_ids$match_id) {
    current_match <- jsonlite::fromJSON(paste0('data/events/',match,'.json'), flatten = T)
    shots = shots + nrow(filter(current_match, type.name == 'Shot' & team.name == 'Barcelona'))
    print(shots)
  }
}
#shots = 7225



                            
############### NOTES ###############

# if you use fromJSON(..., flatten = T), then you can reference the fields with a dot (.)
# if you use fromJSON(..., flatten = F), then you can reference the fields with a $

# Total shots by Barcelona (all seasons) = 7225