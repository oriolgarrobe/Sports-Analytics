# players ratings

library(jsonlite)
library(tidyverse)
library(dplyr)

lineups <- fromJSON('data/lineups/15946.json')

df_test <- fromJSON('data/events/9736.json', flatten = T)

# # now merge all matches
# shots = data.frame()
# counter = 0
# 
# 
# current_season <- fromJSON(paste0('data/matches/11/',season,'.json'))
# match_ids <- current_season %>% select(match_id)
# for (match in match_ids$match_id) {
#   current_match <- fromJSON(paste0('data/events/',match,'.json'), flatten = T)
#   
#   for (i in 1:2) {
#     if (current_match$team.name[i]=='Barcelona') {
#       ratings <- rbind(ratings, current_match$tactics.lineup[i][[1]])
#     }
#     else {
#       ratings <- rbind(ratings, current_match$tactics.lineup[i][[1]][1,] )
#     }
#   }
#   ratings <- distinct(ratings)
#   
# }

competitions <- fromJSON('data/competitions.json')

ratings <- data.frame()
season_ids = competitions %>% filter(competition_id==11) %>% select(season_id)

for (season in season_ids$season_id) {
  current_season <- fromJSON(paste0('data/matches/11/',season,'.json'))
  match_ids <- current_season %>% select(match_id)
  for (match in match_ids$match_id) {
    current_match <- fromJSON(paste0('data/events/',match,'.json'), flatten = T)
    
    for (i in 1:2) {
      if (current_match$team.name[i]=='Barcelona') {
        ratings <- rbind(ratings, current_match$tactics.lineup[i][[1]])
      }
      else {
        ratings <- rbind(ratings, current_match$tactics.lineup[i][[1]][1,] )
      }
    }
    ratings <- ratings %>% distinct(player.name, .keep_all = TRUE)
  }
}
