# players ratings

library(jsonlite)
library(tidyverse)
library(dplyr)

competitions <- fromJSON('data/competitions.json')

season_ids = competitions %>% filter(competition_id==11) %>% select(season_id)


# create df with all Barca players and all goalies for each season
for (season in season_ids$season_id) {
  ratings <- data.frame()
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
    assign(paste0('season_',current_season$season$season_name[1]), ratings)
  }
}

# Map player ratings
