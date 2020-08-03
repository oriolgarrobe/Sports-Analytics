# players ratings

library(jsonlite)
library(tidyverse)
library(dplyr)
library(readxl)

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

# All player names

pref_foot <- data.frame()
for (season in season_ids$season_id) {

  current_season <- fromJSON(paste0('data/matches/11/',season,'.json'))
  match_ids <- current_season %>% select(match_id)
  
  for (match in match_ids$match_id) {
    current_match <- fromJSON(paste0('data/events/',match,'.json'), flatten = T)
    
    for (i in 1:2) {
      if (current_match$team.name[i]=='Barcelona') {
        pref_foot <- rbind(pref_foot, current_match$tactics.lineup[i][[1]])
      }
    }
    pref_foot <- pref_foot %>% distinct(player.name, .keep_all = TRUE)
    
  }
}

# Preferred foot from excel file

pref_foot_df <- read_xlsx('data/preferred_foot.xlsx', col_names = TRUE)

# Ratings from excel files
season_04_05_df <- read_xlsx('data/season_04-05.xlsx', col_names = TRUE)
season_05_06_df <- read_xlsx('data/season_05-06.xlsx', col_names = TRUE)
season_06_07_df <- read_xlsx('data/season_06-07.xlsx', col_names = TRUE)
season_07_08_df <- read_xlsx('data/season_07-08.xlsx', col_names = TRUE)
season_08_09_df <- read_xlsx('data/season_08-09.xlsx', col_names = TRUE)
season_09_10_df <- read_xlsx('data/season_09-10.xlsx', col_names = TRUE)
season_10_11_df <- read_xlsx('data/season_10-11.xlsx', col_names = TRUE)
season_11_12_df <- read_xlsx('data/season_11-12.xlsx', col_names = TRUE)
season_12_13_df <- read_xlsx('data/season_12-13.xlsx', col_names = TRUE)
season_13_14_df <- read_xlsx('data/season_13-14.xlsx', col_names = TRUE)
season_14_15_df <- read_xlsx('data/season_14-15.xlsx', col_names = TRUE)
season_15_16_df <- read_xlsx('data/season_15-16.xlsx', col_names = TRUE)
season_16_17_df <- read_xlsx('data/season_16-17.xlsx', col_names = TRUE)
season_17_18_df <- read_xlsx('data/season_17-18.xlsx', col_names = TRUE)
season_18_19_df <- read_xlsx('data/season_18-19.xlsx', col_names = TRUE)

### NOTES
# fifaindex.com