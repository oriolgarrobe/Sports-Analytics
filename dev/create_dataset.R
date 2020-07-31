library(jsonlite)
library(tidyverse)


# merge all Barca matches
competitions <- fromJSON('data/competitions.json')
shots = data.frame()
counter = 0
season_ids = competitions %>% filter(competition_id==11) %>% select(season_id)
for (season in season_ids$season_id) {
  current_season <- fromJSON(paste0('data/matches/11/',season,'.json'))
  match_ids <- current_season %>% select(match_id)
  for (match in match_ids$match_id) {
    print(match)
    print(counter)
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
              # necessary columns
              shot.aerial_won = NA, shot.one_on_one = NA, shot.first_time = NA,
              # unnecesary columns
              shot.deflected = NA, shot.open_goal = NA, shot.saved_off_target = NA, shot.saved_to_post = NA, shot.redirect = NA, shot.follows_dribble = NA)
    filtered = filtered %>% add_column(!!!cols[!names(cols) %in% names(.)])
    
    # add match_id
    filtered = filtered %>% add_column(match_id = match)
    
    # order columns alphabetically
    filtered = filtered %>% select(order(colnames(.)))
    print(colnames(shots))
    print(colnames(filtered))
    if (counter == 0) {
      shots = filtered
    } else {
      shots = rbind(shots, filtered)
    }
    counter = counter + 1
  }
}
