plot_pitch <- function(active_player = NA, passive_players = NA, index = NA, df = shots, main = 'Plot'){
  ### Plots pitch and a chosen shot on it either based on active player location and freeze frame object or an id provided
  
  # INPUTS
  # active player = list of vector of 2 coordinates
  # passive players = freeze frame dataframe as list
  # index = id of row in dataframe
  # df = name of dataframe
  # main = title of plot
  
  
  # draw empty pitch
  plot(120,80, xlim=c(0,120), ylim = rev(c(0,80)), ylab = '', xlab = '')
  abline(v=c(120, 60),  h = 80)
  
  # draw goalposts
  points(120,36, pch = 3)
  points(120,44, pch = 3)

  # check what kind of input we have
  if (is.na(index)) {
    # if active player and passive players df (freeze frame) is specified
    # convert types
    active_player = unlist(active_player)
    passive_players = as.data.frame(passive_players)
    # add title
    title(main = main)
    
  } else {
    # if only id is specified
    row = filter(df, id == index)
    active_player = unlist(row$location)
    passive_players = as.data.frame(row$shot.freeze_frame)
    # write xg and outcome in title
    title(main = paste('xG: ',round(row$shot.statsbomb_xg,3), '; Goal= ', row$goal))
    # add shot end location
    shot_end_location = unlist(row$shot.end_location)[1:2]
    points(shot_end_location[1], shot_end_location[2], col = 'green', pch = 20)
  }
  

  
  # add active player location
  points(active_player[1], active_player[2], col = "blue", pch = 8)
  
  # add passive player locations
  if (is.null(passive_players)) {
    # don't do anything
  } else {
    for (i in 1:nrow(passive_players)) {
      if (passive_players[['position.name']][i] == 'Goalkeeper') {
        points(passive_players[[i,1]][1],passive_players[[i,1]][2], col = "red", pch = 4)
      }
      if (passive_players[['teammate']][i]) {
        points(passive_players[[i,1]][1], passive_players[[i,1]][2], col = "blue", pch = 20)
      } else {
        if (passive_players[['position.name']][i] != 'Goalkeeper') {
          points(passive_players[[i,1]][1],passive_players[[i,1]][2], col = "red", pch = 20)
        }
      }
    }
  }
}

