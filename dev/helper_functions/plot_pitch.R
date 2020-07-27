plot_pitch <- function(active_player, passive_players, main = 'Plot'){
  # INPUTS
  # active player = vector of 2 coordinates
  # passive players = freeze frame dataframe
  # main = title of plot
  
  # draw empty pitch
  plot(120,80, xlim=c(0,120), ylim = rev(c(0,80)), ylab = '', xlab = '', main = main)
  abline(v=c(120, 60),  h = 80)
  
  # add active player location
  points(active_player[1], active_player[2], col = "blue", pch = 8)
  
  # add passive player locations
  if (is.null(passive_players)) {
    # don't do anything
  } else {
    for (i in 1:nrow(passive_players)) {
      if (passive_players[['position.name']][i] == 'Goalkeeper') {
        points(passive_players[[i,1]][1],passive_players[[i,1]][2], col = "red", pch = 15)
      }
      if (passive_players[['teammate']][i]) {
        points(passive_players[[i,1]][1], passive_players[[i,1]][2], col = "blue", pch = 20)
      } else {
        points(passive_players[[i,1]][1],passive_players[[i,1]][2], col = "red", pch = 20)
      }
    }
  }
}


