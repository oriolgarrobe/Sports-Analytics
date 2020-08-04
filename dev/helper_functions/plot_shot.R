plot_shot <- function(active_player = NA, passive_players = NA, index = NA, df = shots, main = 'Plot'){
  ### Plots pitch and a chosen shot on it either based on active player location and freeze frame object or an id provided
  
  # INPUTS
  # active player = list of vector of 2 coordinates
  # passive players = freeze frame dataframe as list
  # index = id of row in dataframe
  # df = name of dataframe
  # main = title of plot
  
  width = 80
  length = 120
  
  # create dataframe with players
  players = data.frame('name' = character(0),
                       "x" = numeric(0), 
                       "y" = numeric(0), 
                       'teammate' = logical(0), 
                       'position_name' = character(0), 
                       'active' = logical(0))
  
  
  # check what kind of input we have
  if (is.na(index)) {
    # if active player and passive players df (freeze frame) is specified
    # convert types
    active_player = unlist(active_player)
    passive_players = as.data.frame(passive_players)
    
    # add empty shot end location to players df
    shot_end_location = c(NA_real_, NA_real_)
    new_player = data.frame('shot end location', # name
                            shot_end_location[1], # x
                            # invert y coordinate
                            width - shot_end_location[2], # y
                            NA, # teammate
                            '', # position name
                            NA) # active
    names(new_player) = c('name', 'x', 'y', 'teammate', 'position_name', 'active')
    # add player to players df
    players <- rbind(players, new_player)
    
    
    # add active player (shooter) to players df
    # according to the simple input type (active_player, passive_players)
    new_player = data.frame('', # name
                            active_player[1], # x
                            # invert y coordinate
                            width - active_player[2], # y
                            TRUE, # teammate
                            '', # position name
                            TRUE) # active
    names(new_player) = c('name', 'x', 'y', 'teammate', 'position_name', 'active')
    # add player to players df
    players <- rbind(players, new_player)
    player_name = ''
    xg = NA_real_
    outcome = NA
    
    
  } else {
    # if only id is specified, get row from df
    row = filter(df, id == index)
    
    # active player
    active_player = unlist(row$location)
    player_name = row$player.name
    position_name = row$position.name
    
    # passive players
    passive_players = as.data.frame(row$shot.freeze_frame)
    
    # other features
    xg = row$shot.statsbomb_xg
    shot_end_location = unlist(row$shot.end_location)[1:2]
    outcome = row$goal
    
    
    # add shot end location to players df
    new_player = data.frame('shot end location', # name
                            shot_end_location[1], # x
                            # invert y coordinate
                            width - shot_end_location[2], # y
                            NA, # teammate
                            '', # position name
                            NA) # active
    names(new_player) = c('name', 'x', 'y', 'teammate', 'position_name', 'active')
    # add player to players df
    players <- rbind(players, new_player)
    
    # add active player (shooter) to players df
    # according to the complex input type (index)
    new_player = data.frame(player_name,
                            active_player[1], # x
                            # invert y coordinate
                            width - active_player[2], # y
                            TRUE, # teammate
                            position_name,
                            TRUE) # active
    names(new_player) = c('name', 'x', 'y', 'teammate', 'position_name', 'active')
    # add player to players df
    players <- rbind(players, new_player)
  }
  
  
  
  # add passive players to players df (this doesn't depend on what the input type is at this point)
  if (is.null(passive_players)) {
    # don't do anything
  } else {
    for (i in 1:nrow(passive_players)) {
        new_player = data.frame(passive_players[['player.name']][i],
                                passive_players[[i,1]][1], # x
                                # invert y coordinate
                                width - passive_players[[i,1]][2], # y
                                passive_players[['teammate']][i],
                                passive_players[['position.name']][i],
                                FALSE) # active
        names(new_player) = c('name', 'x', 'y', 'teammate', 'position_name', 'active')
        # add player to players df
        players <- rbind(players, new_player)
    }
  }
  
  
  # specify arrow
  arrow = arrow(angle = 30, length = unit(0.3, "cm"),ends = "last", type = "open")
  # draw empty pitch
  p = soccerPitch(lengthPitch = length,
                  widthPitch = width,
                  arrow = 'none', 
                  title = paste0(player_name, 
                                ifelse(outcome == 1, ' GOAL', ' MISS'),
                                ', xG = ',
                                ifelse(is.na(xg), 'unknown', round(xg,3))), 
                  subtitle = NULL, 
                  theme = "grass") + 
    # add active player location
      geom_point(data = filter(players, active), 
                 aes(x = x, y = y), 
                 col = "yellow") +
    # add passive player locations (teammates)
      geom_point(data = filter(players, !active & teammate), 
                 aes(x = x, y = y), 
                 col = "blue") +
    # add passive player locations (opponents)
      geom_point(data = filter(players, !active & !teammate), 
                 aes(x = x, y = y), 
                 col = "red") +
    # add opponent goalkeeper location
      geom_point(data = filter(players, !active & !teammate & position_name == 'Goalkeeper'), 
                 aes(x = x, y = y), 
                 col = "white") +
    # add shot end location
      geom_point(data = filter(players, name == 'shot end location'), 
                 aes(x = x, y = y), 
                 col = "black") +
    # add arrow between shooter and end location
      geom_segment(data = filter(players, active),
                   aes(x = x, y = y, xend = shot_end_location[1], yend = width - shot_end_location[2]), 
                   colour = 'blue',
                   arrow = arrow)
  return(p)
}

######## TO DO

