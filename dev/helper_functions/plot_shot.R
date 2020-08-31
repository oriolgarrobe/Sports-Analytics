plot_shot <- function(active_player = NA, passive_players = NA, index = NA, full_pitch = FALSE, df = shots){
  ### Plots pitch and a chosen shot on it either based on active player location and freeze frame object or an id provided
  
  # INPUTS
  # active player = vector of 2 coordinates
  # passive players = freeze frame dataframe
  # index = id of row in dataframe
  # full pitch = Boolean, if you want to plot the whole pitch, set it to TRUE
  # df = name of dataframe containing shot data
  
  # OUTPUT
  # a ggplot2 plot showing a shot
  
  # pitch dimensions
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
    # if active player and passive players df (freeze frame) are specified
    # convert types
    active_player = unlist(active_player)
    passive_players = as.data.frame(passive_players)
    
    # add empty shot end location to players df
    shot_end_location = c(NA_real_, NA_real_)
    new_player = data.frame('shot end location', # name
                            shot_end_location[1], # x
                            shot_end_location[2], # y
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
                            active_player[2], # y
                            TRUE, # teammate
                            '', # position name
                            TRUE) # active
    names(new_player) = c('name', 'x', 'y', 'teammate', 'position_name', 'active')
    # add player to players df
    players <- rbind(players, new_player)
    
    # set extra variables to empty (only relevant if index is specified)
    player_name = ''
    xg = NA_real_
    outcome = NA
    opponent_team = ''
    
    
  } else {
    # if only id (index) is specified, get row from df
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
    opponent_team = row$opponent_team
    
    
    # add shot end location to players df
    new_player = data.frame('shot end location', # name
                            shot_end_location[1], # x
                            shot_end_location[2], # y
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
                            active_player[2], # y
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
    # loop through all passive players and add them to players df
    for (i in 1:nrow(passive_players)) {
        new_player = data.frame(passive_players[['player.name']][i],
                                passive_players[[i,1]][1], # x
                                passive_players[[i,1]][2], # y
                                passive_players[['teammate']][i],
                                passive_players[['position.name']][i],
                                FALSE) # active
        names(new_player) = c('name', 'x', 'y', 'teammate', 'position_name', 'active')
        # add player to players df
        players <- rbind(players, new_player)
    }
  }
  
  ########### PLOTTING ###########
  
  # specify arrow
  arrow = arrow(angle = 30, length = unit(0.3, "cm"),ends = "last", type = "open")
  point_size = 3
  point_shape = 21
  teammate_colour = 'blue'
  opponent_colour = 'red'
  gk_colour = 'red4'
  
  # draw different plot based on full_pitch attribute
  if (full_pitch) {
    # draw the whole pitch
    p = soccerPitch(lengthPitch = length,
                    widthPitch = width,
                    arrow = 'none', 
                    title = paste0(player_name, 
                                   ifelse(outcome == 1, ' GOAL', ' MISS')
                                   #,', xG = ',ifelse(is.na(xg), 'unknown', round(xg,3))
                                   , ' vs ', opponent_team
                                   ), 
                    subtitle = NULL, 
                    theme = "grass") + 
      # add active player location
      geom_point(data = filter(players, active), 
                 aes(x = x, y = width - y), # invert y
                 col = "black",
                 size = point_size,
                 shape = point_shape,
                 fill = teammate_colour) +
      # add passive player locations (teammates)
      geom_point(data = filter(players, !active & teammate), 
                 aes(x = x, y = width - y),  # invert y
                 col = "black",
                 size = point_size,
                 shape = point_shape,
                 fill = teammate_colour) +
      # add passive player locations (opponents)
      geom_point(data = filter(players, !active & !teammate), 
                 aes(x = x, y = width - y),  # invert y
                 col = "black",
                 size = point_size,
                 shape = point_shape,
                 fill = opponent_colour) +
      # add opponent goalkeeper location
      geom_point(data = filter(players, !active & !teammate & position_name == 'Goalkeeper'), 
                 aes(x = x, y = width - y),  # invert y
                 col = "black",
                 size = point_size,
                 shape = point_shape,
                 fill = gk_colour) +
      # add shot end location
      # geom_point(data = filter(players, name == 'shot end location'), 
      #            aes(x = x, y = width - y),  # invert y
      #            col = "black",
      #            size = point_size,
      #            shape = point_shape,
      #            fill = 'red') +
      # add arrow between shooter and shot end location
      geom_segment(data = filter(players, active),
                   aes(x = x, y = width - y, xend = shot_end_location[1], yend = width - shot_end_location[2]),  # invert y
                   colour = 'blue',
                   arrow = arrow)
  } else {
    # draw half of the pitch
    p = soccerPitchHalf(lengthPitch = 120,
                        widthPitch = 80,
                        arrow = 'none', 
                        title = paste0(player_name, 
                                       ifelse(outcome == 1, ' GOAL', ' MISS')
                                       #,', xG = ',ifelse(is.na(xg), 'unknown', round(xg,3))
                                       , ' vs ', opponent_team
                                       ), 
                        subtitle = NULL, 
                        theme = "grass") + 
      # add active player location
      geom_point(data = filter(players, active), 
                 aes(x = y, y = x), # have to switch up x and y
                 col = "black",
                 size = point_size,
                 shape = point_shape,
                 fill = teammate_colour) +
      # add passive player locations (teammates)
      geom_point(data = filter(players, !active & teammate), 
                 aes(x = y, y = x), # have to switch up x and y
                 col = "black",
                 size = point_size,
                 shape = point_shape,
                 fill = teammate_colour) +
      # add passive player locations (opponents)
      geom_point(data = filter(players, !active & !teammate), 
                 aes(x = y, y = x), # have to switch up x and y
                 col = "black",
                 size = point_size,
                 shape = point_shape,
                 fill = opponent_colour) +
      # add opponent goalkeeper location
      geom_point(data = filter(players, !active & !teammate & position_name == 'Goalkeeper'), 
                 aes(x = y, y = x), # have to switch up x and y
                 col = "black",
                 size = point_size,
                 shape = point_shape,
                 fill = gk_colour) +
      # add shot end location
      # geom_point(data = filter(players, name == 'shot end location'), 
      #            aes(x = y, y = x), # have to switch up x and y
      #            col = "black",
      #            size = point_size,
      #            shape = point_shape,
      #            fill = 'red') +
      # add arrow between shooter and shot end location
      geom_segment(data = filter(players, active),
                   aes(x = y, y = x, xend = shot_end_location[2], yend = shot_end_location[1]), # have to switch up x and y
                   colour = 'blue',
                   arrow = arrow)
  }
  # return plot (this will show the plot automatically)
  return(p)
}

######## TO DO

