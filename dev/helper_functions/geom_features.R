geom_features <- function(active_player, passive_players){
  ### Calculates geometric features for a shot
  
  # INPUTS
  # active player = vector of 2 coordinates (x,y)
  # passive players = freeze frame dataframe
  
  # OUTPUTS
  # dist = distance to center of the goal from shot taker (not metre!!!)
  # angle = angle of the goal from the from shot taker
  # obstacles = number of players (teammates & opponents including GK)
  #             between goal and shot taker (inside the triangle of the goalposts and the shot taker)
  # pressure_prox = closest opponent's proximity to the shot taker
  # pressure_pos = closest opponent's position compared to the shot taker and goal 
  #                (can he block the shot?) (value between 0 (outside the triangle) to 1 (on the line between shot taker and center of goal))
  # gk_pos = goalkeeper's positioning, best if gk is standing on the line connecting shot taker to center of the goal
  #          (value between 0 (far from the line) to 1 (on the line))
  
  # NOTES
  # center of the goal = [120,40]
  goal = c(120,40)
  goalline = 8
  goalpost1 = c(120,36)
  goalpost2 = c(120,44)
  
  # distance to goalposts
  dist_goalpost1 = distance(goalpost1, active_player)
  dist_goalpost2 = distance(goalpost2, active_player)
  
  # dist
  dist = distance(goal, active_player)
  
  # angle
  angle = angle(goalline, dist_goalpost1, dist_goalpost2)
  
  # obstacles
  obstacles = 0
  # find players that can block the shot by being inside the triangle
  for (i in 1:nrow(passive_players)) {
    loc = unlist(passive_players[['location']][i])
    is_in_triangle = is_in_triangle(goalpost1, goalpost2, active_player, loc)
    if (is_in_triangle) {
      obstacles = obstacles + 1
       # print(passive_players[['position.name']][i])
       # print(passive_players[['location']][i])
       # print(passive_players[['teammate']][i])
    }
  }
  
  
  # put results into a list
  result = list(dist = dist,
                angle = angle,
                obstacles = obstacles)
  
  # return list
  return(result)
}

# test
#active_player = shots[[4,'location']]
active_player = c(100,65)
passive_players = shots[[4, 'shot.freeze_frame']]
teammate = shots[[4, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = 'open play')

geom_features(active_player, passive_players)




