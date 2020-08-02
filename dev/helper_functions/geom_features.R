geom_features <- function(active_player, passive_players){
  ### Calculates geometric features for a shot
  
  # INPUTS
  # active player = list of vector of 2 coordinates (x,y)
  # passive players = freeze frame dataframe as list
  
  # OUTPUTS
  # dist = distance to center of the goal from shot taker (not metre!!!)
  # angle = angle of the goal from the from shot taker (in degrees)
  # obstacles = number of players (teammates & opponents NOT including GK)
  #             between goal and shot taker (inside the triangle of the goalposts and the shot taker)
  # pressure_prox = closest opponent's proximity to the shot taker
  # pressure_block = Boolean, can the closest opponent block the shot by being inside the triangle
  # gk_obstacle = Boolean, can the goalkeeper save the shot by being inside the triangle
  # gk_pos = goalkeeper's positioning, best if gk is standing on the line that halves the angle of the shot
  #          (value between 0 (angle is the same), to 1 (angle is halved))
  # gk_pos_adjusted = same as gk_pos, but it is less strict with shots with a tight angle
  # gk_dist_from_player = distance between goalkeeper and shot taker
  # gk_dist_from_goal = distance between goalkeeper and the center of the goal
  
  # NOTES
  # center of the goal = [120,40]
  goal = c(120,40)
  goalline = 8
  goalpost1 = c(120,36)
  goalpost2 = c(120,44)
  
  # convert types to vector and data frame
  active_player = unlist(active_player)
  passive_players = as.data.frame(passive_players)
  
  # distance to goalposts
  dist_goalpost1 = distance(goalpost1, active_player)
  dist_goalpost2 = distance(goalpost2, active_player)
  
  # dist
  dist = distance(goal, active_player)
  
  # angle
  angle = angle(goalline, dist_goalpost1, dist_goalpost2)
  
  # obstacles and pressure
  obstacles = 0
  pressure_prox = 1000 # arbitrary large value
  pressure_block = 1000 # arbitrary large value
  gk_obstacle = FALSE
  # find players that apply pressure or can block the shot by being inside the triangle
  for (i in 1:nrow(passive_players)) {
    loc = unlist(passive_players[['location']][i])
    is_in_triangle = is_in_triangle(goalpost1, goalpost2, active_player, loc)
    if (is_in_triangle) {
      if (passive_players[['position.name']][i] == 'Goalkeeper') {
        gk_obstacle = TRUE
      } else {
        obstacles = obstacles + 1
      }
       # print(passive_players[['position.name']][i])
       # print(passive_players[['location']][i])
       # print(passive_players[['teammate']][i])
    }
    # find closest opponent player to shot taker
    if (!passive_players[['teammate']][i]) {
      d = distance(loc, active_player)
      if (d < pressure_prox) {
        pressure_prox <- d
        # if pressure is inside the triangle, he can block the shot
        if (is_in_triangle) {
          pressure_block <- TRUE
        } else {
          pressure_block <- FALSE
        }
      }
    }

  }
  # set pressure_prox to NULL if there were no players in freeze frame
  if (pressure_prox == 1000) {
    pressure_prox <- NULL
  }
  # set pressure_block to NULL if there were no players in freeze frame
  if (pressure_block == 1000) {
    pressure_block <- NULL
  }
  
  # gk_pos
  # get goalkeeper location
  gk = filter(passive_players, position.name == 'Goalkeeper' & !teammate)
  gk_loc = unlist(gk$location)
  # only relevant, if gk is in the triangle
  if (gk_obstacle) {
    # get distance to upper goalpost (goalpost1)
    gk_to_post1 = distance(gk_loc, goalpost1)
    # calculate angle of shot between keeper and upper goalpost
    split_angle = angle(gk_to_post1, dist_goalpost1, distance(gk_loc, active_player))
    # get gaussian kernel value for split
    gk_pos = gauss_kernel(split_angle, angle)
    gk_pos_adjusted = gauss_kernel(split_angle, angle, adjusted = T)
  } else {
    # if keeper is not in triangle, he is not splitting the angle of the shot
    gk_pos = 0
    gk_pos_adjusted = 0
    split_angle = 0
  }
  
  # gk_dist_from_player, gk_dist_from_goal
  # only if gk has a location (is in the freeze frame)
  if (!is.null(gk_loc)) {
    gk_dist_from_player = distance(gk_loc, active_player)
    gk_dist_from_goal = distance(gk_loc, goal)
  } else {
    # if GK is not in freeze frame, assume that he is 5 "meters" behind shooter
    gk_dist_from_player = 5
    gk_dist_from_goal = dist + 5
  }

  
  # put results into a data frame
  result = data.frame(dist = dist,
                angle = angle,
                obstacles = obstacles,
                pressure_prox = pressure_prox,
                pressure_block = pressure_block,
                gk_obstacle = gk_obstacle,
                gk_pos = gk_pos,
                gk_pos_adjusted = gk_pos_adjusted,
                split_angle = split_angle, # remove this at the end
                gk_dist_from_player = gk_dist_from_player,
                gk_dist_from_goal = gk_dist_from_goal
                )
  
  # return list
  return(result)
}

# test
#active_player = shots[[4,'location']]
active_player = c(115,50)
passive_players = as.data.frame(shots[4,]$shot.freeze_frame)
teammate = passive_players$teammate
plot_pitch(active_player, passive_players, main = 'open play')

geom_features(active_player, passive_players)


######## keeper out
active_player = c(105,45)
passive_players = realmadrid[[6, 'shot.freeze_frame']]
teammate = realmadrid[[6, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid[[6, 'shot.statsbomb_xg']]))

geom_features(active_player, passive_players)


######## tight angle, keeper on the line
id = 3

active_player = c(110,30)
passive_players = realmadrid[[id, 'shot.freeze_frame']]
teammate = realmadrid[[id, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', realmadrid[[id, 'shot.statsbomb_xg']]))

geom_features(active_player, passive_players)



######## no keeper (mistake, because keeper was there, lying on the ground behind player)
# https://youtu.be/nasCTSj2rww?t=448
osa = shots[1351,]
active_player = unlist(osa$location)
passive_players = osa[[1, 'shot.freeze_frame']]
teammate = osa[[1, 'shot.freeze_frame']]$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', osa[[1, 'shot.statsbomb_xg']]))

geom_features(active_player, passive_players)


######## error
er = shots[6467,]
active_player = er$location
passive_players = as.data.frame(er$shot.freeze_frame)
teammate = passive_players$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', er[[1, 'shot.statsbomb_xg']]))

geom_features(active_player, passive_players)



######## check
er = shots[6471,]
active_player = er$location
passive_players = as.data.frame(er$shot.freeze_frame)
teammate = passive_players$teammate
plot_pitch(active_player, passive_players, main = paste0('xG: ', er[[1, 'shot.statsbomb_xg']]))

geom_features(active_player, passive_players)
