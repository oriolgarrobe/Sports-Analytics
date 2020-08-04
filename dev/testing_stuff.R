#################### TESTING STUFF ##########################


### Plot shots on the pitch using soccermatics package

if (!require("devtools")) install.packages("devtools")
devtools::install_github("jogall/soccermatics")

# load libraries
library(soccermatics)
library(ggplot2)
library(tidyverse)


# load scripts
source("dev/helper_functions/gauss_kernel.R")
source("dev/helper_functions/geom_features.R")
source("dev/helper_functions/distance.R")
source("dev/helper_functions/angle.R")
source("dev/helper_functions/triangle_area.R")
source("dev/helper_functions/is_in_triangle.R")
source("dev/helper_functions/plot_shot.R")
source("dev/helper_functions/plot_pitch.R")

### plot shot by index
index = '0ac3b192-4929-4684-94f3-e449888ffed3'
plot_shot(index = index, df = shots, full_pitch = T)
plot_shot(index = index, df = shots, full_pitch = F)
plot_pitch(index = index, df = shots)

## geom features by index
filter(shots, id == index) %>% select(dist,
                                   angle,
                                   obstacles,
                                   pressure_prox,
                                   pressure_block,
                                   gk_obstacle,
                                   gk_pos_adjusted,
                                   gk_dist_from_player,
                                   gk_dist_from_goal,
                                   goal)




# test
#active_player = shots[[3619,'location']]
active_player = c(102, 40)
passive_players = as.data.frame(shots[3619,]$shot.freeze_frame)
teammate = passive_players$teammate
plot_shot(active_player, passive_players)
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
