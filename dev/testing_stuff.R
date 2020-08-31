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
#index = '607646b4-ec84-4629-8728-e5ba6e71bf2b' # dani alves rocket
index = '1f85a34c-9689-4036-9fb1-133918d42a25' # dani alves rocket 2
index = '2066b8f0-fb76-4844-a626-489ccc315fc2' # neymar miss
index = 'd4c5e93b-aee7-43c9-a8a1-0dde6a8d7dda' # guily miss
index = '3c18d70f-2253-41cf-80ad-8d863cef4db8' # etoo miss
index = '89be0308-3b31-481d-8af5-ac7bd324a863' # messi goal
index = 'e43c92e3-70fc-4e32-b055-bf4666080caf' # messi goal

plot_shot(index = index, df = shots, full_pitch = F)
plot_shot(index = index, df = shots, full_pitch = T)
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
active_player = shots[[3619,'location']]
#active_player = c(102, 40)
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
