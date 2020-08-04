### Plot shots on the pitch using soccermatics package

if (!require("devtools")) install.packages("devtools")
devtools::install_github("jogall/soccermatics")

library(soccermatics)

### load data
load("dev/statsbomb.Rda")

statsbomb %>% soccerShotmap(theme = "gray")



active_player = c(18,18)

arrow = arrow(angle = 30, length = unit(0.3, "cm"),ends = "last", type = "closed")
p = soccerPitch(lengthPitch = 120,
                widthPitch = 80, 
                arrow = 'none', 
                title = NULL, 
                subtitle = NULL, 
                theme = "grass") +
  
  geom_point(aes(x=100, y=20), colour="yellow") +
  geom_point(aes(x=90, y=40), colour="blue") +
  geom_point(aes(x=0, y=30), colour="red") +
  geom_point(aes(x=110, y=10), colour="black") +
  geom_point(aes(x=100, y=50), colour="yellow") +
  geom_segment(aes(x = 100, y = 20, xend = NA_real_, yend = NA_real_), colour = 'blue',
               arrow = arrow)

p



####
a = data.frame(x = c(100,90,0,110,100), y = c(20,40,30,10,50))
soccerPitch(lengthPitch = 120,
            widthPitch = 80, 
            arrow = 'none', 
            title = NULL, 
            subtitle = NULL, 
            theme = "grass") + 
  geom_point(data = filter(df, id == index), 
             aes(x = unlist(location)[1], y = unlist(location)[2]), 
             col = "yellow") +
  geom_point(data = filter(df, id == index), 
             aes(x = unlist(as.data.frame(shot.freeze_frame)$location)[1], 
                 y = unlist(as.data.frame(shot.freeze_frame)$location)[2]), 
             col = "blue")

#######################################


b = data.frame(loc1 = c(100,90,0,110,100), loc2 = c(20,40,30,10,50), 
               teammate = c(T, F, F, T, F), 
               posname = c('anyád', 'Goalkeeper', 'anyád', 'anyád', 'anyád'),
               active = c(F, F, T, F, F))

p = soccerPitch(lengthPitch = 120,
            widthPitch = 80, 
            arrow = 'none', 
            title = NULL, 
            subtitle = NULL, 
            theme = "grass") + 
  geom_point(data = filter(b, active), 
             aes(x = loc1, y = loc2), 
             col = "yellow") +
  geom_point(data = filter(b, !active & teammate), 
             aes(x = loc1, y = loc2), 
             col = "blue") +
  geom_point(data = filter(b, !active & !teammate), 
             aes(x = loc1, y = loc2), 
             col = "red") +
  geom_point(data = filter(b, !active & !teammate & posname == 'Goalkeeper'), 
             aes(x = loc1, y = loc2), 
             col = "white")
p



######## half pitch
soccerPitchHalf(lengthPitch = 120, 
                widthPitch = 80, 
                arrow = c("none", "r","l"), 
                theme = "grass", 
                title = 'MESSI GOAL xg = 0.023',
                subtitle = NULL) + 
  geom_point(aes(x = 40, y = 112), colour = 'black', size = 2, shape = 21, fill = 'red')+ 
  geom_point(aes(x = 40, y = 116), colour = 'grey')

             



# draw empty pitch
soccerPitchHalf(lengthPitch = 120,
                widthPitch = 80,
                arrow = 'none', 
                title = paste0(player_name, 
                               ifelse(outcome == 1, ' GOAL', ' MISS'),
                               ', xG = ',
                               ifelse(is.na(xg), 'unknown', round(xg,3))), 
                subtitle = NULL, 
                theme = "grass") + 
  # add active player location
  geom_point(data = filter(players, active), 
             aes(x = y, y = x), 
             col = "yellow") +
  # add passive player locations (teammates)
  geom_point(data = filter(players, !active & teammate), 
             aes(x = y, y = x), 
             col = "blue") +
  # add passive player locations (opponents)
  geom_point(data = filter(players, !active & !teammate), 
             aes(x = y, y = x), 
             col = "red") +
  # add opponent goalkeeper location
  geom_point(data = filter(players, !active & !teammate & position_name == 'Goalkeeper'), 
             aes(x = y, y = x), 
             col = "white") +
  # add shot end location
  geom_point(data = filter(players, name == 'shot end location'), 
             aes(x = y, y = x), 
             col = "black") +
  # add arrow between shooter and end location
  geom_segment(data = filter(players, active),
               aes(x = y, y = x, xend = width - shot_end_location[2], yend = shot_end_location[1]), 
               colour = 'blue',
               arrow = arrow)


