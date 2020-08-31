# PRESENT RESULTS


### load libraries
library(caret)
library(tidyverse)


### load data
load("dev/anal.Rda")
load("dev/shots.Rda")

# get player names
names <- shots %>% select(id, player.name)
anal <- anal %>% left_join(names, by = 'id')


# calculate how many goals each player scored
by_name <- anal %>% group_by(player.name)
goals_actual <- by_name %>% summarise(sum_goals_actual = sum(goal))

# how much more actual goals are there than predictions
multiplier = sum(anal$goal)/sum(anal$pred_bool)

# calculate how many goals each player should have scored (multiplied by the multiplier)
goals_pred <- by_name %>% summarise(sum_goals_pred = sum(pred_bool)*multiplier)

# merge
players <- merge(goals_pred, goals_actual, by = 'player.name')
players <- players %>% mutate(actual_to_prediction_amount = sum_goals_actual - sum_goals_pred)
#players <- players %>% mutate(actual_to_prediction_percent = sum_goals_actual / sum_goals_pred)
players <- players %>% mutate(diff_to_prediction_percent = actual_to_prediction_amount / sum_goals_pred)


### save players dataframe for future use
save(players,file="dev/players.Rda")

players %>% filter(sum_goals_actual>10) %>% arrange(desc(sum_goals_actual))

