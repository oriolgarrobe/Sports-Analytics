library("rjson")
competitions = 'data/competitions.json'
competitions <- fromJSON(file = competitions)

# La Liga : competition id = 11 ([[19]] - [[33]])
# La Liga : matches = 11


season = 'data/matches/11/41.json'
season <- fromJSON(file = season)

# vs Real Madrid : match id = 9736, [[16]],  (2017-12-23, Real - Barca: 3 - 0)

match = 'data/events/9736.json'
match = rjson::fromJSON(file = match)

# check if list names are the same
list_names = list()

for (j in 1:length(match)) {
  names = paste(names(match[[j]]), collapse = ' ')
  if (exists(names, where = list_names)) {
    list_names[names] = list_names[[names]] + 1
  } else {
    list_names[names] = 1
  }
}

# names can be very different between events
list_names

# get all tags
tags = 'id'
for (i in 1:length(list_names)) {
  names = unlist(strsplit(names(list_names[i]), ' '))
  for (j in 1:length(names)) {
    if (names[j] %in% tags) {
      #nothing
    } else {
      tags = append(tags, names[j])
    }
  }
} 

length(tags) #36

# get goals 
for (k in 1:length(match)) {
  event = match[[k]]
  if (event$type$name == "Shot") {
    if (event$shot$outcome$name == "Goal") {
      cat(event$player$name, ";", event$minute, ":", event$second, ";", event$shot$type$name, 
          event$shot$key_pass_id
          ,'\n')
    }
  }
}

# get assist for a goal

for (i in 1:length(match)) {
  event = match[[i]]
  if (event$id == "242a99be-a536-47a0-8e20-e9af7887a204") {
    print(event)
    
  }
}


# print all matches in a season (17/18) (season id = 1)
for (i in 1:length(season)) {
  match = season[[i]]
  cat(match$home_team$home_team_name, match$home_score, " : ", match$away_team$away_team_name, match$away_score, '\n')
}

# print number of matches and size of events file per season (in MB)
season_files = list.files(path = 'data/matches/11')
total_matches = 0
# loop through every season
for (i in 1:length(season_files)) {
  season = paste0('data/matches/11/', season_files[i])
  season <- fromJSON(file = season)
  total_matches = total_matches + length(season)
  cat('\n', season[[1]]$season$season_name, length(season), ' ')
  match_ids = numeric(length = length(season))
  # loop through every match in the season
  for (j in 1:length(season)) {
    match_ids[j] = season[[j]]$match_id
  }
  # get events for every match in the season
  for (k in 1:length(match_ids)) {
    match = paste0('data/events/', match_ids[k], '.json')
    cat(round(file.size(match)/1024/1024, 2), ' ')
  }
}
total_matches #452


##########################################################################
# Barcelona - Real Madrid
# 3-0
# 2017.12.23
# https://www.whoscored.com/Matches/1222139/Live/Spain-LaLiga-2017-2018-Real-Madrid-Barcelona
# https://www.youtube.com/watch?v=RTKb97wltyo&t=94s

match = 'data/events/9736.json'
match = fromJSON(file = match)

# attempts - passes - yellow cards - red cards
attempts_fcb = 0
attempts_rm = 0
passes_fcb = 0
passes_rm = 0
yc_fcb = 0
yc_rm = 0
rc_fcb = 0
rc_rm  = 0

for (i in 1:length(match)) {
  # save current event
  event = match[[i]]
  
  # cards can come from bad behaviour and committed foul
  if (is.list(event$bad_behaviour)) {
    if (event$bad_behaviour$card$name == 'Yellow Card') {
      if (event$team$name == 'Real Madrid') {
        yc_rm = yc_rm + 1
      } else {
        yc_fcb = yc_fcb + 1
      }
    }
    if (event$bad_behaviour$card$name == 'Red Card') {
      if (event$team$name == 'Real Madrid') {
        rc_rm = rc_rm + 1
      } else {
        rc_fcb = rc_fcb + 1
      }
    }
  }
  if (is.list(event$foul_committed)) {
    if (is.list(event$foul_committed$card)) {
      if (event$foul_committed$card$name == 'Yellow Card') {
        if (event$team$name == 'Real Madrid') {
          yc_rm = yc_rm + 1
        } else {
          yc_fcb = yc_fcb + 1
        }
      }
      if (event$foul_committed$card$name == 'Red Card') {
        if (event$team$name == 'Real Madrid') {
          rc_rm = rc_rm + 1
        } else {
          rc_fcb = rc_fcb + 1
        }
      }
    }
  }
  
  # attempts. save freeze_frame for plotting
  if (event$type$name == "Shot") {
    # active player location
    active_player = event$location
    
    # passive player locations
    # save freeze frame
    freeze_frame = event$shot$freeze_frame
    passive_players = matrix(nrow = length(freeze_frame), ncol = 2)
    for (i in 1:length(freeze_frame)) {
      passive_players[i,] = freeze_frame[[i]]$location
    }
    
    # plot
    plot_pitch(active_player, passive_players)
    
    break
  }
}

yc_fcb
yc_rm 
rc_fcb
rc_rm 

# plot pitch
plot_pitch <- function(active_player, passive_players){

  # draw empty pitch
  plot(120,80, xlim=c(0,120), ylim = c(0,80))
  abline(v=c(120, 60),  h = 80)
  
  # add active player location
  points(active_player[1], active_player[2], col = "blue", pch = 20)
  
  # add passive player locations
  points(passive_players, col = "red", pch = 20)
}
plot_pitch()

##########################################################################
# season stats for Barcelona
# win - draw - loss - goals for - goals against




####################### NOTES ####################
# included seasons : 2004/2005 - 2018/2019 (15 seasons)
# total matches = 452
# average matches per season = 30.13
# a lot of matches are missing from 2004/2005, 2005/2006 2006/2007 and 2007/2008 (<30 per season)
# if we don't include these seasons -> total matches = 374, average matches per season = 34
# full seasons with 38 matches : 2014/2015
# timestamp resets after break, i.e after half time it starts at 00:00:00.000
# event structure can be different in events
# goals : event[[i]]$shot$outcome$name == "Goal"
# assists : if there was a goal event -> event$shot$key_pass_id and look this up
# missing matches from season 2017/2018: Levante 5 - FCB 4, Málaga 0 - FCB 2 