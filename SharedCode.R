## Shared code for Sports Analytics Course

# ---------------------------------------------------------------------------
# --------------------------- Load Libraries --------------------------------
# ---------------------------------------------------------------------------
# supply a vector of desired packages. Packages will be installed if not already installed in RStudio

load_packages <- function(packages = c()) {
  if (length(packages) == 0) {
    print('You did not specify any packages/libraries to load')
  } 
  else {
    for (i in packages){
      if(! i %in% installed.packages()){
        install.packages(i, dependencies = TRUE)
      }
      suppressMessages(suppressWarnings(library(i, character.only=T)))
    }
  }
}


# -----------------------------------------------------------------------------------------------
# ------------ Checks for a valid playId and gameId; optionally check for valid frameId ---------
# -----------------------------------------------------------------------------------------------
check_playId_gameId <- function(data,
                                game_Id, 
                                play_Id, 
                                frame_Id = c()) {
  if (!game_Id %in% unique(data$gameId)) {
    return("the game_Id you have used is not in the data frame.")
  }
  if (!play_Id %in% unique(data$playId)) {
    return("the play_Id you have used is not in the data frame.")
  }
  if(!missing(frame_Id)) {
    load_packages(c("dplyr"))
    df <- data %>% filter(gameId == game_Id, playId == play_Id) %>% data.frame()
    if (!frame_Id %in% unique(df$frameId)) {
      return("the frameId you want is not in the data frame for this particular gameId and playId.")
    } 
  } 
  return("ok")
}


# ---------------------------------------------------------------------------------
# --------------------- Load & Merge Data for a Single Week -----------------------
# ---------------------------------------------------------------------------------
# read in the data file for a given week and optionally merge with other data
# x and y coordinates are also revised in order to have consistent play directions
load_data_for_one_week <- function(directory,
                                   weekNumber = 1,
                                   merge      = F) {
  
  if (missing(directory) | missing(weekNumber) | !weekNumber %in% seq(9) ) {
    if (missing(directory)) {
      print('You did not specify a directory for your data files. Where are your files located?')
    }
    if (missing(weekNumber) | !weekNumber %in% seq(9)  ) {
      print('You did not specify a valid week number (valid numbers are 1 to 9).')
    }
    stop('Function aborting: unable to run due to missing parameters.')
  }
  
  load_packages(c("data.table", "dplyr"))
  
  filename <- paste0(directory, "/tracking_week_",weekNumber,".csv")
  df <- fread(filename)
  
  if (merge == T) {
    plays       <- fread(paste0(directory, "/plays.csv"))           #  16124 rows x 50 columns
    players     <- fread(paste0(directory, "/players.csv"))         #   1697 rows x  7 columns
    games       <- fread(paste0(directory, "/games.csv"))           #    136 rows x  9 columns
    player_play <- fread(paste0(directory, "/player_play.csv"))     # 354727 rows x 50 columns
    
    df <- merge(df, plays, by = c("gameId", "playId"), all.x = TRUE)
    df <- merge(df, players, by = c("nflId"), all.x = TRUE)
    df <- merge(df, pff, by = c("gameId", "playId", "nflId"), all.x = TRUE)
    df <- merge(df, games, by = c("gameId"), all.x = TRUE)
  }
  
  df <- df %>%
    mutate( x = ifelse(playDirection == "right", 120-x, x),
            y = ifelse(playDirection == "right", 160/3-y, y ),
            team = factor(team, 
                          levels = c("football",   "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", 
                                     "CIN", "CLE", "DAL", "DEN", "DET", "GB",  "HOU", "IND", 
                                     "JAX", "KC",  "LA",  "LAC", "LV",  "MIA", "MIN", "NE",  
                                     "NO",  "NYG", "NYJ", "PHI", "PIT", "SEA", "SF",  "TB",  
                                     "TEN", "WAS"))
    ) %>%
    data.frame()
  
  return(df)
}





test_print <- function() {
  print("this is a test print function")
}