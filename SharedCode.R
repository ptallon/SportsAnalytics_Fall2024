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

    df <- left_join(df, games,       by = c("gameId"))
    df <- left_join(df, plays,       by = c("gameId", "playId"))
    df <- left_join(df, players,     by = c("nflId", "displayName"))
    df <- left_join(df, player_play, by = c("gameId", "playId", "nflId"))
    
  }
  
  df <- df %>%
    mutate( x = ifelse(playDirection == "right", 120-x, x),
            y = ifelse(playDirection == "right", 160/3-y, y ),
            team = factor(club, 
                          levels = c("football",   "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", 
                                     "CIN", "CLE", "DAL", "DEN", "DET", "GB",  "HOU", "IND", 
                                     "JAX", "KC",  "LA",  "LAC", "LV",  "MIA", "MIN", "NE",  
                                     "NO",  "NYG", "NYJ", "PHI", "PIT", "SEA", "SF",  "TB",  
                                     "TEN", "WAS"))
    ) %>%
    data.frame()
  
  return(df)
}

# ---------------------------------------------------------------------------------
# -------------------- Load & Merge Data for Specified Weeks ----------------------
# ---------------------------------------------------------------------------------
# read in data files for specified weeks and row bind them together to form a single
# data frame
load_data_for_specified_weeks <- function(directory, 
                                          these_weeks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                          merge = F) {
  for (week in these_weeks) {
    df <- load_data_for_one_week(directory, week, merge)
    if (week == these_weeks[1]) {
      big_df <- data.frame(df)
    }
    else {
      big_df <- rbind(big_df, df)
    }
  }
  return(big_df)
}


# ---------------------------------------------------------------------------------
# -------------------- Load & Merge Data for All Weeks ----------------------
# ---------------------------------------------------------------------------------
# read in data files for all weeks and row bind them together to form a single
# data frame
load_data_for_all_weeks <- function(directory, 
                                    merge = F) {
  for (week in seq(1,9)) {
    df <- load_data_for_one_week(directory, week, merge)
    if (week == 1) {
      big_df <- data.frame(df)
    }
    else {
      big_df <- rbind(big_df, df)
    }
  }
  return(big_df)
}

# ---------------------------------------------------------------------------------
# ---------------------- Animate a Single Play in a Game --- ----------------------
# ---------------------------------------------------------------------------------
# pass in a dataframe where you have filtered the gameId and playId.

visualize_single_play <- function(game_df,
                                  highlight_players_in_motion = FALSE) {
  
  if(!length(unique(game_df$gameId)) == 1) {
    stop('There is more than one gameId in your data. Please pass in a dataframe for the exact gameId and playId you want to visualize.')
  }  
  
  load_packages(c("hms", "stringr"))
  
  source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')
  
  g <- ggplot(data = game_df, aes(x = x, y = y), color=team, fill=team) +
    # customize colors, shapes, and sizes of players and the football
    scale_size_manual(values = c(6, 4, 6), guide = "none") +
    scale_shape_manual(values = c(21, 16, 21), guide = "none") +
    scale_fill_manual(values = c("firebrick1", "black", "white", "dodgerblue1"), 
                      na.value = NA,
                      guide="none") + 
    scale_colour_manual(values = c("black", "#663300", "black","black"), 
                        na.value = NA,
                        guide="none") + 
    
    gg_field(yardmin = max(-5,min(game_df$x)-5), yardmax = min(max(game_df$x)+5, 125) ) +
    
    # add points to plot for all players and the football
    geom_point(aes(shape = team, colour = team, size = team, fill = team))
  
  if(highlight_players_in_motion) {
    g <- g + 
      geom_point(data = game_df %>% filter(inMotionAtBallSnap == T), 
                 aes(x = x, y = y), 
                 shape = 21, 
                 colour = ifelse(highlight_players_in_motion == T, "black", "NA"), 
                 size = 6, 
                 fill = ifelse(highlight_players_in_motion == T, "black", "NA"))    
  }
  
  g <- g +
    # insert jersey number for each player
    geom_text( data = game_df %>% filter(team != "football"),
               aes(x = x, y = y, 
                   label = jerseyNumber), colour = "white", size = 3.5, vjust = 0.36 ) +
    
    # add some labels to report the play description
    labs(title = game_df$playDescription) +
    
    # set the theme to dark green to color the areas beyond the end zones
    theme(panel.background = element_rect(fill = "forestgreen", 
                                          color = "forestgreen"), panel.grid = element_blank()) +
    guides(alpha = "none") +
    transition_time(frameId)
  
  frames_to_display <- length(unique(game_df$frameId))
  game_df$hms <- as_hms(game_df$time)
  game_df$seconds <- hour(game_df$hms)*3600 + minute(game_df$hms)*60 + second(game_df$hms)
  fps <- frames_to_display/(max(game_df$seconds) - min(game_df$seconds))
  
  animate(g, 
          fps=fps, 
          nframe = frames_to_display, 
          width = 480, 
          height = 280, 
          renderer = gifski_renderer() 
  ) 
}



# ------------------------------------------------------------------------------------
# ---------------------- Visualize a Single Frame in a Game --- ----------------------
# ------------------------------------------------------------------------------------
# pass in a dataframe where you have filtered the gameId and playId.

visualize_single_frame <- function(game_df,
                                   highlight_players_in_motion = FALSE,
                                   highlight_matchup = FALSE,
                                   show_Matchup = FALSE,
                                   frame_number = 1) { 
  
  if(!length(unique(game_df$gameId)) == 1) {
    stop('There is more than one gameId in your data. Please pass in a dataframe for the exact gameId and playId you want to visualize.')
  }  
  
  if(!frame_number %in% game_df$frameId) {
    stop('The frame number you are looking to view is not in the data frame. Please check the frame number.')
  }
  
  load_packages(c("dplyr", "stringr", "ggrepel"))
  
  game_df <- game_df %>% filter(frameId == frame_number) %>% data.frame()
  
  source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')
  
  g <- ggplot(data = game_df, aes(x = x, y = y), color=team, fill=team) +
    # customize colors, shapes, and sizes of players and the football
    scale_size_manual(values = c(6, 4, 6), guide = "none") +
    scale_shape_manual(values = c(21, 16, 21), guide = "none") +
    scale_fill_manual(values = c("firebrick1", "black", "dodgerblue1"), 
                      na.value = NA,
                      guide="none") + 
    scale_colour_manual(values = c("black", "#663300", "black"), 
                        na.value = NA,
                        guide="none") + 
    
    gg_field(yardmin = max(-5,min(game_df$x)-5), yardmax = min(max(game_df$x)+5, 125) ) 
  
    # highlight with a pink halo those defensive players who are matched up with an offensive player
    if(highlight_matchup) {
      g <- g +
        geom_point( data = game_df %>% filter(!is.na(pff_primaryDefensiveCoverageMatchupNflId) |
                                                !is.na(pff_secondaryDefensiveCoverageMatchupNflId)),
                    aes(x = x, y = y),
                    shape = 21,
                    colour = "hotpink",
                    size = 8,
                    stroke = 1,
                    fill = "hotpink")
    }
  
  # add points to plot for all players and the football
  g <- g + geom_point(aes(shape = team, colour = team, size = team, fill = team))
    
  if(highlight_players_in_motion) {
     g <- g + 
       geom_point(data = game_df %>% filter(inMotionAtBallSnap == T), 
                  aes(x = x, y = y), 
                  shape = 21, 
                  colour = ifelse(highlight_players_in_motion == T, "black", "NA"), 
                  size = 6, 
                  fill = ifelse(highlight_players_in_motion == T, "black", "NA"))     
  }
  
  if(show_Matchup) {
    g <- g +
      geom_label_repel(data = game_df %>% filter(!is.na(pff_primaryDefensiveCoverageMatchupNflId) |
                                                   !is.na(pff_secondaryDefensiveCoverageMatchupNflId)), 
                       aes(x = x, y = y,  
                           label=paste("->", pff_primaryDefensiveCoverageMatchupNflId)),
                       box.padding = 0,
                       point.padding = 1,
                       size = 4,
                       color = "black",
                       min.segment.length = 2,
                       direction = "both",
                       segment.color = 'darkblue')  
  }
  
  g <- g +
    # insert jersey number for each player
    geom_text( data = game_df %>% filter(team != "football"),
               aes(x = x, y = y, 
                   label = jerseyNumber), colour = "white", size = 3.5, vjust = 0.36 ) +
    
    # add some labels to report the play description
    labs(title = str_wrap(game_df$playDescription), 30) +
    
    # set the theme to dark green to color the areas beyond the end zones
    theme(panel.background = element_rect(fill = "forestgreen", 
                                          color = "forestgreen"), panel.grid = element_blank()) +
    guides(alpha = "none")  

  g  
}

