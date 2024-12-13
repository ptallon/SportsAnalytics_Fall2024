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
                                   merge      = F,
                                   columns    = c()) {
  
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
  
  # based on the direction of the play, map the x and y coordinates to be consistently in one direction
  df <- df %>%
    mutate( x = ifelse(playDirection == "right", 120-x, x),
            y = ifelse(playDirection == "right", 160/3-y, y),
            targetX = ifelse(playDirection == "right", 120-targetX, targetX),
            absoluteYardlineNumber = ifelse(playDirection == "right", 100 - absoluteYardlineNumber, absoluteYardlineNumber)
    ) %>%
    data.frame()
  
  df  <- df %>%
    mutate( team = ifelse( club == homeTeamAbbr, "home", ifelse( club == "football", "football", "away"))) %>%
    mutate( team = factor(team, levels = c("away", "football", "home"))) %>%
    data.frame()
  
  if(length(columns) > 0) {
    for(col in columns) {
      if(!col %in% colnames(df)) {
        stop(paste("Check the columns you want as",col,"is not in the data frame. No data frame returned."))
      }
    }
    df <- df %>%
      select(unique(columns)) %>% data.frame()
  }
  return(df)
}


# ---------------------------------------------------------------------------------
# -------------------- Load & Merge Data for Specified Weeks ----------------------
# ---------------------------------------------------------------------------------
# read in data files for specified weeks and row bind them together to form a single
# data frame
load_data_for_specified_weeks <- function(directory, 
                                          these_weeks = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                          merge = F,
                                          columns = c()) {
  for (week in these_weeks) {
    df <- load_data_for_one_week(directory, week, merge, columns)
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
                                    merge = F,
                                    columns = c()) {
  for (week in seq(1,9)) {
    df <- load_data_for_one_week(directory, week, merge, columns)
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
# ------------------------- Data Frame of Red Zone Plays --------------------------
# ---------------------------------------------------------------------------------
# pass in a dataframe; return a data frame of red zone plays.
red_zone_plays <- function(game_df) {
  redzone_df <- game_df %>%
    filter(absoluteYardlineNumber <= 20) %>%
    data.frame()
  return(redzone_df)
}


# ---------------------------------------------------------------------------------
# ---------------- Add Voronoi Polygon Coordinates to each x, y pair --------------
# ---------------------------------------------------------------------------------
# Compute Voronoi Polygons for each x,y pair. Include Voronoi area and Polygon coordinates.
# Depending on the df size, this code could take several hours to run.
include_voronoi_polygons_and_area <- function(df,
                                              remove_frames_before_lineset = FALSE) {
  
  load_packages(c("deldir", "dplyr"))
  
  # Function to compute Voronoi tessellation for a group
  compute_voronoi <- function(data) {
    # Perform Voronoi tessellation
    voronoi <- deldir(data$x, data$y)
  
    # Extract areas
    tryCatch(data$area <- voronoi$summary$dir.area, error = function(err){NA})
  
    # Extract polygons and add as a list-column
    polygons <- tile.list(voronoi)
    tryCatch(data$polygon <- lapply(polygons, function(p) {data.frame(x = p$x, y = p$y)}), error = function(err){NA})
  
    return(data)
  }

  if(remove_frames_before_lineset) {
    df <- remove_frames_before_lineset(df, renumberFrames = TRUE)
  }
  
  frames_df <- df %>% 
    filter(team != "football") %>%
    group_by(gameId, playId, frameId) %>%
    group_modify(~ compute_voronoi(.x)) %>%
    ungroup() %>% 
    data.frame()

  football_df <- df %>% 
    filter(team == "football") %>%
    mutate(area = NA, polygon = NA) %>%
    data.frame()

  frames_df <- rbind(frames_df, football_df) %>% arrange(gameId, playId, frameId, team)
  return(frames_df)
}


# ---------------------------------------------------------------------------------
# ---------------------- Animate a Single Play in a Game --------------------------
# ---------------------------------------------------------------------------------
# pass in a dataframe where you have filtered the gameId and playId.

visualize_single_play <- function(game_df,
                                  highlight_players_in_motion = FALSE,
                                  show_targetXY = FALSE) {
  
  if(!length(unique(game_df$gameId)) == 1) {
    stop('There is more than one gameId in your data. Please pass in a dataframe for the exact gameId and playId you want to visualize.')
  }  
  
  load_packages(c("hms", "stringr", "gganimate", "gifski"))
  
  source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')
  
  g <- ggplot(data = game_df, aes(x = x, y = y), color=team, fill=team) +
    # customize colors, shapes, and sizes of players and the football
    scale_size_manual(values = c(6, 4, 6), guide = "none") +
    scale_shape_manual(values = c(21, 16, 21), guide = "none") +
    scale_fill_manual(values = c("firebrick1", "black", "dodgerblue1", "white"), 
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
  
  # show the target X and Y if both are not NA and the user wants to view this
  if(show_targetXY) {
    if( sum(!is.na(game_df$targetX)) != 0 & sum(!is.na(game_df$targetY)) != 0 ) {
      g <- g + 
        geom_point(aes(x = game_df$targetX, y = game_df$targetY), 
                   shape=21, size = 6, colour="black", fill="white", alpha = 0.5) +
        annotate("text", x = game_df$targetX, y = game_df$targetY, 
                 label = "X", colour = "black", hjust = 0.5, vjust = 0.4)
    }
  }
  
  yardLine <- unique(game_df$absoluteYardlineNumber) + 20
  
  g <- g +
    # insert jersey number for each player
    geom_text( data = game_df %>% filter(team != "football"),
               aes(x = x, y = y, 
                   label = jerseyNumber), colour = "white", size = 3.5, vjust = 0.36 ) +
    
    # show the absolute line
    annotate("segment", x = yardLine, xend = yardLine, y = 0, yend = 53.33, colour = "yellow", linewidth = 1) +

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
# ---------------------- Visualize a Single Frame in a Game --------------------------
# ------------------------------------------------------------------------------------
# pass in a dataframe where you have filtered the gameId and playId. Tell it what frame you want to see.
# also pass in the player_play data frame since it needs this to complete the match up.

visualize_single_frame <- function(game_df,
                                   highlight_players_in_motion = FALSE,
                                   highlight_matchup = FALSE,
                                   show_Matchup = FALSE,
                                   show_Voronoi = FALSE,
                                   frame_number = 1) {  
  
  if(!frame_number %in% unique(game_df$frameId)) {
    stop('The frameId you are using is not found in the data frame. Please check your frameId and try again.')
  }
  
  load_packages(c("dplyr", "ggrepel"))

  game_df <- game_df %>%
    filter(frameId == frame_number) %>%  
    data.frame()
    
  player_play <- data.frame(game_df)
  
  game_df <- game_df %>%
    left_join(player_play %>% select(nflId, jerseyNumber) %>% distinct(), 
              by = c("pff_primaryDefensiveCoverageMatchupNflId" = "nflId" )  ) %>%
    rename("matchup_jerseyNumber1" = "jerseyNumber.y",
           "jerseyNumber" = "jerseyNumber.x") %>%
    left_join(player_play %>% select(nflId, jerseyNumber) %>% distinct(), 
              by = c("pff_secondaryDefensiveCoverageMatchupNflId" = "nflId" )  ) %>%
    rename("matchup_jerseyNumber2" = "jerseyNumber.y",
           "jerseyNumber" = "jerseyNumber.x") %>%
    data.frame() 

  yardLine <- unique(game_df$absoluteYardlineNumber) + 20
  
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
    
    gg_field(yardmin = max(-5,min(game_df$x)-5), yardmax = min(max(game_df$x)+5, 125) ) +
    
    # show the absolute line
    annotate("segment", x = yardLine, y = 0, xend = yardLine, yend = 53.33, color="yellow", linewidth = 1) 
  
  if(show_Voronoi) {
    
    if(! "ggvoronoi" %in% installed.packages()){
      suppressMessages(suppressWarnings(
        remotes::install_github("garretrc/ggvoronoi", dependencies = TRUE, build_opts = c("--no-resave-data"))
      ))
    }
    
    suppressMessages(suppressWarnings(library(ggvoronoi)))
    
    box <- data.frame(
      x = c( max(-5,min(game_df$x)-5), 
             min(max(game_df$x)+5, 125), 
             min(max(game_df$x)+5, 125), 
             max(-5,min(game_df$x)-5)  ),
      y = c( 0,
             0,
             53.33, 
             53.33)
    )
    
    g <- g +
      geom_voronoi(data = game_df, aes(x = x, y = y), alpha = 0.25, fill="white", outline = box) + 
      stat_voronoi(data = game_df, aes(x = x, y = y), geom = "path", outline = box) 
  }
  
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
                           label=paste(position, "\u2192", matchup_jerseyNumber1, 
                                       ifelse(!is.na(pff_secondaryDefensiveCoverageMatchupNflId), 
                                              matchup_jerseyNumber2, 
                                              ""))), 
                       box.padding = 1,
                       point.padding = 1,
                       size = 4,
                       color = "black",
                       direction = "both",
                       segment.color = 'white')  
  }
  
  frame_details <- paste("game:", unique(game_df$gameId), "  play:",unique(game_df$playId), "  frame:",unique(game_df$frameId)) 

  g <- g + 
    # insert jersey number for each player
    geom_text( data = game_df %>% filter(team != "football"),
               aes(x = x, y = y, 
                   label = jerseyNumber), colour = "white", size = 3.5, vjust = 0.36 ) +
    
    annotate("text", x = min(game_df$x)-2, y = 56, label = frame_details, 
             colour = "white", hjust=0) +
    
    # add some labels to report the play description
    labs(title = game_df$playDescription) +
    
    # set the theme to dark green to color the areas beyond the end zones
    theme(panel.background = element_rect(fill = "forestgreen", 
                                          color = "forestgreen"), panel.grid = element_blank()) +
    guides(alpha = "none")  

  g  
}


# ------------------------------------------------------------------------------------
# -------------------------- Motion Stats in a Given Week ----------------------------
# ------------------------------------------------------------------------------------
# pass in a merged df and return all motion stats for that week.

motion_stats_single_week <- function(df, player_play) {  

  # check to see if a column called team exists in df
  if(!"team" %in% colnames(df)){
    df  <- df %>%
      mutate( team = ifelse( club == homeTeamAbbr, "home", ifelse( club == "football", "football", "away"))) %>%
      data.frame()
  }

  # check if week and jerseyNumber are in player_play
  if(!"jerseyNumber" %in% colnames(player_play)){
    player_play <- player_play %>%
      left_join(df %>% select(nflId, jerseyNumber) %>% distinct(), by = c("nflId"))
  }
  if(!"week" %in% colnames(player_play)){
    player_play <- player_play %>%
      left_join(df %>% select(nflId, week) %>% distinct(), by = c("nflId"))
  }
  
  df1 <- player_play[player_play$inMotionAtBallSnap == TRUE,]
  
  plays_with_multiple_players_in_motion <- df1 %>%
    select(gameId, playId, nflId, jerseyNumber, week) %>%
    group_by(gameId, playId, week) %>%
    summarise(n = n(), .groups = 'keep') %>%
    arrange(-n) %>%
    filter(week == 1, n > 1) %>%
    data.frame()
  
  for(each_pair in list(distinct(plays_with_multiple_players_in_motion[c("gameId", "playId")]))) {
    game_Id <- each_pair[,1]
    play_Id <- each_pair[,2]
    
    print(paste(game_Id, play_Id))
  }  
  
  motion_df <- data.frame(displayName   = as.character(),
                          sum_abs_dy    = as.numeric(),
                          max_sum_dy_d1 = as.numeric(),
                          max_sum_dy_d2 = as.numeric(),
                          interval = as.Date(character()),
                          speed_yrds_second = as.numeric(),
                          gameId = as.integer(),
                          playId = as.integer()  ) 
  
  for(row in 1:nrow(plays_with_multiple_players_in_motion)) {
    game_Id <- plays_with_multiple_players_in_motion[row, "gameId"]
    play_Id <- plays_with_multiple_players_in_motion[row, "playId"]
    
    game_df <- df %>%
      
      # isolate the player in motion during this play
      filter(gameId == game_Id, playId == play_Id, 
             frameType == "BEFORE_SNAP", inMotionAtBallSnap == TRUE) %>%
      
      # reduce the number of columns to something more manageable
      select(displayName, jerseyNumber, x, y, dis, frameId, frameType, time, gameId, playId, week) %>%
      
      # to do cumulative sum, you must sort first
      arrange(gameId, playId, displayName, frameId) %>%
      
      # group by
      group_by(gameId, playId, displayName) %>%
      
      # set the direction of the motion
      mutate( motion_direction = ifelse( y - lag(y) < 0, 1, 
                                         ifelse( y - lag(y) > 0, 2, NA)),
              dy = ifelse(y == lag(y), NA,  (y - lag(y)))) %>%
      
      # calculate cumulative totals over the course of the play
      group_by(gameId, playId, displayName, motion_direction) %>%
      mutate(sum_dy     = cumsum(dy),
             sum_dy_d1  = ifelse(dy < 0, cumsum(dy), NA   ),
             sum_dy_d2  = ifelse(dy > 0, cumsum(dy), NA   )) %>%
      
      group_by(gameId, playId, displayName) %>%
      mutate(sum_abs_dy = sum(abs(dy), na.rm = T)) %>%
      mutate(max_sum_dy_d1 = min(0, sum_dy_d1, na.rm = T)) %>%
      mutate(max_sum_dy_d2 = max(0, sum_dy_d2, na.rm = T)) %>%
      mutate(max_time = max(0, time)) %>%
      mutate(min_time = min(Inf, time)) %>%
      mutate(interval = difftime(max_time, min_time,units = "secs")) %>%
      mutate(speed_yrds_second = sum_abs_dy / as.double(interval)) %>%
      
      # report out key metrics on last row
      slice(n()) %>%
      
      select(displayName, sum_abs_dy, max_sum_dy_d1, max_sum_dy_d2, interval, speed_yrds_second, gameId, playId, week) %>% 
      
      # convert it to a data.frame  
      data.frame()  
    
    motion_df <- rbind(motion_df, game_df)
    
  }
  return(motion_df)
}

# ------------------------------------------------------------------------------------
# ------------------------- Remove Frames prior to Lineset ---------------------------
# ------------------------------------------------------------------------------------
# pass in a merged df and return a revised df without the frame prior to lineset.
# renumberFrames will renumber the frame IDs so that the frameId starts at 1

remove_frames_before_lineset <- function(df, renumberFrames = TRUE) {  
  
  # keep rows after line_set is found, NOT EVERY gameId/playId has a line_set
  lineset_df <- df %>%
    arrange(gameId, playId, frameId) %>%
    group_by(playId, gameId) %>%
    mutate(m1 = which(event=='line_set', arr.ind=TRUE)[1] ) %>%
    filter( row_number(gameId) >= unique(m1) )%>%
    ungroup() %>%
    data.frame()
  
  if(renumberFrames) {
    lineset_df <- lineset_df %>%
      group_by(gameId, playId) %>%
      mutate(frameId = frameId - min(frameId) + 1) %>%
      ungroup() %>%
      data.frame()
  }
  return(lineset_df)
}
