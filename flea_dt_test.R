library(data.table)
library(tidyverse)
library(ggplot2)
library(dplyr)

directory <- "D:/Flanders25/Videos/Data_leis/CSV_leis"


all_files <- list.files(
  directory,
  pattern = "_filtered\\.csv$",
  full.names = TRUE
)[11:30]

fps <- 59.94
dt_values <- seq(0.05, 0.5, by = 0.05)


for (infile in all_files) {
  message("\nVerarbeite Datei: ", basename(infile))
  df <- fread(infile)
  
  if (nrow(df) < 2) {
    warning("Datei ", infile, " ist leer oder zu kurz – übersprungen.")
    next
  }
  
  df <- df %>%
    arrange(Frame) %>%
    mutate(Time = Frame / fps)
  
  all_speeds <- list()
  
  for (dt_target in dt_values) {
    time_seq <- seq(from = min(df$Time, na.rm = TRUE),
                    to   = max(df$Time, na.rm = TRUE),
                    by   = dt_target)
    
    chosen_rows <- sapply(time_seq, function(t) {
      idx <- which(df$Time >= t)[1]
      if (is.na(idx)) idx <- nrow(df)
      return(idx)
    })
    
    df_sub <- df[chosen_rows, ]
    
#skip points if to few
    if (nrow(df_sub) < 3) next
    
    df_sub <- df_sub %>%
      group_by(ID, Keypoint) %>%
      arrange(Frame, .by_group = TRUE) %>%
      mutate(
        dx = x - lag(x),
        dy = y - lag(y),
        dz = z - lag(z),
        dF = Frame - lag(Frame),
        dt = ifelse(dF > 0, dF / fps, NA_real_),
        speed = sqrt(dx^2 + dy^2 + dz^2) / dt
      ) %>%
      ungroup() %>%
      mutate(dt_interval = dt_target)
    
    all_speeds[[as.character(dt_target)]] <- df_sub %>% select(dt_interval, speed)
  }
  

  if (length(all_speeds) == 0) next
  
  df_all <- bind_rows(all_speeds)
  df_all$dt_interval <- factor(df_all$dt_interval, levels = unique(dt_values))
  
  p <- ggplot(df_all, aes(x = dt_interval, y = speed)) +
    geom_boxplot(fill = "grey70", outlier.shape = NA, alpha = 0.9) +
    geom_jitter(width = 0.15, alpha = 0.4, size = 0.8) +
    theme_minimal(base_size = 14) +
    labs(
      title = paste("speed at different Δt –", basename(infile)),
      x = expression(Delta*t~"(s)"),
      y = "Speed [m/s]"
    )


