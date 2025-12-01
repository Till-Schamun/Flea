library(data.table)
library(tidyverse)
library(ggplot2)
library(dplyr)

directory <- "D:/Flanders25/Videos/Data_leis/CSV_leis"

# alle Dateien, die C0004 UND _filtered.csv enthalten
all_files <- list.files(
  directory,
  pattern = "20250520_C0002.*_filtered\\.csv$",   # <- hier ggf. andere C-ID einsetzen
  full.names = TRUE
)

fps <- 59.94
dt_values <- seq(0.05, 0.5, by = 0.05)

# HIER: Liste für alle Dateien zusammen
all_speeds_all_files <- list()

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
      idx
    })
    
    df_sub <- df[chosen_rows, ]
    
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
      mutate(
        dt_interval = dt_target
      )
    
    all_speeds[[as.character(dt_target)]] <- df_sub %>%
      select(dt_interval, speed)
  }
  
  if (length(all_speeds) == 0) next
  
  df_all <- bind_rows(all_speeds)
  df_all$dt_interval <- factor(df_all$dt_interval, levels = dt_values)
  
  # HIER: dieses df_all in die große Liste schreiben
  all_speeds_all_files[[basename(infile)]] <- df_all
}

# --- NACH der Schleife: alles zusammen und EIN Plot ---
if (length(all_speeds_all_files) > 0) {
  
  df_all_C0004 <- bind_rows(all_speeds_all_files)
  df_all_C0004$dt_interval <- factor(df_all_C0004$dt_interval, levels = dt_values)
  
  p <- ggplot(df_all_C0004, aes(x = dt_interval, y = speed)) +
    geom_boxplot(fill = "grey70", outlier.shape = NA, alpha = 0.9) +
    geom_jitter(width = 0.15, alpha = 0.4, size = 0.8) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Speed at different Δt – For trail one of bat one",
      x = expression(Delta*t~"(s)"),
      y = "Speed [m/s]"
    )
  
  print(p)
}
