library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(googlesheets4)

# === Verzeichnis ===
directory <- "D:/Flanders25/Videos/Data_leis/CSV_leis"
data_flightcage <- read_sheet("https://docs.google.com/spreadsheets/d/1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA/edit?gid=825089310#gid=825089310", sheet = 3)
# === Alle CSV-Dateien finden ===
all_files <- list.files(
  directory,
  pattern = "_Intervall_15\\.csv$",
  full.names = TRUE
)

# === Parameter ===
fps <- 59.94
dt_target <- 0.35


df_all <- data.table()

for (infile in all_files) {
  message("\nVerarbeite Datei: ", basename(infile))
  df <- fread(infile)
  
  if (nrow(df) < 3) {
    warning("Datei übersprungen (zu wenig Daten): ", infile)
    next
  }
  
 
  file_id  <- str_remove(basename(infile), "_filtered\\.csv$")   # Punkt escapen + ans Ende ankern
  video_id <- str_extract(file_id, "\\d{8}_C\\d+")               # z.B. 20250520_C0002
  

  
#check for video id
  if (is.na(video_id)) {
    warning("Konnte VideoID nicht extrahieren für Datei: ", infile, " | file_id: ", file_id)
    next
  }
  message("VideoID: ", video_id, " | FrameID: ", file_id)
  

  df <- df %>%
    arrange(Frame) %>%
    mutate(Time = Frame / fps)
  
  # --- Zeitpunkte im Abstand dt_target ---
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
  
#speed
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
      dt_interval = dt_target,
      VideoID = .env$video_id,    
      FrameID = .env$file_id    
    ) %>%
    filter(is.finite(speed))
  
  df_all <- rbind(df_all, as.data.table(df_sub), fill = TRUE)a
}


# === Ergebnis prüfen ===
print(df_all)
cat("\n✅ Gesamtanzahl Zeilen:", nrow(df_all), "\n")

# === Optional: als CSV speichern ===
outfile <- file.path(directory, "all_speed_dt035_Intervall_15.csv")
fwrite(df_all, outfile)
cat("✔️ Ergebnisse gespeichert unter:", outfile, "\n")




