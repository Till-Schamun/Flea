library(data.table)
library(dplyr)
library(stringr)
library(googlesheets4)

# --- Metadaten laden ---
data_Straight_flights <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = 2)
meta <- as.data.table(data_Straight_flights)

# Nur Zeilen mit befülltem Intervall_file; Basename extrahieren
meta <- meta[!is.na(Intervall_file) & Intervall_file != ""]
meta[, `:=`(
  start    = as.numeric(start),
  duration = as.numeric(duration),
  end      = as.numeric(end),
  filename = basename(trimws(as.character(Intervall_file)))
)]

# --- Dateien können in mehreren Ordnern liegen -------------------------------
dirs <- c(
  "D:/Flanders25/Videos/20250520/Data",
  "D:/Flanders25/Videos/20250521/Data"
)

find_in_dirs <- function(fname, dirs){
  cand <- file.path(dirs, fname)
  i <- which(file.exists(cand))[1]
  if (length(i) && !is.na(i)) cand[i] else NA_character_
}

meta[, filepath := vapply(filename, find_in_dirs, character(1L), dirs = dirs)]

# Nur die Zeilen, deren Datei tatsächlich existiert
meta_ok <- meta[!is.na(filepath) & file.exists(filepath)]
if (nrow(meta_ok) == 0L) {
  stop("Keine existierenden Dateien zu den Metadaten gefunden.")
}

fps <- 59.94
speedlow <- 2
speedhigh <- 10

# --- NEU: Output-Ordner ------------------------------------------------------
out_dir <- "D:/Flanders25/Videos/Data_leis/CSV_leis"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- Sammeldatei definieren (alte ggf. löschen) -----------------------------
big_csv <- file.path(out_dir, "ALL_Intervall_15.csv")
if (file.exists(big_csv)) file.remove(big_csv)

# --- Pipeline über die existierenden Dateien --------------------------------
for (k in seq_len(nrow(meta_ok))) {
  
  infile <- meta_ok$filepath[k]
  Name_infile <- meta_ok$filename[k]
  
  start   <- meta_ok$start[k]
  length  <- meta_ok$duration[k]
  end     <- start + length
  Bat_id  <- meta_ok$bat[k]
  trail   <- meta_ok$videofile[k]
  date    <- meta_ok$date[k]
  VideoID <- meta_ok$VideoID[k]
  
  message("\n--- Processing file: ", infile, " ---")
  
  # Datei einlesen
  df <- data.table::fread(infile)
  
  # Skalieren + sortieren
  df <- df %>%
    mutate(x = x/1000, y = y/1000, z = z/1000) %>%
    arrange(Frame)
  
  # Speed & TurnAngle
  df <- df %>%
    group_by(ID, Keypoint) %>%
    arrange(Frame, .by_group = TRUE) %>%
    mutate(
      dx = x - lag(x),
      dy = y - lag(y),
      dz = z - lag(z),
      dF = Frame - lag(Frame),
      dt = ifelse(dF > 0, dF / fps, NA_real_),
      speed = sqrt(dx^2 + dy^2 + dz^2) / dt,
      vx = dx/dt, vy = dy/dt, vz = dz/dt,
      num = lag(vx)*vx + lag(vy)*vy + lag(vz)*vz,
      den = sqrt(lag(vx)^2+lag(vy)^2+lag(vz)^2) * sqrt(vx^2+vy^2+vz^2),
      cosang = pmax(-1, pmin(1, num/den)),
      TurnAngle = acos(cosang)
    ) %>%
    ungroup()
  
  # Filtern: TurnAngle > 1 UND Speed im Bereich
  flight <- df %>%
    filter(Frame > start,
           Frame < end,
           Keypoint == "centroid_cm",
           TurnAngle < 1,
           between(speed, speedlow, speedhigh))
  
  print(summary(flight$speed))
  
  # VideoID + Frame_ID in Sammel-CSV anhängen
  if (nrow(flight) > 0) {
    flight <- flight %>%
      mutate(
        VideoID  = VideoID,
        Frame_ID = paste0(VideoID, "_", start)
      )
    data.table::fwrite(flight, big_csv, append = TRUE)
  }
  
  # Einzel-CSV pro Intervall
  outfile_CSV <- file.path(out_dir, paste0(VideoID, "_", start,"_", "Intervall_15.csv"))
  print(outfile_CSV)
  write.csv(flight, outfile_CSV, row.names = FALSE)
}

