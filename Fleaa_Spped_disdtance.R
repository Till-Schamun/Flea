library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(googlesheets4)

library(stringr)
library(stringr)

directory <- "D:/Flanders25/videos/Data_leis/CSV_leis"



all_files <- list.files(  directory,
                          pattern = "Intervall_15\\.csv$",
             full.names = TRUE)

all_files <- all_files[!grepl("ALL_Intervall_15.csv", all_files)]
all_files <- all_files[!grepl("all_speed_dt035_Intervall_15.csv" , all_files)]


for (infile in all_files)
{

df <- fread(infile)

basename_in <- basename(infile)  
Base <- sub("_filtered\\.csv$", "", basename_in)
print(Base)


if ("Frame" %in% names(df)) {
  df <- df %>% arrange(Frame)
}

df <- fread(infile) 
df  <- df %>%
  select(x, y, z, speed)
df

df <- df %>%
  mutate(
    dx = x - lag(x),
    dz = z - lag(z),
    dist_step_zx = sqrt(dx^2 + dz^2),
    
    # Distanz zum Startpunkt (0-Punkt)
    dist_from_start_zx = sqrt((x - first(x))^2 + (z - first(z))^2)
  )

# Gesamtdistanz entlang ZX
total_dist_zx <- sum(df$dist_step_zx, na.rm = TRUE)

cat("Gesamtdistanz (ZX-Ebene):", total_dist_zx, "\n")




p1 = ggplot(df, aes(x = dist_from_start_zx, y = speed)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_line(alpha = 0.4) +
  labs(
    x = "Kumulative Distanz (ZX) [m]",
    y = "Geschwindigkeit [m/s]",
    title = "Speed vs. kumulative Distanz (ZX)"
  ) +
  theme_minimal()

print(p1)

#filename <- paste0("plot_leis_Speed_dist_Interval_15", Base, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")

#ggsave(
#  filename = filename,
#  plot     = p1,
#  path     = outdir,
#  width    = 8, height = 5, dpi = 300
#)


#cat("✅ Plot gespeichert:", file.path(outdir, filename), "\n")


}

