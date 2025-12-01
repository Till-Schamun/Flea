# --- Libraries ---
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(googlesheets4)
library(purrr)
library(lme4)

# --- Verzeichnis ---
directory <- "D:/Flanders25/Videos/Data_leis/CSV_leis"
infile_15 <- file.path(directory, "ALL_Intervall_15.csv")
inter<- fread(infile_15)
print (inter)


# y-filter
inter <- inter %>%
  group_by(Frame_ID) %>%
  mutate(min_y = min(y, na.rm = TRUE)) %>%
  filter(y <= min_y + 0.4) %>%
  ungroup()


data_flightcage <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA/edit?gid=825089310#gid=825089310",
  sheet = 3
)

data_flightcage <- as.data.table(data_flightcage) %>%
  select(VideoID, bat, trial, percent, total_weight) %>%
  slice(seq(3, length.out = 16)) %>%
  mutate(
    bat = as.character(bat),
    trial = as.character(trial),
    percent = as.factor(percent),
    total_weight = as.character(total_weight)
  )

inter <- inter[, c("Frame", "speed", "VideoID", "Frame_ID")]

inter <- inter %>%
  mutate(
    VideoID = VideoID %>%
      str_replace_all("\\.", "") %>%    # Punkte entfernen
      str_replace_all("__", "_")        # Doppel-Unterstrich zu einfach
  )



# --- Merge mit FlightCage-Daten ---
inter <- inter %>%
  left_join(data_flightcage %>% select(VideoID, bat, percent, total_weight),
            by = "VideoID") %>%
  mutate(
    C_ID = str_extract(VideoID, "C\\d+"),
    percent = as.factor(percent)
  ) %>%
  filter(!is.na(speed) & !is.na(percent))
inter %>% reframe(n = n(), .by = c(bat))


p_relativ <- ggplot(inter, aes(x = round(as.numeric(percent),2), y = speed)) +
  #geom_violin(fill = "skyblue", alpha = 0.6, trim = FALSE) +   # Violin statt Boxplot
  #geom_jitter(width = 0.2, size = 0.8, alpha = 0.4) +          # Punkte
  geom_point(alpha = 0.1)+
  geom_smooth(method = "lm")+
  theme_minimal() +
  labs(title = "Speed total (%)",
       x = "relativ weight [%]",
       y = "Speed [m/s]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~bat, nrow = 2)
summary(inter)


#Outdir <- "D:/Flanders25/Videos/Data_leis/Graphs_leis/"
#ggsave(
#  filename = paste0("Plot_Leis_relativweight", format(Sys.time(),"%Y%m%d_%H%M%S"),".png"),
#  plot     = p_relativ,
#  path     = outdir,
#  bg       = "white"   
#)#


p_total <- ggplot(df, aes(x = round(as.numeric(total_weight),2), y = speed)) +
  #geom_violin(fill = "skyblue", alpha = 0.6, trim = FALSE) +   # Violin statt Boxplot
  #geom_jitter(width = 0.2, size = 0.8, alpha = 0.4) +          # Punkte
  geom_point(alpha = 0.1)+
  geom_smooth(method = "lm")+
  theme_minimal() +
  labs(title = "Speed total (%)",
       x = "total weight [g]",
       y = "Speed [m/s]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~bat, nrow = 2)

#ggsave(
#  filename = paste0("Plot_Leis_totalweight", format(Sys.time(),"%Y%m%d_%H%M%S"),".png"),
#  plot     = p_total,
#  path     = outdir,
#  bg       = "white"   
#)

#outdir <- "D:/Flanders25/Videos/Data_leis/CSV_leis/"

#outfile <- file.path(directory, "dt_0.35_weight.csv")
#fwrite(df, outfile)



#all datapouints for percent
fit = lmer(speed~ as.numeric(total_weight)+(1|bat), data = inter)
summary(fit)
plot(fit)
hist(inter$speed)





df_summary_Video <- df %>%
  group_by(VideoID) %>%
  summarise(
    mean_speed = mean(speed, na.rm = TRUE),
    sd_speed   = sd(speed, na.rm = TRUE),
    min_speed    = min(speed, na.rm = TRUE),   # 🔹 Minimum
    max_speed    = max(speed, na.rm = TRUE),   # 🔹 Maximum
    range_speed  = max(speed, na.rm = TRUE) - min(speed, na.rm = TRUE),
    percent    = unique(percent),       # bleibt gleich pro Video
    total_weight = unique(total_weight),  # bleibt gleich pro Video
    bat    = unique(bat),         # bleibt gleich pro Video
    .groups = "drop"
  )
df_summary_bat <- df %>%
  group_by(bat) %>%
  summarise(
    mean_speed = mean(speed, na.rm = TRUE),
    sd_speed   = sd(speed, na.rm = TRUE),
    min_speed    = min(speed, na.rm = TRUE),   # 🔹 Minimum
    max_speed    = max(speed, na.rm = TRUE),   # 🔹 Maximum
    range_speed  = max(speed, na.rm = TRUE) - min(speed, na.rm = TRUE),
    bat    = unique(bat),         # bleibt gleich pro Video
    .groups = "drop"
  )
print(df_summary_Video)
print(df_summary_bat)

# Ergebnis C_ID# Ergebnis # Ergebnis # Ergebnis ansehen



#relative

fit_Video = lmer(mean_speed ~ as.numeric(percent)+(1|bat), data = df_summary_Video)
summary(fit_Video)
plot(fit_Video)
hist(df_summary_Video$mean_speed)



#for trend and error
df_slopes_relative <- df_summary_Video %>%
  group_by(bat) %>%
  summarise({
    m <- lm(mean_speed ~ as.numeric(percent))
    tibble(
      slope     = coef(summary(m))[2, "Estimate"],
      se_slope  = coef(summary(m))[2, "Std. Error"],
      t_value   = coef(summary(m))[2, "t value"],
      p_value   = coef(summary(m))[2, "Pr(>|t|)"],
      intercept = coef(summary(m))[1, "Estimate"],
      r2        = summary(m)$r.squared
    )
  }, .groups = "drop")

print(df_slopes_relative)


#plot for trebnd per batr
p_relativ_Video <- ggplot(df_summary_Video, aes(x = round(as.numeric(percent),2), y = mean_speed)) +
  #geom_violin(fill = "skyblue", alpha = 0.6, trim = FALSE) +   # Violin statt Boxplot
  #geom_jitter(width = 0.2, size = 0.8, alpha = 0.4) +          # Punkte
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm")+
  theme_minimal() +
  labs(title = "Speed total (%)",
       x = "total weight [g]",
       y = "Speed [m/s]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~bat, nrow = 2)



#Total

fit_Video = lmer(mean_speed ~ as.numeric(total_weight)+(1|bat), data = df_summary_Video)
summary(fit_Video)
plot(fit_Video)
hist(df_summary_Video$mean_speed)


#for trend and error
df_slopes_total <- df_summary_Video %>%
  group_by(bat) %>%
  summarise({
    m <- lm(mean_speed ~ as.numeric(total_weight))
    tibble(
      slope     = coef(summary(m))[2, "Estimate"],
      se_slope  = coef(summary(m))[2, "Std. Error"],
      t_value   = coef(summary(m))[2, "t value"],
      p_value   = coef(summary(m))[2, "Pr(>|t|)"],
      intercept = coef(summary(m))[1, "Estimate"],
      r2        = summary(m)$r.squared
    )
  }, .groups = "drop")

print(df_slopes_total)


#plot for trebnd per batr
p_relativ_Video <- ggplot(df_summary_Video, aes(x = round(as.numeric(total_weight),2), y = mean_speed)) +
  #geom_violin(fill = "skyblue", alpha = 0.6, trim = FALSE) +   # Violin statt Boxplot
  #geom_jitter(width = 0.2, size = 0.8, alpha = 0.4) +          # Punkte
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm")+
  theme_minimal() +
  labs(title = "Speed total (%)",
       x = "total weight [g]",
       y = "Speed [m/s]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~bat, nrow = 2)










