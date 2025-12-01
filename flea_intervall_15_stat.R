library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(googlesheets4)
library(purrr)
library(lme4)
library(broom)



directory <- "D:/Flanders25/Videos/20250521/Data/Interval_15_CSV"
infile <- file.path(directory, "c")
interval <- fread(infile)


data_flightcage <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = 3)
data_Straight_flights <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = 2)
  
  data_flightcage <- as.data.table(data_flightcage) %>%
  select(VideoID, bat, trial, percent, total_weight) %>%
  slice(seq(3, length.out = 16)) %>%
  mutate(
    bat = as.character(bat),
    trial = as.character(trial),
    percent = as.factor(percent),
    total_weight = as.character(total_weight)
  )


 
  

interval <- interval %>%
  left_join(data_flightcage %>% select(VideoID, bat, percent, total_weight),
            by = "VideoID")

interval_filtered <- interval %>%
  group_by(Frame_ID) %>%
  mutate(min_y = min(y, na.rm = TRUE)) %>%
  filter(y <= min_y + 0.4) %>%
  ungroup()


ggplot (interval, aes(x = percent, y = speed))+
  geom_violin()



#percent


fit = lmer(speed ~ as.numeric(percent) + (1|bat), data = interval)
summary(fit)
plot(fit)


# individuals for percent
df_slopes_total <- interval %>%
  group_by(bat) %>%
  summarise({
    m <- lm(speed ~ as.numeric(percent))
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




#Plot 
interval_plot <- interval %>%
  mutate(
    total_weight_num = as.numeric(percent),
    bat = factor(bat, levels = c("1 red","2 blue","3 yellow","4 pink"))
  ) %>%
  filter(!is.na(speed), !is.na(total_weight_num), !is.na(bat))

# Plot wie im Screenshot
p <- ggplot(interval_plot, aes(x = total_weight_num, y = speed)) +
  geom_point(color = "grey50", alpha = 0.25, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~ bat, nrow = 2) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Speed total (%)",
    x = "total weight [g]",
    y = "Speed [m/s]"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

print(p)





df_slopes_relative_15 <- df_summary_Video %>%
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





df_summary_Frame <- interval %>%
  group_by(Frame_ID, bat, VideoID, percent, total_weight) %>%  # gruppieren pro Flug
  summarise(
    mean_speed = mean(speed, na.rm = TRUE),
    sd_speed   = sd(speed, na.rm = TRUE),
    min_speed  = min(speed, na.rm = TRUE),
    max_speed  = max(speed, na.rm = TRUE),
    range_speed = max(speed, na.rm = TRUE) - min(speed, na.rm = TRUE),
    n_points   = n(),                     # Anzahl Datenpunkte pro Frame_ID
    .groups = "drop"
  )



ggplot (df_summary_Frame, aes(x = percent, y = mean_speed))+
  geom_point()
       
df_slopes_total <- df_summary_Frame %>%
  group_by(bat) %>%
  summarise({
    m <- lm(mean_speed  ~ as.numeric(total_weight))
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


df_summary_Frame <- df_summary_Frame %>%
  mutate(total_weight = as.numeric(total_weight))


p <- ggplot(df_summary_Frame, aes(x = total_weight, y = mean_speed)) +
  geom_point(color = "grey50", alpha = 0.25, size = 1) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~ bat, nrow = 2) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Speed total (%)",
    x = "total weight [g]",
    y = "Speed [m/s]"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

print(p)

fit = lmer(speed ~ as.numeric(total_weight) + (1|bat), data = interval)
summary(fit)
plot(fit)