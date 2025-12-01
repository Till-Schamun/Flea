# --- Libraries ---
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(googlesheets4)
library(purrr)
library(lme4)
library(lmerTest)  
library(tidyr)


directory <- "D:/Flanders25/Videos/Data_leis/CSV_leis/"
infile    <- file.path(directory, "all_speed_dt035_Intervall_15.csv")
inerval   <- fread(infile)
print(inerval)


data_flightcage <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA/edit?gid=825089310#gid=825089310",
  sheet = 3
)

data_flightcage <- as.data.table(data_flightcage) %>%
  select(VideoID, bat, trial, percent, total_weight) %>%
  slice(seq(3, length.out = 16)) %>%
  mutate(
    bat          = as.character(bat),
    trial        = as.character(trial),
    percent      = as.character(percent),
    total_weight = as.character(total_weight)
  )


inerval <- inerval[, c("Frame", "speed", "VideoID", "FrameID")]


inerval <- inerval %>%
  left_join(data_flightcage %>% select(VideoID, bat, percent, total_weight),
            by = "VideoID") %>%
  mutate(
    C_ID    = str_extract(VideoID, "C\\d+"),
    bat     = as.factor(bat)
  ) %>%
  filter(!is.na(speed), !is.na(percent), !is.na(total_weight))


inerval %>% reframe(n = n(), .by = bat)


num <- function(x) as.numeric(stringr::str_replace_all(as.character(x), "[^0-9\\.-]", ""))

#numeric for percent
inerval <- inerval %>%
  mutate(
    percent_num      = num(percent),
    total_weight_num = num(total_weight)
  )


#all points relative

inerval_slopes_relative <- inerval %>%
  group_by(bat) %>%
  summarise({
    m  <- lm(speed ~ percent_num)
    cs <- coef(summary(m))
    tibble(
      slope     = cs[2, "Estimate"],
      se_slope  = cs[2, "Std. Error"],
      t_value   = cs[2, "t value"],
      p_value   = cs[2, "Pr(>|t|)"],
      intercept = cs[1, "Estimate"],
      r2        = summary(m)$r.squared
    )
  }, .groups = "drop")

print(inerval_slopes_relative)


#fit_rel_points <- lmer(speed ~ percent_num + (1 + percent_num | bat), data = inerval)
#summary(fit_rel_points)

ranef(fit_rel_points)
coef(fit_rel_points)$bat



fit_rel_points <- lmer(speed ~ percent_num + (1 | bat), data = inerval)
summary(fit_rel_points)
plot(fit_rel_points)
hist(inerval$speed)


p_relativ <- ggplot(inerval, aes(x = percent_num, y = speed)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    title = "Speed ~ relative weight (all points)",
    x     = "relative weight [%]",
    y     = "Speed [m/s]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ bat, nrow = 2)+
  scale_x_continuous(limits = c(1, 15)) +
  scale_y_continuous(limits = c(0, 7))

print(p_relativ)


# all points total

inerval_slopes_total <- inerval %>%
  group_by(bat) %>%
  summarise({
    m  <- lm(speed ~ total_weight_num)
    cs <- coef(summary(m))
    tibble(
      slope     = cs[2, "Estimate"],
      se_slope  = cs[2, "Std. Error"],
      t_value   = cs[2, "t value"],
      p_value   = cs[2, "Pr(>|t|)"],
      intercept = cs[1, "Estimate"],
      r2        = summary(m)$r.squared
    )
  }, .groups = "drop")

print(inerval_slopes_total)

fit_tot_points <- lmer(speed ~ total_weight_num + (1 | bat), data = inerval)
summary(fit_tot_points)
plot(fit_tot_points)
hist(inerval$speed)


ranef(fit_tot_points)
coef(fit_tot_points)$bat


p_total <- ggplot(inerval, aes(x = total_weight_num, y = speed)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    title = "Speed ~ total weight (all points)",
    x     = "total weight [g]",
    y     = "Speed [m/s]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ bat, nrow = 2)

print(p_total)



















