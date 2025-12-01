## ---- Pakete ----
# Installieren (nur einmal nötig):
# install.packages(c("data.table","dplyr","ggplot2","stringr","lme4","lmerTest","performance","broom.mixed","DHARMa","emmeans","ggeffects"))
install.packages(c(
  "lme4",          # Mixed Models
  "lmerTest",      # p-Werte für Mixed Models
  "performance",   # R², Modellgüte
  "broom.mixed",   # tabellarische Ausgabe
  "DHARMa",        # Residualdiagnostik
  "emmeans",       # Post-hoc Vergleiche
  "ggeffects",     # Effektplots
  "ggplot2",       # Visualisierung
  "dplyr", "data.table", "stringr"  # Grund-Tools
))
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(lme4)
library(lmerTest)     # p-Werte für Fixed Effects
library(performance)  # R2, Checks
library(broom.mixed)  # Aufbereitung für Tabellen
library(DHARMa)       # Modell-Diagnostik (Residuals)
library(emmeans)      # Marginal means
library(ggeffects)    # Effektplots

## ---- Daten laden ----
# Pfad anpassen falls nötig:
file <- "D:/Flanders25/Videos/Data_leis/CSV_leis/dt_0.35_weight.csv"
df   <- fread(file)

# Schneller Überblick
glimpse(df)
summary(df)

## ---- Spaltennamen robust erkennen ----
# Versuche Standardnamen herzuleiten (falls die Spalten anders heißen)
# Erwarte: Geschwindigkeit, Gewicht, Individuum, Trial und (optional) Flug-ID
name_like <- function(pattern, choices) {
  ix <- grep(pattern, names(choices), ignore.case = TRUE, value = TRUE)
  if (length(ix) == 0) return(NA_character_) else return(ix[1])
}
names(df)

col_speed  <- "speed"
col_weight <- "total_weight"    # vorläufig, falls Gewicht in VideoID steckt
col_bat    <- "bat"
col_trial  <- "VideoID"
col_flight <- "FrameID"
if (any(is.na(c(col_speed, col_weight, col_bat, col_trial)))) {
  stop("Konnte zentrale Spalten nicht sicher erkennen. Bitte benenne sie manuell in col_* um.")
}

## ---- Aufräumen / Typen setzen ----
df <- df %>%
  rename(
    speed        = all_of(col_speed),
    total_weight = all_of(col_weight),
    bat          = all_of(col_bat),
    trial        = all_of(col_trial)
  ) %>%
  mutate(
    bat          = factor(bat),
    trial        = factor(trial),
    total_weight = suppressWarnings(as.numeric(total_weight)),
    speed        = suppressWarnings(as.numeric(speed))
  )

# Optional: Flug-ID ableiten/benennen
if (!is.na(col_flight)) {
  df <- df %>% rename(flight_id = all_of(col_flight)) %>%
    mutate(flight_id = factor(flight_id))
} else {
  # Falls nicht vorhanden, innerhalb bat:trial eine Laufnummer als "Flug"
  df <- df %>%
    group_by(bat, trial) %>%
    mutate(flight_id = factor(cur_group_id()*1e6 + data.table::rleid(round(speed, 3)))) %>%
    ungroup()
}

## ---- Grundchecks ----
df <- df %>%
  filter(is.finite(speed), is.finite(total_weight)) %>%
  filter(speed >= 0) # negative Geschwindigkeiten entfernen

# Optional: sehr unrealistische Ausreißer kappen (z.B. > 30 m/s)
df <- df %>% filter(speed <= 30)

# Deskriptives
df %>% count(bat, trial) %>% arrange(bat, trial) %>% print(n = 99)
summary(df$speed); summary(df$total_weight)

## ---- Visualisierung: Rohdaten ----
ggplot(df, aes(total_weight, speed, color = bat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ bat, scales = "free_y") +
  labs(x = "Gesamtgewicht", y = "Geschwindigkeit [m/s]",
       title = "Geschwindigkeit vs. Gewicht je Individuum") +
  theme_minimal()

## ---- Modell 1: Random Intercept je Fledermaus ----
# Idee: Viele kurze Flüge (messwiederholt) → Mixed Model mit zufälligem Intercept für bat.
m1 <- lmer(speed ~ total_weight + (1 | bat), data = df, REML = TRUE)
summary(m1)

## ---- Modell 2: Trial als weiterer zufälliger Effekt (in bat verschachtelt) ----
# Wenn pro Individuum mehrere Trials (mit unterschiedlichen Gewichten) vorliegen,
# ist (1 | bat:trial) sinnvoll, weil Messungen innerhalb eines Trials abhängiger sind.
m2 <- lmer(speed ~ total_weight + (1 | bat) + (1 | bat:trial), data = df, REML = TRUE)
summary(m2)

## ---- Modell 3: Zufällige Steigung des Gewichtseffekts pro Individuum ----
# Erlaubt, dass der Gewichtseffekt je bat unterschiedlich ist.
m3 <- lmer(speed ~ total_weight + (1 + total_weight | bat), data = df, REML = TRUE)
summary(m3)

## ---- Modellvergleich ----
# REML für unterschiedliche Random-Strukturen: vergleiche mit ML (REML=FALSE)
m1_ml <- update(m1, REML = FALSE)
m2_ml <- update(m2, REML = FALSE)
m3_ml <- update(m3, REML = FALSE)

anova(m1_ml, m2_ml, m3_ml)    # Likelihood-Ratio-Test
AIC(m1_ml, m2_ml, m3_ml)      # AIC-Vergleich

# Gütemaße (R2 marginal/konditional)
rbind(
  m1 = performance::r2_nakagawa(m1) %>% as.data.frame(),
  m2 = performance::r2_nakagawa(m2) %>% as.data.frame(),
  m3 = performance::r2_nakagawa(m3) %>% as.data.frame()
)

## ---- Diagnostik (wichtig!) ----
par(mfrow = c(1, 1))
simulateResiduals(m2, plot = TRUE)  # ggf. bestes Modell einsetzen
check_model(m2)                      # weitere Checks

## ---- Effektstärke & p-Werte ----
# lmerTest liefert t/p in summary(); hier noch sauber als Tabelle:
tidy(m2, effects = "fixed") %>% print()

## ---- Effektplot (Predicted) ----
# Vorhersage über Gewichtsbereich, Mittel über Random Effects
pdat <- ggeffect(m2, terms = "total_weight [all]")  # oder z.B. "[minmax]"
plot(pdat) + labs(x = "Gesamtgewicht", y = "Vorhergesagte Geschwindigkeit [m/s]",
                  title = "Marginaler Effekt des Gewichts (Mixed Model)")

## ---- Post-hoc / Emmeans (optional) ----
# Falls Gewicht nur wenige diskrete Stufen hat (z.B. 4 Lasten), kann man Gruppen vergleichen:
if (n_distinct(df$total_weight) <= 8) {
  df$weight_fac <- factor(df$total_weight)
  m2_fac <- lmer(speed ~ weight_fac + (1 | bat) + (1 | bat:trial), data = df)
  emmeans(m2_fac, ~ weight_fac) %>% pairs(adjust = "tukey") %>% print()
}

## ---- Alternative: Aggregation auf Flug- oder Trial-Ebene ----
# Um Pseudoreplikation zu reduzieren, kann man je (bat, trial, flight) mitteln:
df_flight <- df %>%
  group_by(bat, trial, flight_id) %>%
  summarise(mean_speed = mean(speed, na.rm = TRUE),
            total_weight = dplyr::first(total_weight),
            .groups = "drop")

# Modell auf Flugmitteln:
mF <- lmer(mean_speed ~ total_weight + (1 | bat) + (1 | bat:trial), data = df_flight)
summary(mF)
performance::r2_nakagawa(mF)

# Oder noch gröber: je Trial mitteln:
df_trial <- df %>%
  group_by(bat, trial) %>%
  summarise(mean_speed = mean(speed, na.rm = TRUE),
            total_weight = dplyr::first(total_weight),
            .groups = "drop")
mT <- lmer(mean_speed ~ total_weight + (1 | bat), data = df_trial)
summary(mT)

## ---- Ergebnis-Report (kompakt) ----
best <- m2  # hier das aus dem Vergleich gewählte Modell einsetzen
cat("\n=== FIXED EFFECTS (bestes Modell) ===\n")
print(tidy(best, effects = "fixed", conf.int = TRUE, conf.level = 0.95))

cat("\n=== VARIANCE (Random Effects) ===\n")
print(VarCorr(best), comp = "Variance")

cat("\n=== R2 (Nakagawa) ===\n")
print(performance::r2_nakagawa(best))

