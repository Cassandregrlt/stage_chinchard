# ============================== #
#      Analyse des données       #
#           PELGAS               #
# ============================== #

# ---- Packages nécessaires ----
library(tidyverse)
library(readr)
library(lubridate)  # pour la manipulation de dates
library(scales)     # pour les formats en pourcentage
library(viridis)    # pour les heatmaps

# ---- Import des bases de données ----

# Résumé biomasse & abondance (TRAC-MED + TRAC-TRU, déjà filtré)
pelgas_summary <- read_delim("data/pelgas/biomAbunCVallSpPELGASseriesLong.csv",
                             delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE) %>%
  filter(year >= 2007)

# Stations d’échantillonnage
pelgas_stations <- read_delim("data/pelgas/biomres2.csv",
                              delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE) %>%
  filter(GENR_ESP %in% c("TRAC-MED", "TRAC-TRU")) %>%
  mutate(dateend = ymd_hms(dateend),
         year = year(dateend)) %>%
  filter(year >= 2007)

# Données de tailles et poids individuels
pelgas_sizes <- read_delim("data/pelgas/biomres-size_PELGAS.csv",
                           delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE) %>%
  filter(sp %in% c("TRAC-MED", "TRAC-TRU"), year >= 2007)

# ---- Histogrammes empilés ----

## Proportions par espèce
pelgas_biomass_prop <- pelgas_summary %>%
  group_by(year, sp) %>%
  summarise(biomass = sum(wbiom, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(prop = biomass / sum(biomass))

# Graphe : proportions
ggplot(pelgas_biomass_prop, aes(x = year, y = prop, fill = sp)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Proportions annuelles des biomasses (PELGAS)",
       x = "Année", y = "Proportion", fill = "Espèce") +
  theme_minimal()

# Graphe : biomasses absolues
ggplot(pelgas_biomass_prop, aes(x = year, y = biomass, fill = sp)) +
  geom_bar(stat = "identity") +
  labs(title = "Évolution des biomasses totales (PELGAS)",
       x = "Année", y = "Biomasse totale (kg)", fill = "Espèce") +
  theme_minimal()

# ---- Structure en tailles TRAC-MED ----

pelgas_size_freq <- pelgas_sizes %>%
  filter(sp == "TRAC-MED") %>%
  group_by(year, L) %>%
  summarise(N = sum(N, na.rm = TRUE), .groups = "drop")

ggplot(pelgas_size_freq, aes(x = L, y = N)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~year, scales = "free_y") +
  labs(title = "Structure en tailles de T. mediterraneus (PELGAS)",
       x = "Longueur (cm)", y = "Nombre d'individus") +
  theme_minimal()

# ---- Longueurs moyennes par an ----

pelgas_length_means <- pelgas_size_freq %>%
  group_by(year) %>%
  summarise(mean_length = sum(L * N) / sum(N), .groups = "drop") %>%
  mutate(campagne = "PELGAS")

# ---- Modèle allométrique (global) ----

pelgas_allometry_data <- pelgas_sizes %>%
  filter(sp == "TRAC-MED", !is.na(L), !is.na(W), !is.na(N), L > 0, W > 0, N > 0) %>%
  mutate(
    poids_moyen = (W * 1000) / N,  # poids moyen en grammes
    logL = log(L),
    logW = log(poids_moyen),
    campagne = "PELGAS"
  )

# Régression log-log
pelgas_allometry_model <- lm(logW ~ logL, data = pelgas_allometry_data)
a <- exp(coef(pelgas_allometry_model)[1])
b <- coef(pelgas_allometry_model)[2]
r2 <- summary(pelgas_allometry_model)$r.squared

cat("Modèle allométrique PELGAS : a =", round(a, 5),
    ", b =", round(b, 3),
    ", R² =", round(r2, 3), "\n")

# Graphe global
ggplot(pelgas_allometry_data, aes(x = logL, y = logW)) +
  geom_point(alpha = 0.2, size = 0.8, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "#0072B2") +
  labs(title = "Relation allométrique log-log – PELGAS",
       x = "log(Longueur) (cm)", y = "log(Poids) (g)") +
  theme_minimal()

# ---- Allométrie par année ----

pelgas_allometry_annual <- pelgas_allometry_data %>%
  mutate(year = as.factor(year))

ggplot(pelgas_allometry_annual, aes(x = logL, y = logW)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#0072B2") +
  facet_wrap(~year, scales = "free_y") +
  labs(title = "Relation Taille-Poids par année – PELGAS",
       x = "log(Longueur) (cm)", y = "log(Poids) (g)") +
  theme_minimal()

