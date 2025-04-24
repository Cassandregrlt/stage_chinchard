# ============================== #
#      Analyse des données       #
#            EVHOE               #
# ============================== #

# ---- Packages nécessaires ----
library(tidyverse)
library(readr)
library(lubridate)
library(scales)
library(viridis)

# ---- Import des bases de données ----

## Opérations de pêche (EVHOE)
evhoe_operations <- read_delim("data/evhoe/EVHOE_1987_2024_ELFIC.V1.4_operation_2025-03-28.csv",
                               delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE) %>%
  filter(year >= 2007)

## Captures (espèces cibles)
evhoe_catch <- read_delim("data/evhoe/EVHOE_1987_2024_ELFIC.V1.4_catch_2025-03-28.csv",
                          delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE) %>%
  filter(scientificName %in% c("Trachurus mediterraneus", "Trachurus trachurus"),
         year >= 2007)

## Paramètres biologiques (longueurs et poids)
evhoe_bioparams <- read_delim("data/evhoe/EVHOE_1987_2024_ELFIC.V1.4_biologicalParameters_2025-03-28.csv",
                              delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE) %>%
  filter(scientificName %in% c("Trachurus mediterraneus", "Trachurus trachurus"),
         year >= 2007) %>%
  rename(
    TL = `Longueur totale (LT) - individu - totale - Mesure au cm par un observateur`,
    W = `Poids - produit/lot - totale - Mesure par un observateur`
  )

## Autres paramètres
evhoe_other <- read_delim("data/evhoe/EVHOE_1987_2024_ELFIC.V1.4_otherParameters_2025-03-28.csv",
                          delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE) %>%
  filter(year >= 2007)

# ---- Histogrammes empilés ----

## Proportions par espèce
evhoe_biomass_prop <- evhoe_catch %>%
  group_by(year, scientificName) %>%
  summarise(biomass = sum(totalWeightKg, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(prop = biomass / sum(biomass))

ggplot(evhoe_biomass_prop, aes(x = year, y = prop, fill = scientificName)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Proportions annuelles des biomasses (EVHOE)",
       x = "Année", y = "Proportion", fill = "Espèce") +
  theme_minimal()

## Valeurs absolues
ggplot(evhoe_biomass_prop, aes(x = year, y = biomass, fill = scientificName)) +
  geom_bar(stat = "identity") +
  labs(title = "Évolution des biomasses totales (EVHOE)",
       x = "Année", y = "Biomasse totale (kg)", fill = "Espèce") +
  theme_minimal()

# ---- Structure en tailles TRAC-MED ----

evhoe_size_freq <- evhoe_bioparams %>%
  filter(scientificName == "Trachurus mediterraneus") %>%
  group_by(year, TL) %>%
  summarise(N = sum(number, na.rm = TRUE), .groups = "drop")

ggplot(evhoe_size_freq, aes(x = TL, y = N)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~year, scales = "free_y") +
  labs(title = "Structure en tailles de T. mediterraneus (EVHOE)",
       x = "Longueur (cm)", y = "Nombre d'individus") +
  theme_minimal()

# ---- Longueurs moyennes ----

evhoe_length_means <- evhoe_size_freq %>%
  group_by(year) %>%
  summarise(mean_length = sum(TL * N) / sum(N), .groups = "drop") %>%
  mutate(campagne = "EVHOE")

# ---- Modèle allométrique (global) ----

evhoe_allometry_data <- evhoe_bioparams %>%
  filter(scientificName == "Trachurus mediterraneus",
         !is.na(TL), !is.na(W), !is.na(number),
         TL > 0, W > 0, number > 0) %>%
  mutate(
    poids_indiv = (W / number) * 1000,  # en grammes
    logL = log(TL),
    logW = log(poids_indiv),
    campagne = "EVHOE"
  )

# Régression log-log
evhoe_model <- lm(logW ~ logL, data = evhoe_allometry_data)

a_evhoe <- exp(coef(evhoe_model)[1])
b_evhoe <- coef(evhoe_model)[2]
r2_evhoe <- summary(evhoe_model)$r.squared

cat("Modèle allométrique EVHOE : a =", round(a_evhoe, 5),
    ", b =", round(b_evhoe, 3),
    ", R² =", round(r2_evhoe, 3), "\n")

ggplot(evhoe_allometry_data, aes(x = logL, y = logW)) +
  geom_point(alpha = 0.1, size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "#F8766D") +
  labs(title = "Relation allométrique log-log – EVHOE",
       x = "log(Longueur) (cm)", y = "log(Poids) (g)") +
  theme_minimal()

# ---- Allométrie par année ----

evhoe_allometry_annual <- evhoe_allometry_data %>%
  mutate(year = as.factor(year))

ggplot(evhoe_allometry_annual, aes(x = logL, y = logW)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#F8766D") +
  facet_wrap(~ year, scales = "free_y") +
  labs(title = "Relation Taille-Poids par année – EVHOE",
       x = "log(Longueur) (cm)", y = "log(Poids) (g)") +
  theme_minimal()
