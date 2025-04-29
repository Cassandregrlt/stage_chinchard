# ============================== #
#      Analyse des données       #
#           OBSMER               #
# ============================== #

# ---- Packages nécessaires ----
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)


# ---- Import de la base de donnée ----

load("data/ObsMer/obsmer_chinchard.rdata")

# Filtrage à partir de 2007
marees <- marees %>% filter(YEAR >= 2007)
operations <- operations %>% filter(TRIP_NUMBER %in% marees$TRIP_NUMBER)
poids_ech <- poids_ech %>% filter(TRIP_NUMBER %in% marees$TRIP_NUMBER)
longueurs <- longueurs %>% filter(TRIP_NUMBER %in% marees$TRIP_NUMBER)

head(marees)
head(operations)
head(poids_ech)
head(longueurs)


# Élévation halieutique OBSMER ----

## 1. Filtrage et préparation des données ----

poids_ech_tmed <- poids_ech %>%
  filter(SPECIES == "Trachurus mediterraneus") %>%
  group_by(TRIP_NUMBER, STATION_NUMBER) %>%
  summarise(
    WEIGHT = sum(WEIGHT, na.rm = TRUE),
    SUBSAMPLE_WEIGHT = sum(SUBSAMPLE_WEIGHT, na.rm = TRUE),
    .groups = "drop"
  )

longueurs_tmed <- longueurs %>%
  filter(SPECIES == "Trachurus mediterraneus")

## 2. Élévation à l'échelle de l'opération ----

distri_op <- longueurs_tmed %>%
  left_join(poids_ech_tmed, by = c("TRIP_NUMBER", "STATION_NUMBER")) %>%
  filter(!is.na(SUBSAMPLE_WEIGHT), !is.na(WEIGHT), !is.na(LENGTH_CLASS)) %>%
  mutate(
    elevation_factor = WEIGHT / SUBSAMPLE_WEIGHT,
    dist_op = NUMBER_AT_LENGTH * elevation_factor
  )

## 3. Élévation à l'échelle de la marée ----

marees_reduit <- marees %>%
  select(TRIP_NUMBER, VESSEL_IDENTIFIER)

distri_maree <- distri_op %>%
  left_join(marees_reduit, by = "TRIP_NUMBER") %>%
  group_by(YEAR, TRIP_NUMBER, VESSEL_IDENTIFIER, LENGTH_CLASS) %>%
  summarise(dist_maree = sum(dist_op, na.rm = TRUE), .groups = "drop")

## 4. Élévation à l'échelle du navire ----

poids_par_navire <- distri_maree %>%
  group_by(VESSEL_IDENTIFIER) %>%
  summarise(poids_marees_navire = sum(dist_maree, na.rm = TRUE), .groups = "drop")

poids_total_flotte <- sum(poids_par_navire$poids_marees_navire, na.rm = TRUE)

distri_navire <- distri_maree %>%
  group_by(VESSEL_IDENTIFIER, LENGTH_CLASS, YEAR) %>%
  summarise(dist_navire = sum(dist_maree, na.rm = TRUE), .groups = "drop") %>%
  left_join(poids_par_navire, by = "VESSEL_IDENTIFIER") %>%
  mutate(dist_navire_corr = dist_navire * (poids_marees_navire / poids_total_flotte))

## 5. Élévation à l'échelle du métier ----

navire_metier <- operations %>%
  select(TRIP_NUMBER, FISHING_ACT_CAT_EU_LVL_5) %>%
  left_join(marees_reduit, by = "TRIP_NUMBER") %>%
  distinct(VESSEL_IDENTIFIER, FISHING_ACT_CAT_EU_LVL_5)

metier_join <- distri_navire %>%
  left_join(navire_metier, by = "VESSEL_IDENTIFIER")

poids_par_metier <- metier_join %>%
  group_by(FISHING_ACT_CAT_EU_LVL_5) %>%
  summarise(poids_metier = sum(dist_navire_corr, na.rm = TRUE), .groups = "drop")

distri_totale <- metier_join %>%
  left_join(poids_par_metier, by = "FISHING_ACT_CAT_EU_LVL_5") %>%
  mutate(dist_tot_metier = dist_navire_corr * (poids_metier / sum(poids_metier, na.rm = TRUE)))

## 6. Agrégation finale ----

distribution_totale <- distri_totale %>%
  group_by(YEAR, FISHING_ACT_CAT_EU_LVL_5, LENGTH_CLASS) %>%
  summarise(total_estimated_count = sum(dist_tot_metier, na.rm = TRUE), .groups = "drop")


# ---- Histogrammes de distribution en classes de tailles ----

ggplot(distribution_totale, aes(x = LENGTH_CLASS, y = total_estimated_count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~YEAR, scales = "free_y") +
  labs(title = "Distribution en classes de tailles par année (OBSMER)",
       x = "Classe de longueur (cm)",
       y = "Nombre total estimé d'individus") +
  theme_minimal()

# ---- Histogramme empilé ----

distribution_totale_global <- distribution_totale %>%
  group_by(LENGTH_CLASS) %>%
  summarise(total_count = sum(total_estimated_count, na.rm = TRUE), .groups = "drop")

ggplot(distribution_totale_global, aes(x = LENGTH_CLASS, y = total_count)) +
  geom_col(fill = "#2C3E50") +
  labs(
    title = "Distribution globale des tailles – OBSMER (toutes années et métiers)",
    x = "Classe de longueur (cm)",
    y = "Nombre total estimé d'individus"
  ) +
  theme_minimal()

# ---- Longueur moyenne par années ----

longueur_moyenne_obsmer <- distribution_totale %>%
  group_by(YEAR) %>%
  summarise(
    mean_length = sum(LENGTH_CLASS * total_estimated_count) / sum(total_estimated_count),
    .groups = "drop"
  )

ggplot(longueur_moyenne_obsmer, aes(x = YEAR, y = mean_length)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Longueur moyenne annuelle (OBSMER)",
       x = "Année",
       y = "Longueur moyenne (cm)") +
  theme_minimal()

# ---- Top métiers ----
top_metiers <- distribution_totale %>%
  group_by(FISHING_ACT_CAT_EU_LVL_5) %>%
  summarise(total_count = sum(total_estimated_count, na.rm = TRUE), .groups = "drop")

ggplot(top_metiers, aes(x = FISHING_ACT_CAT_EU_LVL_5, y = total_count)) +
  geom_col(fill = "#1f77b4") +
  coord_flip() +
  labs(
    title = "Nombre total d'individus estimés par métier (OBSMER)",
    x = "Métier (code DCF)", y = "Nombre total estimé d'individus"
  ) +
  theme_minimal()

# ---- Histogrammes de distribution par métier ----
metier_exemple <- "OTB_DEF"

ggplot(distribution_totale %>% filter(FISHING_ACT_CAT_EU_LVL_5 == metier_exemple),
       aes(x = LENGTH_CLASS, y = total_estimated_count)) +
  geom_col(fill = "darkorange") +
  facet_wrap(~YEAR, scales = "free_y") +
  labs(title = paste("Distribution en tailles par année pour le métier", metier_exemple),
       x = "Classe de longueur (cm)",
       y = "Nombre total estimé d'individus") +
  theme_minimal()


