# ============================== #
#      Analyse des données       #
#           OBSMER               #
# ============================== #

# ---- Packages nécessaires ----
library(dplyr)
library(ggplot2)

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


# ---- Elévation ----

# Filtrer et préparer les données
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

# ÉTAPE 1 : élévation à l'échelle de l'opération

distri_op <- longueurs_tmed %>%
  left_join(poids_ech_tmed, by = c("TRIP_NUMBER", "STATION_NUMBER")) %>%
  filter(!is.na(SUBSAMPLE_WEIGHT), !is.na(WEIGHT), !is.na(LENGTH_CLASS)) %>%
  mutate(
    elevation_factor = WEIGHT / SUBSAMPLE_WEIGHT,
    dist_op = NUMBER_AT_LENGTH * elevation_factor
  )

# ÉTAPE 2 : élévation à l'échelle de la marée
# on ajoute l'année et le navire
marees_reduit <- marees %>% select(TRIP_NUMBER, VESSEL_IDENTIFIER)

# somme des distributions par marée
distri_maree <- distri_op %>%
  left_join(marees_reduit, by = "TRIP_NUMBER") %>%
  group_by(YEAR, TRIP_NUMBER, VESSEL_IDENTIFIER, LENGTH_CLASS) %>%
  summarise(dist_maree = sum(dist_op, na.rm = TRUE), .groups = "drop")


# ÉTAPE 3 : élévation à l'échelle du navire (pondération)
# poids total échantillonné par navire
poids_par_navire <- distri_maree %>%
  group_by(VESSEL_IDENTIFIER) %>%
  summarise(poids_marees_navire = sum(dist_maree, na.rm = TRUE), .groups = "drop")

# total des poids échantillonnés (tous navires)
poids_total_echant <- sum(poids_par_navire$poids_marees_navire, na.rm = TRUE)

# somme par navire et classe de taille
distri_navire <- distri_maree %>%
  group_by(VESSEL_IDENTIFIER, LENGTH_CLASS, YEAR) %>%
  summarise(dist_navire = sum(dist_maree, na.rm = TRUE), .groups = "drop") %>%
  left_join(poids_par_navire, by = "VESSEL_IDENTIFIER") %>%
  mutate(dist_navire_corr = dist_navire * (poids_marees_navire / poids_total_echant))


# ÉTAPE 4 : élévation à l'échelle du métier
# récupérer le métier de chaque navire
navire_metier <- operations %>%
  select(TRIP_NUMBER, FISHING_ACT_CAT_EU_LVL_5) %>%
  left_join(marees %>% select(TRIP_NUMBER, VESSEL_IDENTIFIER), by = "TRIP_NUMBER") %>%
  distinct(VESSEL_IDENTIFIER, FISHING_ACT_CAT_EU_LVL_5)

# joindre les métiers aux distributions corrigées
metier_join <- distri_navire %>%
  left_join(navire_metier, by = "VESSEL_IDENTIFIER")

# poids total par métier
poids_par_metier <- metier_join %>%
  group_by(FISHING_ACT_CAT_EU_LVL_5) %>%
  summarise(poids_metier = sum(dist_navire_corr, na.rm = TRUE), .groups = "drop")

# application de la pondération finale
distri_totale <- metier_join %>%
  left_join(poids_par_metier, by = "FISHING_ACT_CAT_EU_LVL_5") %>%
  mutate(dist_tot_metier = dist_navire_corr * (poids_metier / sum(poids_metier, na.rm = TRUE)))

# résultat final : distribution totale estimée par année, classe de taille et métier
distribution_totale <- distri_totale %>%
  group_by(YEAR, FISHING_ACT_CAT_EU_LVL_5, LENGTH_CLASS) %>%
  summarise(total_estimated_count = sum(dist_tot_metier, na.rm = TRUE), .groups = "drop")

