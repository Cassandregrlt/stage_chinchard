# ============================== #
#      Analyse des données       #
#            SACROIS             #
# ============================== #

# ---- Packages nécessaires ----
library(tidyverse)
library(readr)
library(lubridate)
library(scales)
library(mapplots)
library(vmstools)

# ---- Import des données SACROIS ----
# Chargement du fichier .RData déjà filtré pour TRAC-MED et TRAC-TRU
load("data/sacrois/compile_sacrois.rdata")  # <- charge l'objet compile_SAC
sacrois_data <- compile_SAC

sacrois_data <- sacrois_data %>%
  mutate(ANNEE = as.numeric(ANNEE)) %>%
  filter(ANNEE >= 2007)

# ---- Histogrammes empilés ----

## Proportions HMM, HOM, JAX
sacrois_biomass_prop <- sacrois_data %>%
  filter(ESP_COD_FAO %in% c("HMM", "HOM", "JAX")) %>%
  group_by(ANNEE, ESP_COD_FAO) %>%
  summarise(biomass = sum(QUANT_POIDS_VIF_SACROIS, na.rm = TRUE), .groups = "drop") %>%
  group_by(ANNEE) %>%
  mutate(prop = biomass / sum(biomass))

ggplot(sacrois_biomass_prop, aes(x = ANNEE, y = prop, fill = ESP_COD_FAO)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Évolution des proportions HMM / HOM / JAX (SACROIS)",
       x = "Année", y = "Proportion", fill = "Espèce") +
  theme_minimal()

## Valeurs absolues
sacrois_biomass_total <- sacrois_data %>%
  group_by(ANNEE, ESP_COD_FAO) %>%
  summarise(biomass = sum(QUANT_POIDS_VIF_SACROIS, na.rm = TRUE), .groups = "drop")

ggplot(sacrois_biomass_total, aes(x = ANNEE, y = biomass, fill = ESP_COD_FAO)) +
  geom_bar(stat = "identity") +
  labs(title = "Évolution des biomasses totales (SACROIS)",
       x = "Année", y = "Biomasse (kg)", fill = "Espèce") +
  theme_minimal()

# ---- Cartographie spatio-temporelle ----

## Paramètres carto
annee_carto <- 2020

## Extraction T. mediterraneus (HMM) pour une année donnée
sacrois_map_data <- sacrois_data %>%
  filter(ANNEE == annee_carto, ESP_COD_FAO == "HMM")

# Agrégation par rectangle ICES
sacrois_map_agg <- aggregate(
  list(QUANTITE = sacrois_map_data$QUANT_POIDS_VIF_SACROIS),
  by = list(RECT = sacrois_map_data$SECT_COD_SACROIS_NIV5),
  FUN = sum, na.rm = TRUE
)

# Conversion rectangles vers coordonnées géographiques
sacrois_map_agg$Lat <- 0
sacrois_map_agg$Lon <- 0
for (i in seq_len(nrow(sacrois_map_agg))) {
  coord <- ices.rect(sacrois_map_agg$RECT[i])
  sacrois_map_agg$Lat[i] <- coord$lat
  sacrois_map_agg$Lon[i] <- coord$lon
}

# Création de la grille
byx <- 1
byy <- 0.5
xli <- c(-15.5, 0)
yli <- c(43, 56)

grd <- make.grid(sacrois_map_agg$Lon, sacrois_map_agg$Lat, sacrois_map_agg$QUANTITE, byx, byy, xli, yli)
colnames(grd) <- as.numeric(colnames(grd)) - 0.25
breaks <- breaks.grid(grd, zero = FALSE)

# Tracé de la carte
data(europa)
basemap(xli, yli, main = paste("Débarquements de T. mediterraneus (SACROIS –", annee_carto, ")"))
draw.grid(grd, breaks)
plot(europa, xlim = xli, ylim = yli, add = TRUE, border = "black", col = "grey")
draw.rect()
legend.grid("topleft", breaks = breaks / 1000, type = 2, inset = 0.02, title = "tonnes", cex = 0.6)
