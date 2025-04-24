# ============================== #
#     Mise en page des figures   #
#     PELGAS / EVHOE / SACROIS   #
# ============================== #

# ---- Packages nécessaires ----
library(tidyverse)
library(scales)
library(patchwork)

# ---- Couleurs standardisées ----
couleurs_especes <- c("T. mediterraneus" = "#00BFC4",
                      "T. trachurus" = "#F8766D",
                      "JAX" = "#7CAE00")

couleurs_campagnes <- c("PELGAS" = "#0072B2",
                        "EVHOE" = "#D55E00",
                        "SACROIS" = "#009E73")

# ========================== #
#  BIOMASSES RELATIVES ----
# ========================== #

# Recodage des espèces
pelgas_biomass_prop$espece <- recode(pelgas_biomass_prop$sp,
                                     "TRAC-MED" = "T. mediterraneus",
                                     "TRAC-TRU" = "T. trachurus")

sacrois_biomass_prop$espece <- recode(sacrois_biomass_prop$ESP_COD_FAO,
                                      "HMM" = "T. mediterraneus",
                                      "HOM" = "T. trachurus",
                                      "JAX" = "JAX")

evhoe_biomass_prop$espece <- recode(evhoe_biomass_prop$scientificName,
                                    "Trachurus mediterraneus" = "T. mediterraneus",
                                    "Trachurus trachurus" = "T. trachurus")

# Graphiques
gp <- ggplot(pelgas_biomass_prop, aes(x = year, y = prop, fill = espece)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = couleurs_especes) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "PELGAS (indice de biomasse standardisé)",
    x = "Année", y = "Proportion relative", fill = "Espèce"
  ) +
  theme_minimal()

gs <- ggplot(sacrois_biomass_prop, aes(x = ANNEE, y = prop, fill = espece)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = couleurs_especes) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "SACROIS (poids débarqué réel)",
    x = "Année", y = "Proportion relative", fill = "Espèce"
  ) +
  theme_minimal()

ge <- ggplot(evhoe_biomass_prop, aes(x = year, y = prop, fill = espece)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = couleurs_especes) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "EVHOE (biomasse extrapolée par trait)",
    x = "Année", y = "Proportion relative", fill = "Espèce"
  ) +
  theme_minimal()

(gp + gs + ge) + plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

# ========================== #
#   BIOMASSES ABSOLUES ----
# ========================== #

pelgas_biomass_total <- pelgas_biomass_prop %>% select(year, sp, biomass)
pelgas_biomass_total$espece <- recode(pelgas_biomass_total$sp,
                                      "TRAC-MED" = "T. mediterraneus",
                                      "TRAC-TRU" = "T. trachurus")

sacrois_biomass_total$espece <- recode(sacrois_biomass_total$ESP_COD_FAO,
                                       "HMM" = "T. mediterraneus",
                                       "HOM" = "T. trachurus",
                                       "JAX" = "JAX")

evhoe_biomass_total <- evhoe_biomass_prop %>% select(year, scientificName, biomass)
evhoe_biomass_total$espece <- recode(evhoe_biomass_total$scientificName,
                                     "Trachurus mediterraneus" = "T. mediterraneus",
                                     "Trachurus trachurus" = "T. trachurus")

gp2 <- ggplot(pelgas_biomass_total, aes(x = year, y = biomass, fill = espece)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = couleurs_especes) +
  labs(
    title = "PELGAS – Indice de biomasse standardisé",
    x = "Année", y = "Indice (unité relative)", fill = "Espèce"
  ) +
  theme_minimal()

gs2 <- ggplot(sacrois_biomass_total, aes(x = ANNEE, y = biomass, fill = espece)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = couleurs_especes) +
  labs(
    title = "SACROIS – Poids débarqué réel (kg)",
    x = "Année", y = "Biomasse réelle (kg)", fill = "Espèce"
  ) +
  theme_minimal()

ge2 <- ggplot(evhoe_biomass_total, aes(x = year, y = biomass, fill = espece)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = couleurs_especes) +
  labs(
    title = "EVHOE – Biomasse extrapolée (kg)",
    x = "Année", y = "Biomasse estimée (kg)", fill = "Espèce"
  ) +
  theme_minimal()

(gp2 + gs2 + ge2) + plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")





library(tidyverse)

# === Proportions relatives ===
## PELGAS
pelgas_prop_values <- pelgas_biomass_prop %>%
  mutate(
    espece = recode(sp,
                    "TRAC-MED" = "T. mediterraneus",
                    "TRAC-TRU" = "T. trachurus"),
    campagne = "PELGAS"
  ) %>%
  select(campagne, year, espece, prop)

## EVHOE
evhoe_prop_values <- evhoe_biomass_prop %>%
  mutate(
    espece = recode(scientificName,
                    "Trachurus mediterraneus" = "T. mediterraneus",
                    "Trachurus trachurus" = "T. trachurus"),
    campagne = "EVHOE"
  ) %>%
  select(campagne, year, espece, prop)

## SACROIS
sacrois_prop_values <- sacrois_biomass_prop %>%
  mutate(
    espece = recode(ESP_COD_FAO,
                    "HMM" = "T. mediterraneus",
                    "HOM" = "T. trachurus"),
    campagne = "SACROIS",
    year = ANNEE
  ) %>%
  select(campagne, year, espece, prop)

# Fusion des 3
prop_all <- bind_rows(pelgas_prop_values, evhoe_prop_values, sacrois_prop_values) %>%
  arrange(campagne, year, espece)

# === Biomasses absolues ===
## PELGAS
pelgas_total_values <- pelgas_biomass_total %>%
  mutate(
    espece = recode(sp,
                    "TRAC-MED" = "T. mediterraneus",
                    "TRAC-TRU" = "T. trachurus"),
    campagne = "PELGAS"
  ) %>%
  select(campagne, year, espece, biomass)

## EVHOE
evhoe_total_values <- evhoe_biomass_total %>%
  mutate(
    espece = recode(scientificName,
                    "Trachurus mediterraneus" = "T. mediterraneus",
                    "Trachurus trachurus" = "T. trachurus"),
    campagne = "EVHOE"
  ) %>%
  select(campagne, year, espece, biomass)

## SACROIS
sacrois_total_values <- sacrois_biomass_total %>%
  mutate(
    espece = recode(ESP_COD_FAO,
                    "HMM" = "T. mediterraneus",
                    "HOM" = "T. trachurus"),
    campagne = "SACROIS",
    year = ANNEE
  ) %>%
  select(campagne, year, espece, biomass)

# Fusion des 3
biomass_all <- bind_rows(pelgas_total_values, evhoe_total_values, sacrois_total_values) %>%
  arrange(campagne, year, espece)

# Aperçu
View(prop_all)
View(biomass_all)


# Exemple : exporter les proportions
write.csv(prop_all, "prop_all.csv", row.names = FALSE)

# Idem pour les biomasses
write.csv(biomass_all, "biomass_all.csv", row.names = FALSE)



# ========================== #
#   LONGUEURS MOYENNES ----
# ========================== #

length_means_all <- bind_rows(pelgas_length_means, evhoe_length_means)

ggplot(length_means_all, aes(x = year, y = mean_length, color = campagne)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = couleurs_campagnes) +
  labs(title = "Évolution de la longueur moyenne de T. mediterraneus",
       x = "Année", y = "Longueur moyenne (cm)", color = "Campagne") +
  theme_minimal()


# ========================== #
# STRUCTURE TAILLES COMBI ----
# ========================== #

pelgas_size_freq <- pelgas_size_freq %>%
  rename(TL = L) %>% 
  mutate(campagne = "PELGAS")

evhoe_size_freq <- evhoe_size_freq %>%
  mutate(campagne = "EVHOE")

combined_size_freq <- bind_rows(pelgas_size_freq, evhoe_size_freq) %>%
  group_by(year, TL, campagne) %>%
  summarise(N = sum(N, na.rm = TRUE), .groups = "drop")


ggplot(combined_size_freq, aes(x = TL, y = N)) +
  geom_col(fill = "#00BFC4") +
  facet_wrap(~ year, scales = "free_y") +
  labs(title = "Structure en tailles de T. mediterraneus (PELGAS + EVHOE)",
       x = "Longueur totale (cm)", y = "Nombre d'individus") +
  theme_minimal()

# en proportions relatives

combined_size_freq_norm <- combined_size_freq %>%
  group_by(year, campagne) %>%
  mutate(freq_rel = N / max(N, na.rm = TRUE)) %>%
  ungroup()


ggplot(combined_size_freq_norm, aes(x = TL, y = freq_rel, color = campagne)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ year, scales = "free_y") +
  scale_color_manual(values = couleurs_campagnes) +
  labs(title = "Structures en tailles de T. mediterraneus (proportion max par campagne)",
       x = "Longueur totale (cm)", y = "Proportion relative", color = "Campagne") +
  theme_minimal()


ggplot(combined_size_freq_norm, aes(x = TL, y = freq_rel, fill = campagne)) +
  geom_area(alpha = 0.5, position = "identity") +
  facet_wrap(~ year, scales = "free_y") +
  scale_fill_manual(values = couleurs_campagnes) +
  labs(
    title = "Structures en tailles normalisées de T. mediterraneus",
    x = "Longueur totale (cm)", y = "Proportion relative", fill = "Campagne"
  ) +
  theme_minimal()

## OU

ggplot(combined_size_freq_norm, aes(x = TL, y = freq_rel, fill = campagne)) +
  geom_col(position = "identity", alpha = 0.5) +
  facet_wrap(~ year, scales = "free_y") +
  scale_fill_manual(values = couleurs_campagnes) +
  labs(
    title = "Structures en tailles normalisées de T. mediterraneus",
    x = "Longueur totale (cm)", y = "Proportion relative", fill = "Campagne"
  ) +
  theme_minimal()


