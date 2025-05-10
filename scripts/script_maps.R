

# ------------------------------------------------------------------------------------------------------------------------------------------------
# script to make maps ----------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(readr)
library(lubridate)  # pour la manipulation de dates
library(scales)     # pour les formats en pourcentage
library(viridis)    # pour les heatmaps
library(dplyr)
library(ggplot2)
library(sp)
library(sf)
library(viridis) 
library(here)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(maps)
theme_set(theme_bw())

# PELGAS -----------------------------------------------------------------------------------------------------------------------------------------

# Stations d’échantillonnage pour tailles
# Données de tailles et poids individuels
pelgas_sizes <- read_delim("data/pelgas/biomres-size_PELGAS.csv",
                           delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE) %>%
  filter(sp %in% c("TRAC-MED", "TRAC-TRU"), year >= 2007)

# example one year -------------------------------------------------------------------------------------------------------------------------------
ex1 <- pelgas_sizes %>% filter(year=="2018", sp=="TRAC-MED", N>0)

ex_1temp <- ex1 %>% group_by(esdu.id, year) %>% mutate(Ntot=sum(N))

ex1_1 <- ex_1temp %>% group_by(esdu.id) %>% mutate(L_weighted = L*N/Ntot)
p1 <- ggplot(ex1_1) + geom_point(aes(x=LONG, y=LAT, size=log(L_weighted), color=log(L_weighted)))
p1



# package poour ajouter le continent -------------------------------------------------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
xlims <- c(-6,0)#range(pretty(Data_Geostat$Lon))
ylims <- c(43,48.5)#range(pretty(Data_Geostat$Lat))


p2 <- ggplot() + geom_point(ex1_1, map=aes(x=LONG, y=LAT, color=(L_weighted)))+
  geom_sf(data=world, col=NA, fill="black")+
  coord_sf(xlim = xlims, ylim = ylims)+
  scale_color_continuous(type = "viridis", option = "B", direction = -1,na.value="lightgrey")
p2


# example all years -------------------------------------------------------------------------------------------------------------------------------
ex1 <- pelgas_sizes %>% filter(sp=="TRAC-MED", N>0)

ex_1temp <- ex1 %>% group_by(esdu.id, year) %>% mutate(Ntot=sum(N))

ex1_1 <- ex_1temp %>% group_by(esdu.id, year) %>% mutate(L_weighted = L*N/Ntot)




# package poour ajouter le continent -------------------------------------------------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
xlims <- c(-6,0)#range(pretty(Data_Geostat$Lon))
ylims <- c(43,48.5)#range(pretty(Data_Geostat$Lat))


p2 <- ggplot() + geom_point(ex1_1, map=aes(x=LONG, y=LAT, color=(L_weighted)))+
  geom_sf(data=world, col=NA, fill="black")+
  coord_sf(xlim = xlims, ylim = ylims)+
  scale_color_continuous(type = "viridis", option = "B", direction = -1,na.value="lightgrey") + facet_wrap(~year)
p2
