# Reset session
rm(list = ls())


# ---- Length-based indicators ----

# Adapted from T. Miethe and C. Silva, WKLIFE-V, Oct2015
# Modif L. Pawlowski, adapté pour ton cas 2024


# Install Packages #

library(reshape2)
library(lattice)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
source("scripts/lbi_table.R")



## ---- Input parameters ----

S <- c("N")  # Unsexed only
stock <- "MED"  # Nom du stock
setwd(getwd())  # Reste dans ton dossier actuel

# Paramètres biologiques
Linf <- c(190, 190, 190)
Lmat <- c(106, 106, 106)

# Années
startyear <- 2007
endyear <- 2024


## ---- Load data: Numbers-at-length ----


# Effectifs par longueur par année
IC2 <- pelgas_sizes %>%
  filter(sp == "TRAC-MED") %>%
  group_by(L, year) %>%
  summarise(N = sum(N, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = N, values_fill = 0) %>%
  arrange(L) %>%
  rename(MeanLength = L)

# Poids moyen par longueur par année
IC1 <- pelgas_sizes %>%
  filter(sp == "TRAC-MED") %>%
  mutate(W_mean = ifelse(N > 0, W / N, NA)) %>%
  group_by(L, year) %>%
  summarise(W_mean = mean(W_mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = W_mean, values_fill = 0) %>%
  arrange(L) %>%
  rename(MeanLength = L)

# Définir les jeux de données pour LBI
m <- NULL
f <- NULL
ns <- IC2
mw <- NULL
fw <- NULL
nsw <- IC1


length_data <- ns
MeanLengthLB <- FALSE  # Ici MeanLength est au milieu de la classe
sex <- "N"

if (MeanLengthLB) {
  length_data$MeanLength <- length_data$MeanLength + 5
}


## ---- Step 1 : Vérification des distributions de longueur ---- 

length_plot<- function(length_data, ClassInt = 10, filename = "length_distribution.png", save_plot = FALSE) {
  df0 <- length_data
  
  df0.long <- melt(df0, id.vars = 'MeanLength')
  df0.long$variable <- gsub("X", "", as.character(df0.long$variable))
  
  minCL <- floor((min(df0$MeanLength) - .5) / ClassInt) * ClassInt
  maxCL <- ceiling((max(df0$MeanLength) + .5) / ClassInt) * ClassInt
  
  df0$LC <- cut(df0$MeanLength, breaks = seq(minCL, maxCL, ClassInt), include.lowest = TRUE)
  
  df0.gr <- aggregate(df0[, 2:ncol(df0)-1], by = list(df0$LC), sum)
  names(df0.gr)[1] <- 'lclass'
  lmidp <- sapply(as.character(final.gr$lclass), function(cl) {
    bounds <- unlist(regmatches(cl, gregexpr("[0-9]+", cl)))
    if (length(bounds) == 2) {
      mean(as.numeric(bounds))
    } else {
      NA
    }
  })
  df0.gr <- cbind(lclass = df0.gr$lclass, lmidp = lmidp, df0.gr[, 2:ncol(df0.gr)])
  
  df0.gr.long <- melt(df0.gr[, -1], id.vars = 'lmidp')
  names(df0.gr.long)[2:3] <- c('year', 'Number')
  df0.gr.long$year <- as.numeric(as.character(df0.gr.long$year))
  
  length_bars <- barchart(Number ~ lmidp | as.factor(year),
                          data = df0.gr.long,
                          horizontal = FALSE,
                          as.table = TRUE,
                          ylim = c(0, NA),
                          xlab = 'Length (mm)',
                          ylab = 'Number (x1000)',
                          scales = list(x = list(relation = "free")),
                          main = paste0("Length distribution, ClassInt = ", ClassInt, " mm"))
  
  if (save_plot) {
    png(filename, width = 35, height = 18, units = "cm", res = 600)
    print(length_bars)
    dev.off()
  } else {
    print(length_bars)
  }
}

# Afficher si besoin
length_plot(length_data, ClassInt = 10, save_plot = FALSE)

ClassInt <- 10  


## ---- Step 2 : Calcul des indicateurs par sexe ----


Year <- startyear:endyear

for (s in 1:length(S)) {
  sex <- S[s]
  
  if (sex == "M") { final <- m; weight <- mw }
  if (sex == "F") { final <- f; weight <- fw }
  if (sex == "N") { final <- ns; weight <- nsw }
  
  Ind <- data.frame(matrix(ncol = 24, nrow = length(Year)))
  names(Ind) <- c('Year', 'L75', 'L25', 'Lmed', 'L90', 'L95', 'Lmean', 'Lc', 'LFeM', 'Lmaxy', 'Lmat', 'Lopt', 'Linf', 'Lmax5', 'Lmean_LFeM', 'Lc_Lmat', 'L25_Lmat', 'Lmean_Lmat', 'Lmean_Lopt', 'L95_Linf', 'Lmaxy_Lopt', 'Lmax5_Linf', 'Pmega', 'Pmegaref')
  Ind$Year <- Year
  
  minCL <- floor((min(final$MeanLength) - .5) / ClassInt) * ClassInt
  maxCL <- ceiling((max(final$MeanLength) + .5) / ClassInt) * ClassInt
  final$LC <- cut(final$MeanLength, breaks = seq(minCL, maxCL, ClassInt), include.lowest = TRUE)
  
  final.gr <- aggregate(final[, 2:ncol(final)-1], by = list(final$LC), sum)
  names(final.gr)[1] <- 'lclass'
  lmidp <- sapply(as.character(final.gr$lclass), function(cl) {
    bounds <- unlist(regmatches(cl, gregexpr("[0-9]+", cl)))
    if (length(bounds) == 2) {
      mean(as.numeric(bounds))
    } else {
      NA
    }
  })
  final.gr <- cbind(lclass = final.gr$lclass, lmidp = lmidp, final.gr[, 2:ncol(final.gr)])
  
  for (j in 1:length(Year)) {
    year_label <- as.character(Year[j])
    
    if (!(year_label %in% colnames(final.gr))) next
    
    df <- final.gr[, c("lmidp", year_label)]
    names(df) <- c("lngth", "number")
    df <- df[df$number > 0, ]
    
    if (nrow(df) == 0) next
    
    df <- df[order(df$lngth), ]
    df$cumsum <- cumsum(df$number)
    df$cumsum_perc <- df$cumsum / sum(df$number)
    
    Ind$L25[j] <- min(df$lngth[df$cumsum_perc >= 0.25])
    Ind$L75[j] <- min(df$lngth[df$cumsum_perc >= 0.75])
    Ind$Lmed[j] <- min(df$lngth[df$cumsum_perc >= 0.5])
    Ind$L90[j] <- min(df$lngth[df$cumsum_perc >= 0.90])
    Ind$L95[j] <- min(df$lngth[df$cumsum_perc >= 0.95])
    
    Lopt <- 2/3 * Linf[s]
    Lmat_val <- Lmat[s]
    Linf_val <- Linf[s]
    
    Ind$Lopt[j] <- Lopt
    Ind$Lmat[j] <- Lmat_val
    Ind$Linf[j] <- Linf_val
    
    df_above_Lc <- df[df$lngth >= Ind$L25[j], ]
    if (nrow(df_above_Lc) > 0) {
      Ind$Lmean[j] <- sum(df_above_Lc$lngth * df_above_Lc$number) / sum(df_above_Lc$number)
    }
    
    numb <- df[rev(order(df$lngth)), ]
    numb$cum <- cumsum(numb$number)
    numb$cumperc <- numb$cum / sum(numb$number)
    numb$num5 <- ifelse(numb$cumperc <= 0.05, numb$number, 0)
    numb$num5[max(which(numb$cumperc <= 0.05)) + 1] <- (0.05 - numb$cumperc[max(which(numb$cumperc <= 0.05))]) * sum(df$number)
    Ind$Lmax5[j] <- sum(numb$num5 * numb$lngth) / sum(numb$num5)
    
    Ind$Pmega[j] <- sum(df[df$lngth >= (Lopt + 0.1 * Lopt), "number"]) / sum(df$number)
    Ind$Pmegaref[j] <- 0.3
    Ind$Lc[j] <- Ind$L25[j]
    Ind$LFeM[j] <- 0.75 * Ind$Lc[j] + 0.25 * Linf[s]
  }
  
  # Ratios
  Ind$Lmean_LFeM <- Ind$Lmean / Ind$LFeM
  Ind$Lc_Lmat <- Ind$Lc / Ind$Lmat
  Ind$L25_Lmat <- Ind$L25 / Ind$Lmat
  Ind$Lmean_Lmat <- Ind$Lmean / Ind$Lmat
  Ind$Lmean_Lopt <- Ind$Lmean / Ind$Lopt
  Ind$L95_Linf <- Ind$L95 / Ind$Linf
  Ind$Lmax5_Linf <- Ind$Lmax5 / Ind$Linf
  
  if (sex == "M") Males <- Ind
  if (sex == "F") Females <- Ind
  if (sex == "N") Unsexed <- Ind
}

lbi_table(Ind)


## ---- step 3 plot indicator time series per sex ----


file_path <- ""  # adapte si tu veux un dossier de sortie

for (s in 1:length(S)) {
  sex <- S[s]
  if (sex == "M") Ind <- Males
  if (sex == "F") Ind <- Females
  if (sex == "N") Ind <- Unsexed
  
  # Filtrage des années valides (au moins une colonne non NA)
  Ind_valid <- Ind %>% filter(!is.na(Lmean_LFeM))
  
  ## ---- FIGURE 1 : longitudes brutes ----
  png(paste0(file_path, stock, "_", sex, "_timeseries.png"),
      bg = "white", pointsize = 5, units = "cm", width = 10, height = 18, res = 600)
  
  par(mar = c(5, 4, 3, 4), mfrow = c(3, 1), family = "serif", cex = 1.5)
  
  # (a) Conservation
  plot(Linf ~ Year, data = Ind_valid, ylab = "Length", col = "transparent",
       main = "(a) Conservation", xlab = "Year",
       xlim = c(min(Ind_valid$Year), max(Ind_valid$Year) + 1),
       ylim = c(min(Ind_valid$Lc, na.rm = TRUE) * 0.9, max(Ind_valid$Linf, na.rm = TRUE) * 1.1),
       bty = "l")
  axis(1, at = Ind_valid$Year, labels = FALSE, tick = TRUE, cex.axis = 0.1)
  lines(L95 ~ Year, data = Ind_valid, lwd = 2, col = "purple")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$L95, 1), expression(L["95%"]), col = "purple", cex = 1.1)
  lines(Lmax5 ~ Year, data = Ind_valid, lwd = 2, col = "black")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$Lmax5, 1), expression(L["max5%"]), col = "black", cex = 0.9)
  lines(Lmat ~ Year, data = Ind_valid, lwd = 1, col = "black", lty = "dashed")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$Lmat, 1), expression(L["mat"]), col = "black", cex = 1.1)
  lines(Lc ~ Year, data = Ind_valid, lwd = 2, col = "blue")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$Lc, 1), expression(L["c"]), col = "blue", cex = 1.1)
  lines(Linf ~ Year, data = Ind_valid, lwd = 1, col = "black", lty = "dashed")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$Linf, 1), expression(L["inf"]), col = "black", cex = 1.1)
  lines(L25 ~ Year, data = Ind_valid, lwd = 1, col = "red")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$L25, 1), expression(L["25%"]), col = "red", cex = 1.1)
  
  # (b) Optimal Yield
  plot(Linf ~ Year, data = Ind_valid, ylab = "Length", main = "(b) Optimal Yield", col = "transparent",
       xlab = "Year", xlim = c(min(Ind_valid$Year), max(Ind_valid$Year) + 1),
       ylim = c(min(Ind_valid$Lc, na.rm = TRUE) * 0.9, max(Ind_valid$Linf, na.rm = TRUE) * 1.1),
       bty = "l")
  axis(1, at = Ind_valid$Year, labels = FALSE, tick = TRUE, cex.axis = 0.1)
  lines(L75 ~ Year, data = Ind_valid, lwd = 1, col = "red")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$L75, 1), expression(L["75%"]), col = "red", cex = 1.1)
  lines(Lmean ~ Year, data = Ind_valid, lwd = 2, col = "darkred")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$Lmean, 1), expression(L["mean"]), col = "darkred", cex = 1.1)
  lines(Lopt ~ Year, data = Ind_valid, lwd = 1, col = "black", lty = "dashed")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$Lopt, 1), expression(L["opt"]), col = "black", cex = 1.2)
  lines(Lmaxy ~ Year, data = Ind_valid, lwd = 2, col = "green")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$Lmaxy, 1), expression(L["maxy"]), col = "green", cex = 1.2)
  
  # (c) Maximum Sustainable Yield
  plot(Lmat ~ Year, data = Ind_valid, type = "l", ylab = "Length",
       main = "(c) Maximum Sustainable Yield", col = "black", lty = "dashed",
       xlab = "Year", xlim = c(min(Ind_valid$Year), max(Ind_valid$Year) + 1),
       ylim = c(min(Ind_valid$Lc, na.rm = TRUE) * 0.9, max(Ind_valid$Linf, na.rm = TRUE) * 1.1),
       bty = "l")
  axis(1, at = Ind_valid$Year, labels = FALSE, tick = TRUE, cex.axis = 0.1)
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$Lmat, 1), expression(L["mat"]), col = "black", cex = 1.2)
  lines(Lmean ~ Year, data = Ind_valid, lwd = 2, col = "darkred")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$Lmean, 1), expression(L["mean"]), col = "darkred", cex = 1.1)
  lines(LFeM ~ Year, data = Ind_valid, lwd = 2, col = "blue", lty = "dashed")
  text(max(Ind_valid$Year) + 1, tail(Ind_valid$LFeM, 1), expression(L["F=M"]), col = "blue", cex = 1.2)
  
  dev.off()
  
  ## ---- FIGURE 2 : Ratios ----
  png(paste0(file_path, stock, "_", sex, "_timeseries_ratios.png"),
      bg = "white", pointsize = 5, units = "cm", width = 10, height = 18, res = 600)
  
  par(mar = c(5, 4, 3, 4), mfrow = c(3, 1), family = "serif", cex = 1.5)
  
  # (a) Conservation
  plot(c(min(Ind_valid$Year), max(Ind_valid$Year) + 3), c(0, 1.5),
       ylab = "Indicator Ratio", col = "transparent", main = "(a) Conservation",
       xlab = "Year", bty = "l")
  axis(1, at = Ind_valid$Year, labels = FALSE, tick = TRUE, cex.axis = 0.1)
  lines(Lmax5_Linf ~ Year, data = Ind_valid, lwd = 2, col = "black")
  text(max(Ind_valid$Year) + 2, tail(Ind_valid$Lmax5_Linf, 1), expression(L["max5%"]/L["inf"]), col = "black", cex = 1.0)
  lines(L95_Linf ~ Year, data = Ind_valid, lwd = 2, col = "purple")
  text(max(Ind_valid$Year) + 2, tail(Ind_valid$L95_Linf, 1), expression(L["95%"]/L["inf"]), col = "purple", cex = 1.1)
  lines(Pmega ~ Year, data = Ind_valid, lwd = 2, col = "blue")
  text(max(Ind_valid$Year) + 2, tail(Ind_valid$Pmega, 1), expression(P["mega"]), col = "blue", cex = 1.2)
  lines(Pmegaref ~ Year, data = Ind_valid, lwd = 1, col = "black", lty = "dashed")
  text(max(Ind_valid$Year) + 2, tail(Ind_valid$Pmegaref, 1), "30%", col = "black", cex = 1.0)
  lines(Lc_Lmat ~ Year, data = Ind_valid, lwd = 2, col = "red")
  text(max(Ind_valid$Year) + 2, tail(Ind_valid$Lc_Lmat, 1), expression(L["c"]/L["mat"]), col = "red", cex = 1.1)
  lines(L25_Lmat ~ Year, data = Ind_valid, lwd = 2, col = "darkred")
  text(max(Ind_valid$Year) + 2, tail(Ind_valid$L25_Lmat, 1), expression(L["25"]/L["mat"]), col = "darkred", cex = 1.1)
  
  # (b) Optimal yield
  plot(c(min(Ind_valid$Year), max(Ind_valid$Year) + 3), c(0, 1.6),
       ylab = "Indicator Ratio", col = "transparent", main = "(b) Optimal yield",
       xlab = "Year", bty = "l")
  axis(1, at = Ind_valid$Year, labels = FALSE, tick = TRUE, cex.axis = 0.1)
  lines(Lmean_Lopt ~ Year, data = Ind_valid, lwd = 2, col = "darkred")
  text(max(Ind_valid$Year) + 2, tail(Ind_valid$Lmean_Lopt, 1), expression(L["mean"]/L["opt"]), col = "darkred", cex = 1.1)
  lines(Lmaxy_Lopt ~ Year, data = Ind_valid, lwd = 2, col = "green")
  text(max(Ind_valid$Year) + 2, tail(Ind_valid$Lmaxy_Lopt, 1), expression(L["maxy"]/L["opt"]), col = "green", cex = 1.1)
  
  # (c) Maximum sustainable yield
  plot(c(min(Ind_valid$Year), max(Ind_valid$Year) + 3), c(0, 1.6),
       ylab = "Indicator Ratio", col = "transparent", main = "(c) Maximum sustainable yield",
       xlab = "Year", bty = "l")
  axis(1, at = Ind_valid$Year, labels = FALSE, tick = TRUE, cex.axis = 0.1)
  lines(Lmean_LFeM ~ Year, data = Ind_valid, lwd = 2, col = "blue")
  text(max(Ind_valid$Year) + 2, tail(Ind_valid$Lmean_LFeM, 1), expression(L["mean"]/L["F=M"]), col = "blue", cex = 1.1)
  
  dev.off()
}





## ---- Graphiques pour les combinaisons Linf et Lmat de la littérature ----


# Paramètres biologiques par zone

params <- data.frame(
  zone = c("Malaga", "Espagne_Med", "Turquie", "Mer_Noire"),
  Linf = c(382, 445, 280, 190),
  Lmat = c(164, 160, 192, 106)
)


### ---- Graphique (c) : Ratio Lmean / LFeM ----


# Initialisation
results_Lmean_LFeM <- list()

for (i in 1:nrow(params)) {
  Linf_val <- params$Linf[i]
  Lmat_val <- params$Lmat[i]
  
  Ind_tmp <- Unsexed
  Ind_tmp$Linf <- Linf_val
  Ind_tmp$Lmat <- Lmat_val
  Ind_tmp$LFeM <- 0.75 * Ind_tmp$Lc + 0.25 * Linf_val
  Ind_tmp$Lmean_LFeM <- Ind_tmp$Lmean / Ind_tmp$LFeM
  Ind_tmp$source <- params$zone[i]
  
  results_Lmean_LFeM[[i]] <- Ind_tmp %>%
    select(Year, Lmean_LFeM, source)
}

df_Lmean_LFeM <- bind_rows(results_Lmean_LFeM) %>%
  filter(!is.na(Lmean_LFeM))

# Tracé
ggplot(df_Lmean_LFeM, aes(x = Year, y = Lmean_LFeM, color = source)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "(c) Rendement durable maximal",
    subtitle = "Comparaison du ratio Lmoy / LFeM selon les paramètres biologiques",
    y = expression(L["moy"] / L["F=M"]),
    x = "Année",
    color = "Zone"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
  )


### ---- Graphique (a) : Ratio Lc / Lmat ----

# Initialisation
results_Lc_Lmat <- list()

for (i in 1:nrow(params)) {
  Lmat_val <- params$Lmat[i]
  
  Ind_tmp <- Unsexed
  Ind_tmp$Lmat <- Lmat_val
  Ind_tmp$Lc_Lmat <- Ind_tmp$Lc / Ind_tmp$Lmat
  Ind_tmp$source <- params$zone[i]
  
  results_Lc_Lmat[[i]] <- Ind_tmp %>%
    select(Year, Lc_Lmat, source)
}

df_Lc_Lmat <- bind_rows(results_Lc_Lmat) %>%
  filter(!is.na(Lc_Lmat))

# Tracé
ggplot(df_Lc_Lmat, aes(x = Year, y = Lc_Lmat, color = source)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "(a) Conservation",
    subtitle = "Comparaison du ratio Lc / Lmat selon les paramètres biologiques",
    y = expression(L["c"] / L["mat"]),
    x = "Année",
    color = "Zone"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
  )
