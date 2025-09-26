# jonction des jeux de données 
library(data.table)

chemin <- "/Users/audreylapie/Desktop/M2_agronome/Projet avion /asp-cie-2010-2024"
files <- list.files(chemin, pattern = "^ASP_CIE_\\d{4}\\.csv$", full.names = TRUE)

all <- rbindlist(lapply(files, fread))
all$ANNEE <- substr(all$ANMOIS, 1, 4)     
all$MOIS  <- substr(all$ANMOIS, 5, 6)

all$CIE <- as.factor(all$CIE)
all$CIE_NOM <- as.factor(all$CIE_NOM)
all$CIE_NAT <- as.factor(all$CIE_NAT)
all$CIE_PAYS <- as.factor(all$CIE_PAYS)
View(all)


library(dplyr)
library(ggplot2)

# agrégation par année : somme des passagers
trafic_annuel <- all %>%
  group_by(ANNEE) %>%
  summarise(PAX_TOTAL = sum(CIE_PAX, na.rm = TRUE), .groups = "drop")


# graphique
ggplot(trafic_annuel, aes(x = as.integer(ANNEE), y = PAX_TOTAL)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Évolution du trafic aérien entre 2010 et 2024",
       subtitle = "Nombre total de passagers transportés par année",
       x = "Année", y = "Passagers (total)") +
  theme_minimal() 

trafic_airfrance <- all %>%
  filter(CIE_NOM == "AIR FRANCE") %>%
  group_by(ANNEE) %>%
  summarise(PAX_TOTAL = sum(CIE_PAX, na.rm = TRUE), .groups = "drop")

ggplot(trafic_airfrance, aes(x = as.integer(ANNEE), y = PAX_TOTAL)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "Évolution du trafic - Air France",
       x = "Année", y = "Passagers transportés") +
  theme_minimal()

trafic_par_pays <- all %>%
  group_by(ANNEE, CIE_PAYS) %>%
  summarise(PAX_TOTAL = sum(CIE_PAX, na.rm = TRUE), .groups = "drop")

ggplot(trafic_par_pays, aes(x = as.integer(ANNEE), y = PAX_TOTAL, color = CIE_PAYS)) +
  geom_line(linewidth = 1) +
  labs(title = "Évolution du trafic aérien par compagnie",
       x = "Année", y = "Passagers transportés",
       color = "Compagnie") +
  theme_minimal()

trafic_2018 <- all %>%
  filter(ANNEE == "2018") %>%
  group_by(MOIS) %>%
  summarise(PAX_TOTAL = sum(CIE_PAX, na.rm = TRUE), .groups = "drop")

ggplot(trafic_2018, aes(x = as.integer(MOIS), y = PAX_TOTAL)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Évolution mensuelle du trafic aérien en 2018",
       x = "Mois", y = "Passagers transportés (total)") +
  theme_minimal()

# essayer de voir la crise des gilets jaunes novembre 2023 
trafic_2023_France <- all %>%
  filter(ANNEE == "2023" & CIE_PAYS == "FRANCE") %>%
  group_by(MOIS) %>%
  summarise(PAX_TOTAL = sum(CIE_PAX, na.rm = TRUE), .groups = "drop")

ggplot(trafic_2023_France, aes(x = as.integer(MOIS), y = PAX_TOTAL)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Évolution mensuelle du trafic aérien en 2023 en France",
       x = "Mois", y = "Passagers transportés (total)") +
  theme_minimal()
