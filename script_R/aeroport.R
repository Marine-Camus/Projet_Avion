# jonction des jeux de données 
library(data.table)

chemin <- "/Users/audreylapie/Desktop/M2_agronome/Projet avion /asp-apt-1990-2024"
files <- list.files(chemin, pattern = "^ASP_APT_\\d{4}\\.csv$", full.names = TRUE)

all_apt <- rbindlist(lapply(files, fread))

all_apt$ANNEE <- substr(all_apt$ANMOIS, 1, 4)     
all_apt$MOIS  <- substr(all_apt$ANMOIS, 5, 6)

all_apt$APT <- as.factor(all_apt$APT)
all_apt$APT_ZON <- as.factor(all_apt$APT_ZON)

View(all_apt)

aero_tourisme <- c("LFMN", "LFKJ", "LFKB")   # Nice, Ajaccio, Bastia
aero_business <- c("LFPG", "LFPO")          # Charles-de-Gaulle, Orly

aero_selection <- c(aero_tourisme, aero_business)

trafic_heatmap <- all_apt %>%
  filter(APT %in% aero_selection) %>%
  group_by(ANNEE, MOIS, APT) %>%
  summarise(PAX_TOTAL = sum(APT_PAX_dep, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    ANNEE = as.integer(ANNEE),
    MOIS  = as.integer(MOIS)
  )

ggplot(trafic_heatmap, aes(x = MOIS, y = ANNEE, fill = PAX_TOTAL)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", trans = "log10") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  facet_wrap(~APT, ncol = 2) +
  labs(title = "Heatmap du trafic passagers (mois × années)",
       subtitle = "Tourisme : Nice, Ajaccio, Bastia — Business : CDG, Orly",
       x = "Mois", y = "Année", fill = "Passagers") +
  theme_minimal()
