library(tidyverse)
library(scales)

actes_chir_bariatriques <- c(
  "HFMC007", "HFMA009", "HFKC001", "HFKA002", "HFMC008", "HFMA011", "HFCC003",
  "HFCA001", "HFFC018", "HFFA011", "HFMC006", "HFMA010", "HGCC027", "HGCA009",
  "HFFC004", "HFFA001")

nb_chir_bariatriques_par_codegeo <- function(année) {

  ct(MCO, ACTE, année)
  ct(MCO, FIXE, année)

  ((
    ACTE
    %>% filter(str_sub(acte, end =7L) %in% actes_chir_bariatriques)
    %>% distinct(ident)
  ) -> IDENT_CHIR_BARIATRIQUES)


  ((
    FIXE
    %>% inner_join(IDENT_CHIR_BARIATRIQUES)
    %>% select(codegeo, sexe, age)
    %>% transforme_age_en_classe()
    %>% group_by(codegeo, sexe, cl_age)
    %>% summarise(recours_chir_bariatrique = n())
    %>% ungroup()
  ) -> NB_CHIR_BARIATRIQUES_PAR_CODEGEO)
}


taux_recours_bariatrique <- function(année) {

  ((
    population_toutes_zones(année)
    %>% left_join(nb_chir_bariatriques_par_codegeo(année))
    %>% standardisation_focus_HDF()
    %>% collect()
    %>% arrange(zone_var, zone)
    %>% mutate(année = {{année}})
  ) -> x) %>% pinf()
}

((
  2016:2025
  %>% map(taux_recours_bariatrique)
  %>% bind_rows()
) -> taux_recours_bariatrique_2016_2025)


# Palettes
couleur_2016 <- "#1f78b4"
couleur_2025 <- "#e31a1c"

# 1) Niveaux de facteur selon le Taux 2025 (ordre décroissant)
niv_2025 <- taux_recours_bariatrique_2016_2025 %>%
  filter(zone_var %in% c("Région", "Héxgone"), année %in% c(2016, 2025)) %>%
  mutate(zone = if_else(zone == "HEXAGONE", "FRANCE ENTIÈRE", zone)) %>%
  filter(année == 2025) %>%
  arrange(desc(`Taux_std_pour_1e+05`)) %>%
  pull(zone) %>%
  unique()

# 2) Graphique
(
  taux_recours_bariatrique_2016_2025
  %>% filter(zone_var %in% c("Région", "Héxgone"), année %in% c(2016, 2025))
  %>% mutate(
    zone  = if_else(zone == "HEXAGONE", "FRANCE ENTIÈRE", zone),
    annee = factor(année),
    # 👉 impose l'ordre décroissant d'après 2025
    zone  = factor(zone, levels = rev(niv_2025))
  )
  %>% ggplot(aes(x = `Taux_std_pour_1e+05`, y = zone))

  # Lignes pointillées
  + geom_line(aes(group = zone),
              linetype = "dotted",
              color = "grey40",
              size = 0.8)

  # Points 2016 (bleu) & 2025 (rouge)
  + geom_point(data = ~ filter(.x, annee == "2016"),
               aes(color = annee),
               size = 3)
  + geom_point(data = ~ filter(.x, annee == "2025"),
               aes(color = annee),
               size = 3)

  # 1re échelle de couleurs : par année (points)
  + scale_color_manual(values = c("2016" = couleur_2016,
                                  "2025" = couleur_2025),
                       name = "Année")

  # Nouvelle échelle indépendante pour les labels
  + ggnewscale::new_scale_color()

  # Labels des régions (HDF en vert, FRANCE ENTIÈRE en bleu, autres en noir)
  + geom_text(
    data = ~ filter(.x, annee == "2025"),
    aes(
      label = zone,
      x = `Taux_std_pour_1e+05`,
      color = case_when(
        zone == "HAUTS-DE-FRANCE" ~ "HDF",
        zone == "FRANCE ENTIÈRE"  ~ "FR",
        TRUE                      ~ "AUTRES"
      )
    ),
    hjust = 0.03,  # léger décalage à droite du point
    vjust = -0.70,
    size  = 3.5
  )

  # 2e échelle de couleurs : pour les labels de régions
  + scale_color_manual(
    values = c(
      "HDF"    = "forestgreen",
      "FR"     = "dodgerblue3",
      "AUTRES" = "black"
    ),
    guide = "none"
  )

  # Valeurs des taux 2016 (texte bleu, décalé à droite)
  + geom_text(
    data = ~ filter(.x, annee == "2016"),
    aes(label = scales::number_format(accuracy = 0.1)(`Taux_std_pour_1e+05`),
        x = `Taux_std_pour_1e+05` + 3.5),
    hjust = 1, vjust = 0.5, size = 3, color = couleur_2016
  )

  # Valeurs des taux 2025 (texte rouge, décalé à gauche)
  + geom_text(
    data = ~ filter(.x, annee == "2025"),
    aes(label = scales::number_format(accuracy = 0.1)(`Taux_std_pour_1e+05`),
        x = `Taux_std_pour_1e+05` - 3),
    hjust = 0, vjust = 0.5, size = 3, color = couleur_2025
  )

  + labs(
    title = "Baisse du taux de recours de chirurgie bariatrique sur 10 ans : 2016 à 2025",
    x = "Taux de recours standardisé de chirurgie bariatrique pour 100 000 habitants",
    y = NULL
  )
  + theme_minimal(base_size = 12)
  + theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "top"
  )
  + coord_cartesian(clip = "off")
)



taux_recours_bariatrique_2016_2025 %>%
  # Astuce : corrigez si nécessaire l'orthographe de la zone
  # filter(zone_var %in% c("Hexagone"))
  filter(zone_var %in% c("Héxgone")) %>%  # <- laissez la valeur qui existe réellement dans vos données
  # Optionnel si 'année' n'est pas numérique :
  # mutate(année = as.integer(année)) %>%
  ggplot(aes(x = `année`, y = `Taux_std_pour_1e+05`)) +
  # Tracé
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_point(color = "#2C7FB8", size = 2.6) +
  # Axe X : toutes les années affichées (entiers)
  scale_x_continuous(
    breaks = 2016:2025,
    labels = 2016:2025,
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  # Axe Y : libellé et formatage en français
  scale_y_continuous(
    name = "Taux de recours (pour 100 000 habitants)",
    labels = label_number(accuracy = 0.1, decimal.mark = ",", big.mark = " ")
  ) +
  # Titres
  labs(
    title = "Taux de recours chirurgie bariatrique en France entre 2016 et 2025",
    x = "Année",
    y = "Taux de recours (pour 100 000 habitants)"
    # , caption = "Source : à compléter"
  ) +
  # Esthétique
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(vjust = 0.5),
    plot.margin = margin(10, 15, 10, 10)
  )




((
  nb_chir_bariatriques_par_codegeo(2025)
  %>% dplyr::rename(observation_chir_bariatrique = 4)
  %>% mutate(code_département = substr(codegeo, 1, 2))
  %>% inner_join(départements_hdf, copy = T)
  %>% select(-codegeo, -code_département)
  %>% group_by(lib_département, sexe, cl_age)
  %>% summarise(across(observation_chir_bariatrique, sum))
) -> recours_bariatrique)

projections_insee(recours_bariatrique, 2025, 2030)
