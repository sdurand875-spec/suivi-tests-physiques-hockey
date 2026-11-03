# Étape 1 — les librairies :
  
  library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)

# Étape 2 — la fonction de catégorie FFHG
  
  categorie_ffhg <- function(date_naissance, date_reference = Sys.Date()) {
    # La saison commence en septembre : de janvier à août, on prend l'année d'avant
    annee_saison <- if_else(month(date_reference) >= 9,
                            year(date_reference),
                            year(date_reference) - 1)
    age_reference <- annee_saison - year(date_naissance)

    case_when(
      age_reference <= 6  ~ "U7",
      age_reference <= 8  ~ "U9",
      age_reference <= 10 ~ "U11",
      age_reference <= 12 ~ "U13",
      age_reference <= 14 ~ "U15",
      age_reference <= 17 ~ "U18",
      age_reference <= 19 ~ "U20",
      TRUE ~ "Senior"
    )
  }
  
  # Étape 3 — importer et empiler tous les fichiers du dossier Stage/
  
  charger_tous_les_stages <- function(dossier = "Stage") {
    fichiers <- list.files(dossier, pattern = "\\.xlsx$", full.names = TRUE)
    fichiers <- fichiers[!str_detect(basename(fichiers), "^~\\$")]
    
    map_dfr(fichiers, function(f) {
      read_excel(f) %>%
        mutate(across(everything(), as.character)) %>%
        mutate(nom_stage = tools::file_path_sans_ext(basename(f)))
    }) %>%
      filter(!is.na(nom))
  }
  
  donnees_brutes <- charger_tous_les_stages()
  
  
# Étape 4 — reconvertir les colonnes dans leur vrai type
  
  donnees <- donnees_brutes %>%
    mutate(
      date_naissance = as.Date(date_naissance),
      date_test = as.Date(date_test),
      valeur = as.numeric(valeur),
      age = as.numeric(floor(interval(date_naissance, date_test) / years(1))),
      categorie = categorie_ffhg(date_naissance, date_test),
      nom_complet = paste(prenom, nom)
    )
  
  normes <- read_excel("normes_reference.xlsx")
  
# Étape 5 — fusionner avec le fichier de normes
  
  normes <- normes %>%
    filter(!is.na(categorie_ffhg))
  

# Étape 6 — la partie centrale : joindre chaque résultat à sa référence (si elle existe), et calculer le score de comparaison.
  
  donnees <- donnees %>%
    left_join(normes, by = c("test", "genre", "categorie" = "categorie_ffhg"))
  
  donnees %>% 
    select(nom_complet, test, valeur, genre, categorie, moyenne, ecart_type, sens_inverse)
  
  
# Étape 7 — calculer le score de comparaison.
  
  donnees <- donnees %>%
    mutate(
      a_reference = !is.na(moyenne) & !is.na(ecart_type),
      z_score = case_when(
        a_reference & sens_inverse == "TRUE"  ~ (moyenne - valeur) / ecart_type,
        a_reference & sens_inverse == "FALSE" ~ (valeur - moyenne) / ecart_type,
        TRUE ~ NA_real_
      )
    )
  
  donnees %>% 
    select(nom_complet, test, valeur, a_reference, z_score)
  
# Étape 8 — le calcul de secours : comparaison au groupe
  
  donnees_brutes <- charger_tous_les_stages()
  
  donnees <- donnees_brutes %>%
    mutate(
      date_naissance = as.Date(date_naissance),
      date_test = as.Date(date_test),
      valeur = as.numeric(valeur),
      age = as.numeric(floor(interval(date_naissance, date_test) / years(1))),
      categorie = categorie_ffhg(date_naissance, date_test),
      nom_complet = paste(prenom, nom)
    )
  
  normes <- read_excel("normes_reference.xlsx") %>%
    filter(!is.na(categorie_ffhg))
  
  donnees <- donnees %>%
    left_join(normes, by = c("test", "genre", "categorie" = "categorie_ffhg")) %>%
    mutate(
      a_reference = !is.na(moyenne) & !is.na(ecart_type),
      z_score = case_when(
        a_reference & sens_inverse == "TRUE"  ~ (moyenne - valeur) / ecart_type,
        a_reference & sens_inverse == "FALSE" ~ (valeur - moyenne) / ecart_type,
        TRUE ~ NA_real_
      )
    )
  
  sens_test <- tibble(
    test = c("CMJ", "SJ", "Illinois", "VIFT"),
    sens_inverse_test = c("FALSE", "FALSE", "TRUE", "FALSE")
  )
  
  donnees <- donnees %>%
    left_join(sens_test, by = "test") %>%
    group_by(nom_stage, test, categorie, genre) %>%
    mutate(
      n_groupe = n_distinct(id_athlete),
      mediane_groupe = median(valeur, na.rm = TRUE),
      ecart_pct_groupe = ifelse(n_groupe >= 3,
                                ifelse(sens_inverse_test == "TRUE",
                                       (mediane_groupe - valeur) / mediane_groupe * 100,
                                       (valeur - mediane_groupe) / mediane_groupe * 100),
                                NA_real_)
    ) %>%
    ungroup()
  
# Étape 9 — calculer la progression.
  
  donnees <- donnees %>%
    arrange(id_athlete, test, date_test) %>%
    group_by(id_athlete, test) %>%
    mutate(
      valeur_precedente = lag(valeur),
      date_test_precedente = lag(date_test),
      progression = ifelse(sens_inverse_test == "TRUE",
                           valeur_precedente - valeur,
                           valeur - valeur_precedente),
      progression_pct = progression / valeur_precedente * 100
    ) %>%
    ungroup()
  
  donnees %>% 
    select(nom_complet, test, date_test, valeur, progression, mediane_groupe, ecart_pct_groupe) %>%
    arrange(nom_complet, test, date_test)
  
  
  
  #------------------------------------------------
    
  # =========================================================================
  # GRAPHIQUE DU BILAN pour un test : une barre par jeune, groupée par
  # catégorie + genre
  # -------------------------------------------------------------------------
  # - une barre par jeune (Prénom + initiale du nom), du meilleur en haut au
  #   moins bon en bas, avec la valeur au bout
  # - trait rouge = moyenne des études (normes_reference.xlsx), seulement
  #   quand elle existe pour cette catégorie et ce genre
  # - vert = au-dessus des études, orange = en dessous, gris = pas d'étude
  # =========================================================================
  graphique_bilan_test <- function(donnees, nom_stage_cible, test_cible, titre) {
    
    # --- 1. Les résultats de ce test, pour ce stage (1 seul résultat par jeune) ---
    groupe <- donnees %>%
      filter(nom_stage == nom_stage_cible, test == test_cible, !is.na(valeur)) %>%
      group_by(id_athlete) %>%
      slice_max(date_test, n = 1, with_ties = FALSE) %>%   # s'il y a 2 essais : le plus récent
      ungroup()
    
    if (nrow(groupe) == 0) return(NULL)
    
    # --- 2. Sens du test, unité, décimales ---
    # (nom différent de la colonne "sens_inverse" des normes, sinon dplyr prend la colonne)
    plus_bas_mieux <- isTRUE(groupe$sens_inverse_test[1] == "TRUE")   # Illinois : plus bas = mieux
    info <- infos_tests %>% filter(test == test_cible)
    decimales <- if (nrow(info) == 1) info$decimales else 1
    
    # --- 3. Titre de chaque groupe ("U15 M  (n=12)") et nom court de chaque jeune ("Victor D.") ---
    ordre_categories <- c("U7", "U9", "U11", "U13", "U15", "U18", "U20", "Senior")
    
    groupe <- groupe %>%
      group_by(categorie, genre) %>%
      mutate(ligne = paste0(categorie, " ", genre, "  (n=", n(), ")")) %>%
      ungroup() %>%
      mutate(
        categorie = factor(categorie, levels = ordre_categories),
        nom_court = paste0(str_to_title(prenom), " ", str_sub(nom, 1, 1), "."),
        # couleur de la barre : comparaison à la moyenne des études
        couleur = case_when(
          is.na(moyenne)                       ~ "pas d'étude",
          plus_bas_mieux  & valeur <= moyenne    ~ "au-dessus des études",
          !plus_bas_mieux & valeur >= moyenne    ~ "au-dessus des études",
          TRUE                                 ~ "en dessous des études"
        ),
        texte_valeur = format_nombre(valeur, decimales)
      ) %>%
      arrange(categorie, genre)
    
    # Ordre des groupes : la plus jeune catégorie en haut
    groupe$ligne <- factor(groupe$ligne, levels = unique(groupe$ligne))
    
    # Ordre des barres : le meilleur en haut de son groupe
    # (ggplot dessine le 1er niveau en bas, donc on range du moins bon au meilleur)
    groupe <- groupe %>% arrange(if (plus_bas_mieux) desc(valeur) else valeur)
    groupe$id_athlete <- factor(groupe$id_athlete, levels = unique(groupe$id_athlete))
    noms_courts <- setNames(groupe$nom_court, groupe$id_athlete)
    
    # --- 4. Le trait rouge : la moyenne des études, quand elle existe ---
    etudes <- groupe %>%
      filter(!is.na(moyenne)) %>%
      distinct(ligne, moyenne)
    
    # --- 5. Le graphique ---
    couleurs <- c("au-dessus des études"  = "#1B7837",
                  "en dessous des études" = "#E08214",
                  "pas d'étude"           = "#9E9E9E")
    
    texte_axe <- if (plus_bas_mieux) paste0(titre, "      plus court = mieux") else titre
    
    p <- ggplot(groupe, aes(x = valeur, y = id_athlete, fill = couleur)) +
      geom_col(width = 0.75) +
      geom_text(aes(label = texte_valeur), hjust = -0.15, size = 3, color = "grey20")
    
    if (nrow(etudes) > 0) {
      p <- p + geom_vline(data = etudes, aes(xintercept = moyenne),
                          color = "#C0392B", linewidth = 1.2, inherit.aes = FALSE)
    }
    
    p +
      # Un bloc par groupe (catégorie + genre), titre du groupe à gauche ;
      # space = "free_y" : un groupe de 12 jeunes est plus haut qu'un groupe de 1
      facet_grid(rows = vars(ligne), scales = "free_y", space = "free_y", switch = "y") +
      scale_y_discrete(labels = function(id) noms_courts[id]) +   # affiche "Victor D." au lieu de l'identifiant
      scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
      scale_fill_manual(values = couleurs, name = NULL) +
      labs(x = texte_axe, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top",
            legend.justification = "left",
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            strip.placement = "outside",
            strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1, size = 10),
            panel.spacing.y = unit(10, "pt"),
            axis.title.x = element_text(color = "grey30", margin = margin(t = 8)))
  }
  
  
  library(ggplot2)
  
  # =========================================================================
  # CARTE D'UN TEST pour la fiche d'un jeune (barre de niveau + progression + avant/après)
  # -------------------------------------------------------------------------
  # Pour modifier :
  #   - noms, unités, décimales, légendes de la barre -> tableau infos_tests
  #   - couleurs de la barre                          -> vecteur "couleurs"
  #   - taille des textes                             -> arguments "size"
  #   - taille de l'image                             -> width / height de ggsave
  # =========================================================================
  
  # Tableau des tests : à compléter quand on ajoute un nouveau test.
  # La barre va toujours de "moins bien" (gauche) à "mieux" (droite), même pour l'Illinois.
  # Le sens (plus haut = mieux, ou plus bas = mieux) vient de sens_test plus haut dans ce fichier.
  infos_tests <- tibble::tribble(
    ~test,      ~nom_affiche,              ~unite, ~decimales, ~gauche,        ~droite,
    "CMJ",      "Détente verticale (CMJ)", "cm",   1,          "moins haut",   "plus haut",
    "SJ",       "Saut sans élan (SJ)",     "cm",   1,          "moins haut",   "plus haut",
    "Illinois", "Agilité (Illinois)",      "s",    2,          "plus lent",    "plus rapide",
    "VIFT",     "Endurance (VIFT)",        "km/h", 1,          "moins rapide", "plus rapide"
  )
  
  # Mois en français, sans dépendre de la langue de l'ordinateur ("oct. 2026")
  mois_fr <- function(d) {
    mois <- c("janv.", "févr.", "mars", "avr.", "mai", "juin",
              "juil.", "août", "sept.", "oct.", "nov.", "déc.")
    paste(mois[lubridate::month(d)], lubridate::year(d))
  }
  
  # Nombre avec virgule décimale et le bon nombre de décimales ("31,7")
  format_nombre <- function(x, decimales) {
    format(round(x, decimales), nsmall = decimales, decimal.mark = ",", trim = TRUE)
  }
  
  # Dessine la carte d'un test pour un jeune et l'enregistre en image (chemin_png).
  # Renvoie NULL si le jeune n'a pas passé ce test.
  carte_test_athlete <- function(donnees, id_athlete_cible, test_cible, chemin_png) {
    
    # --- 1. Le dernier résultat de ce jeune pour ce test ---
    ligne <- donnees %>%
      filter(id_athlete == id_athlete_cible, test == test_cible) %>%
      arrange(desc(date_test)) %>%
      slice(1)
    
    if (nrow(ligne) == 0) return(NULL)
    if (is.na(ligne$valeur)) return(NULL)
    if (is.na(ligne$sens_inverse_test)) {
      stop("Test inconnu dans sens_test (import_donnees.R) : ", test_cible)
    }
    
    # --- 2. Les textes d'affichage (nom, unité, légendes de la barre) ---
    info <- infos_tests %>% filter(test == test_cible)
    if (nrow(info) == 0) {
      info <- tibble::tibble(test = test_cible, nom_affiche = test_cible, unite = "",
                             decimales = 1, gauche = "moins bien", droite = "mieux")
    }
    valeur_txt <- trimws(paste(format_nombre(ligne$valeur, info$decimales), info$unite))
    
    # --- 3. Le groupe de comparaison : même stage, même test, même catégorie, même genre ---
    groupe <- donnees %>%
      filter(test == test_cible,
             nom_stage == ligne$nom_stage,
             categorie == ligne$categorie,
             genre == ligne$genre)
    n_groupe <- n_distinct(groupe$id_athlete)
    
    # --- 4. Le haut de la carte : nom du test à gauche, résultat en gros à droite ---
    couleurs <- c("#E08214", "#FDDA0D", "#7FBF7B", "#1B7837")  # moins bien -> mieux
    
    p <- ggplot() +
      annotate("text", x = 0, y = 3.35, label = info$nom_affiche, hjust = 0, size = 4.2) +
      annotate("text", x = 4, y = 3.35, label = valeur_txt, hjust = 1, size = 7.5, fontface = "bold")
    
    # --- 5. La barre de niveau (seulement si le groupe compte au moins 3 jeunes) ---
    if (n_groupe >= 3) {
      
      # Pour l'Illinois (plus bas = mieux), on inverse les valeurs : ainsi, pour
      # tous les tests, "plus grand" veut dire "mieux", et la barre va dans le même sens.
      sens <- if (ligne$sens_inverse_test == "TRUE") -1 else 1
      x_groupe <- sens * groupe$valeur
      x_jeune  <- sens * ligne$valeur
      
      # q = minimum, 1er quart, médiane, 3e quart, maximum du groupe
      q <- quantile(x_groupe, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE)
      
      # Dans quel morceau de la barre (1 à 4) se trouve le jeune ? Même règle que les anciennes fiches.
      segment <- as.integer(1 + (x_jeune >= q[2]) + (x_jeune > q[3]) + (x_jeune > q[4]))
      
      # Position exacte du rond dans ce morceau (entre 0 et 4 sur la barre)
      bas  <- q[segment]
      haut <- q[segment + 1]
      fraction <- if (haut > bas) (x_jeune - bas) / (haut - bas) else 0.5
      position <- min(max((segment - 1) + fraction, 0.1), 3.9)
      
      p <- p +
        annotate("rect", xmin = 0:3, xmax = 1:4, ymin = 1.9, ymax = 2.5, fill = couleurs) +
        annotate("point", x = position, y = 2.2, shape = 21, size = 7,
                 fill = "white", color = "black", stroke = 1.6) +
        annotate("text", x = 0, y = 1.5, label = info$gauche, hjust = 0, size = 3.1, color = "grey40") +
        annotate("text", x = 4, y = 1.5, label = info$droite, hjust = 1, size = 3.1, color = "grey40")
      
    } else {
      p <- p +
        annotate("text", x = 0, y = 2.2, label = "Pas encore de repère pour ce groupe",
                 hjust = 0, size = 3.6, color = "grey40")
    }
    
    # --- 6. La progression depuis le dernier stage (seulement s'il y en a un) ---
    a_progression <- !is.na(ligne$progression)
    couleur_prog <- "#2E8B57"
    
    if (a_progression) {
      monte <- ligne$progression >= 0
      couleur_prog <- if (monte) "#2E8B57" else "#C0392B"
      fleche <- if (monte) "\u25B2" else "\u25BC"
      signe <- if (isTRUE(ligne$progression_pct >= 0)) "+" else ""
      texte_prog <- paste0(fleche, " ", signe, round(ligne$progression_pct), " % depuis le dernier stage")
      
      p <- p +
        annotate("text", x = 0, y = 0.5, label = texte_prog, hjust = 0,
                 size = 4.3, fontface = "bold", color = couleur_prog)
    }
    
    y_bas <- if (a_progression) 0 else 1.2
    p <- p +
      coord_cartesian(xlim = c(0, 4), ylim = c(y_bas, 3.9), expand = FALSE, clip = "off") +
      theme_void() +
      theme(plot.margin = margin(4, 10, 4, 10))
    
    hauteur_niveau <- (3.9 - y_bas) * 0.4   # hauteur de l'image en pouces
    
    # --- 7. Avant / après (seulement s'il y a un stage précédent), puis enregistrement ---
    if (a_progression) {
      
      donnees_d <- tibble::tibble(
        y = c(2, 1),
        etiquette = c(mois_fr(ligne$date_test_precedente), mois_fr(ligne$date_test)),
        valeur = c(ligne$valeur_precedente, ligne$valeur),
        couleur = c("#9E9E9E", couleur_prog)
      )
      vmax <- max(donnees_d$valeur)
      
      p_d <- ggplot(donnees_d) +
        geom_rect(aes(xmin = 0, xmax = valeur, ymin = y - 0.32, ymax = y + 0.32, fill = couleur)) +
        geom_text(aes(x = -0.03 * vmax, y = y, label = etiquette),
                  hjust = 1, size = 3.2, color = "grey30") +
        geom_text(aes(x = valeur + 0.03 * vmax, y = y, label = format_nombre(valeur, info$decimales)),
                  hjust = 0, size = 3.6) +
        scale_fill_identity() +
        coord_cartesian(xlim = c(-0.42 * vmax, 1.3 * vmax), ylim = c(0.5, 2.5),
                        expand = FALSE, clip = "off") +
        theme_void() +
        theme(plot.margin = margin(0, 10, 4, 10))
      
      hauteur_d <- 0.8
      p_final <- patchwork::wrap_plots(p, p_d, ncol = 1, heights = c(hauteur_niveau, hauteur_d))
      ggsave(chemin_png, p_final, width = 4.2, height = hauteur_niveau + hauteur_d,
             dpi = 150, bg = "white")
      
    } else {
      ggsave(chemin_png, p, width = 4.2, height = hauteur_niveau, dpi = 150, bg = "white")
    }
    
    invisible(chemin_png)
  }
    
  
  # =========================================================================
  # PETIT TABLEAU "RÉFÉRENCE ÉTUDES" sous la carte d'un test
  # -------------------------------------------------------------------------
  # Renvoie le code LaTeX d'un petit tableau qui compare le dernier résultat
  # du jeune à la valeur des études (normes_reference.xlsx).
  # Renvoie "" (rien) s'il n'y a pas de référence pour ce test, cette
  # catégorie et ce genre : la fiche n'affiche alors aucun tableau.
  # Le texte court de la source vient de la colonne "source_courte" de l'Excel.
  # =========================================================================
  
  # Protège les caractères qui posent problème en LaTeX (&, %, #, _)
  echapper_latex <- function(x) {
    x <- gsub("&", "\\\\&", x)
    x <- gsub("%", "\\\\%", x)
    x <- gsub("#", "\\\\#", x)
    gsub("_", "\\\\_", x)
  }
  
  tableau_reference <- function(donnees, id_athlete_cible, test_cible) {
    
    # --- 1. Le dernier résultat de ce jeune pour ce test ---
    ligne <- donnees %>%
      filter(id_athlete == id_athlete_cible, test == test_cible) %>%
      arrange(desc(date_test)) %>%
      slice(1)
    
    # Pas de résultat ou pas de référence -> pas de tableau
    if (nrow(ligne) == 0) return("")
    if (is.na(ligne$valeur) || is.na(ligne$moyenne)) return("")
    
    # --- 2. Unité et nombre de décimales (tableau infos_tests) ---
    info <- infos_tests %>% filter(test == test_cible)
    unite <- if (nrow(info) == 1) info$unite else ""
    decimales <- if (nrow(info) == 1) info$decimales else 1
    
    resultat <- trimws(paste(format_nombre(ligne$valeur, decimales), unite))
    
    # --- 3. La référence : "36,8 ± 3,4 (hockey élite, Kierot 2024)" ---
    reference <- format_nombre(ligne$moyenne, decimales)
    if (!is.na(ligne$ecart_type)) {
      reference <- paste0(reference, " ± ", format_nombre(ligne$ecart_type, decimales))
    }
    source_txt <- if ("source_courte" %in% names(ligne) && !is.na(ligne$source_courte)) {
      ligne$source_courte
    } else {
      ligne$note_qualite
    }
    if (!is.na(source_txt)) reference <- paste0(reference, " (", source_txt, ")")
    
    # --- 4. L'écart : "+1,1 écart-type, au-dessus" ---
    # Pour l'Illinois (plus bas = mieux), on dit "plus rapide" / "plus lent"
    sens_inverse <- isTRUE(ligne$sens_inverse_test == "TRUE")
    mot_mieux <- if (sens_inverse) "plus rapide" else "au-dessus"
    mot_moins <- if (sens_inverse) "plus lent" else "en dessous"
    
    if (!is.na(ligne$z_score)) {
      # z_score tient déjà compte du sens du test : positif = mieux
      z <- ligne$z_score
      signe <- if (z >= 0) "+" else "-"
      ecart <- paste0(signe, format_nombre(abs(z), 1), " écart-type, ",
                      if (z >= 0) mot_mieux else mot_moins)
    } else {
      # Pas d'écart-type publié : on dit seulement mieux ou moins bien
      mieux <- if (sens_inverse) ligne$valeur <= ligne$moyenne else ligne$valeur >= ligne$moyenne
      ecart <- paste0(if (mieux) mot_mieux else mot_moins, " (sans écart-type publié)")
    }
    
    # --- 5. Le code LaTeX du tableau ---
    colonne <- function(largeur) paste0(">{\\raggedright\\arraybackslash}p{", largeur, "}")
    paste0(
      "\\par\\vspace{3pt}{\\scriptsize\\renewcommand{\\arraystretch}{1.2}%\n",
      "\\begin{tabularx}{\\linewidth}{@{}", colonne("0.95cm"), colonne("1.25cm"),
      ">{\\raggedright\\arraybackslash}X", colonne("2.1cm"), "@{}}\n",
      "\\textbf{Test} & \\textbf{Résultat} & \\textbf{Référence études} & \\textbf{Écart}\\\\\\hline\n",
      echapper_latex(test_cible), " & ", echapper_latex(resultat), " & ",
      echapper_latex(reference), " & ", echapper_latex(ecart), "\\\\\n",
      "\\end{tabularx}}\n"
    )
  }
