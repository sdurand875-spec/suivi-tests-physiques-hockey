library(dplyr)
library(rmarkdown)

source("import_donnees.R")

# =========================================================================
# PARTIE 1 — Identifier le dernier stage
# -------------------------------------------------------------------------
# Même logique que generer_toutes_les_fiches.R : on travaille toujours sur
# le stage le plus récent (celui qu'on vient de terminer), jamais tout
# l'historique d'un coup.
# =========================================================================

date_dernier_stage <- max(donnees$date_test, na.rm = TRUE)
nom_dernier_stage <- donnees %>%
  filter(date_test == date_dernier_stage) %>%
  pull(nom_stage) %>%
  first()

cat("Bilan à générer pour :", nom_dernier_stage, "(", format(date_dernier_stage, "%d/%m/%Y"), ")\n")

if (is.na(nom_dernier_stage)) {
  stop("Aucun stage trouvé : vérifie le dossier Stage/ et les dates dans l'Excel.")
}


# =========================================================================
# PARTIE 2 — Créer le dossier de sortie
# -------------------------------------------------------------------------
# Résultat : Bilan_Stage/ (à la racine du projet, séparé du dossier Fiches/
# qui contient les fiches individuelles des athlètes)
# =========================================================================

dossier_sortie <- "Bilan_Stage"
dir.create(dossier_sortie, showWarnings = FALSE)


# =========================================================================
# PARTIE 3 — Générer le PDF
# -------------------------------------------------------------------------
# Nom du fichier : bilan_ + le nom du stage en minuscules
# (ex: "Stage_Hiver_2027" -> "bilan_stage_hiver_2027.pdf")
# =========================================================================

nom_fichier <- paste0("bilan_", tolower(nom_dernier_stage), ".pdf")

render(
  input = "fiche_bilan_stage.Rmd",
  output_file = nom_fichier,
  output_dir = dossier_sortie,
  params = list(nom_stage_cible = nom_dernier_stage),
  envir = new.env()
)

cat("Terminé :", file.path(dossier_sortie, nom_fichier), "\n")

# Nettoyage des images temporaires utilisées pour construire le PDF
unlink("temp_img", recursive = TRUE)
