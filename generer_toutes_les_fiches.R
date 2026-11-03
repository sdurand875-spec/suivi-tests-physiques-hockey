library(dplyr)
library(rmarkdown)
library(stringr)

source("import_donnees.R")

# =========================================================================
# PARTIE 1 — Identifier le dernier stage
# -------------------------------------------------------------------------
# On ne génère jamais les fiches de TOUT l'historique, seulement celles
# du stage le plus récent (celui qu'on vient de terminer).
# "Le plus récent" = la date_test la plus grande dans toutes les données.
# =========================================================================

date_dernier_stage <- max(donnees$date_test, na.rm = TRUE)

# nom_dernier_stage vient de la colonne nom_stage qu'on a ajoutée dans
# charger_tous_les_stages() (Étape 3 de import_donnees.R) : c'est le nom
# du fichier Excel d'origine, sans l'extension .xlsx
# (ex: "Stage_Hiver_2027.xlsx" -> "Stage_Hiver_2027")
nom_dernier_stage <- donnees %>%
  filter(date_test == date_dernier_stage) %>%
  pull(nom_stage) %>%
  first()

cat("Dernier stage détecté :", nom_dernier_stage, "(", format(date_dernier_stage, "%d/%m/%Y"), ")\n")

if (is.na(nom_dernier_stage)) {
  stop("Aucun stage trouvé : vérifie le dossier Stage/ et les dates dans l'Excel.")
}


# =========================================================================
# PARTIE 2 — Lister les athlètes présents à ce stage
# -------------------------------------------------------------------------
# distinct() évite les doublons : un athlète qui a passé 4 tests au même
# stage n'apparaît qu'une fois dans cette liste.
# =========================================================================

athletes_du_stage <- donnees %>%
  filter(nom_stage == nom_dernier_stage) %>%
  distinct(id_athlete, nom_complet)

cat(nrow(athletes_du_stage), "athlète(s) à générer\n")


# =========================================================================
# PARTIE 3 — Créer le dossier de sortie pour ce stage
# -------------------------------------------------------------------------
# Résultat : Fiches/Stage_Hiver_2027/ (créé automatiquement s'il n'existe
# pas déjà). showWarnings = FALSE évite un message inutile si le dossier
# existe déjà (ex: si on relance le script deux fois).
# =========================================================================

dossier_sortie <- file.path("Fiches", nom_dernier_stage)
dir.create(dossier_sortie, showWarnings = FALSE, recursive = TRUE)


# =========================================================================
# PARTIE 4 — Générer une fiche PDF par athlète
# -------------------------------------------------------------------------
# Boucle sur chaque ligne de athletes_du_stage (donc chaque athlète unique).
# Pour chacun : on construit le nom de fichier (Prénom_Nom.pdf, espaces
# remplacés par des underscores), puis on appelle render() sur notre
# modèle fiche_athlete_v3.Rmd en lui passant juste l'id_athlete en paramètre
# — exactement comme quand on cliquait sur Knit à la main, mais ici R le
# fait tout seul pour chaque personne.
# =========================================================================

for (i in seq_len(nrow(athletes_du_stage))) {
  
  id_cible <- athletes_du_stage$id_athlete[i]
  nom_cible <- athletes_du_stage$nom_complet[i]
  
  nom_fichier <- str_replace_all(nom_cible, " ", "_")
  chemin_sortie <- file.path(dossier_sortie, paste0(nom_fichier, ".pdf"))
  
  cat("Génération :", nom_cible, "...\n")
  
  render(
    input = "fiche_athlete_v3.Rmd",
    output_file = basename(chemin_sortie),
    output_dir = dossier_sortie,
    params = list(id_athlete = id_cible),
    envir = new.env()  # environnement propre à chaque fiche, pour éviter
    # que les variables d'un athlète "polluent" le suivant
  )
}

cat("Terminé :", nrow(athletes_du_stage), "fiches générées dans", dossier_sortie, "\n")

# Nettoyage : on supprime les images temporaires utilisées pour construire les PDF
unlink("temp_img", recursive = TRUE)

