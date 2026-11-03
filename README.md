# Suivi Stage Red Bears

Outil développé dans le cadre d'un stage pour un club de hockey sur glace, permettant de générer automatiquement les fiches de résultats physiques de chaque jeune athlète à l'issue d'un stage, ainsi qu'un bilan global du groupe.

## Contexte

À chaque stage, les jeunes passent une série de tests physiques (CMJ, SJ, test Illinois, VIFT...). L'objectif était de remplacer la création manuelle d'une fiche par enfant par un système automatisé, fiable et réutilisable d'un stage à l'autre.

## Fonctionnalités

- **Import multi-stages** : les résultats de chaque stage sont saisis dans un fichier Excel au format long (un athlète + un test + une date = une ligne). Le système agrège automatiquement tous les stages passés.
- **Catégories d'âge calculées dynamiquement** : la catégorie FFHG de chaque athlète (U13, U15, U18...) est recalculée à partir de sa date de naissance et de la date du test — aucune mise à jour manuelle nécessaire d'une saison à l'autre.
- **Comparaison intelligente** :
  - Pour les garçons, comparaison à des normes issues d'études scientifiques sourcées (hockey, football), quand elles existent pour le test et la catégorie concernés.
  - Repli automatique sur une comparaison au groupe (médiane, quartiles) quand aucune norme scientifique fiable n'est disponible, ou pour les filles.
  - Seuil minimum d'effectif avant d'afficher une comparaison de groupe, pour éviter les conclusions non représentatives.
- **Suivi de la progression** : chaque athlète est automatiquement comparé à ses propres résultats du stage précédent, avec gestion du sens de progression selon le test (un temps plus bas est une amélioration, par exemple).
- **Génération automatique des documents** :
  - Une fiche PDF individuelle par athlète, avec identité visuelle du club.
  - Un bilan de stage global, avec la répartition des performances par catégorie et par genre.

## Stack technique

- **R** / **RStudio** pour le traitement des données (tidyverse : dplyr, tidyr, purrr, lubridate)
- **R Markdown** pour la génération des documents
- **ggplot2** pour les visualisations (boxplots, indicateurs de progression)
- **LaTeX (XeLaTeX)** pour la mise en forme PDF et l'intégration de la typographie et de l'identité visuelle du club

## Note sur les données

Les données présentes dans ce dépôt sont fictives, à des fins de démonstration. Les véritables données des athlètes ne sont jamais versionnées.

---

*Projet réalisé dans le cadre d'un stage, avec un apprentissage progressif de R tout au long du développement.*
