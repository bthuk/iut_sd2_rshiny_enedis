# 1. Charger le package 'dplyr' (installez-le si besoin)
 install.packages("dplyr")
library(dplyr)

print("Début de la fusion et du nettoyage...")

# 2. Charger nos deux fichiers de données brutes
df_existants <- readRDS("donnees_dpe_existants_31.rds")
df_neufs <- readRDS("donnees_dpe_neufs_31.rds")

print(paste("Logements existants chargés :", nrow(df_existants), "lignes"))
print(paste("Logements neufs chargés :", nrow(df_neufs), "lignes"))

# 3. Ajouter la colonne "type_logement" pour les différencier
df_existants$type_logement <- "Existant"
df_neufs$type_logement <- "Neuf"

# 4. Combiner les deux tableaux
# bind_rows() est intelligent : il garde toutes les colonnes
# et met "NA" si une colonne n'existe pas dans l'autre (ex: annee_construction)
df_complet <- bind_rows(df_existants, df_neufs)

print(paste("Données combinées :", nrow(df_complet), "lignes"))

# 5. Nettoyage et calcul
df_propre <- df_complet %>%
  
  # Sélectionner seulement les colonnes qui nous intéressent
  select(
    type_batiment,
    surface_habitable_logement,
    nombre_niveau_logement,
    qualite_isolation_menuiseries,
    qualite_isolation_murs,
    qualite_isolation_plancher_bas,
    etiquette_dpe,
    etiquette_ges,
    emission_ges_5_usages_par_m2,
    ubat_w_par_m2_k,
    conso_5_usages_par_m2_ep,
    conso_5_usages_par_m2_ef,
    conso_chauffage_ep,
    conso_ecs_ep,
    conso_eclairage_ep,
    cout_chauffage,
    cout_ecs,
    cout_eclairage,
    cout_total_5_usages,
    code_postal_ban,
    nom_commune_ban,
    coordonnee_cartographique_x_ban,
    coordonnee_cartographique_y_ban
  ) %>%
  
  # Supprimer les lignes avec des données essentielles manquantes
  filter(
    !is.na(etiquette_dpe),
    !is.na(surface_habitable_logement)
  ) %>%
  
  # Supprimer les données aberrantes (surface > 0 et classe_dpe valide)
  filter(surface_habitable_logement > 0) %>%
  filter(etiquette_dpe%in% c("A", "B", "C", "D", "E", "F", "G")) %>%
  


print(paste("Données nettoyées :", nrow(df_propre), "lignes restantes"))

# 6. Sauvegarder le fichier final, prêt pour l'analyse !
saveRDS(df_propre , "dpe_data.rds")

print("Terminé ! Le fichier 'dpe_data.rds' est prêt pour le projet.")

# 7. (Optionnel) Afficher un aperçu
print("Aperçu des données finales :")
print(head(df_propre))