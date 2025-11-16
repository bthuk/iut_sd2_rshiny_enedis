# 1. Charger les packages
install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)

# 2. Définir les infos de l'API (Logements Neufs - Dép. 31)
base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe02neuf/lines"
page_size <- 1000  
start_offset <- 0   
total_rows_fetched <- 0
all_data_list <- list()

print("Début de la récupération des données (Logements Neufs - Dép. 31)...")

# 3. Démarrer la boucle
while (TRUE) {
  
  # --- On télécharge tout, sans paramètre 'select' ---
  query_params <- list(
    rows = page_size,
    start = start_offset,
    qs = 'code_departement_ban:"31"' # Filtre sur le Dép. 31
  )
  
  response <- GET(url = base_url, query = query_params)
  
  if (response$status_code == 200) {
    data_content <- content(response, as = "text", encoding = "UTF-8")
    data_json <- fromJSON(data_content)
    current_page_data <- data_json$results
    rows_in_this_page <- nrow(current_page_data)
    
    if (!is.null(rows_in_this_page) && rows_in_this_page > 0) {
      all_data_list[[length(all_data_list) + 1]] <- current_page_data
      start_offset <- start_offset + rows_in_this_page
      total_rows_fetched <- total_rows_fetched + rows_in_this_page
      print(paste("... ", total_rows_fetched, "lignes (neufs) récupérées..."))
      Sys.sleep(0.1) 
    } else {
      print("Fin de la récupération (neufs).")
      break 
    }
  } else {
    print(paste("Erreur : Statut", response$status_code))
    print(content(response, as = "text")) 
    break 
  }
} 

# 4. Combiner et Sauvegarder
if (length(all_data_list) > 0) {
  df_neufs_31 <- do.call(rbind, all_data_list)
  saveRDS(df_neufs_31, "donnees_dpe_neufs_31.rds")
  print("Terminé ! Les données NEUFS sont dans 'donnees_dpe_neufs_31.rds'")
} else {
  print("Aucune donnée n'a été récupérée pour les logements neufs.")
}