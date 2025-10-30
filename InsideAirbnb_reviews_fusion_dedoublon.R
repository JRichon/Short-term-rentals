#### script permettant de fusionner les fichiers InsideAirbnb reviews ensemble et de dédoublonner les commentaires par leur identifiant
#Script cree par Clementine Jager

library(lubridate)
library(dplyr)
library(tools)
library(readr)
options(scipen=999)

path_input <- 'C:/Users/janou/Documents/SCRAPING TEMPORAIRE/InsideAirbnb_MGP/Fusion_donnees_reviews_MGP/'
path_output <- 'C:/Users/janou/Documents/SCRAPING TEMPORAIRE/InsideAirbnb_MGP/Fusion_donnees_reviews_MGP/fusion/'

files <- list.files(path_input) # Fonction permettant de récupérer tous les noms de fichiers du dossier mis en argument ici c'est l'objet `path` qui contient le chemin
files # Pour voir dans la console l'objet `files` qui contient la liste des fichiers présents dans le dossier `path`

# on ne garde que les fichiers finissant par l'extension .csv
files_csv <- files[tolower(tools::file_ext(files)) %in% c("csv", "gz")]
files_csv_sans_ext <- file_path_sans_ext(files_csv)

# importation et fusion des tables

for(i in seq_along(files_csv_sans_ext)){
  if(i %in% 1){
    base <- readr::read_csv(paste0(path_input, files_csv[i]), col_types =  cols(
      listing_id = col_character(),
      id = col_character(),
      date = col_date(format = ""),
      reviewer_id = col_character(),
      reviewer_name = col_character(),
      comments = col_character()
    ))
    
  }else{
    base_to_add <- readr::read_csv(paste0(path_input, files_csv[i]), col_types =  cols(
      listing_id = col_character(),
      id = col_character(),
      date = col_date(format = ""),
      reviewer_id = col_character(),
      reviewer_name = col_character(),
      comments = col_character()
    ))
    
    base <- bind_rows(base, base_to_add)
    base <- base %>%
      distinct(id, .keep_all = TRUE)
  }
}

names(base)

base <- base %>% select(id, listing_id, date, reviewer_id, reviewer_name, comments)

write.csv(base, file = paste0(path_output,"fusion_reviews.csv")) #fichier contenant l'ensemble des commentaires dédoublonnés

#on peut ensuite appliquer à cette grosse base fusionnée le script "InsideAirbnb_reviews_fusion_decoup_annee" qui permet de la découper en fichiers par année

