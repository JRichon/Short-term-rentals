#### Script permettant de découper en plusieurs fichiers la base fusionnée de commentaires (InsideAirbnb reviews) selon l'année de dépot des commentaires
#script à lancer après l'autre "InsideAirbnb_reviews_fusion_dedoublon"
#script cree par Clementine Jager

library(lubridate)
library(dplyr)
library(tools)

options(scipen=999) # pour éviter d'avoir de l'écriture scientifique

path_input <- '~/Fusion_donnees_reviews/'
path_output <- '~/CUT_fusion_reviews/'

files <- list.files(path_input) # Fonction permettant de récupérer tous les noms de fichiers du dossier mis en argument ici c'est l'objet `path` qui contient le chemin
files # Pour voir dans la console l'objet `files` qui contient la liste des fichiers présents dans le dossier `path`

# on ne garde que les fichiers finissant par l'extension .xlsx
files_csv <- files[tolower(tools::file_ext(files)) %in% c("csv", "gz")]

files_csv_sans_ext <- file_path_sans_ext(files_csv)

for(i in seq_along(files_csv)){
  data_ <- readr::read_csv(paste0(path_input, files_csv[i]), col_types =  cols(
    listing_id = col_character(),
    id = col_character(),
    date = col_date(format = ""),
    reviewer_id = col_character(),
    reviewer_name = col_character(),
    comments = col_character()
  ))
  
  data_ <- data_ %>%
    mutate(date_annee = year(date))
  
  liste_date <- unique(data_$date_annee)
  
  sapply(liste_date, FUN = function(x){
    extr_data <- data_[data_$date_annee %in% x, ]
    if(!is.null(extr_data$comments)){
      extr_data$comments <- gsub("/r?/n|/r", " ",extr_data$comments ) 
        
    }
    file_name <- paste0(path_output, files_csv_sans_ext[i], "__", x, ".csv")
      
    write.csv(extr_data, file_name)
    
  })
  
} #on obtient dans le dossier indiqué en output autant de fichiers différents que d'années durant lesquelles des commentaires ont été publié sur la plateforme airbnb



