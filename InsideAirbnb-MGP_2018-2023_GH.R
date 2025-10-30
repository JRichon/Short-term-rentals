#Script de travail sur les données InsideAirbnb MGP
#script de fusion des jeux de données IA MGP après les voir découpés spatialement (car fichiers France entière initialement) et leur avoir ajouté à chacun des variables de localisation (voir script "Decoupage LISTING_InsideAirbnb France.R")

library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(questionr)
library(lubridate)
library(tools)
library(readr)
library(stringr)
library(anytime)
library(franc)
library(sf)
library(gt)
library(ggplot2)
library(data.table)
library(kableExtra) #- pour la mise en page des tableaux
library(FactoMineR) #- pour la réalisation d'une ACM et CAH
library(factoextra) #- pour mettre en page rapidement les résultats de l'ACM - notamment via la fonction fviz_eig()
library(FactoInvestigate)
library(plotly)
library(RColorBrewer) #- pour utiliser les palettes de couleur de ce package
library(corrplot)
library(textshape)
library(webshot2)
library(gtsummary)
library(ggvenn)


options(scipen=999) # pour éviter d'avoir de l'écriture scientifique

#### Fusion des fichiers InsideAirbnb ("IA") entre eux pour parvenir à une base unique ----

col_types <- c(
  id ="character",
  scrape_id = "character",
  listing_url = "character",
  host_id = "character",
  license = "character",
  host_listings_count = "integer",
  host_total_listings_count= "integer",
  accommodates = "numeric",
  bathrooms = "integer",
  bedrooms = "integer",
  beds = "integer",
  minimum_nights = "integer",
  maximum_nights = "integer",
  minimum_minimum_nights  = "integer",
  maximum_minimum_nights  = "integer",
  maximum_nights_avg_ntm = "numeric",
  minimum_maximum_nights = "numeric",   
  maximum_maximum_nights = "numeric",
  minimum_nights_avg_ntm = "numeric",                  
  availability_30 = "integer",
  availability_60 = "integer",
  availability_90 = "integer",
  availability_365 = "integer",
  number_of_reviews = "numeric",
  number_of_reviews_ltm = "numeric",
  number_of_reviews_l30d = "numeric",
  review_scores_rating ="numeric",
  review_scores_value ="numeric",
  review_scores_cleanliness = "numeric",
  review_scores_checkin = "numeric",
  review_scores_communication= "numeric",
  review_scores_accuracy = "numeric",
  review_scores_location = "numeric",     
  calculated_host_listings_count_entire_homes = "numeric",
  calculated_host_listings_count_private_rooms = "numeric",
  calculated_host_listings_count_shared_rooms = "numeric",
  calculated_host_listings_count= "numeric",
  calendar_updated = "character",
  calendar_last_scraped  = "character",
  first_review = "character",
  last_review = "character",
  reviews_per_month = "numeric",
  latitude = "character",
  longitude ="character",
  requires_license = "character",
  OBJECTID= "character",
  N_SQ_CO= "character",
  C_COINSEE= "character",
  C_DEP= "character",
  L_CO= "character",
  C_POSTAL= "character",
  C_POSTSEC= "character",
  C_AGGLO= "character",
  C_VILLENOU= "character",
  C_METROP= "character",
  B_LIMITRO= "character",
  B_UNITEURB= "character",
  N_SQ_DE= "character",
  N_SQ_EPCI= "character",
  NB_POP= "numeric",
  SHAPE_Leng= "character",
  SHAPE_Area= "character",
  zipcode= "character",
  square_feet= "character"
)

base2018 <- fread(file = files_IA_MGP[1], sep = ",", encoding='UTF-8', colClasses = col_types, na.strings = c("N/A", "", "NA"), fill = TRUE) #il met des msgs d'avis comme quoi il n'a pas transformé toutes les colonnes selon nos souhaits car parfois il y a des vides etc : pour Minstay, Superhost et Arrivée autonome
base2020_01 <- fread(file = files_IA_MGP[2], sep = ",", encoding='UTF-8', colClasses = col_types, na.strings = c("N/A", "", "NA"), fill = TRUE) 
base2021_11 <- fread(file = files_IA_MGP[3], sep = ",", encoding='UTF-8', colClasses = col_types, na.strings = c("N/A", "", "NA"), fill = TRUE) 
base2022_05 <- fread(file = files_IA_MGP[4], sep = ",", encoding='UTF-8', colClasses = col_types, na.strings = c("N/A", "", "NA"), fill = TRUE) 
base2022_08 <- fread(file = files_IA_MGP[5], sep = ",", encoding='UTF-8', colClasses = col_types, na.strings = c("N/A", "", "NA"), fill = TRUE) 
base2023_02 <- fread(file = files_IA_MGP[6], sep = ",", encoding='UTF-8', colClasses = col_types, na.strings = c("N/A", "", "NA"), fill = TRUE) 
base2023_05 <- fread(file = files_IA_MGP[7], sep = ",", encoding='UTF-8', colClasses = col_types, na.strings = c("N/A", "", "NA"), fill = TRUE) 
base2022_11 <- fread("~/Murray France/IA_listings_2022-11_MGP.csv", sep = ",", encoding='UTF-8', na.strings = c("N/A", "", "NA"), colClasses = col_types, fill = TRUE) 
base2023_08 <- fread("~/Murray France/IA_listings_2023-08_MGP.csv", sep = ",", encoding='UTF-8', na.strings = c("N/A", "", "NA"), colClasses = col_types, fill = TRUE) 
base2023_11 <- fread("~/Murray France/IA_listings_2023-11_MGP.csv", sep = ",", encoding='UTF-8', na.strings = c("N/A", "", "NA"), colClasses = col_types, fill = TRUE) 

base2018 <- base2018 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)
base2020_01 <- base2020_01 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)
base2021_11 <- base2021_11 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)
base2022_05 <- base2022_05 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)
base2022_08 <- base2022_08 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)
base2023_02 <- base2023_02 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)
base2023_05 <- base2023_05 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)
base2022_11 <- base2022_11 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)
base2023_08 <- base2023_08 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)
base2023_11 <- base2023_11 %>% select(-neighbourhood, -requires_license, -host_neighbourhood, -region_id, -region_name, -region_parent_id, -region_parent_name, -region_parent_parent_id, -region_parent_parent_name)

#pour checker les soucis de characters non numeriques dans les variables qui devraient être numériques (souvent des virgules en séparateurs décimal)
non_numeric_rows <- base2023_11[grepl("[^0-9]", base2023_11$review_scores_accuracy), ]

# Remplacer les virgules par des points dans plusieurs colonnes
cols_to_check <- names(base2018)[sapply(base2018, function(col) any(grepl(",", col)))]
cols_to_check <- names(base2020_01)[sapply(base2020_01, function(col) any(grepl(",", col)))]
cols_to_check <- names(base2021_11)[sapply(base2021_11, function(col) any(grepl(",", col)))]
cols_to_check <- names(base2022_05)[sapply(base2022_05, function(col) any(grepl(",", col)))]
cols_to_check <- names(base2022_08)[sapply(base2022_08, function(col) any(grepl(",", col)))]
cols_to_check <- names(base2023_02)[sapply(base2023_02, function(col) any(grepl(",", col)))]
cols_to_check <- names(base2023_05)[sapply(base2023_05, function(col) any(grepl(",", col)))]
cols_to_check <- names(base2022_11)[sapply(base2022_11, function(col) any(grepl(",", col)))]
cols_to_check <- names(base2023_08)[sapply(base2023_08, function(col) any(grepl(",", col)))]
cols_to_check <- names(base2023_11)[sapply(base2023_11, function(col) any(grepl(",", col)))]

cols_to_check

cols_to_convert <- c("maximum_nights_avg_ntm", "review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness", "review_scores_checkin", "review_scores_communication", "reviews_per_month")

# Appliquer la conversion pour chaque colonne de la liste
base2018[, (cols_to_convert) := lapply(.SD, function(x) as.numeric(gsub(",", ".", x))), .SDcols = cols_to_convert]
base2020_01[, (cols_to_convert) := lapply(.SD, function(x) as.numeric(gsub(",", ".", x))), .SDcols = cols_to_convert]
base2021_11[, (cols_to_convert) := lapply(.SD, function(x) as.numeric(gsub(",", ".", x))), .SDcols = cols_to_convert]
base2022_05[, (cols_to_convert) := lapply(.SD, function(x) as.numeric(gsub(",", ".", x))), .SDcols = cols_to_convert]
base2022_08[, (cols_to_convert) := lapply(.SD, function(x) as.numeric(gsub(",", ".", x))), .SDcols = cols_to_convert]
base2023_02[, (cols_to_convert) := lapply(.SD, function(x) as.numeric(gsub(",", ".", x))), .SDcols = cols_to_convert]
base2023_05[, (cols_to_convert) := lapply(.SD, function(x) as.numeric(gsub(",", ".", x))), .SDcols = cols_to_convert]


##Remplacer les valeurs vides par des NA, y compris les " " et "  "
base2018 <- base2018 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped
base2020_01 <- base2020_01 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped
base2021_11 <- base2021_11 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped
base2022_05 <- base2022_05 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped
base2022_08 <- base2022_08 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped
base2023_02 <- base2023_02 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped
base2023_05 <- base2023_05 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped

base2022_11 <- base2022_11 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped

base2023_08 <- base2023_08 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped

base2023_11 <- base2023_11 %>%
  mutate(across(where(is.character), ~ ifelse(trimws(.) == "", NA, .))) #restreindre aux col characters permet de ne pas transformer ma colonne last_scraped


# Liste des dataframes à traiter
bases <- list(
  base2018, 
  base2020_01, 
  base2021_11, 
  base2022_05, 
  base2022_08,
  base2022_11, 
  base2023_02,
  base2023_05,
  base2023_08, 
  base2023_11
)

# Noms des colonnes à convertir en numérique
colonnes_a_convertir <- c(
  "host_listings_count",
  "host_total_listings_count",
  "bathrooms",
  "minimum_nights",
  "minimum_maximum_nights",
  "maximum_maximum_nights",
  "minimum_nights_avg_ntm",
  "maximum_nights_avg_ntm",
  "availability_30",
  "availability_365",
  "number_of_reviews",
  "review_scores_rating",
  "review_scores_cleanliness",
  "review_scores_checkin"
)

noms_bases <- c("base2018",  "base2020_01", "base2021_11", "base2022_05", "base2022_08", "base2022_11", "base2023_02", "base2023_05", "base2023_08", "base2023_11")

# Boucle pour appliquer la transformation des colonnes
for (i in seq_along(bases)) {
  base <- bases[[i]]
  for (col in colonnes_a_convertir) {
    if (col %in% names(base)) {
      base[[col]] <- as.numeric(base[[col]])
    }
  }
  assign(noms_bases[i], base)
}

for (nom in noms_bases) {
  base <- get(nom)
  if ("calendar_last_scraped" %in% names(base)) {
    base$calendar_last_scraped <- as.character(base$calendar_last_scraped)
  }
  assign(nom, base)  # réécrit la base modifiée dans l’environnement global
}

IA_MGP18_23 <- bind_rows(base2018, base2020_01, base2021_11, base2022_05, base2022_08, base2022_11, base2023_02, base2023_05, base2023_08, base2023_11)
str(IA_MGP18_23) #voir s'il y a encore des soucis de formats dans les colonnes, genre "minimum_maximum_nights" en characters

rm(base, base2018, base2020_01, base2021_11, base2022_05, base2022_08, base2022_11, base2023_02, base2023_05, base2023_08, base2023_11)

#Récupérer les id à partir de l'URL (car on a de l'écriture scientifique, dès l'importation...)
IA_MGP18_23$ID_URL <- sub(".*rooms/([0-9]+)$", "\\1", IA_MGP18_23$listing_url)

IA_MGP18_23 <- IA_MGP18_23 %>% select(ID_URL, everything()) #le placer en 1ere place
IA_MGP18_23 <- IA_MGP18_23 %>% select(-id) #supprimer le id initial


#### Création id_uniq dans la base pour disposer d'un id unique melant id_URL et date de scraping ----
IA_MGP18_23 <- IA_MGP18_23 %>% mutate (ID_uniq = str_c(as.character(ID_URL),as.character(last_scraped),sep="_")) %>%
  select(ID_URL, ID_uniq, everything()) #création de l'id unique dans base et le placer en 2

#Création nouveau last_scraped pour avoir 1 ligne par date de scraping quand on a besoin (car scraping échelonné sur plusieurs dates)----

IA_MGP18_23	<- IA_MGP18_23 %>% mutate(Annee_extraction= str_sub(last_scraped,1,4))
IA_MGP18_23	<- IA_MGP18_23 %>% mutate(Mois_extraction= str_sub(last_scraped,6,7))

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (last_scraped_YM = str_c(Annee_extraction, Mois_extraction, sep="-"))

freq(IA_MGP18_23$last_scraped_YM)
IA_MGP18_23$last_scraped_YM <- gsub("2018-05", "2018-04", IA_MGP18_23$last_scraped_YM)
IA_MGP18_23$last_scraped_YM <- gsub("2021-12", "2021-11", IA_MGP18_23$last_scraped_YM)
IA_MGP18_23$last_scraped_YM <- gsub("2022-06", "2022-05", IA_MGP18_23$last_scraped_YM)
IA_MGP18_23$last_scraped_YM <- gsub("2023-03", "2023-02", IA_MGP18_23$last_scraped_YM)
IA_MGP18_23$last_scraped_YM <- gsub("2023-06", "2023-05", IA_MGP18_23$last_scraped_YM)
IA_MGP18_23$last_scraped_YM <- gsub("2022-12", "2022-11", IA_MGP18_23$last_scraped_YM)
IA_MGP18_23$last_scraped_YM <- gsub("2023-12", "2023-11", IA_MGP18_23$last_scraped_YM)

# Trouver la valeur la plus récente de room_type pour chaque id_URL (car parfois une annonce change de room_type) ----
recent_types <- IA_MGP18_23 %>%
  group_by(ID_URL) %>%
  filter(last_scraped == max(last_scraped, na.rm = TRUE)) %>%
  select(ID_URL, room_type_recent = room_type)

# Joindre l'information la plus récente au data frame original
IA_MGP18_23 <- IA_MGP18_23 %>% select(-room_type_recent) 

IA_MGP18_23 <- IA_MGP18_23 %>%
  left_join(recent_types, by = "ID_URL")

freq(IA_MGP18_23$room_type)
freq(IA_MGP18_23$room_type_recent)

rm(recent_types)


#Mini tableau pour créer nos graphiques dans le CHAP 2 - méthodo ----

IA_MGP18_23_HH <- IA_MGP18_23 %>% filter(!room_type=="Hotel room")

freq(IA_MGP18_23_HH$room_type)
names(IA_MGP18_23)

Tab_compa_Ch2 <- IA_MGP18_23_HH %>%
  group_by (last_scraped_YM) %>% 
  summarise (nb_annonces =n(),
             nb_logt_ent = sum(room_type=="Entire home/apt"),
             '%_logt_ent'= nb_logt_ent/nb_annonces*100, 
             nb_ch_priv = sum(room_type=="Private room"),
             '%_ch_priv'= nb_ch_priv/nb_annonces*100, 
             nb_ch_partag = sum(room_type=="Shared room"),
             '%_ch_partag'= nb_ch_partag/nb_annonces*100, 
             nb_host = n_distinct(host_id),
             'nb_ann/nb_host'= nb_annonces/nb_host)

write.table(Tab_compa_Ch2, "~/Murray France/TAB_IA_MGP_18_23_compa_Ch2_10jeux.csv", sep=";", row.names=F)

rm(Tab_compa_Ch2, IA_MGP18_23_HH)

# Travail sur variable "last_searched" ----

IA_MGP18_23_HH <- IA_MGP18_23_HH %>%
  mutate(across(everything(), ~ ifelse(trimws(.) == "", NA, .)))

Tab_LSearch <- IA_MGP18_23_HH %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annonces = n(),
    nb_LS_NA = sum(is.na(last_searched)),
    '%_nb_LS_NA' = sum(is.na(last_searched)) / n() * 100
  )

write.table(Tab_LSearch, "~/Murray France/TAB_IA_MGP_18_23_Source_last_searched.csv", sep=";", row.names=F)

freq(IA_MGP18_23_HH$last_searched)
rm(Tab_LSearch)


###Convertir les dates en date : ----
IA_MGP18_23 <- IA_MGP18_23 %>%
  mutate(last_scraped = as.Date(last_scraped, format = "%Y-%m-%d"),
         last_searched = as.Date(last_searched, format = "%Y-%m-%d"),
         host_since = as.Date(host_since, format = "%Y-%m-%d"),
         calendar_last_scraped = as.Date(calendar_last_scraped, format = "%Y-%m-%d"),
         first_review = as.Date(first_review, format = "%Y-%m-%d"),
         last_review = as.Date(last_review, format = "%Y-%m-%d"))

str(IA_MGP18_23)

#On retire les colonnes ne contenant que des NA ----
IA_MGP18_23_orig <- IA_MGP18_23

IA_MGP18_23 <- IA_MGP18_23 %>%
  select(where(~ !all(is.na(.))))

rm(IA_MGP18_23_orig)

#Créer la tab des NA pour comptabiliser le nb de NA par variable ----
Tab_NA <- IA_MGP18_23 %>%
  group_by(last_scraped_YM) %>%
  summarise_all(~ sum(is.na(.))) %>%
  mutate(total_ann = IA_MGP18_23 %>%
           group_by(last_scraped_YM) %>%
           summarise(total = n()) %>%
           pull(total)) 

write.table(Tab_NA, "~/Murray France/TAB_NA_InsideAirbnb_MGP2.csv", sep=";", row.names=F)

rm(Tab_NA)

##Ajouter les codes EPT ----
codes_EPT <- read.csv2("~/Codes_EPT_MGP.csv", encoding = 'UTF-8') #tableau sous format Commune MGP;Commune code insee;Departement;EPT. Exemple : Ablon-sur-Seine;94001;94;T12 - Grand-Orly Seine Bièvre

IA_MGP18_23 <- IA_MGP18_23 %>% left_join(codes_EPT, by = c("L_CO" = "Commune.MGP"))
IA_MGP18_23 <- IA_MGP18_23 %>% select(-Commune.code.insee, -Departement )

#Creation du tableau permettant de faire le graphique par date et EPT du nb d'annonces (chap 2) ----

IA_MGP18_23HH <- IA_MGP18_23 %>% filter(!room_type =="Hotel room")

TAB_EPT_IAMGP <- IA_MGP18_23HH %>% group_by(EPT) %>%
  summarise(Nb_ann_201804= sum(last_scraped_YM =="2018-04"),
            Nb_ann_202002= sum(last_scraped_YM =="2020-02"),
            Nb_ann_202111= sum(last_scraped_YM =="2021-11"),
            Nb_ann_202205= sum(last_scraped_YM =="2022-05"),
            Nb_ann_202208= sum(last_scraped_YM =="2022-08"),
            Nb_ann_202211= sum(last_scraped_YM =="2022-11"),
            Nb_ann_202302= sum(last_scraped_YM =="2023-02"),
            Nb_ann_202305= sum(last_scraped_YM =="2023-05"),
            Nb_ann_202308= sum(last_scraped_YM =="2023-08"),
            Nb_ann_202311= sum(last_scraped_YM =="2023-11"), 
            Total_EPT = n())

# Ajouter la ligne avec les totaux
total_row <- TAB_EPT_IAMGP %>%
  summarise(
    EPT = "Total",
    Nb_ann_201804 = sum(Nb_ann_201804),
    Nb_ann_202002 = sum(Nb_ann_202002),
    Nb_ann_202111 = sum(Nb_ann_202111),
    Nb_ann_202205 = sum(Nb_ann_202205),
    Nb_ann_202208 = sum(Nb_ann_202208),
    Nb_ann_202211 = sum(Nb_ann_202211),
    Nb_ann_202302 = sum(Nb_ann_202302),
    Nb_ann_202305 = sum(Nb_ann_202305),
    Nb_ann_202308 = sum(Nb_ann_202308),
    Nb_ann_202311 = sum(Nb_ann_202311),
    Total_EPT = n()
  )

# Combiner les deux tables
TAB_EPT_IAMGP <- bind_rows(TAB_EPT_IAMGP, total_row)

write.table(TAB_EPT_IAMGP, "C:/Users/janou/OneDrive - UPEC/SCRAPING/TAB_EPT_IAMGP_Graph_chap3.csv", sep=";", row.names=F)
rm(total_row, TAB_EPT_IAMGP)

#Decoupage des fichiers pour faire les cartes de densité par date (chap3) ----
freq(IA_MGP18_23$last_scraped_YM)
freq(IA_MGP18_23$room_type)

IA_201804HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2018-04") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)
IA_202002HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2020-02") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)
IA_202111HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2021-11") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)
IA_202205HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2022-05") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)
IA_202208HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2022-08") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)
IA_202211HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2022-11") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)
IA_202302HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2023-02") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)
IA_202305HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2023-05") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)
IA_202308HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2023-08") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)
IA_202311HH <- IA_MGP18_23 %>% filter(last_scraped_YM =="2023-11") %>% filter(!room_type =="Hotel room") %>%
  select(ID_URL, longitude, latitude, room_type)

fwrite(IA_201804HH, "~/carto_densite/IA_MGP_lis_2018-04HH_carto.csv", sep=",", row.names=F)
fwrite(IA_202002HH, "~/carto_densite/IA_MGP_lis_2020-02HH_carto.csv", sep=",", row.names=F)
fwrite(IA_202111HH, "~/carto_densite/IA_MGP_lis_2021-11HH_carto.csv", sep=",", row.names=F)
fwrite(IA_202205HH, "~/carto_densite/IA_MGP_lis_2022-05HH_carto.csv", sep=",", row.names=F)
fwrite(IA_202208HH, "~/carto_densite/IA_MGP_lis_2022-08HH_carto.csv", sep=",", row.names=F)
fwrite(IA_202211HH, "~/carto_densite/IA_MGP_lis_2022-11HH_carto.csv", sep=",", row.names=F)
fwrite(IA_202302HH, "~/carto_densite/IA_MGP_lis_2023-02HH_carto.csv", sep=",", row.names=F)
fwrite(IA_202305HH, "~/carto_densite/IA_MGP_lis_2023-05HH_carto.csv", sep=",", row.names=F)
fwrite(IA_202308HH, "~/carto_densite/IA_MGP_lis_2023-08HH_carto.csv", sep=",", row.names=F)
fwrite(IA_202311HH, "~/carto_densite/IA_MGP_lis_2023-11HH_carto.csv", sep=",", row.names=F)


###Insertion des codes arrondissements de Paris dans la base et les IRIS, de sorte à avoir L_CO qui aurait que les comm avec Paris en mode unique + C_POSTAL idem. Mais créer un C_POSTAL dans lequel il y aurait les arr de Paris ----

MGP <- st_read("~/Couches fond/JR_Iris MGP_dapresIRIS_Com_joint/JR_Iris MGP_dI_C_j.shp") #Ici fichier MGP découpage à l'iris mais avec aussi code commune
names(MGP)

MGP <- MGP %>% select(-OBJECTID, -N_SQ_IR, -SHAPE_Leng, -SHAPE_Area)

IA_ex <- st_read("~/sf_annonces/sf_annonces.shp") # utile simplement pour récupérer son SCR WGS84

lambert_CRS <- st_crs(MGP)
WGS84_CRS <- st_crs(IA_ex)

IA_MGP18_23_geo <- st_as_sf(IA_MGP18_23, coords = c("longitude", "latitude"), crs = WGS84_CRS, remove = FALSE) # On créé une couche SHP à partir des latitudes et longitudes contenues dans le fichier csv

IA_MGP18_23_geo <- st_transform(IA_MGP18_23_geo, 2154) # ici on transforme le SCR de la couche SHP pour qu'il soit en lambert93 comme nos couches de formes MGP/IDF
MGP <- st_transform(MGP, 2154) # ici on transforme le SCR de la couche SHP pour qu'il soit en lambert93 comme nos couches de formes MGP/IDF

IA_MGP18_23_geo <- st_intersection(IA_MGP18_23_geo, MGP) # ici on fait les intersections entre l'IDF et le fichier IA France pour ne garder que les annonces en IDF

rm(MGP, IA_ex, lambert_CRS, WGS84_CRS)

#Création d'un tableau pour créer une carte par commune de la MGP avec le nb d'annonces (hors hotels) et l'évol des annonces entre 2018 et 2023

TAB_COMMUNES_IAMGP <- IA_MGP18_23_geo %>%
  filter(!room_type =="Hotel room") %>%
  group_by(C_COINSEE) %>%
  summarise(Nb_ann_201804= sum(last_scraped_YM =="2018-04"),
            Nb_ann_202002= sum(last_scraped_YM =="2020-02"),
            Nb_ann_202111= sum(last_scraped_YM =="2021-11"),
            Nb_ann_202205= sum(last_scraped_YM =="2022-05"),
            Nb_ann_202208= sum(last_scraped_YM =="2022-08"),
            Nb_ann_202211= sum(last_scraped_YM =="2022-11"),
            Nb_ann_202302= sum(last_scraped_YM =="2023-02"),
            Nb_ann_202305= sum(last_scraped_YM =="2023-05"),
            Nb_ann_202308= sum(last_scraped_YM =="2023-08"),
            Nb_ann_202311= sum(last_scraped_YM =="2023-11"), 
            Total_EPT = n())

st_write(TAB_COMMUNES_IAMGP, "~/Murray France/TAB_COMMUNES_IAMGP_carto2.csv")

st_write(TAB_COMMUNES_IAMGP, "~/Murray France/TAB_COMMUNES_IAMGP_carto2.csv")

names(IA_MGP18_23_geo)
IA_MGP18_23_geo <- IA_MGP18_23_geo %>% select(-C_COINSEE.1)
IA_MGP18_23_geo <- IA_MGP18_23_geo %>% select(-C_POSTAL)

IA_MGP18_23 <- IA_MGP18_23_geo %>% as.data.frame() %>% select(-geometry)

rm(TAB_COMMUNES_IAMGP)

####Volatilité des annonces (chap 3)----

IA_MGP18_23 <- IA_MGP18_23 %>% arrange(last_scraped)

base_ann_uniq_IA <- IA_MGP18_23 %>%
  group_by (ID_URL) %>% 
  summarise (nb_observation =n(),
             room_type = last(room_type),
             prem_obser_calc = first(last_scraped),
             der_obser_calc = last(last_scraped),
             host_since = first(host_since),
             first_review = first(first_review),
             first_review_count = first(number_of_reviews),
             last_review_count = last(number_of_reviews),
             lat = first(latitude),
             lon = first(longitude),
             EPT =first(EPT),
             J_C_COINSE = first(J_C_COINSE),
             C_IR = first(C_IR),
             J_L_CO =first(J_L_CO),
             L_CO = first(L_CO))

base_ann_uniq_IA <- base_ann_uniq_IA %>%
  mutate(diff_PDobs_month = time_length(interval(start = ymd(prem_obser_calc), end = ymd(der_obser_calc)), unit = "months"),
         diff_PDobs_year = time_length(interval(start = ymd(prem_obser_calc), end = ymd(der_obser_calc)), unit = "years"),
         diff_PobsFrev_y = time_length(interval(start = ymd(prem_obser_calc), end = ymd(first_review)), unit = "years"),
         Pobs_post_Frev = ifelse(diff_PobsFrev_y<0, T, F),
         diff_PDobs_month_per_nb_obs = diff_PDobs_month/nb_observation,
         calc_Frev_2019mini = time_length(interval(start = ymd("2018-04-23"), end = ymd(first_review)), unit = "months"),
         calc_Frev_2019mini_TF = case_when(calc_Frev_2019mini<=0 ~ "2018-04-23",
                                           calc_Frev_2019mini>0 ~ as.character(first_review),
                                           TRUE ~ as.character(prem_obser_calc)))

base_ann_uniq_IA <- base_ann_uniq_IA %>%
  mutate(calc_orig = case_when(diff_PobsFrev_y<=0 ~ as.character(first_review),
                               diff_PobsFrev_y>0 ~ as.character(prem_obser_calc),
                               TRUE ~ as.character(prem_obser_calc)))

base_ann_uniq_IA <- base_ann_uniq_IA %>%
  mutate(duree_activ_y = time_length(interval(start = ymd(calc_orig), end = ymd(der_obser_calc)), unit = "years"),
         duree_activ_mth = time_length(interval(start = ymd(calc_orig), end = ymd(der_obser_calc)), unit = "months"))

base_ann_uniq_IA <- base_ann_uniq_IA %>%
  mutate(calc_Frev_2019mini_TF = as.Date(calc_Frev_2019mini_TF, format = "%Y-%m-%d"),
         calc_orig = as.Date(calc_orig, format = "%Y-%m-%d"))
         
base_ann_uniqHH_IA <- base_ann_uniq_IA %>% filter(!room_type =="Hotel room")

options(scipen=999)
fwrite(base_ann_uniqHH_IA, "~/Tab_IA_MGPHH_ann_uniq_volatilite.csv", sep=";", row.names=F)
fwrite(base_ann_uniq_IA, "~/Tab_IA_MGP_ann_uniq_volatilite.csv", sep=";", row.names=F)

#Jointure de "duree_activ_y" et qq autres colonnes à la baseHH
names(base_ann_uniq_IA)

IA_MGP18_23 <- IA_MGP18_23 %>%
  left_join (base_ann_uniq_IA[,c(1,2,4, 5, 8, 9, 17, 18, 21, 24, 25, 26)], by = "ID_URL")

IA_MGP18_23HH <- IA_MGP18_23 %>% filter(!room_type =="Hotel room")

##Comparaison annonces Altri (autres source de données contenant les annonces Airbnb) et InsideAirbnb ----
#Comparaison des 2 "base_ann_uniq" Altri/IA - donc il faut avoir fait de même dans Altri (chap 2)

ann_comm_IA_Altri <- base_ann_uniqHH_IA %>%
  inner_join(base_ann_uniqHH, by = "ID_URL") %>%
  mutate(
    nb_observation = coalesce(nb_observation.x, nb_observation.y),
    room_type = coalesce(room_type.x, room_type.y),
    prem_obser_calc = coalesce(prem_obser_calc.x, prem_obser_calc.y),
    der_obser_calc = coalesce(der_obser_calc.x, der_obser_calc.y),
    first_review = coalesce(first_review.x, first_review.y),
    first_review_count = coalesce(first_review_count.x, first_review_count.y),
    last_review_count = coalesce(last_review_count.x, last_review_count.y),
    lat = coalesce(lat.x, lat.y),
    lon = coalesce(lon.x, lon.y),
    EPT = coalesce(EPT.x, EPT.y),
    J_C_COINSE = coalesce(J_C_COINSE.x, J_C_COINSE.y),
    C_IR = coalesce(C_IR.x, C_IR.y),
    J_L_CO = coalesce(J_L_CO.x, J_L_CO.y),
    L_CO = coalesce(L_CO.x, L_CO.y),
    diff_PDobs_month = coalesce(diff_PDobs_month.x, diff_PDobs_month.y),
    diff_PDobs_year = coalesce(diff_PDobs_year.x, diff_PDobs_year.y),
    diff_PobsFrev_y = coalesce(diff_PobsFrev_y.x, diff_PobsFrev_y.y),
    Pobs_post_Frev = coalesce(Pobs_post_Frev.x, Pobs_post_Frev.y),
    diff_PDobs_month_per_nb_obs = coalesce(diff_PDobs_month_per_nb_obs.x, diff_PDobs_month_per_nb_obs.y),
    calc_Frev_2019mini = coalesce(calc_Frev_2019mini.x, calc_Frev_2019mini.y),
    calc_Frev_2019mini_TF = coalesce(calc_Frev_2019mini_TF.x, calc_Frev_2019mini_TF.y),
    calc_orig = coalesce(calc_orig.x, calc_orig.y),
    duree_activ_y = coalesce(duree_activ_y.x, duree_activ_y.y),
    duree_activ_mth = coalesce(duree_activ_mth.x, duree_activ_mth.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

total_ann_obs_IA_Altri <- base_ann_uniqHH_IA %>%
  full_join(base_ann_uniqHH, by = "ID_URL") %>%
  mutate(
    nb_observation = coalesce(nb_observation.x, nb_observation.y),
    room_type = coalesce(room_type.x, room_type.y),
    prem_obser_calc = coalesce(prem_obser_calc.x, prem_obser_calc.y),
    der_obser_calc = coalesce(der_obser_calc.x, der_obser_calc.y),
    first_review = coalesce(first_review.x, first_review.y),
    first_review_count = coalesce(first_review_count.x, first_review_count.y),
    last_review_count = coalesce(last_review_count.x, last_review_count.y),
    lat = coalesce(lat.x, lat.y),
    lon = coalesce(lon.x, lon.y),
    EPT = coalesce(EPT.x, EPT.y),
    J_C_COINSE = coalesce(J_C_COINSE.x, J_C_COINSE.y),
    C_IR = coalesce(C_IR.x, C_IR.y),
    J_L_CO = coalesce(J_L_CO.x, J_L_CO.y),
    L_CO = coalesce(L_CO.x, L_CO.y),
    diff_PDobs_month = coalesce(diff_PDobs_month.x, diff_PDobs_month.y),
    diff_PDobs_year = coalesce(diff_PDobs_year.x, diff_PDobs_year.y),
    diff_PobsFrev_y = coalesce(diff_PobsFrev_y.x, diff_PobsFrev_y.y),
    Pobs_post_Frev = coalesce(Pobs_post_Frev.x, Pobs_post_Frev.y),
    diff_PDobs_month_per_nb_obs = coalesce(diff_PDobs_month_per_nb_obs.x, diff_PDobs_month_per_nb_obs.y),
    calc_Frev_2019mini = coalesce(calc_Frev_2019mini.x, calc_Frev_2019mini.y),
    calc_Frev_2019mini_TF = coalesce(calc_Frev_2019mini_TF.x, calc_Frev_2019mini_TF.y),
    calc_orig = coalesce(calc_orig.x, calc_orig.y),
    duree_activ_y = coalesce(duree_activ_y.x, duree_activ_y.y),
    duree_activ_mth = coalesce(duree_activ_mth.x, duree_activ_mth.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

options(scipen=999)
fwrite(ann_comm_IA_Altri, "~/IA_Altri_MGPHH_ann_uniq_commun.csv", sep=";", row.names=F)
fwrite(total_ann_obs_IA_Altri, "~/IA_Altri_MGPHH_ann_uniq_tot.csv", sep=";", row.names=F)

ann_Altri_HIA <- base_ann_uniqHH %>% anti_join (base_ann_uniqHH_IA, by = "ID_URL")
ann_IA_HAltri <- base_ann_uniqHH_IA %>% anti_join (base_ann_uniqHH, by = "ID_URL")

freq(ann_Altri_HIA$L_CO)
freq(ann_IA_HAltri$L_CO)
freq(ann_Altri_HIA$prem_obser_calc)
freq(ann_IA_HAltri$prem_obser_calc)

###Travail sur les recouvrements entre Altri et InsideAirbnb
ex_aubervill <- ann_IA_HAltri %>% filter(L_CO=="Aubervilliers")
freq(ex_aubervill$prem_obser_calc)

ex_aubervill_Altri <- ann_Altri_HIA %>% filter(L_CO=="Aubervilliers")
freq(ex_aubervill_Altri$prem_obser_calc)

ex_aubervill_tot <- total_ann_obs_IA_Altri %>% filter(L_CO=="Aubervilliers")
  
freq(ann_Altri_HIA$nb_observation)
freq(ann_IA_HAltri$nb_observation)
freq(total_ann_obs_IA_Altri$nb_observation)

rm(ex_aubervill, ex_aubervill_Altri, ex_aubervill_tot)

#2020-01
Ex2020_IA <- IA_MGP18_23HH %>% filter(last_scraped_YM=="2020-02")
Ex2020_Altri <- baseHH %>% filter(last_scraped=="2020-01-22")

ann_comm_2020_IA_Altri <- Ex2020_IA %>%
  inner_join(Ex2020_Altri, by = "ID_URL")

ann_tot_2020_IA_Altri <- Ex2020_IA %>%
  full_join(Ex2020_Altri, by = "ID_URL")

#Test de vérif en prenant les LONG LAT : 
Ex2020_IA <- Ex2020_IA %>% mutate(XY = paste(latitude, "_", longitude))
Ex2020_Altri <- Ex2020_Altri %>% mutate(XY = paste(Geo_wgs_lat, "_", Geo_wgs_lon))

ann_commXY_2020_IA_Altri <- Ex2020_IA %>%
  inner_join(Ex2020_Altri, by = "XY") 

ann_commXY_2020_IA_Altri_L <- ann_commXY_2020_IA_Altri %>%
  select(ID_URL.x, ID_URL.y, description, Description_longue, name, Name, host_name, Host_name, host_since, Host_since, room_type, Air_room_type)

ann_commXY_2020_IA_Altri_L <- ann_commXY_2020_IA_Altri_L %>%
  filter(ID_URL.x != ID_URL.y)

rm(ann_commXY_2020_IA_Altri,ann_commXY_2020_IA_Altri_L)

#2021-11
Ex2021_IA <- IA_MGP18_23HH %>% filter(last_scraped_YM=="2021-11")
Ex2021_Altri <- baseHH %>% filter(last_scraped=="2021-11-15")

ann_comm_2021_IA_Altri <- Ex2021_IA %>%
  inner_join(Ex2021_Altri, by = "ID_URL")

ann_tot_2021_IA_Altri <- Ex2021_IA %>%
  full_join(Ex2021_Altri, by = "ID_URL")

#2022-05
Ex202205_IA <- IA_MGP18_23HH %>% filter(last_scraped_YM=="2022-05")
Ex202205_Altri <- baseHH %>% filter(last_scraped=="2022-05-23")

ann_comm_202205_IA_Altri <- Ex202205_IA %>%
  inner_join(Ex202205_Altri, by = "ID_URL")

ann_tot_202205_IA_Altri <- Ex202205_IA %>%
  full_join(Ex202205_Altri, by = "ID_URL")

#2022-08
Ex202208_IA <- IA_MGP18_23HH %>% filter(last_scraped_YM=="2022-08")
Ex202208_Altri <- baseHH %>% filter(last_scraped=="2022-08-16")

ann_comm_202208_IA_Altri <- Ex202208_IA %>%
  inner_join(Ex202208_Altri, by = "ID_URL")

ann_tot_202208_IA_Altri <- Ex202208_IA %>%
  full_join(Ex202208_Altri, by = "ID_URL")


#2022-11
Ex202211_IA <- IA_MGP18_23HH %>% filter(last_scraped_YM=="2022-11")
Ex202211_Altri <- baseHH %>% filter(last_scraped=="2022-11-08")

ann_comm_202211_IA_Altri <- Ex202211_IA %>%
  inner_join(Ex202211_Altri, by = "ID_URL") %>%
  select(ID_URL) %>%
  mutate(orig_scrap = "IA_Altri")

ann_tot_202211_IA_Altri <- Ex202211_IA %>%
  full_join(Ex202211_Altri, by = "ID_URL")

#2023-02
Ex202302_IA <- IA_MGP18_23HH %>% filter(last_scraped_YM=="2023-02")
Ex202302_Altri <- baseHH %>% filter(last_scraped=="2023-02-28")

ann_comm_202302_IA_Altri <- Ex202302_IA %>%
  inner_join(Ex202302_Altri, by = "ID_URL")

ann_tot_202302_IA_Altri <- Ex202302_IA %>%
  full_join(Ex202302_Altri, by = "ID_URL")

#Test de vérif en prenant les LONG LAT : 
Ex202302_IA <- Ex202302_IA %>% mutate(XY = paste(latitude, "_", longitude))
Ex202302_Altri <- Ex202302_Altri %>% mutate(XY = paste(Geo_wgs_lat, "_", Geo_wgs_lon))

ann_commXY_202302_IA_Altri <- Ex202302_IA %>%
  inner_join(Ex202302_Altri, by = "XY") 

ann_commXY_202302_IA_Altri_L <- ann_commXY_202302_IA_Altri %>%
  select(ID_URL.x, ID_URL.y, description, Description_longue, name, Name, host_name, Host_name, host_since, Host_since, room_type, Air_room_type)

ann_commXY_202302_IA_Altri_L <- ann_commXY_202302_IA_Altri_L %>%
  filter(ID_URL.x != ID_URL.y)

ann_commXY_202302_IA_Altri_L_uniX <- ann_commXY_202302_IA_Altri_L %>%
  distinct(ID_URL.x, .keep_all = TRUE)
ann_commXY_202302_IA_Altri_L_uniY <- ann_commXY_202302_IA_Altri_L %>%
  distinct(ID_URL.y, .keep_all = TRUE)

#2023-05
Ex202305_IA <- IA_MGP18_23HH %>% filter(last_scraped_YM=="2023-05")
Ex202305_Altri <- baseHH %>% filter(last_scraped=="2023-05-23")

ann_comm_202305_IA_Altri <- Ex202305_IA %>%
  inner_join(Ex202305_Altri, by = "ID_URL")

ann_tot_202305_IA_Altri <- Ex202305_IA %>%
  full_join(Ex202305_Altri, by = "ID_URL")

#2023-08
Ex202308_IA <- IA_MGP18_23HH %>% filter(last_scraped_YM=="2023-08")
         
Ex202308_Altri <- baseHH %>% filter(last_scraped=="2023-08-15")

ann_comm_202308_IA_Altri <- Ex202308_IA %>%
  inner_join(Ex202308_Altri, by = "ID_URL")

ann_tot_202308_IA_Altri <- Ex202308_IA %>%
  full_join(Ex202308_Altri, by = "ID_URL")

#2023-11
Ex202311_IA <- IA_MGP18_23HH %>% filter(last_scraped_YM=="2023-11")
Ex202311_Altri <- baseHH %>% filter(last_scraped=="2023-11-07")

ann_comm_202311_IA_Altri <- Ex202311_IA %>%
  inner_join(Ex202311_Altri, by = c("ID_URL"))

ann_tot_202311_IA_Altri <- Ex202311_IA %>%
  full_join(Ex202311_Altri,by = c("ID_URL"))

rm(Ex202302_IA, Ex202302_Altri, ann_commXY_202302_IA_Altri, ann_commXY_202302_IA_Altri_L, Ex202211_IA, Ex202211_Altri, ann_comm_202211_IA_Altri, ann_tot_2021_IA_Altri)
rm(Ex2020_IA, Ex2020_Altri, ann_comm_2020_IA_Altri, ann_tot_2020_IA_Altri)
rm(ann_tot_202311_IA_Altri, ann_comm_202311_IA_Altri, ann_tot_202308_IA_Altri, ann_comm_202308_IA_Altri, Ex202311_IA, Ex202311_Altri, Ex202308_IA, Ex202308_Altri, Ex202305_IA, Ex202305_Altri, ann_comm_202305_IA_Altri, ann_tot_202305_IA_Altri)

####Etude des annonces non communes aux deux sources Altri et InsideAirbnb
ann_Altri_HIA <- ann_Altri_HIA %>%
  mutate(orig_scrap = "Altri")

ann_Altri_HIA <- ann_Altri_HIA %>%
  rename(host_since_Altri = host_since)

ann_IA_HAltri <- ann_IA_HAltri %>%
  mutate(orig_scrap = "IA", 
         prem_obser_calc_ym = format(as.Date(prem_obser_calc), "%Y-%m"))

annHH_non_comm_IA_Altri <- bind_rows(ann_Altri_HIA, ann_IA_HAltri)

freq(annHH_non_comm_IA_Altri$orig_scrap)
freq(annHH_non_comm_IA_Altri$prem_obser_calc_ym)

options(scipen=999)
fwrite(annHH_non_comm_IA_Altri, "~/IA_Altri_MGPHH_ann_uniq_noncommun.csv", sep=";", row.names=F)

annHH_non_comm_IA_Altri_H2018 <- annHH_non_comm_IA_Altri %>%
  filter(!prem_obser_calc_ym %in% c("2018-04", "2018-05"))

annHH_non_comm_IA_Altri_H2018fin23 <- annHH_non_comm_IA_Altri_H2018 %>%
  filter(!prem_obser_calc %in% c("2023-11-21", "2023-11-28","2023-12-05","2023-12-15","2023-12-20","2023-12-27"))

freq(annHH_non_comm_IA_Altri_H2018fin23$prem_obser_calc)

test_Altri_nb_obs <- annHH_non_comm_IA_Altri_H2018fin23 %>%
  filter(orig_scrap=="Altri")

test_IA_nb_obs <- annHH_non_comm_IA_Altri_H2018fin23 %>%
  filter(orig_scrap=="IA")

freq(test_Altri_nb_obs$nb_observation)  
freq(test_IA_nb_obs$nb_observation)  

rm(annHH_non_comm_IA_Altri_H2018fin23,annHH_non_comm_IA_Altri_H2018, annHH_non_comm_IA_Altri)
rm(ann_Altri_HIA, ann_IA_HAltri,total_ann_obs_IA_Altri, ann_comm_IA_Altri, test_Altri_nb_obs, test_IA_nb_obs)


#Comparaison pour Paris pour nov 2023 pour IA-Paris et IA-MGP ----
IA_Paris <- fread("~/IA_Paris/IA_listings_com_iris_2015_2023.csv", #ce fichier rassemble l'ensemble des jeux de donnée InsideAirbnb pour Paris que nous avons fusionnés
                     encoding = "UTF-8",
                     na.strings = c("N/A", "", "NA"),
                     colClasses = col_types)

IA_Paris$id <- as.character(IA_Paris$id)

IA_Paris_202311 <- IA_Paris %>% filter(last_scraped %in% c("2023-11-10", "2023-11-11", "2023-11-24"))

ann_tot_202311_IA_Altri_IA2_Paris <- ann_tot_202311_IA_Altri_Paris %>%
  full_join(IA_Paris_202311, by = c("ID_URL" = "id"))

ann_comm_202311_IAMGP_IAP_Paris <- Ex202311_IA_Paris %>% 
  inner_join(IA_Paris_202311, by = c("ID_URL" = "id"))

ann_tot_202311_IAMGP_IAP_Paris <- Ex202311_IA_Paris %>% 
  full_join(IA_Paris_202311, by = c("ID_URL" = "id"))

rm(IA_Paris_202311, ann_tot_202311_IA_Altri_IA2_Paris, ann_comm_202311_IAMGP_IAP_Paris, ann_tot_202311_IAMGP_IAP_Paris)

###---- Nettoyage de la variable "license" ----

freq(IA_MGP18_23$license)

IA_MGP18_23$license_Nett <- str_extract_all(IA_MGP18_23$license, "\\b\\d{13}\\b")
IA_MGP18_23$license_Nett <- sapply(IA_MGP18_23$license_Nett, paste, collapse = ", ")

IA_MGP18_23 <- IA_MGP18_23 %>%
  mutate(
    license_Nett = if_else(
      license_Nett == "",   # Condition : si la modalité est une chaîne vide
      NA_character_,       # Remplacer par NA
      license_Nett          # Sinon, conserver la valeur actuelle
    )
  )

IA_MGP18_23 <- IA_MGP18_23 %>%
  mutate(
    license = if_else(
      license == "",   # Condition : si la modalité est une chaîne vide
      NA_character_,       # Remplacer par NA
      license          # Sinon, conserver la valeur actuelle
    )
  )

## Tableau nombre d'ann par commune par room_type ----

IA_MGP18_23HH <- IA_MGP18_23 %>% filter(!room_type =="Hotel room")

count_table_list <- IA_MGP18_23HH %>%
  group_by(J_L_CO, last_scraped_YM) %>%
  summarise(count = n(), .groups = "drop")

count_table_list <- count_table_list %>% arrange(last_scraped_YM)

pivot_table_list <- count_table_list %>%
  pivot_wider(names_from = last_scraped_YM, values_from = count, values_fill = 0)

#que pour les annonces room-type "logements entiers" (LE)
count_table_listLE <- IA_MGP18_23HH %>% filter(room_type=="Entire home/apt") %>%
  group_by(J_L_CO, last_scraped_YM) %>%
  summarise(count = n(), .groups = "drop")

count_table_listLE <- count_table_listLE %>% arrange(last_scraped_YM)

pivot_table_listLE <- count_table_listLE %>%
  pivot_wider(names_from = last_scraped_YM, values_from = count, values_fill = 0, names_prefix = "LE_")

#que pour les PR
count_table_listPR <- IA_MGP18_23HH %>% filter(room_type=="Private room") %>%
  group_by(J_L_CO, last_scraped_YM) %>%
  summarise(count = n(), .groups = "drop")

count_table_listPR <- count_table_listPR %>% arrange(last_scraped_YM)

pivot_table_listPR <- count_table_listPR %>%
  pivot_wider(names_from = last_scraped_YM, values_from = count, values_fill = 0, names_prefix = "PR_")

#que pour les SR
count_table_listSR <- IA_MGP18_23HH %>% filter(room_type=="Shared room") %>%
  group_by(J_L_CO, last_scraped_YM) %>%
  summarise(count = n(), .groups = "drop")

count_table_listSR <- count_table_listSR %>% arrange(last_scraped_YM)

pivot_table_listSR <- count_table_listSR %>%
  pivot_wider(names_from = last_scraped_YM, values_from = count, values_fill = 0, names_prefix = "SR_")

Tab_list_IA_MGP_roomHH <- pivot_table_list %>%
  left_join(pivot_table_listLE, by = "J_L_CO") %>%
  left_join(pivot_table_listPR, by = "J_L_CO") %>%
  left_join(pivot_table_listSR, by = "J_L_CO")

write.table(Tab_list_IA_MGP_roomHH, "~/Murray France/Tab_lis_IA_MGP_roomHH.csv", sep=",", dec=".", row.names=F)

rm(pivot_table_list, pivot_table_listLE, pivot_table_listPR, pivot_table_listSR, count_table_listLE, count_table_list, count_table_listPR, count_table_listSR)

## Preparation donnees en vu creation d'une carte avec le nombre moyens de commentaires au cours des LTM ----
#LTM = last twelve months

names(IA_MGP18_23HH)
freq(IA_MGP18_23HH$last_scraped_YM)

Tab_IAMGPHH_revLTM_com <- IA_MGP18_23HH %>% filter(room_type == "Entire home/apt") %>%
  group_by (J_L_CO, last_scraped_YM) %>% 
  summarise (rev_moy_LE_ltm =mean(number_of_reviews_ltm) , .groups = "drop")

Tab_IAMGPHH_revLTM_com <- Tab_IAMGPHH_revLTM_com %>% arrange(last_scraped_YM)

pivot_Tab_IAMGPHH_revLTM_com <- Tab_IAMGPHH_revLTM_com %>%
  pivot_wider(names_from = last_scraped_YM, values_from = rev_moy_LE_ltm, values_fill = 0)

Tab_IAMGPHH_nbLE_com <- IA_MGP18_23HH %>% filter(room_type == "Entire home/apt") %>%
  group_by (J_L_CO, last_scraped_YM) %>% 
  summarise (nb_LE =n() , .groups = "drop")

Tab_IAMGPHH_nbLE_com <- Tab_IAMGPHH_nbLE_com %>% arrange(last_scraped_YM)

pivot_Tab_IAMGPHH_nbLE_com <- Tab_IAMGPHH_nbLE_com %>%
  pivot_wider(names_from = last_scraped_YM, values_from = nb_LE, values_fill = 0, names_prefix = "NbLE_")

Tab_IAMGPHH_revLTM_com <- pivot_Tab_IAMGPHH_revLTM_com %>%
  left_join(pivot_Tab_IAMGPHH_nbLE_com, by = "J_L_CO")

write.table(Tab_IAMGPHH_revLTM_com, "~/Murray France/Tab_IA_MGP_rev_LTM_com.csv", sep=",", dec=".", row.names=F) #carte effectuée dans QGIS

#Par iris
Tab_IAMGPHH_revLTM_iris <- IA_MGP18_23HH %>% filter(room_type == "Entire home/apt") %>%
  group_by (C_IR, last_scraped_YM) %>% 
  summarise (rev_moy_LE_ltm =mean(number_of_reviews_ltm) , .groups = "drop")

Tab_IAMGPHH_revLTM_iris <- Tab_IAMGPHH_revLTM_iris %>% arrange(last_scraped_YM)

pivot_Tab_IAMGPHH_revLTM_iris <- Tab_IAMGPHH_revLTM_iris %>%
  pivot_wider(names_from = last_scraped_YM, values_from = rev_moy_LE_ltm, values_fill = 0)

Tab_IAMGPHH_nbLE_iris <- IA_MGP18_23HH %>% filter(room_type == "Entire home/apt") %>%
  group_by (C_IR, last_scraped_YM) %>% 
  summarise (nb_LE =n() , .groups = "drop")

Tab_IAMGPHH_nbLE_iris <- Tab_IAMGPHH_nbLE_iris %>% arrange(last_scraped_YM)

pivot_Tab_IAMGPHH_nbLE_iris <- Tab_IAMGPHH_nbLE_iris %>%
  pivot_wider(names_from = last_scraped_YM, values_from = nb_LE, values_fill = 0, names_prefix = "NbLE_")

Tab_IAMGPHH_revLTM_iris <- pivot_Tab_IAMGPHH_revLTM_iris %>%
  left_join(pivot_Tab_IAMGPHH_nbLE_iris, by = "C_IR")

write.table(Tab_IAMGPHH_revLTM_iris, "~/Murray France/Tab_IA_MGP_rev_LTM_iris.csv", sep=",", dec=".", row.names=F) #carte effectuée dans QGIS

rm(pivot_Tab_IAMGPHH_nbLE_com, pivot_Tab_IAMGPHH_nbLE_iris, pivot_Tab_IAMGPHH_revLTM_com, pivot_Tab_IAMGPHH_revLTM_iris, Tab_IAMGPHH_nbLE_com, Tab_IAMGPHH_revLTM_com)


####Travail variable "calendar_updated" ----
#En fait problème la variable calendar_updated n'est dispo que dans les 2 premiers jeux IA MGP...
freq(IA_MGP18_23$calendar_updated)

IA_MGP18_23$calendar_updated_cat <- as.character(IA_MGP18_23$calendar_updated)

IA_MGP18_23$calendar_updated_cat[IA_MGP18_23$calendar_updated_cat %in% c("4 months ago", "5 months ago", "6 months ago")] <- "Plus de 3 mois à 6 mois"
IA_MGP18_23$calendar_updated_cat[IA_MGP18_23$calendar_updated_cat %in% c("today","yesterday")] <- "Aujourd'hui ou hier"
IA_MGP18_23$calendar_updated_cat[IA_MGP18_23$calendar_updated_cat %in% c( "7 months ago", "8 months ago", "9 months ago", "10 months ago","11 months ago", "12 months ago")] <- "Plus de 6 mois à 1 an"
IA_MGP18_23$calendar_updated_cat[IA_MGP18_23$calendar_updated_cat %in% c("never")] <- "Plus de 1 an ou jamais"
IA_MGP18_23$calendar_updated_cat[IA_MGP18_23$calendar_updated_cat %in% c("2 days ago", "3 days ago","4 days ago","5 days ago", "6 days ago","7 days ago", "1 week ago", "a week ago"  )] <- "De 2 à 7 jours"
IA_MGP18_23$calendar_updated_cat[IA_MGP18_23$calendar_updated_cat %in% c("2 months ago", "3 months ago", "5 weeks ago", "6 weeks ago", "7 weeks ago" )] <- "Plus de 1 mois à 3 mois"
IA_MGP18_23$calendar_updated_cat[IA_MGP18_23$calendar_updated_cat %in% c("2 weeks ago", "3 weeks ago", "4 weeks ago" )] <- "Plus d'une semaine à 1 mois"

freq(IA_MGP18_23$calendar_updated_cat)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (calendar_updated_cat = ifelse(str_length(IA_MGP18_23$calendar_updated_cat)==13, "Plus de 1 an ou jamais", calendar_updated_cat))

IA_MGP18_23$calendar_updated_cat <- as.factor(IA_MGP18_23$calendar_updated_cat)

levels(IA_MGP18_23$calendar_updated_cat) <- c("Aujourd'hui ou hier","De 2 à 7 jours","Plus d'une semaine à 1 mois","Plus de 1 mois à 3 mois", "Plus de 3 mois à 6 mois", "Plus de 6 mois à moins de 1 an" ,"Plus de 1 an ou jamais" )

levels(IA_MGP18_23$calendar_updated_cat)

#Travail sur host_acceptance et host_response_rate ----

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (host_acceptance_rateNETT = str_replace (host_acceptance_rate, fixed("%"), ""))
IA_MGP18_23$host_acceptance_rateNETT <- as.numeric(IA_MGP18_23$host_acceptance_rateNETT)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (host_response_rateNETT = str_replace (host_response_rate, fixed("%"), ""))
IA_MGP18_23$host_response_rateNETT <- as.numeric(IA_MGP18_23$host_response_rateNETT)

#Nettoyage price / monthly_price / weekly_price / fee et autres qui contenaient des $ ----

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (price = str_replace (price, fixed("$"), "")) %>%
  mutate(price=str_replace(price, fixed(","), ""))
IA_MGP18_23$price <- as.numeric(IA_MGP18_23$price)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (weekly_price = str_replace (weekly_price, fixed("$"), "")) %>%
  mutate(weekly_price=str_replace(weekly_price, fixed(","), ""))
IA_MGP18_23$weekly_price <- as.numeric(IA_MGP18_23$weekly_price)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (monthly_price = str_replace (monthly_price, fixed("$"), "")) %>%
  mutate(monthly_price=str_replace(monthly_price, fixed(","), ""))
IA_MGP18_23$monthly_price <- as.numeric(IA_MGP18_23$monthly_price)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (security_deposit = str_replace (security_deposit, fixed("$"), "")) %>%
  mutate(security_deposit=str_replace(security_deposit, fixed(","), ""))
IA_MGP18_23$security_deposit <- as.numeric(IA_MGP18_23$security_deposit)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (cleaning_fee = str_replace (cleaning_fee, fixed("$"), "")) %>%
  mutate(cleaning_fee=str_replace(cleaning_fee, fixed(","), ""))
IA_MGP18_23$cleaning_fee <- as.numeric(IA_MGP18_23$cleaning_fee)

##On créé de nouvelles variables : prix par personne / prix au mois_logical t or f (donc y a-t-il un prix au mois renseigné)
IA_MGP18_23 <- IA_MGP18_23 %>% mutate(price_pp = price/accommodates) %>%
  mutate(monthly_price_logi = ifelse(!is.na (monthly_price), "t", "f"))

##On créé available_365_logical t or f
IA_MGP18_23 <- IA_MGP18_23 %>% mutate(avail_365_logi = ifelse(IA_MGP18_23$availability_365>= 1, "t", "f"))
freq(IA_MGP18_23$avail_365_logi)


#Travail sur les baux mobilité et la longue/moyenne durée ----
test_longterm <- c("long terme","longue durée", "moyenne durée", "\\ban minimum", "long-term", "long stay", "LONG STAY", "LONG TERME", "MOBILITY LEASE", "mobility lease", "bail mobilité", "BAIL MOBILITE", "BAIL MOBILITÉ") #termes associés à la longue ou moyenne duree qu'on va chercher dans le contenu des annonces

for (i in 1:length(test_longterm)) { 
  if (i == 1) { 
    # Initialiser la colonne lors de la première itération
    IA_MGP18_23 <- IA_MGP18_23 %>%
      mutate(long_term_descri = grepl(test_longterm[i], description))
  } else { 
    # Ajouter les résultats des itérations suivantes avec "|"
    IA_MGP18_23 <- IA_MGP18_23 %>%
      mutate(long_term_descri = long_term_descri | grepl(test_longterm[i], description))
  }
}

freq(IA_MGP18_23$long_term_descri)

IA_MGP18_23 <- IA_MGP18_23 %>%
  mutate(bail_mob = grepl("Available with a mobility lease only", license))

freq(IA_MGP18_23$bail_mob)

IA_MGP18_23$long_term_descri <- as.logical(IA_MGP18_23$long_term_descri)
IA_MGP18_23$bail_mob <- as.logical(IA_MGP18_23$bail_mob)

test <- IA_Paris %>% 
  filter(str_starts(last_scraped, "2020") | str_starts(last_scraped, "2021")) %>%
  filter(grepl("Available with a mobility lease only", license)) #uniquement pour voir à quelle période cette mention arrive
freq(test$last_scraped)

IA_Paris <- IA_Paris %>% mutate(Annee_extraction= str_sub(last_scraped,1,4))
IA_Paris <- IA_Paris %>% mutate(Mois_extraction= str_sub(last_scraped,6,7))
IA_Paris <- IA_Paris %>% mutate (last_scraped_YM = str_c(Annee_extraction, Mois_extraction, sep="-"))

IA_Paris$license_Nett <- str_extract_all(IA_Paris$license, "\\b\\d{13}\\b")
IA_Paris$license_Nett <- sapply(IA_Paris$license_Nett, paste, collapse = ", ")

#nettoyage variable numero d enregistrement dans base Paris
IA_Paris <- IA_Paris %>% 
  mutate(
    license_Nett = if_else(
      license_Nett == "",
      NA_character_,
      license_Nett  
    )
  )

IA_Paris <- IA_Paris %>%
  mutate(
    license = if_else(
      license == "",
      NA_character_,
      license
    )
  )

#Tableau longue durée/ann bloquees a Paris
df_LT_Paris <- IA_Paris %>% 
  filter(room_type =="Entire home/apt") %>% 
  #filter(last_scraped_YM %in% c("2018-04", "2020-01", "2021-11", "2022-05", "2022-08", "2022-11", "2023-02", "2023-05", "2023-08", "2023-11")) %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    LE365 = sum(minimum_nights ==365),
    LE365_pct = LE365/nb_annoncesLE*100,
    LEsup364 = sum(minimum_nights > 364),
    LEsup364_pct = LEsup364/nb_annoncesLE*100,
    LEsup27 = sum(minimum_nights>27),
    LEsup27_pct = LEsup27/nb_annoncesLE*100,
    LElicenseNett = sum(!is.na(license_Nett)),
    LElicenseNett_pct = LElicenseNett/nb_annoncesLE*100,
    LE_licenseNettCD = sum(!is.na(license_Nett) & minimum_nights < 365),
    LE_licenseNettCD_pct = LE_licenseNettCD/nb_annoncesLE*100,
    LE_licenseNettLD = sum(!is.na(license_Nett) & minimum_nights > 364),
    LE_licenseNettLD_pct = LE_licenseNettLD/nb_annoncesLE*100,
    LE_licenseNA.LD = sum(is.na(license) & minimum_nights > 364),
    LE_licenseNA.LD_pct = LE_licenseNA.LD/nb_annoncesLE*100,
    LE_licenseNA.CD = sum(is.na(license) & minimum_nights < 365),
    LE_licenseNA.CD_pct = LE_licenseNA.CD/nb_annoncesLE*100,
    LE_bailmob = sum(grepl("Available with a mobility lease only", license), na.rm =TRUE),
    LE_bailmob_pct = LE_bailmob/nb_annoncesLE*100,
    LE_exempthotel = sum(license=="Exempt - hotel-type listing", na.rm =TRUE),
    LE_exempthotel_pct = LE_exempthotel/nb_annoncesLE*100,
    LE_licenseFAUX.CD = sum(is.na(license_Nett) & (!is.na(license)) & (!license=="Exempt - hotel-type listing") & (!license=='Available with a mobility lease only (""""bail mobilité"""")') & minimum_nights < 365),
    LE_licenseFAUX.CD_pct = LE_licenseFAUX.CD/nb_annoncesLE*100
    )


df_LT_MGP <- IA_MGP18_23 %>% 
  filter(room_type =="Entire home/apt") %>% 
  group_by(last_scraped_YM) %>%
  summarise(
    MGP_nb_annoncesLE = n(),
    LE365 = sum(minimum_nights ==365),
    LE365_pct = LE365/MGP_nb_annoncesLE*100,
    LEsup364 = sum(minimum_nights > 364),
    LEsup364_pct = LEsup364/MGP_nb_annoncesLE*100,
    LEsup27 = sum(minimum_nights>27),
    LEsup27_pct = LEsup27/MGP_nb_annoncesLE*100, 
    nb_bail_mob = sum(bail_mob==TRUE),
    nb_longTdescr_Hbailmob = sum(bail_mob==FALSE & long_term_descri==TRUE),
    nb_longTdescr_Hbailmob_minstaysup27 = sum(bail_mob==FALSE & long_term_descri==TRUE & minimum_nights >27)
    )

df_LT <- bind_cols (df_LT_Paris,df_LT_MGP)

rm(df_LT, df_LT_MGP, df_LT_Paris)

#Travail sur les annonces bloquées par Airbnb en minstay365 en juillet 2021
ann.blok2021 <- IA_Paris %>%
  filter(room_type =="Entire home/apt") %>%
  filter(last_scraped_YM=="2021-07") %>%
  filter(minimum_nights==365 & is.na(license))

id_blok <- ann.blok2021$id

ann.blok2021 <- IA_Paris %>%
  filter(id %in% id_blok)

ann.blok2021 <- ann.blok2021 %>% arrange(last_scraped_YM)

ann.blok2021_uniq <- ann.blok2021 %>%
  filter(last_scraped_YM > "2021-06") %>%
  filter(!last_scraped_YM=="2022-03") %>% #on vire cette date car très peu de recup on sait pas pq
  group_by (id) %>% 
  summarise (nb_observation =n(),
             room_type_recent = last(room_type),
             room_type_LE = sum(room_type=="Entire home/apt"),
             LE_conservation = room_type_LE / nb_observation*100,
             prem_obser_calc = first(last_scraped_YM),
             der_obser_calc = last(last_scraped_YM),
             first_review = first(first_review),
             last_review = last(last_review),
             first_revltm = first(number_of_reviews_ltm),
             last_revltm = last(number_of_reviews_ltm),
             first_review_count = first(number_of_reviews),
             last_review_count = last(number_of_reviews),
             first_minstay = first(minimum_nights),
             last_minstay = last(minimum_nights),
             license.notNA = sum(!is.na(license)),
             bailmob.notNA = sum(grepl("Available with a mobility lease only", license), na.rm =TRUE),
             last_license = last(license),
             lat = first(latitude),
             lon = first(longitude),
             J_C_COINSE = first(J_C_COINSE),
             C_IR = first(C_IR),
             J_L_CO =first(J_L_CO))

#date de creation des 14 544 ann bloquees : 
sum(str_starts(ann.blok2021_uniq$prem_obser_calc, "2015")) #2529 en 2015
sum(str_starts(ann.blok2021_uniq$prem_obser_calc, "2016")) #3429 en 2016
sum(str_starts(ann.blok2021_uniq$prem_obser_calc, "2017")) #2529 en 2017
sum(str_starts(ann.blok2021_uniq$prem_obser_calc, "2018")) #2499 en 2018
sum(str_starts(ann.blok2021_uniq$prem_obser_calc, "2019")) #2575 en 2019
sum(str_starts(ann.blok2021_uniq$prem_obser_calc, "2020")) #1650 en 2020
sum(str_starts(ann.blok2021_uniq$prem_obser_calc, "2021")) #272 en 2021

ann.blok2021_uniq <- ann.blok2021_uniq %>%
  filter(der_obser_calc =="2023-12")

freq(ann.blok2021_uniq$nb_observation) #après leur blocage : 212 qui sont supprimées après juillet 2021 mais sinon 10 424 qui restent jusqu'à au moins la fin de l'année 2023
freq(ann.blok2021_uniq$der_obser_calc)
freq(ann.blok2021_uniq$room_type_recent)  #il y en a seulement 28 qui changent de room_type
freq(ann.blok2021_uniq$LE_conservation) #la très grande majorité, 14 513 restent room_type LE
freq(ann.blok2021_uniq$license.notNA) # 13680 donc 94,1% reste sans numéro d'enregist et le reste en prend un à un moment
freq(ann.blok2021_uniq$bailmob.notNA) # 151 donc 1% se déclare bail mobilité
freq(ann.blok2021_uniq$first_minstay) #il n'y en avait que 9 qui avaient déjà un minstay de 365 jours avant juillet 2021
freq(ann.blok2021_uniq$last_minstay) # il n'y en a que 703 qui retrouvent un minstay inf à 28 jours après juillet 2021
freq(ann.blok2021_uniq$first_revltm) # 13581 n'ont pas eu de commentaire sur les 12 mois avant juillet 2021

test <- ann.blok2021_uniq %>%
  filter(is.na(last_license)) %>%
  filter(room_type_recent=="Entire home/apt")

freq(test$first_review_count)

id_test <- test$id

test <- IA_Paris %>%
  filter(id %in% id_test) %>%
  filter(last_scraped_YM == "2023-12") %>%
  select(id, last_scraped_YM, license, number_of_reviews, availability_30, availability_60, availability_90, availability_365)

freq(test$availability_365)
sum(test$availability_365 == "365")

rm(ann.blok2021, ann.blok2021_uniq)

#Graph num_enregistr VS Minstay ann_sup365 jours : pour voir l'évolution des ann bloquees et de la progression des numeros d enregistrement
df_LT_Paris %>%
  filter(last_scraped_YM>"2020-12") %>%
  filter(!last_scraped_YM=="2022-03") %>%
  ggplot(aes(x = last_scraped_YM)) +  
  geom_line(aes(y = LE365_pct, color = "LE365_pct", group = 1), linewidth = 1) +  
  geom_line(aes(y = LElicenseNett_pct, color = "LElicenseNett_pct", group = 1), linewidth = 1) +  
  geom_line(aes(y = LE_bailmob_pct, color = "LE_bailmob_pct", group = 1), linewidth = 1) +  
  
  
  scale_color_manual(values = c("LE365_pct" = "#ECA72C", "LElicenseNett_pct" = "#8F2D56", "LE_bailmob_pct"="#3399FF"),
                     labels = c("LE365_pct" = "Annonces réservables pour des\nséjours de 365 jours minimum", "LElicenseNett_pct" = "Annonces avec numéro\nd'enregistrement", "LE_bailmob_pct" = "Annonces déclarées en bail\nmobilité")) +  # Renomme les variables dans la légende 
  labs(
    title = "Évolution du taux d'annonces réservables pour des séjours de 365 jours minimum,\ndu taux d'annonces avec numéro d'enregistrement ou déclarées en bail mobilité",
    x = "Date de collecte",
    y = "Pourcentage par rapport à l'effectif\nd'annonces de type logement entier",
    color = "Annonces de type logements entiers"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Met les dates en biais


#Travail sur l'inactivité des annonces (chap3) ----
#Creation variable ancienneté égale ou sup à 1 an par rapport à chaque date de scraping (sup1an_scrpg)

IA_MGP18_23 <- IA_MGP18_23 %>%
  mutate(sup1an_scrpg = if_else(
    time_length(interval(start = ymd(calc_orig), end = ymd(last_scraped)), unit = "years") >= 1,
    true = TRUE,
    false = FALSE
  )
  )

Tab_anciennete_LE <- IA_MGP18_23 %>% 
  filter(room_type =="Entire home/apt") %>% 
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    annoncesLE_sup1an = sum(sup1an_scrpg =="TRUE"),
    annoncesLE_sup1an_pct = annoncesLE_sup1an/nb_annoncesLE*100)

write.table(Tab_anciennete_LE, "~/Tab_ann_sup_1_an_IA_MGP_LE.csv", sep=";", row.names=F)

#Tableaux sur l'inactivité
Tab_inact_LE <- IA_MGP18_23 %>% 
  filter(room_type =="Entire home/apt") %>% 
  filter(bail_mob == FALSE) %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    nb_annoncesLE_sup1 = sum(sup1an_scrpg ==TRUE),
    C1_s1.0rev.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 == 0, na.rm = TRUE),
    C1_pct = C1_s1.0rev.avail0/nb_annoncesLE_sup1*100,
    C2_s1.0rev.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 > 0, na.rm = TRUE),
    C2_pct = C2_s1.0rev.avail1/nb_annoncesLE_sup1*100,
    C3_s1.1rev.0revLTM.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
    C3_pct = C3_s1.1rev.0revLTM.avail0/nb_annoncesLE_sup1*100,
    C4_s1.1rev.0revLTM.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 > 0, na.rm = TRUE),
    C4_pct = C4_s1.1rev.0revLTM.avail1/nb_annoncesLE_sup1*100,
    avail0 = sum(availability_365 == 0, na.rm = TRUE),
    avail0_pct = avail0/nb_annoncesLE*100,
    zer_rev = sum(number_of_reviews == 0, na.rm = TRUE),
    zer_rev_pct = zer_rev/nb_annoncesLE*100,
    zer_revLTM = sum(number_of_reviews_ltm == 0, na.rm = TRUE),
    zer_revLTM_pct = zer_revLTM/nb_annoncesLE*100,
    accept_NA_0 = sum(host_acceptance_rateNETT== 0 | is.na(host_acceptance_rateNETT)),
    accept_NA_0_pct = accept_NA_0/nb_annoncesLE*100,
    accept_0 = sum(host_acceptance_rateNETT== 0, na.rm = TRUE),
    accept_0_pct = accept_0/nb_annoncesLE*100,
    inact_simpl = sum(number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
    inact_simpl_pct = inact_simpl/nb_annoncesLE*100
  )

write.table(Tab_inact_LE, "~/Murray France/Tab_inact_IA_MGP_LE_1823.csv", sep=",", dec=".", row.names=F)

testC4 <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>% 
  filter(last_scraped_YM == "2023-02") %>% 
  filter(bail_mob == FALSE) %>%
  filter(sup1an_scrpg == TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 > 0)

freq(testC4$minimum_nights)

rm(testC4, Tab_anciennete_LE, Tab_inact_LE)

#Communes pour lesquelles le blocage Airbnb du minstay à 365 est effectif en fev 2023 : 
co_bloK2023_02 <- c("Paris", "Issy-les-Moulineaux", "Asnières-sur-Seine", "Boulogne-Billancourt", "Chaville", "Courbevoie", "Levallois-Perret", "Meudon", "Neuilly-sur-Seine", "Puteaux", "Rueil-Malmaison", "Sèvres", "Vanves", "Nanterre", "Colombes", "Créteil", "Vincennes" )

Tab_inact_LE_bloK <- IA_MGP18_23 %>% 
  filter(room_type =="Entire home/apt") %>% 
  filter(L_CO %in% co_bloK2023_02) %>%
  filter(bail_mob == FALSE) %>%
  filter(minimum_nights == 365 & is.na(license)) %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    nb_annoncesLE_sup1 = sum(sup1an_scrpg ==TRUE),
    C1_s1.0rev.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 == 0, na.rm = TRUE),
    C1_pct = C1_s1.0rev.avail0/nb_annoncesLE_sup1*100,
    C2_s1.0rev.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 > 0, na.rm = TRUE),
    C2_pct = C2_s1.0rev.avail1/nb_annoncesLE_sup1*100,
    C3_s1.1rev.0revLTM.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
    C3_pct = C3_s1.1rev.0revLTM.avail0/nb_annoncesLE_sup1*100,
    C4_s1.1rev.0revLTM.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 > 0, na.rm = TRUE),
    C4_pct = C4_s1.1rev.0revLTM.avail1/nb_annoncesLE_sup1*100,
    avail0 = sum(availability_365 == 0, na.rm = TRUE),
    avail0_pct = avail0/nb_annoncesLE*100,
    zer_rev = sum(number_of_reviews == 0, na.rm = TRUE),
    zer_rev_pct = zer_rev/nb_annoncesLE*100,
    zer_revLTM = sum(number_of_reviews_ltm == 0, na.rm = TRUE),
    zer_revLTM_pct = zer_revLTM/nb_annoncesLE*100,
    accept_NA_0 = sum(host_acceptance_rateNETT== 0 | is.na(host_acceptance_rateNETT)),
    accept_NA_0_pct = accept_NA_0/nb_annoncesLE*100,
    accept_0 = sum(host_acceptance_rateNETT== 0, na.rm = TRUE),
    accept_0_pct = accept_0/nb_annoncesLE*100,
    inact_simpl = sum(number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
    inact_simpl_pct = inact_simpl/nb_annoncesLE*100
  )

write.table(Tab_inact_LE_bloK, "~/Murray France/Tab_inact_IA_MGP_LE_1823_bloK.csv", sep=",", dec=".", row.names=F)

rm(Tab_inact_LE_bloK)

#Creation d'une variable pour les ann bloK à partir de juillet 2021 par Airbnb ----

IA_MGP18_23 <- IA_MGP18_23 %>% 
 mutate(bloK_Air = if_else(room_type =="Entire home/apt" & minimum_nights==365 & is.na(license) & (L_CO %in% co_bloK2023_02) & last_scraped_YM > "2020-02", TRUE, FALSE))

freq(IA_MGP18_23$bloK_Air)

#Par commune en 2020 (pour creation carto) : 

Tab_inact_comm_2020_LE <- IA_MGP18_23 %>% 
  filter(room_type =="Entire home/apt") %>% 
  filter(bail_mob == FALSE) %>%
  filter(last_scraped_YM=="2020-02") %>%
  group_by(J_L_CO) %>%
    summarise(
      nb_annoncesLE = n(),
      nb_annoncesLE_sup1 = sum(sup1an_scrpg ==TRUE),
      C1_s1.0rev.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 == 0, na.rm = TRUE),
      C1_pct = C1_s1.0rev.avail0/nb_annoncesLE_sup1*100,
      C2_s1.0rev.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 > 0, na.rm = TRUE),
      C2_pct = C2_s1.0rev.avail1/nb_annoncesLE_sup1*100,
      C3_s1.1rev.0revLTM.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
      C3_pct = C3_s1.1rev.0revLTM.avail0/nb_annoncesLE_sup1*100,
      C4_s1.1rev.0revLTM.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 > 0, na.rm = TRUE),
      C4_pct = C4_s1.1rev.0revLTM.avail1/nb_annoncesLE_sup1*100,
      avail0 = sum(availability_365 == 0, na.rm = TRUE),
      avail0_pct = avail0/nb_annoncesLE*100,
      zer_rev = sum(number_of_reviews == 0, na.rm = TRUE),
      zer_rev_pct = zer_rev/nb_annoncesLE*100,
      zer_revLTM = sum(number_of_reviews_ltm == 0, na.rm = TRUE),
      zer_revLTM_pct = zer_revLTM/nb_annoncesLE*100,
      accept_NA_0 = sum(host_acceptance_rateNETT== 0 | is.na(host_acceptance_rateNETT)),
      accept_NA_0_pct = accept_NA_0/nb_annoncesLE*100,
      accept_0 = sum(host_acceptance_rateNETT== 0, na.rm = TRUE),
      accept_0_pct = accept_0/nb_annoncesLE*100,
      inact_simpl = sum(number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
      inact_simpl_pct = inact_simpl/nb_annoncesLE*100
    )

write.table(Tab_inact_comm_2020_LE, "~/Murray France/Tab_inact_IA_MGP_LE_comm_2020.csv", sep=",", dec=".", row.names=F)

rm(Tab_inact_comm_2020_LE)

#Par commune en 2023 : 

Tab_inact_comm_2023 <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>% 
  filter(bail_mob == FALSE) %>%
  filter(last_scraped_YM=="2023-02") %>%
  group_by(J_L_CO) %>%
  summarise(
    nb_annoncesLE = n(),
    nb_annoncesLE_sup1 = sum(sup1an_scrpg ==TRUE),
    C1_s1.0rev.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 == 0, na.rm = TRUE),
    C1_pct = C1_s1.0rev.avail0/nb_annoncesLE_sup1*100,
    C2_s1.0rev.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 > 0, na.rm = TRUE),
    C2_pct = C2_s1.0rev.avail1/nb_annoncesLE_sup1*100,
    C3_s1.1rev.0revLTM.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
    C3_pct = C3_s1.1rev.0revLTM.avail0/nb_annoncesLE_sup1*100,
    C4_s1.1rev.0revLTM.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 > 0, na.rm = TRUE),
    C4_pct = C4_s1.1rev.0revLTM.avail1/nb_annoncesLE_sup1*100,
    avail0 = sum(availability_365 == 0, na.rm = TRUE),
    avail0_pct = avail0/nb_annoncesLE*100,
    zer_rev = sum(number_of_reviews == 0, na.rm = TRUE),
    zer_rev_pct = zer_rev/nb_annoncesLE*100,
    zer_revLTM = sum(number_of_reviews_ltm == 0, na.rm = TRUE),
    zer_revLTM_pct = zer_revLTM/nb_annoncesLE*100,
    accept_NA_0 = sum(host_acceptance_rateNETT== 0 | is.na(host_acceptance_rateNETT)),
    accept_NA_0_pct = accept_NA_0/nb_annoncesLE*100,
    accept_0 = sum(host_acceptance_rateNETT== 0, na.rm = TRUE),
    accept_0_pct = accept_0/nb_annoncesLE*100,
    inact_simpl = sum(number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
    inact_simpl_pct = inact_simpl/nb_annoncesLE*100
  )

write.table(Tab_inact_comm_2023, "~/Murray France/Tab_inact_IA_MGP_LE_comm_202302.csv", sep=",", dec=".", row.names=F)

Tab_inact_comm_2023_bloK <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>% 
  filter(L_CO %in% co_bloK2023_02) %>%
  filter(minimum_nights == 365 & is.na(license)) %>%
  filter(bail_mob == FALSE) %>%
  filter(last_scraped_YM=="2023-02") %>%
  group_by(J_L_CO) %>%
  summarise(
    nb_annoncesLE = n(),
    nb_annoncesLE_sup1 = sum(sup1an_scrpg ==TRUE),
    C1_s1.0rev.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 == 0, na.rm = TRUE),
    C1_pct = C1_s1.0rev.avail0/nb_annoncesLE_sup1*100,
    C2_s1.0rev.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews == 0 & availability_365 > 0, na.rm = TRUE),
    C2_pct = C2_s1.0rev.avail1/nb_annoncesLE_sup1*100,
    C3_s1.1rev.0revLTM.avail0 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
    C3_pct = C3_s1.1rev.0revLTM.avail0/nb_annoncesLE_sup1*100,
    C4_s1.1rev.0revLTM.avail1 = sum(sup1an_scrpg ==TRUE & number_of_reviews > 0 & number_of_reviews_ltm == 0 & availability_365 > 0, na.rm = TRUE),
    C4_pct = C4_s1.1rev.0revLTM.avail1/nb_annoncesLE_sup1*100,
    avail0 = sum(availability_365 == 0, na.rm = TRUE),
    avail0_pct = avail0/nb_annoncesLE*100,
    zer_rev = sum(number_of_reviews == 0, na.rm = TRUE),
    zer_rev_pct = zer_rev/nb_annoncesLE*100,
    zer_revLTM = sum(number_of_reviews_ltm == 0, na.rm = TRUE),
    zer_revLTM_pct = zer_revLTM/nb_annoncesLE*100,
    accept_NA_0 = sum(host_acceptance_rateNETT== 0 | is.na(host_acceptance_rateNETT)),
    accept_NA_0_pct = accept_NA_0/nb_annoncesLE*100,
    accept_0 = sum(host_acceptance_rateNETT== 0, na.rm = TRUE),
    accept_0_pct = accept_0/nb_annoncesLE*100,
    inact_simpl = sum(number_of_reviews_ltm == 0 & availability_365 == 0, na.rm = TRUE),
    inact_simpl_pct = inact_simpl/nb_annoncesLE*100
  )

write.table(Tab_inact_comm_2023_bloK, "~/Murray France/Tab_inact_IA_MGP_LE_comm_202302bloK.csv", sep=",", dec=".", row.names=F)

rm(Tab_inact_comm_2023_bloK, Tab_inact_comm_2023)


####Travail sur les doublons inter/ Altri et intra Airbnb à partir du numéro d'enregistrement (chap2) ----

#On ouvre les jeux de données dont on a besoin
#D'abord avec ABRITEL
col_types <- c(
  ID_scraping = "character",
  ID_URL = "character",
  Dte_prem_vue_Nett = "character",
  Dte_der_vue_Nett = "character",
  Min_date_comm = "character",
  Max_date_comm = "character",
  Minstay = "numeric",
  Superhost_or_premium = "logical",
  Code_postal = "character",
  abr_num_enreg_txt = "character",
  Numero_enregistr ="character",
  last_scraped ="character",
  Host_since="character",
  Cal_date_debut_Nett ="character",
  Averagerating = "numeric")

base <- fread("~/Altri/Abritel/Altri_Abr_lis_2019-20231211_MGP2.csv", #donnees issues de la source Altri
              encoding = "UTF-8",
              na.strings = c("N/A", "", "NA"),
              colClasses = col_types)

names(base)

test <- base[!duplicated(base$Numero_enregistr), ]
test <- test %>% select(ID_URL, ID_uniq, Numero_enregistr) #on compare ensuite les numeros d'enregistrement avec ceux dans les fichiers originaux pour s'assurer qu'il n'y a pas eu de souci avec les formats des numéros
fwrite(test, file = "~/Altri/Test_num_enregistr_Abritel.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)

#Nettoyage colonne num_enregistr
base$Numero_enregistr_Nett <- str_extract_all(base$Numero_enregistr, "\\b\\d{13}\\b")
base$Numero_enregistr_Nett <- sapply(base$Numero_enregistr_Nett, paste, collapse = ", ")

freq(base$Numero_enregistr)
freq(base$Numero_enregistr_Nett)

#Creation de la table de numero d'enregistrement
Tab_license <- base %>%
  distinct(ID_URL, .keep_all = TRUE) %>%
  group_by(Numero_enregistr_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license <- Tab_license %>% 
  filter(!is.na(Numero_enregistr_Nett)) %>%
  filter(!Numero_enregistr_Nett=="") %>%
  filter(!Numero_enregistr_Nett=="NA")

view(Tab_license_Abr)
str(Tab_license)

#Enregistrement de la table
fwrite(Tab_license, file = "~/Altri/Tab_Numero_enregistr_Abritel.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)

Tab_license_Abr <- Tab_license
  
#On prend que les numéros du jeu du 27/01/2020
base <- base %>% filter(last_scraped =="2020-01-27")

#Creation de la table de numero d'enregistrement 2020-01-27
Tab_license_20200127 <- base %>%
  distinct(ID_URL, .keep_all = TRUE) %>%
  group_by(Numero_enregistr_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_20200127 <- Tab_license_20200127 %>% 
  filter(!is.na(Numero_enregistr_Nett)) %>%
  filter(!Numero_enregistr_Nett=="") %>%
  filter(!Numero_enregistr_Nett=="NA")

Tab_license_Abr_20200127 <- Tab_license_20200127

####Idem pour Booking
col_types <- c(
  ID_scraping = "character",
  ID_URL = "character",
  Date_extraction = "character",
  Dte_prem_vue = "character",
  Dte_der_vue = "character",
  Min_date_comm = "character",
  Max_date_comm = "character",
  Code_postal = "character",
  Numero_enregistr ="character"
)

base <- fread("~/Altri/Booking/Altri_Boo_lis_2019-20231225_MGP2.csv", 
              encoding = "UTF-8",
              colClasses = col_types)

names(base)
str(base$Numero_enregistr)

test <- base[!duplicated(base$Numero_enregistr), ]
test <- test %>% select(ID_URL, Numero_enregistr) #on compare ensuite les num avec ceux dans les fichiers originaux pour s'assurer qu'il n'y a pas eu de souci avec les formats des numéros
fwrite(test, file = "~/Altri/Test_num_enregistr_Booking.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)

#Nettoyage colonne num_enregistr
base_num <- base %>% 
  mutate(Numero_enregistr = str_remove_all(Numero_enregistr, "[{}]")) %>%
  separate_rows(Numero_enregistr, sep = ",") 

base_num$Numero_enregistr_Nett <- str_extract_all(base_num$Numero_enregistr, "\\b\\d{13}\\b")
base_num$Numero_enregistr_Nett <- sapply(base_num$Numero_enregistr_Nett, paste, collapse = ", ")

freq(base$Numero_enregistr)
freq(base_num$Numero_enregistr_Nett)

#Creation de la table de numero d'enregistrement
Tab_license <- base_num %>%
  distinct(ID_URL, .keep_all = TRUE) %>%
  group_by(Numero_enregistr_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license <- Tab_license %>% 
  filter(!is.na(Numero_enregistr_Nett)) %>%
  filter(!Numero_enregistr_Nett=="") %>%
  filter(!Numero_enregistr_Nett=="NA")

view(Tab_license)
str(Tab_license)

#Enregistrement de la table
fwrite(Tab_license, file = "~/Altri/Tab_Numero_enregistr_Booking.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)

Tab_license_Boo <- Tab_license

rm(base_num)

#On prend que les numéros du jeu du 2020-01-27
base_num <- base_num %>% filter(last_scraped =="2020-01-27")

#Creation de la table de numero d'enregistrement 2020-01-27
Tab_license_20200127 <- base_num %>%
  distinct(ID_URL, .keep_all = TRUE) %>%
  group_by(Numero_enregistr_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_20200127 <- Tab_license_20200127 %>% 
  filter(!is.na(Numero_enregistr_Nett)) %>%
  filter(!Numero_enregistr_Nett=="") %>%
  filter(!Numero_enregistr_Nett=="NA")

Tab_license_Boo_20200127 <- Tab_license_20200127


####Maintenant TripAdvisor
col_types <- c(
  ID_scraping = "character",
  ID_URL = "character",
  Host_ID = "character",
  Host_since = "character",
  Date_extraction = "character",
  Dte_prem_vue_Nett = "character",
  Dte_der_vue_Nett = "character",
  Min_date_comm = "character",
  Max_date_comm = "character",
  Minstay = "numeric",
  Code_postal = "character",
  Numero_enregistrement = "character")

base <- fread("~/Altri/TripAdvisor/Altri_Trip_lis_2019-20230118_MGP2.csv", 
              encoding = "UTF-8",
              na.strings = c("N/A", "", "NA"),
              colClasses = col_types)

names(base)
str(base$Numero_enregistrement)

test <- base[!duplicated(base$Numero_enregistrement), ]
test <- test %>% select(ID_URL, Numero_enregistrement) #on compare ensuite les num avec ceux dans les fichiers originaux pour s'assurer qu'il n'y a pas eu de souci avec les formats des numéros

view(test)
fwrite(test, file = "~/Altri/Test_num_enregistr_TripAdvisor.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)

#Nettoyage colonne num_enregistr
base$Numero_enregistr_Nett <- str_extract_all(base$Numero_enregistrement, "\\b\\d{13}\\b")
base$Numero_enregistr_Nett <- sapply(base$Numero_enregistr_Nett, paste, collapse = ", ")

freq(base$Numero_enregistrement)
freq(base$Numero_enregistr_Nett)

#Creation de la table de numero d'enregistrement
Tab_license <- base %>%
  distinct(ID_URL, .keep_all = TRUE) %>%
  group_by(Numero_enregistr_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license <- Tab_license %>% 
  filter(!is.na(Numero_enregistr_Nett)) %>%
  filter(!Numero_enregistr_Nett=="") %>%
  filter(!Numero_enregistr_Nett=="NA")

view(Tab_license)
str(Tab_license)

#Enregistrement de la table
fwrite(Tab_license, file = "~/Altri/Tab_Numero_enregistr_TripAdvisor.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)

Tab_license_Trip <- Tab_license

rm(test)


#On prend que les numéros du jeu du 2020-04-20 (jeu disponible à une date la plus proche des autres)
base <- base %>% filter(last_scraped =="2020-04-20")

#Creation de la table de numero d'enregistrement 2020-04-20
Tab_license_20200420 <- base %>%
  distinct(ID_URL, .keep_all = TRUE) %>%
  group_by(Numero_enregistr_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_20200420 <- Tab_license_20200420 %>% 
  filter(!is.na(Numero_enregistr_Nett)) %>%
  filter(!Numero_enregistr_Nett=="") %>%
  filter(!Numero_enregistr_Nett=="NA")

Tab_license_Trip_20200420 <- Tab_license_20200420

#Creation de la table de numero d'enregistrement pour IAMGP
combinations <- IA_MGP18_23 %>% 
  filter(room_type=="Entire home/apt") %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  distinct(ID_URL, license_Nett)

Tab_license_Air <- combinations %>%
  group_by(license_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_Air <- Tab_license_Air %>%
  rename(Numero_enregistr_Nett = license_Nett)

view(Tab_license_Air)
str(Tab_license_Air)

#Enregistrement de la table
fwrite(Tab_license_Air, file = "~/Altri/Tab_Numero_enregistr_Airbnb_IA_MGP.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)

#Table de numéros d'enregistrement qui fusionne ceux trouvés sur toutes les plateformes
merged_data <- bind_rows(
  Tab_license_Abr %>% mutate(source = "Abr"),
  Tab_license_Boo %>% mutate(source = "Boo"),
  Tab_license_Trip %>% mutate(source = "Trip"),
  Tab_license_Air %>% mutate(source = "Air")
)

#Nombre couples ID/numéros d'enregistrement par source
id_source_count <- merged_data %>%
  distinct(Numero_enregistr_Nett, source) %>% 
  group_by(Numero_enregistr_Nett) %>%
  summarise(source_count = n()) 

id_sources_concat <- merged_data %>%
  distinct(Numero_enregistr_Nett, source) %>%       
  group_by(Numero_enregistr_Nett) %>%
  summarise(sources = paste(sort(source), collapse = ", ")) #coller les diff plateformes sur lesquelles le num d'enregist apparait

freq(id_sources_concat$sources)

merged_data <- merged_data %>%
  left_join(id_source_count, by = "Numero_enregistr_Nett") %>%
  left_join(id_sources_concat, by = "Numero_enregistr_Nett")

id_sources_concat <- id_sources_concat %>%
  left_join(merged_data, by = "Numero_enregistr_Nett")

id_sources_concat <- id_sources_concat %>%
  select(-sources.y)

fwrite(id_sources_concat, file = "~/Tab_Numero_enregistr_AirIAMGP_Abr_Boo_Trip.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)
#id_sources_concat <- fread("~/Tab_Numero_enregistr_AirIAMGP_Abr_Boo_Trip.csv") #si besoin ensuite

rm(id_source_count, id_sources_concat)

#Flechage des numéros faux et on retire des analyses ceux qui sont trop explicitement faux et donc utilisés potentiellement par plusieurs loueurs
names(id_sources_concat)

num_a_retirer <- c(0000000000000,1234567890123,1234567891011,1234567891234,7500000000000,7512345678900,7512345678901,7512345678910,7512345678912,9212345678912,9312345678912,9412345678912)

id_sources_concat <- id_sources_concat %>%
  filter(!Numero_enregistr_Nett %in% num_a_retirer)

id_sources_concat <- id_sources_concat %>%
  mutate(license_faux = if_else(
    str_detect(Numero_enregistr_Nett, "^(7510|7511|75120|94004|92012|92022|92024|92025|92026|92040|92044|92046|92048|92050|92051|92062|92063|92072|92073|92075|94017|94028|94067|94069|94080|94081|93051)"),  # Vérifie si la chaîne commence par l'un des codes de numéros d'enregistrement en place dans la MGP fin 2023-2024
    NA_character_,
    "FAUX"                        # Si la condition est fausse c'est un FAUX numéro d'enregistrement
  ))

#On créé id_source_concat dédoublonné par Numero_enregistr_Nett en classant d'abord par "nb_id_associe" pour venir faire nos calculs de répartition des num d'enregistr par plateforme
id_sources_concat <- id_sources_concat %>%
  arrange(nb_id_associe)

id_sources_concat_uniq <- id_sources_concat %>%
  distinct(Numero_enregistr_Nett, .keep_all = TRUE)

freq(id_sources_concat_uniq$license_faux)
freq(id_sources_concat_uniq$sources.x)

#On ne garde que les numéros qui sont dans la base Airbnb pour construire de nouvelles variables dans IA_MGP
Tab_num_air <- id_sources_concat %>%
  filter(source=="Air")

freq(id_sources_concat$source)

Tab_num_air <- Tab_num_air %>%                    #on donne aux variables des noms plus parlants
  rename(license_dupli_pltf = sources.x) %>%
  rename(license_dupli_pltf_count = source_count) %>%
  rename(license_dupli_air_count = nb_id_associe) %>% 
  select(-source)

str(Tab_num_air)

#On colle les nouvelles infos sur les duplications de num d'enregist à notre tableau IA_MGP
IA_MGP18_23 <- IA_MGP18_23 %>% 
  left_join(Tab_num_air, by = c("license_Nett" = "Numero_enregistr_Nett")) 

#On retire de license_NETT les faux numéros d'enregistrement
sum(is.na(IA_MGP18_23$license_Nett))

IA_MGP18_23 <- IA_MGP18_23 %>%
  mutate(license_Nett = if_else(license_Nett %in% num_a_retirer, NA_character_, license_Nett)) %>%
  mutate(license_Nett = if_else(license_Nett == "", NA_character_, license_Nett)) %>%
  mutate(license_Nett = if_else(license_Nett == "0000000000000", NA_character_, license_Nett))

view(IA_MGP18_23[,130:147])

#On fait quasi la même chose mais cette fois avec un croisement des plateformes pour janv 2020
#de sorte à ne pas comptabiliser des doublons qui ne seraient pas simultanés

#Creation de la table de numero d'enregistrement pour IAMGP - 2020-01-23
combinations <- IA_MGP18_23 %>% 
  filter(last_scraped_YM=="2020-02") %>%
  filter(room_type=="Entire home/apt") %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  distinct(ID_URL, license_Nett)

Tab_license_Air_202002 <- combinations %>%
  group_by(license_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_Air_202002 <- Tab_license_Air_202002 %>%
  rename(Numero_enregistr_Nett = license_Nett)

#Table de numéros d'enregistrement commune
merged_data_2020 <- bind_rows(
  Tab_license_Abr_20200127 %>% mutate(source = "Abr"),
  Tab_license_Boo_20200127 %>% mutate(source = "Boo"),
  Tab_license_Trip_20200420 %>% mutate(source = "Trip"),
  Tab_license_Air_202002 %>% mutate(source = "Air")
)

#Nombre d'ID distincts par source
id_source_count_2020 <- merged_data_2020 %>%
  distinct(Numero_enregistr_Nett, source) %>%           
  group_by(Numero_enregistr_Nett) %>%
  summarise(source_count = n()) 

id_sources_concat_2020 <- merged_data_2020 %>%
  distinct(Numero_enregistr_Nett, source) %>%
  group_by(Numero_enregistr_Nett) %>%
  summarise(sources = paste(sort(source), collapse = ", "))

freq(id_sources_concat_2020$sources)

merged_data_2020 <- merged_data_2020 %>%
  left_join(id_source_count_2020, by = "Numero_enregistr_Nett") %>%
  left_join(id_sources_concat_2020, by = "Numero_enregistr_Nett")

id_sources_concat_2020 <- id_sources_concat_2020 %>%
  left_join(merged_data_2020, by = "Numero_enregistr_Nett")

id_sources_concat_2020 <- id_sources_concat_2020 %>%
  select(-sources.y)

fwrite(id_sources_concat_2020, file = "~/Tab_Numero_enregistr_AirIAMGP_Abr_Boo_Trip_202001.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)
rm(id_source_count_2020)

#On retire des analyses les num qui sont trop explicitement faux et donc utilisés potentiellement par plusieurs loueurs
id_sources_concat_2020 <- id_sources_concat_2020 %>%
  filter(!Numero_enregistr_Nett %in% num_a_retirer)

id_sources_concat_2020 <- id_sources_concat_2020 %>%
  arrange()

id_sources_concat_uniq_2020 <- id_sources_concat_2020 %>%
  distinct(Numero_enregistr_Nett, .keep_all = TRUE)

#On garde que les numéros qui sont dans la base Airbnb pour ensuite venir les coller à la base
Tab_num_air_2020 <- id_sources_concat_2020 %>%
  filter(source=="Air")

Tab_num_air_2020 <- Tab_num_air_2020 %>%
  rename(license_dupli_pltf = sources.x) %>%
  rename(license_dupli_pltf_count = source_count) %>%
  rename(license_dupli_air_count = nb_id_associe) %>% 
  select(-source)

str(Tab_num_air)

#On colle les nouvelles infos sur les duplications de num d'enregist à IA_MGP_LE_2020_02 (qui est temporaire, pour faire nos stats)
IA_MGP_LE_2020_02 <- IA_MGP18_23 %>%
  filter(room_type=="Entire home/apt") %>% 
  filter(last_scraped_YM =="2020-02")

IA_MGP_LE_2020_02 <- IA_MGP_LE_2020_02 %>% select(-license_dupli_pltf, -license_dupli_air_count, -license_dupli_pltf_count) #On fait ca car sinon les colonne existent déjà de notre traitement sr l'ensemble des dates

IA_MGP_LE_2020_02 <- IA_MGP_LE_2020_02 %>% 
  left_join(Tab_num_air_2020, by = c("license_Nett" = "Numero_enregistr_Nett"))

#Décompte des doublons inter dans IA_MGP_2020
freq(IA_MGP_LE_2020_02$license_dupli_pltf_count)
freq(IA_MGP_LE_2020_02$license_dupli_pltf)

sum(is.na(IA_MGP_LE_2020_02$license_Nett))
sum(!is.na(IA_MGP_LE_2020_02$license_Nett))

#Selon les localisations 
Paris_IA_LE_2020_02 <- IA_MGP_LE_2020_02 %>% filter(L_CO=="Paris")
freq(Paris_IA_LE_2020_02$license_dupli_air_count)
sum(!is.na(Paris_IA_LE_2020_02$license_Nett))

HParis_IA_LE_2020_02 <- IA_MGP_LE_2020_02 %>% filter(!L_CO=="Paris") 
freq(HParis_IA_LE_2020_02$license_dupli_pltf_count) #on se rend compte qu'il n'y a que très peu de numéros d'enregistr. hors Paris à cette époque
sum(!is.na(HParis_IA_LE_2020_02$license_Nett))

#On ajoute d'autres variables dans la comparaison des num d'enregistrement INTRA Airbnb, de sorte à amoidrir les cas de faux doublons intra
Paris_IA_LE_2020_02 <- Paris_IA_LE_2020_02 %>%
  group_by(license_Nett, accommodates, C_IR) %>%   #on compare les codes d'Iris, façon d'approcher les localisations, et le nb de couchage (au final avec les algo différents de localisation des plateformes, C_IR est peut-être trop "sévère")
  mutate(verif_doubl = ifelse(is.na(license_Nett), 
                              NA,  # Si license_Nett est NA, mettre NA
                              ifelse(n() > 1, paste(ID_URL, collapse = ";"), NA))) %>%
  ungroup()

Paris_IA_LE_2020_02 <- Paris_IA_LE_2020_02 %>%
  group_by(verif_doubl) %>% # Grouper par "verif_doubl"
  mutate(license_dupli_air_count_verif = ifelse(is.na(verif_doubl), 
                                                NA,  # Si "verif_doubl" est NA, mettre NA
                                                n())) %>% # Sinon compter les lignes dans chaque groupe
  ungroup()

freq(Paris_IA_LE_2020_02$license_dupli_air_count_verif)

sum(!is.na(Paris_IA_LE_2020_02$license_dupli_air_count_verif))

rm(Paris_IA_LE_2020_02, HParis_IA_LE_2020_02)

#On n'ajoute pas cette variable "doublon_intra_license202002" dans IA_MGP car il faudrait le faire pour chaque jeu de données (manque de temps)

###On teste cette fois-ci sur l'ensemble de l'année 2022 et 2023 les doublons intra à partir des num d'enregistr
#Creation de la table de numero d'enregistrement pour IAMGP 2022, de nov 2021 à nov 2022
combinations22 <- IA_MGP18_23 %>% 
  filter(last_scraped_YM %in% c("2021-11", "2022-05", "2022-08", "2022-11")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  filter(L_CO=="Paris") %>%
  distinct(ID_URL, license_Nett)

Tab_license_Air_2022 <- combinations22 %>%
  group_by(license_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_Air_2022 <- Tab_license_Air_2022 %>%
  rename(Numero_enregistr_Nett = license_Nett)

#On retire des analyses les num qui sont trop explicitement faux et donc utilisés potentiellement par plusieurs loueurs
num_a_retirer <- c(0000000000000,1234567890123,1234567891011,1234567891234,7500000000000,7512345678900,7512345678901,7512345678910,7512345678912,9212345678912,9312345678912,9412345678912)

Tab_license_Air_2022 <- Tab_license_Air_2022 %>%
  filter(!Numero_enregistr_Nett %in% num_a_retirer)

Tab_license_Air_2022 <- Tab_license_Air_2022 %>%
  rename(license_dupli_air_count = nb_id_associe)

#On colle les nouvelles infos sur les duplications de num d'enregist à IA_MGP_LE_2022_uniq (temporaire)
IA_MGP_LE_2022_uniq <- IA_MGP18_23 %>% 
  filter(last_scraped_YM %in% c("2021-11", "2022-05", "2022-08", "2022-11")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  filter(L_CO=="Paris") %>%
  distinct(ID_URL, .keep_all = TRUE)

IA_MGP_LE_2022_uniq <- IA_MGP_LE_2022_uniq %>% 
  left_join(Tab_license_Air_2022, by = c("license_Nett" = "Numero_enregistr_Nett"))

#Décompte des doublons intra dans IA_MGP_2022
freq(IA_MGP_LE_2022_uniq$license_dupli_air_count.y)

#On teste de comparer d'autres variables dans la comparaison des num d'enregistrement
IA_MGP_LE_2022_uniq <- IA_MGP_LE_2022_uniq %>%
  group_by(license_Nett, accommodates, C_IR) %>%
  mutate(verif_doubl = ifelse(is.na(license_Nett), 
                              NA,  # Si license_Nett est NA, mettre NA
                              ifelse(n() > 1, paste(ID_URL, collapse = ";"), NA))) %>%
  ungroup()

IA_MGP_LE_2022_uniq <- IA_MGP_LE_2022_uniq %>%
  group_by(verif_doubl) %>% # Grouper par "verif_doubl"
  mutate(license_dupli_air_count_verif = ifelse(is.na(verif_doubl), 
                                                NA,  # Si "verif_doubl" est NA, mettre NA
                                                n())) %>% # Sinon compter les lignes dans chaque groupe
  ungroup()

freq(IA_MGP_LE_2022_uniq$license_dupli_air_count_verif)
sum(!is.na(IA_MGP_LE_2022_uniq$license_dupli_air_count_verif))

rm(IA_MGP_LE_2022_uniq, Tab_license_Air_2022, combinations22)

#Creation de la table de numero d'enregistrement pour IAMGP 2022, de nov 2022 à aout 2023 (car nov 2023 n'a pas de variable num d'enregistr) - 
combinations23 <- IA_MGP18_23 %>% 
  filter(last_scraped_YM %in% c("2022-11", "2023-02", "2023-05", "2023-08")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  filter(L_CO=="Paris") %>%
  distinct(ID_URL, license_Nett)

Tab_license_Air_2023 <- combinations23 %>%
  group_by(license_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_Air_2023 <- Tab_license_Air_2023 %>%
  rename(Numero_enregistr_Nett = license_Nett)

#On retire des analyses les num qui sont trop explicitement faux et donc utilisés potentiellement par plusieurs loueurs
num_a_retirer <- c(0000000000000,1234567890123,1234567891011,1234567891234,7500000000000,7512345678900,7512345678901,7512345678910,7512345678912,9212345678912,9312345678912,9412345678912)

Tab_license_Air_2023 <- Tab_license_Air_2023 %>%
  filter(!Numero_enregistr_Nett %in% num_a_retirer)

Tab_license_Air_2023 <- Tab_license_Air_2023 %>%
  rename(license_dupli_air_count = nb_id_associe)

#On colle les nouvelles infos sur les duplications de num d'enregist à IA_MGP_LE_2023_uniq (temporaire)
IA_MGP_LE_2023_uniq <- IA_MGP18_23 %>% 
  filter(last_scraped_YM %in% c("2022-11", "2023-02", "2023-05", "2023-08")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  filter(L_CO=="Paris") %>%
  distinct(ID_URL, .keep_all = TRUE)

IA_MGP_LE_2023_uniq <- IA_MGP_LE_2023_uniq %>% 
  left_join(Tab_license_Air_2023, by = c("license_Nett" = "Numero_enregistr_Nett"))

#Décompte des doublons intra dans IA_MGP_2023
freq(IA_MGP_LE_2023_uniq$license_dupli_air_count.y)

#On teste de comparer d'autres variables dans la comparaison des num d'enregistrement
IA_MGP_LE_2023_uniq <- IA_MGP_LE_2023_uniq %>%
  group_by(license_Nett, accommodates, C_IR) %>%
  mutate(verif_doubl = ifelse(is.na(license_Nett), 
                              NA,  # Si license_Nett est NA, mettre NA
                              ifelse(n() > 1, paste(ID_URL, collapse = ";"), NA))) %>%
  ungroup()

IA_MGP_LE_2023_uniq <- IA_MGP_LE_2023_uniq %>%
  group_by(verif_doubl) %>% # Grouper par "verif_doubl"
  mutate(license_dupli_air_count_verif = ifelse(is.na(verif_doubl), 
                                                NA,  # Si "verif_doubl" est NA, mettre NA
                                                n())) %>% # Sinon compter les lignes dans chaque groupe
  ungroup()

freq(IA_MGP_LE_2023_uniq$license_dupli_air_count_verif)
sum(!is.na(IA_MGP_LE_2023_uniq$license_dupli_air_count_verif))

test <- IA_MGP_LE_2023_uniq %>% filter(number_of_reviews_ltm>0)
freq(test$license_dupli_air_count_verif)

rm(IA_MGP_LE_2023_uniq, Tab_license_Air_2023, combinations23)

#Creation de la table de numero d'enregistrement pour IA PARIS 2022, de janv à décembre 2022 - 

#Reouverture de la base (si pas deja ouverte): 
col_types <- c(
  id ="character",
  scrape_id = "character",
  listing_url = "character",
  host_id = "character",
  license = "character",
  host_listings_count = "integer",
  host_total_listings_count= "integer",
  accommodates = "numeric",
  bathrooms = "integer",
  bedrooms = "integer",
  beds = "integer",
  minimum_nights = "integer",
  maximum_nights = "integer",
  minimum_minimum_nights  = "integer",
  maximum_minimum_nights  = "integer",
  maximum_nights_avg_ntm = "numeric",
  minimum_maximum_nights = "numeric",   
  maximum_maximum_nights = "numeric",
  minimum_nights_avg_ntm = "numeric",                  
  availability_30 = "integer",
  availability_60 = "integer",
  availability_90 = "integer",
  availability_365 = "integer",
  number_of_reviews = "numeric",
  number_of_reviews_ltm = "numeric",
  number_of_reviews_l30d = "numeric",
  review_scores_rating ="numeric",
  review_scores_value ="numeric",
  review_scores_cleanliness = "numeric",
  review_scores_checkin = "numeric",
  review_scores_communication= "numeric",
  review_scores_accuracy = "numeric",
  review_scores_location = "numeric",     
  calculated_host_listings_count_entire_homes = "numeric",
  calculated_host_listings_count_private_rooms = "numeric",
  calculated_host_listings_count_shared_rooms = "numeric",
  calculated_host_listings_count= "numeric",
  calendar_updated = "character",
  reviews_per_month = "numeric",
  latitude = "character",
  longitude ="character",
  OBJECTID= "character",
  N_SQ_CO= "character",
  C_COINSEE= "character",
  C_DEP= "character",
  L_CO= "character",
  C_POSTSEC= "character",
  C_AGGLO= "character",
  C_VILLENOU= "character",
  C_METROP= "character",
  B_LIMITRO= "character",
  B_UNITEURB= "character",
  N_SQ_DE= "character",
  N_SQ_EPCI= "character",
  NB_POP= "numeric",
  SHAPE_Leng= "character",
  SHAPE_Area= "character",
  zipcode= "character",
  square_feet= "character", 
  last_scraped_YM= "character"
)

IAParis <- fread("~/IA_Paris/IA_listings_com_iris_2015_2023.csv", 
                 encoding = "UTF-8",
                 na.strings = c("N/A", "", "NA"),
                 colClasses = col_types)

IAParis$license_Nett <- str_extract_all(IAParis$license, "\\b\\d{13}\\b")
IAParis$license_Nett <- sapply(IAParis$license_Nett, paste, collapse = ", ")

IAParis <- IAParis %>% #transformation des modalites vides en NA
  mutate(
    license_Nett = if_else(
      license_Nett == "",
      NA_character_,
      license_Nett
    )
  )

IAParis <- IAParis %>%
  mutate(
    license = if_else(
      license == "",
      NA_character_,
      license  
    )
  )

combinationsParis <- IAParis %>% 
  filter(str_starts(last_scraped, "2022")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(str_starts(J_C_POSTAL, "75")) %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  distinct(id, license_Nett)

Tab_license_Air_Paris22 <- combinationsParis %>%
  group_by(license_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_Air_Paris22 <- Tab_license_Air_Paris22 %>%
  rename(Numero_enregistr_Nett = license_Nett)

#On retire des analyses les num qui sont trop explicitement faux et donc utilisés potentiellement par plusieurs loueurs
num_a_retirer <- c(0000000000000,1234567890123,1234567891011,1234567891234,7500000000000,7512345678900,7512345678901,7512345678910,7512345678912,9212345678912,9312345678912,9412345678912)

Tab_license_Air_Paris22 <- Tab_license_Air_Paris22 %>%
  filter(!Numero_enregistr_Nett %in% num_a_retirer)

Tab_license_Air_Paris22 <- Tab_license_Air_Paris22 %>%
  rename(license_dupli_air_count = nb_id_associe)

#On colle les nouvelles infos sur les duplications de num d'enregist à IA_Paris_LE_2022_uniq (temporaire)
IA_Paris_LE_2022_uniq <- IAParis %>% 
  filter(str_starts(last_scraped, "2022")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(str_starts(J_C_POSTAL, "75")) %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  distinct(id, .keep_all = TRUE)

IA_Paris_LE_2022_uniq <- IA_Paris_LE_2022_uniq %>% 
  left_join(Tab_license_Air_Paris22, by = c("license_Nett" = "Numero_enregistr_Nett"))

#Décompte des doublons intra dans IA_Paris_LE_2022_uniq
freq(IA_Paris_LE_2022_uniq$license_dupli_air_count)

#On teste de comparer d'autres variables dans la comparaison des num d'enregistrement
IA_Paris_LE_2022_uniq <- IA_Paris_LE_2022_uniq %>%
  group_by(license_Nett, accommodates, C_IR) %>%
  mutate(verif_doubl = ifelse(is.na(license_Nett), 
                              NA,  # Si license_Nett est NA, mettre NA
                              ifelse(n() > 1, paste(id, collapse = ";"), NA))) %>%
  ungroup()

IA_Paris_LE_2022_uniq <- IA_Paris_LE_2022_uniq %>%
  group_by(verif_doubl) %>% # Grouper par "verif_doubl"
  mutate(license_dupli_air_count_verif = ifelse(is.na(verif_doubl), 
                                                NA,  # Si "verif_doubl" est NA, mettre NA
                                                n())) %>% # Sinon compter les lignes dans chaque groupe
  ungroup()

freq(IA_Paris_LE_2022_uniq$license_dupli_air_count_verif)
sum(!is.na(IA_Paris_LE_2022_uniq$license_dupli_air_count_verif))

#Brève analyse des caractéristiques des annonces doublonnées dans IA_Paris_LE_2022_uniq (a poursuivre, analyse pas mise dans la these)
doub_Paris_22 <- IA_Paris_LE_2022_uniq %>% filter(!is.na(license_dupli_air_count_verif))

freq(IA_Paris_LE_2022_uniq$J_L_CO)
freq(doub_Paris_22$J_L_CO)

summary(IA_Paris_LE_2022_uniq$number_of_reviews_ltm)
summary(doub_Paris_22$number_of_reviews_ltm)

freq(IA_Paris_LE_2022_uniq$number_of_reviews_ltm)
freq(doub_Paris_22$number_of_reviews_ltm)

summary(IA_Paris_LE_2022_uniq$review_scores_rating)
summary(doub_Paris_22$review_scores_rating)

#Creation de la table de numero d'enregistrement pour IA PARIS 2023, de janv à décembre 2023 - 
combinationsParis23 <- IAParis %>% 
  filter(str_starts(last_scraped, "2023")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(str_starts(J_C_POSTAL, "75")) %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  distinct(id, license_Nett)

Tab_license_Air_Paris23 <- combinationsParis23 %>%
  group_by(license_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_Air_Paris23 <- Tab_license_Air_Paris23 %>%
  rename(Numero_enregistr_Nett = license_Nett)

#On retire des analyses les num qui sont trop explicitement faux et donc utilisés potentiellement par plusieurs loueurs
Tab_license_Air_Paris23 <- Tab_license_Air_Paris23 %>%
  filter(!Numero_enregistr_Nett %in% num_a_retirer)

Tab_license_Air_Paris23 <- Tab_license_Air_Paris23 %>%
  rename(license_dupli_air_count = nb_id_associe)

#On colle les nouvelles infos sur les duplications de num d'enregist à IA_MGP_LE_2023_uniq (temporaire)
IA_Paris_LE_2023_uniq <- IAParis %>% 
  filter(str_starts(last_scraped, "2023")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(str_starts(J_C_POSTAL, "75")) %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  distinct(id, .keep_all = TRUE)

IA_Paris_LE_2023_uniq <- IA_Paris_LE_2023_uniq %>% 
  left_join(Tab_license_Air_Paris23, by = c("license_Nett" = "Numero_enregistr_Nett"))

#Décompte des doublons intra dans IA_Paris_LE_2023_uniq
freq(IA_Paris_LE_2023_uniq$license_dupli_air_count)

#On teste de comparer d'autres variables dans la comparaison des num d'enregistrement
IA_Paris_LE_2023_uniq <- IA_Paris_LE_2023_uniq %>%
  group_by(license_Nett, accommodates, C_IR) %>%
  mutate(verif_doubl = ifelse(is.na(license_Nett), 
                              NA,  # Si license_Nett est NA, mettre NA
                              ifelse(n() > 1, paste(id, collapse = ";"), NA))) %>%
  ungroup()

IA_Paris_LE_2023_uniq <- IA_Paris_LE_2023_uniq %>%
  group_by(verif_doubl) %>% # Grouper par "verif_doubl"
  mutate(license_dupli_air_count_verif = ifelse(is.na(verif_doubl), 
                                                NA,  # Si "verif_doubl" est NA, mettre NA
                                                n())) %>% # Sinon compter les lignes dans chaque groupe
  ungroup()

freq(IA_Paris_LE_2023_uniq$license_dupli_air_count_verif)
sum(!is.na(IA_Paris_LE_2023_uniq$license_dupli_air_count_verif))

#Creation de la table de numero d'enregistrement pour IA PARIS 2021, de janv à décembre 2021 - 
combinationsParis21 <- IAParis %>% 
  filter(str_starts(last_scraped, "2021")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(str_starts(J_C_POSTAL, "75")) %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  distinct(id, license_Nett)

Tab_license_Air_Paris21 <- combinationsParis21 %>%
  group_by(license_Nett) %>%
  summarise(nb_id_associe = n()) %>%
  ungroup()

Tab_license_Air_Paris21 <- Tab_license_Air_Paris21 %>%
  rename(Numero_enregistr_Nett = license_Nett)

#On retire des analyses les num qui sont trop explicitement faux et donc utilisés potentiellement par plusieurs loueurs
Tab_license_Air_Paris21 <- Tab_license_Air_Paris21 %>%
  filter(!Numero_enregistr_Nett %in% num_a_retirer)

Tab_license_Air_Paris21 <- Tab_license_Air_Paris21 %>%
  rename(license_dupli_air_count = nb_id_associe)

#On colle les nouvelles infos sur les duplications de num d'enregist à IA_MGP_LE_2023_uniq (temporaire)
IA_Paris_LE_2021_uniq <- IAParis %>% 
  filter(str_starts(last_scraped, "2021")) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(str_starts(J_C_POSTAL, "75")) %>%
  filter(!is.na(license_Nett)) %>%
  filter(!license_Nett==""|license_Nett==" ") %>%
  distinct(id, .keep_all = TRUE)

IA_Paris_LE_2021_uniq <- IA_Paris_LE_2021_uniq %>% 
  left_join(Tab_license_Air_Paris21, by = c("license_Nett" = "Numero_enregistr_Nett"))

#Décompte des doublons intra dans IA_Paris_LE_2021_uniq
freq(IA_Paris_LE_2021_uniq$license_dupli_air_count)

#On teste de comparer d'autres variables dans la comparaison des num d'enregistrement
IA_Paris_LE_2021_uniq <- IA_Paris_LE_2021_uniq %>%
  group_by(license_Nett, accommodates, C_IR) %>%
  mutate(verif_doubl = ifelse(is.na(license_Nett), 
                              NA,  # Si license_Nett est NA, mettre NA
                              ifelse(n() > 1, paste(id, collapse = ";"), NA))) %>%
  ungroup()

IA_Paris_LE_2021_uniq <- IA_Paris_LE_2021_uniq %>%
  group_by(verif_doubl) %>% # Grouper par "verif_doubl"
  mutate(license_dupli_air_count_verif = ifelse(is.na(verif_doubl), 
                                                NA,  # Si "verif_doubl" est NA, mettre NA
                                                n())) %>% # Sinon compter les lignes dans chaque groupe
  ungroup()

freq(IA_Paris_LE_2021_uniq$license_dupli_air_count_verif)
sum(!is.na(IA_Paris_LE_2021_uniq$license_dupli_air_count_verif))

rm(IAParis, IA_Paris_LE_2023_uniq, IA_Paris_LE_2021_uniq, IA_Paris_LE_2022_uniq, Tab_license_Air_Paris22, combinationsParis)
rm(combinations23, combinationsParis23, doub_Paris_22, Tab_license_Air_Paris23, combinationsParis21, Tab_license_Air_Paris21)
rm(Tab_license_20200127, Tab_license_20200420, Tab_license_20200928, Tab_license_Abr, Tab_license_Abr_20200127, Tab_license_Air, Tab_license_Air_202002, Tab_license_Trip_20200420, Tab_license_Trip, Tab_license_Boo_20200127, Tab_license_Boo, Tab_license_Air_2023)

##Travail sur les annonces hôtelieres et autres types d'annonces qui ne seraient pas des STR ----
#Annonces type coworking etc : au final ce sont tout de même des locations meublées dans la plupart des cas, mais qui peuvent servir de showrooms
test_usages <- c("showroom","coworking", "salle de réunion")

for (i in 1:length(test_usages)) { 
  if (i == 1) { 
    # Initialiser la colonne lors de la première itération
    IA_MGP18_23 <- IA_MGP18_23 %>%
      mutate(autre_usage = grepl(test_usages[i], description))
  } else { 
    # Ajouter les résultats des itérations suivantes avec "|"
    IA_MGP18_23 <- IA_MGP18_23 %>%
      mutate(autre_usage = autre_usage | grepl(test_usages[i], description))
  }
}

freq(IA_MGP18_23$autre_usage)
test <- IA_MGP18_23 %>% filter(autre_usage==TRUE)
freq(test$last_scraped_YM)
freq(IA_MGP18_23$property_type)

#Et la property_type "Other" mêle plein de choses, pas du tout que des locaux étant autre chose que des meublés
test <- IA_MGP18_23 %>%
  filter(property_type=="Other") %>%
  filter(!room_type=="Hotel room")  
  
IA_MGP18_23 <- IA_MGP18_23 %>% select(-autre_usage) #on supprime la variable car pas très utile

#Annonces "hotel_room" avec numéros d'enregistrement : on ne les rappatrie pas car il y en a trop peu et que c'est dur de savoir dans quel "room_type" les mettre (au final dans l'estimation des annonces non occasionnelles on les prend dans la cohorte)
test <- IA_MGP18_23 %>%
  filter(room_type=="Hotel room") %>% 
#  filter(!host_id=="98715148") %>%
  filter(!is.na(license_Nett)) %>%
  select(ID_URL, last_scraped_YM, name, description, bedrooms , property_type, license, license_Nett, host_id, host_name)

freq(test$last_scraped_YM)

#A l'inverse les "exempt - hotel-type listing" dans license sont-ils des hôtels ?
Tab_RT_hotels <- IA_MGP18_23 %>%
  group_by(last_scraped_YM) %>%
  summarise(nb_ann = n(),
            nb_LE = sum(room_type=="Entire home/apt"),
            nb_hotel = sum(room_type=="Hotel room"),
            hotel_pct = nb_hotel/nb_ann*100,
            nb_hotel_ac_license = sum(room_type=="Hotel room" & !is.na(license_Nett)), 
            nb_LE_license_exempthotel = sum(room_type=="Entire home/apt" & license=="Exempt - hotel-type listing", na.rm =TRUE),
            nb_LE_license_exempthotel_pctLE = nb_LE_license_exempthotel/sum(room_type=="Entire home/apt")*100,
            nb_LE_license_exempthotel_pct = nb_LE_license_exempthotel/nb_ann*100)

Tab_RT_hotels <- Tab_RT_hotels %>%
  mutate(hotel_pct= round(hotel_pct, 2),
         nb_LE_license_exempthotel_pctLE= round(nb_LE_license_exempthotel_pctLE, 2),
         nb_LE_license_exempthotel_pct=round(nb_LE_license_exempthotel_pct, 2))

freq(test$last_scraped_YM)

sampled_indices <- sample(nrow(test), 20) 
test <- test[sampled_indices, ] #on a regardé pour 20 d'entre elles et la réponse est : non ce ne sont pas des hotels sur cet échantillon, même pas un

#regarder dans property_type le nombre d'hotels potentiels qu'on a dans "entire home" : au final vraiment quasi rien
test_hotel <- c("Aparthotel","Boutique hotel", "Guest suite", "Room in aparthotel", "Room in boutique hotel")

test <- IA_MGP18_23 %>%
  filter(!last_scraped_YM=="2018-04") %>%
  #filter(room_type == "Entire home/apt") %>%
  #filter(room_type == "Private room") %>%
  filter(str_detect(property_type, str_c(test_hotel, collapse = "|"))) %>%
  distinct(host_id, .keep_all = TRUE) %>%
  select(ID_uniq, room_type, host_url, host_id, host_name, host_about, license, number_of_reviews_ltm)

freq(test$last_scraped_YM)
freq(test$room_type)
freq(test$property_type)

#Ici on a testé les différents noms d'hotels ou de résidence de tourisme qu'on trouvait sur le site des OT de la MGP et à chaque fois on mis les noms qui en rapportait dans un vecteur "hotelresid"
test <- IA_MGP18_23 %>%
  filter(!room_type=="Hotel room") %>%
  filter(grepl("Adagio", host_name)) %>%
  select(listing_url, last_scraped_YM, host_id, host_name, description, room_type, hotelRdT,  license, C_CAINSEE, host_total_listings_count)

hotelresid <- c("Adagio","ADAGIO", "\\bCitadines\\b", "St Christophers Inn", "Holiday Inn", "Tulip Residences", "Odalys", "\\bNemea\\b", "\\bReside\\b", "Residhome", "Residhome", "Séjours & Affaires", "\\bRh\\b", "Sejours & Affaires", "Sejours Affaires", "Generator", "JO&JOE Paris Gentilly", "Jo&Joe Paris - Nation", "Smartplace Hostel", "Vintage Paris", "Arty Paris", "Les Piaules", "The People Hostel", "Oops Hostel")

#On en créé une nouvelle colonne dans IA_MGP18_23, hotelRdT, qui contient en TRUE les annonces de résidences de tourisme qui ont le statut et sont donc pas des STR comme les autres
for (i in 1:length(hotelresid)) { 
  if (i == 1) { 
    # Initialiser la colonne lors de la première itération
    IA_MGP18_23 <- IA_MGP18_23 %>%
      mutate(hotelRdT = grepl(hotelresid[i], host_name))
  } else { 
    # Ajouter les résultats des itérations suivantes
    IA_MGP18_23 <- IA_MGP18_23 %>%
      mutate(hotelRdT = hotelRdT | grepl(hotelresid[i], host_name))
  }
}

hotelresid_id <- IA_MGP18_23 %>%
  filter(hotelRdT==TRUE) %>%
  distinct(host_id) #ces étapes et les 2 qui suivent permet de transformer la recherche par nom en un vecteur dans lequel on peut ajouter des "host_id", de cette façon on peut facilement en ajouter si besoin

hotelresid_id <- hotelresid_id$host_id
hotelresid_id <- c(hotelresid_id, "374533732", "36972647", "422830837", "425992457") #36972647 / 422830837 / 425992457 correspondent au groupe "Twenty Business Flats" qui propose des résidences étudiantes. On les considère de la mm manière que des hôtels mais cela pose questions.

IA_MGP18_23$hotelRdT <- ifelse(IA_MGP18_23$host_id %in% hotelresid_id, TRUE , FALSE) #là on a la colonne contenant les loueurs concernés
freq(IA_MGP18_23$hotelRdT)

test <- IA_MGP18_23 %>%
  filter(hotelRdT==TRUE) %>%
  filter(!room_type=="Hotel room") %>%
  select(ID_URL, last_scraped_YM, room_type, host_id, host_name, number_of_reviews_ltm, number_of_reviews)

freq(test$last_scraped_YM)

df_hotelRdT <- IA_MGP18_23 %>% 
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annonces = n(),
    nb_ann_HH = sum(!room_type =="Hotel room"),
    nb_ann_hotelRdT = sum(hotelRdT==TRUE),
    nb_ann_hotelRdT_pct = nb_ann_hotelRdT/nb_annonces*100,
    nb_ann_hotelRdT_pctHH = nb_ann_hotelRdT/nb_ann_HH*100)

rm(df_hotelRdT)

#Annonces issues de résidences de tourisme non classées (rnc), et donc potentiellement STR pro 

Rnc <- c("344541100", "569192973", "370674305", "404875280", "413841392", "268502766", "404647748", "449978239", "515934127", "147018685", "193463287", "166531476") #résidences de tourisme non classees

test <- IA_MGP18_23 %>%
  filter(hotelRdT==FALSE) %>%
  filter(room_type=="Entire home/apt") %>%
  filter(host_id %in% Rnc) %>%
  #filter(last_scraped_YM=="2023-02") %>%
  #filter(sup1an_scrpg==TRUE) %>%
  select(ID_URL, last_scraped_YM, host_id, host_name, number_of_reviews_ltm, number_of_reviews, license, license_Nett, availability_365, host_acceptance_rateNETT)

freq(test$number_of_reviews_ltm)
summary(test$number_of_reviews_ltm)

df_Rnc <- test %>% 
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annonces = n())

test <- test %>%
  filter(number_of_reviews_ltm==0)

testid <- test$ID_URL

test2 <- IA_MGP18_23 %>%
  filter(ID_URL %in% testid) %>%
  select(ID_URL, last_scraped_YM, host_id, host_name, number_of_reviews_ltm, number_of_reviews, license, license_Nett, availability_365, host_acceptance_rateNETT)
#interessant ça donne envie de créer une variable "first_avail365" qui compterait le nb de jours ouverts la première fois que l'annonce a été croisée car on voit que chez les ann louees a l'annee souvent ça descend au fil du temps

##Creation variable first_avail365 qui contient le nombre de jours proposés lors de la première observation ----
first_avail <- IA_MGP18_23 %>%
  arrange(last_scraped) %>%
  group_by (ID_URL) %>% 
  summarise (first_avail365 = first(availability_365))

IA_MGP18_23 <- IA_MGP18_23 %>%
  left_join(first_avail, by = "ID_URL")
#on n'en a rien fait finalement mais piste a creuser a l avenir

#Part d'ann instant_bookable au cours du temps ----
freq(IA_MGP18_23$instant_bookable)

Tab_evol_IB <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    nb_ann_IB = sum(instant_bookable=="t"),
    nb_ann_IB_pct = nb_ann_IB/nb_annoncesLE*100)


Tab_evol_IB_Paris <- IA_Paris %>%
  filter(room_type =="Entire home/apt") %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    nb_ann_IB = sum(instant_bookable=="t"),
    nb_ann_IB_pct = nb_ann_IB/nb_annoncesLE*100)


##Reflexion sur les ann. non occasionnelles, estimation multi criteres et correlations ----
#HF = haute frequence car longtemps on a appele comme tel les ann. non occasionnelles

Tab_HF202302 <- IA_MGP18_23 %>%
  group_by(last_scraped_YM) %>%
  summarise(nb_ann = n(),
            nb_hotel_licenseNA = sum(room_type=="Hotel room" & is.na(license_Nett)),
            nb_hotel_licenseNA_pct = round(nb_hotel_licenseNA/nb_ann*100, 2),
            nb_hotel_ac_license = sum(room_type=="Hotel room" & !is.na(license_Nett)), 
            nb_hotel_ac_license_pct = round(nb_hotel_ac_license/nb_ann*100, 2),
            nb_LE_hotelRdT = sum(room_type=="Entire home/apt" & hotelRdT==TRUE),
            nb_LE_hotelRdT_pct = round(nb_LE_hotelRdT/nb_ann*100, 2),
            nb_PR = sum(room_type=="Private room"),
            nb_PR_pct = round(nb_PR/nb_ann*100, 2),
            nb_SR = sum(room_type=="Shared room"),
            nb_SR_pct = round(nb_SR/nb_ann*100, 2),
            nb_LE_bloK = sum(bloK_Air == TRUE & hotelRdT==FALSE),
            nb_LE_bloK_pct = round(nb_LE_bloK/nb_ann*100, 2),
            nb_LE_bailmob = sum(room_type=="Entire home/apt" & bail_mob == TRUE & hotelRdT==FALSE),   #car il y a des bail mob en SR et PR
            nb_LE_bailmob_pct = round(nb_LE_bailmob/nb_ann*100, 2),
            nb_LE_mini28j = sum(room_type=="Entire home/apt" & hotelRdT==FALSE & bail_mob == FALSE & minimum_nights >27 & bloK_Air == FALSE), #ça sert à regarder la proportion d'ann. potentiellement de type bail mob quand ce statut n'existait pas dans Airbnb
            nb_LE_mini28j_pct = round(nb_LE_mini28j/nb_ann*100, 2),
            nb_LE_inact_s1an_nonbloK = sum(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights < 28 & sup1an_scrpg ==TRUE & number_of_reviews_ltm == 0),
            nb_LE_inact_s1an_nonbloK_pct = round(nb_LE_inact_s1an_nonbloK/nb_ann*100, 2),
            nb_LE_inf1an = sum(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & sup1an_scrpg ==FALSE & minimum_nights < 28),
            nb_LE_inf1an_pct = round(nb_LE_inf1an/nb_ann*100, 2),
            nb_LE_activ_s1an = sum(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & sup1an_scrpg ==TRUE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28),
            nb_LE_activ_s1an_pct = round(nb_LE_activ_s1an/nb_ann*100, 2),
            tot_categ = nb_hotel_licenseNA + nb_hotel_ac_license + nb_LE_hotelRdT + nb_PR + nb_SR + nb_LE_bloK + nb_LE_bailmob +  nb_LE_mini28j + nb_LE_inact_s1an_nonbloK + nb_LE_inf1an + nb_LE_activ_s1an,
            diff_nb_ann.tot_categ = nb_ann - tot_categ)

Tab_HF202302_graph <- IA_MGP18_23 %>%
  group_by(last_scraped_YM) %>%
  summarise(nb_ann = n(),
            nb_hotel_licenseNA = sum(room_type=="Hotel room" & is.na(license_Nett)),
            nb_LE_hotelRdT = sum(room_type=="Entire home/apt" & hotelRdT==TRUE),
            nb_total_hotelRdT = nb_hotel_licenseNA + nb_LE_hotelRdT,
            nb_hotel_ac_license = sum(room_type=="Hotel room" & !is.na(license_Nett)), 
            nb_PR = sum(room_type=="Private room"),
            nb_SR = sum(room_type=="Shared room"),
            nb_PRetSR = nb_PR + nb_SR, 
            nb_LE_bloK = sum(bloK_Air == TRUE & hotelRdT==FALSE),
            nb_LE_bailmob = sum(room_type=="Entire home/apt" & bail_mob == TRUE & hotelRdT==FALSE & bloK_Air == FALSE),   #car il y a des bail mob en SR et PR
            nb_LE_mini28j = sum(room_type=="Entire home/apt" & bail_mob == FALSE & hotelRdT==FALSE & minimum_nights >27 & bloK_Air == FALSE), #ça sert à regarder la proportion d'ann. potentiellement de type bail mob quand ce statut n'existait pas dans Airbnb
            nb_total_mob_moy = nb_LE_bailmob+nb_LE_mini28j,
            nb_LE_inact_inf1an_nonbloK = sum(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights < 28 & sup1an_scrpg ==FALSE & number_of_reviews_ltm == 0),
            nb_LE_inact_s1an_nonbloK = sum(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights < 28 & sup1an_scrpg ==TRUE & number_of_reviews_ltm == 0),
            nb_LE_activ_inf1an = sum(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg ==FALSE & number_of_reviews_ltm > 0),
            nb_LE_activ_s1an = sum(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg ==TRUE & number_of_reviews_ltm > 0),
            tot_categ = 0,
            diff_nb_ann.tot_categ = nb_ann - tot_categ)

freq(test$room_type)

HF_202302 <- IA_MGP18_23 %>%
  filter(last_scraped_YM =="2023-02") %>%
  filter(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & sup1an_scrpg ==TRUE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28)

HF_202302 <- HF_202302 %>%
  filter(!ID_URL =="568093760836051568") #on enlève l'annonce "568093760836051568" qui est en fait un hotel (mais difficile à enlever car société Sweett qui a de multiples appart en STR + 1 seul vrai hotel, celui-ci)

freq(HF_202302$minimum_nights) #avant qu'on enlève les plu de 27 jours on avait 96,0% des ann avec minstay inf ou égal à 10 jours

HF_202001 <- IA_MGP18_23 %>%
  filter(last_scraped_YM =="2020-02") %>%
  filter(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & sup1an_scrpg ==TRUE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28)

freq(HF_202001$minimum_nights) #a peu près pareil mais un peu moins d'ann avec minstay inf ou égal à 10j donc ça peut provenir du fait qu'il y a des ann en bail mobilité

#On cree categ spé des baux mobilité + de la longue durée (non bloK, peut importe la date de création de l'annonce/l'age de l'ann.)
HF_202302_bailmob <- IA_MGP18_23 %>%
  filter(last_scraped_YM =="2023-02") %>%
  filter(room_type=="Entire home/apt" & bail_mob == TRUE &  bloK_Air == FALSE & hotelRdT==FALSE)

HF_202302_LT <- IA_MGP18_23 %>%
  filter(last_scraped_YM =="2023-02") %>%
  filter(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights >27 )

HF_202302_bailmobLT <- bind_rows (HF_202302_bailmob,HF_202302_LT)

rm(HF_202302_bailmob, HF_202302_LT)

#Essai de correlation entre avail_365 et reviews_LTM
#Nuage de points
HF_202302 %>%
  ggplot(aes(x=availability_365, y=number_of_reviews_ltm)) +
  geom_point(color = "#009F81", alpha = 0.1) +
  theme_minimal() +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Nombre de commentaires au cours des 12 mois derniers \nen fonction des nuitées disponibles à la réservation", x ="Nombre de nuitées disponibles \nau cours des 12 mois suivants", y = "Nombre de commentaires reçus \nau cours des 12 derniers mois")

#Coeff de correlation
cor(HF_202302$availability_365,HF_202302$number_of_reviews_ltm, use = "complete.obs") #egal à 0,20 donc pas énorme mais tout de même, le sens est intéressant

#Travail sur reviews_LTM_ann sup 1 an en ancienneté

df_summary_revLTM_LE <- IA_MGP18_23 %>% 
  filter(room_type =="Entire home/apt") %>% 
  filter(sup1an_scrpg =="TRUE") %>%
  filter(bail_mob =="FALSE") %>%
  filter(bloK_Air =="FALSE") %>%
  filter(hotelRdT =="FALSE") %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    zer_rev = sum(number_of_reviews == 0, na.rm = TRUE),
    zer_rev_pct = zer_rev/nb_annoncesLE*100,
    zer_revLTM = sum(number_of_reviews_ltm == 0, na.rm = TRUE),
    zer_revLTM_pct = zer_revLTM/nb_annoncesLE*100,
    Min_ltm = min(number_of_reviews_ltm, na.rm = TRUE),
    Q1_ltm = quantile(number_of_reviews_ltm, 0.25, na.rm = TRUE),
    Median_ltm = median(number_of_reviews_ltm, na.rm = TRUE),
    Mean_ltm = mean(number_of_reviews_ltm, na.rm = TRUE),
    Q3_ltm = quantile(number_of_reviews_ltm, 0.75, na.rm = TRUE),
    Max_ltm = max(number_of_reviews_ltm, na.rm = TRUE),
  ) %>%
  arrange(last_scraped_YM)  # Trier par date

df_summary_revLTM_LE_sup0revLTM <- IA_MGP18_23 %>% 
  filter(room_type =="Entire home/apt") %>% 
  filter(sup1an_scrpg =="TRUE") %>%
  filter(bail_mob =="FALSE") %>%
  filter(bloK_Air =="FALSE") %>%
  filter(hotelRdT =="FALSE") %>%
  filter(number_of_reviews_ltm>0) %>% 
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    Min_ltm = min(number_of_reviews_ltm, na.rm = TRUE),
    Q1_ltm = quantile(number_of_reviews_ltm, 0.25, na.rm = TRUE),
    Median_ltm = median(number_of_reviews_ltm, na.rm = TRUE),
    Mean_ltm = mean(number_of_reviews_ltm, na.rm = TRUE),
    Q3_ltm = quantile(number_of_reviews_ltm, 0.75, na.rm = TRUE),
    Max_ltm = max(number_of_reviews_ltm, na.rm = TRUE),
  ) %>%
  arrange(last_scraped_YM) %>%
  mutate(last_scraped_YM = paste(last_scraped_YM, "_sup0revLTM"))

df_summary_revLTM_LE_sup1an <- bind_rows (df_summary_revLTM_LE,df_summary_revLTM_LE_sup0revLTM)
  
df_summary_revLTM_LE_sup1an <- df_summary_revLTM_LE_sup1an %>%
  mutate(zer_rev_pct= round(zer_rev_pct, 2),
         zer_revLTM_pct= round(zer_revLTM_pct, 2), 
         Median_ltm= round(Median_ltm, 2),
         Mean_ltm=round(Mean_ltm, 2))

#Montrer le profil de reviewsLTM (=nombre de commentaires sur les last twelve months -LTM) dans la MGP - fev 2023
ggplot(HF_202302, aes(x = number_of_reviews_ltm)) +
  geom_histogram(binwidth = 1, fill = "#ECA72C", color = "white") +
  labs(
    title = "Histogramme du nombre de commentaires sur 12 mois dans la MGP \n- février 2023",
    x = "Nombre de commentaires",
    y = "Nombre d'annonces"
  ) +
  ylim(0, 2500) +
  xlim(-10, 200) +
  theme_minimal()

ggplot(HF_202302, aes(x = number_of_reviews_ltm)) +
  geom_histogram(aes(y = ..count../sum(..count..)*100), binwidth = 1, fill = "#ECA72C", color = "white") +
  labs(
    title = "Histogramme du nombre de commentaires sur 12 mois dans la MGP \n- février 2023",
    x = "Nombre de commentaires",
    y = "Pourcentage d'annonces (%)"
  ) +
  ylim(0, 10) +  # Ajuste en fonction de la distribution
  xlim(-10, 200) +
  theme_minimal()


#Montrer profil de reviewsLTM dans la MGP - janv2020
ggplot(HF_202001, aes(x = number_of_reviews_ltm)) +
  geom_histogram(binwidth = 1, fill = "#009F81", color = "white") +
  labs(
    title = "Histogramme du nombre de commentaires sur 12 mois dans la MGP \n- janvier 2020",
    x = "Nombre de commentaires",
    y = "Nombre d'annonces"
  ) +
  ylim(0, 2500) +
  xlim(-10, 200) +
  theme_minimal()

ggplot(HF_202001, aes(x = number_of_reviews_ltm)) +
  geom_histogram(aes(y = ..count../sum(..count..)*100), binwidth = 1, fill = "#009F81", color = "white") +
  labs(
    title = "Histogramme du nombre de commentaires sur 12 mois dans la MGP \n- janvier 2020",
    x = "Nombre de commentaires",
    y = "Pourcentage d'annonces (%)"
  ) +
  ylim(0, 10) +  # Ajuste en fonction de la distribution
  xlim(-10, 200) +
  theme_minimal()

##reviews_LTM_categ : 
summary(HF_202302$number_of_reviews_ltm)
summary(HF_202001$number_of_reviews_ltm)

HF_202001$number_of_reviews_ltm_categ <- cut(HF_202001$number_of_reviews_ltm, c(0, 3.9999, 6.9999, 10.9999, 22.9999, 30.9999, 200), right=FALSE, include.lowest = TRUE, labels = c("1 à 3 commentaires", "de 4 à 6 commentaires", "de 7 à 10 commentaires", "de 11 à 22 commentaires", "de 23 à 30 commentaires", "Plus de 30 commentaires"))
HF_202302$number_of_reviews_ltm_categ <- cut(HF_202302$number_of_reviews_ltm, c(0, 3.9999, 6.9999, 10.9999, 22.9999, 30.9999, 200), right=FALSE, include.lowest = TRUE, labels = c("1 à 3 commentaires", "de 4 à 6 commentaires", "de 7 à 10 commentaires", "de 11 à 22 commentaires", "de 23 à 30 commentaires", "Plus de 30 commentaires"))

freq(HF_202001$number_of_reviews_ltm_categ)
freq(HF_202302$number_of_reviews_ltm_categ)

###corrélation également entre reviewsLTM et acceptance_rate sur MGP
freq(HF_202302$number_of_reviews_ltm)
freq(HF_202302$host_acceptance_rateNETT)

#Nuage de points
HF_202302 %>%
  ggplot(aes(x=host_acceptance_rateNETT, y=number_of_reviews_ltm)) +
  geom_point(color = "#009F81", alpha = 0.1) +
  theme_minimal() +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Nombre de commentaires au cours des 12 mois derniers \nen fonction du taux d'acceptation des réservations", x ="Taux d acceptation des réservations", y = "Nombre de commentaires reçus \nau cours des 12 derniers mois")

#Coeff de correlation
cor(HF_202302$host_acceptance_rateNETT,HF_202302$number_of_reviews_ltm, use = "complete.obs")

###corrélation également entre response_rate et acceptance_rate sur MGP

freq(HF_202302$host_response_rateNETT)
freq(HF_202302$host_acceptance_rateNETT)

#Nuage de points
HF_202302 %>%
  ggplot(aes(x=host_response_rateNETT, y=host_acceptance_rateNETT)) +
  geom_point(color = "#009F81", alpha = 0.1) +
  theme_minimal() +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Taux d'acceptation des réservations \nen fonction du taux de réponse aux messages", x ="Taux de réponse aux messages", y = "Taux d acceptation des réservations")

#Coeff de correlation
cor(HF_202302$host_response_rateNETT,HF_202302$host_acceptance_rateNETT, use = "complete.obs") #0,37 de coeff de corrélation

#Quantiles de number_of_reviews_LTM
test <- summary(IA_MGP18_23$number_of_reviews_ltm)

names(test)

deciles_rev_LTM <- IA_MGP18_23 %>%
  filter(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & sup1an_scrpg ==TRUE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28) %>%
  group_by(last_scraped_YM) %>%
  summarise(
    Effectif = n(),
    Min. = min(number_of_reviews_ltm, na.rm = TRUE),
    '10%' = quantile(number_of_reviews_ltm, 0.10, na.rm = TRUE),
    '20%' = quantile(number_of_reviews_ltm, 0.20, na.rm = TRUE),
    'Q1-25%' = quantile(number_of_reviews_ltm, 0.25, na.rm = TRUE),
    '30%' = quantile(number_of_reviews_ltm, 0.30, na.rm = TRUE),
    '40%' = quantile(number_of_reviews_ltm, 0.40, na.rm = TRUE),
    Med. = quantile(number_of_reviews_ltm, 0.50, na.rm = TRUE),  # médiane
    '60%' = quantile(number_of_reviews_ltm, 0.60, na.rm = TRUE),
    '70%' = quantile(number_of_reviews_ltm, 0.70, na.rm = TRUE),
    'Q3-75%' = quantile(number_of_reviews_ltm, 0.75, na.rm = TRUE),
    '80%' = quantile(number_of_reviews_ltm, 0.80, na.rm = TRUE),
    '90%' = quantile(number_of_reviews_ltm, 0.90, na.rm = TRUE),
    Moy. = mean(number_of_reviews_ltm, na.rm = TRUE)
  )

deciles_rev_LTM_maj <- deciles_rev_LTM %>%
  gt(rowname_col = "last_scraped_YM") %>%
  tab_header(title = md("**Distribution du nombre de commentaires reçus au cours des 12 derniers mois**")) %>%
  tab_stubhead(label = "Date de collecte") %>%
  tab_source_note(source_note = "Source : InsideAirbnb MGP, réalisation J. Richon sous R, 2025 ") %>%
  tab_footnote(
    footnote = "Lecture : 30% des annonces (troisième décile), ont reçu 7 commentaires ou plus au cours de l'année précédente.",
  ) %>%
  fmt_number(columns = 15, decimals = 1) %>% # Col. moyenne avec 1 chiffre après la virgule
  tab_options(footnotes.font.size = 11)

deciles_rev_LTM_maj

deciles_rev_LTM_totanc <- IA_MGP18_23 %>%
  filter(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28) %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_ann = n(),
    d0 = min(number_of_reviews_ltm, na.rm = TRUE),
    d10 = quantile(number_of_reviews_ltm, 0.10, na.rm = TRUE),
    d20 = quantile(number_of_reviews_ltm, 0.20, na.rm = TRUE),
    d30 = quantile(number_of_reviews_ltm, 0.30, na.rm = TRUE),
    d40 = quantile(number_of_reviews_ltm, 0.40, na.rm = TRUE),
    d50 = quantile(number_of_reviews_ltm, 0.50, na.rm = TRUE),  # médiane
    d60 = quantile(number_of_reviews_ltm, 0.60, na.rm = TRUE),
    d70 = quantile(number_of_reviews_ltm, 0.70, na.rm = TRUE),
    d80 = quantile(number_of_reviews_ltm, 0.80, na.rm = TRUE),
    d90 = quantile(number_of_reviews_ltm, 0.90, na.rm = TRUE),
    d100 = max(number_of_reviews_ltm, na.rm = TRUE)
  )


###ACM pour tenter d'estimer la haute freq ----
#on transforme plusieurs variables quanti en quali afin de tester une ACM pour février 2023 dans la MGP
#de nombreux morceaux de code de cette section sont issus des scripts d'Aliette Roux
d_acm <- HF_202302

#Categ host_acceptance : msie en classe de la variable host_acceptance
freq(d_acm$host_acceptance_rateNETT)
summary(d_acm$host_acceptance_rateNETT)

d_acm %>% ggplot(aes(x=host_acceptance_rateNETT))+ geom_bar(stat="count")+
theme_minimal() +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Histogramme du taux d'acceptation des réservations \nen février 2023", x ="Taux d'acceptation des réservations", y = "Nombre d'annonces")

d_acm$host_acceptance_rate_categ <- cut(d_acm$host_acceptance_rateNETT, c(0, 49.9999, 74.99999, 94.9999, 100.9999), right=FALSE, include.lowest = TRUE, labels = c("moins de 50%", "de 50% à 74%", "de 75% à 94%", "95% et plus"))
freq(d_acm$host_acceptance_rate_categ)

#Categ availability
freq(d_acm$availability_365)

d_acm %>% ggplot(aes(x=availability_365))+ geom_bar(stat="count")+
  theme_minimal() +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Histogramme du nombre de nuitées ouvertes à la réservation dans le calendrier \ncollecte de février 2023", x ="Nombre de nuits disponibles à la réservation", y = "Nombre d'annonces")

d_acm$availability_365_categ <- cut(d_acm$availability_365, c(0, 0.9999, 11.9999, 30.9999, 60.9999, 180.9999, 240.9999, 365.9999), right=FALSE, include.lowest = TRUE, labels = c("Aucune disponibilité", "Moins de 12 nuits", "de 12 à 30 nuits", "de 31 à 60 nuits", "de 61 à 180 nuits", "de 181 à 340 nuits", "Plus de 340 nuits"))
summary(d_acm$availability_365)

freq(d_acm$availability_365_categ)

#Categ superhost
freq(d_acm$host_is_superhost)
d_acm$host_is_superhost <- as.factor(d_acm$host_is_superhost)

d_acm <- d_acm %>%
  mutate (host_is_superhost_categ = recode_factor (host_is_superhost, f ="not_superhost", t="is_superhost"))

d_acm <- d_acm %>% filter(!is.na(host_is_superhost_categ)) #On enlève les 6 NA de super_host apres avoir été regardé les annonces (mais comme elles sont "normales" et n'ont qu'un commentaire au cours des 12 LTM on les retire sans se poser plus de questions)

#Categ Instant_Bookable

freq(d_acm$instant_bookable)
d_acm$instant_bookable <- as.factor(d_acm$instant_bookable)

d_acm <- d_acm %>%
  mutate (instant_bookable_categ = recode_factor (instant_bookable, f ="not_instantbook", t="is_instantbook"))

#Transformation des NA
d_acm <- d_acm %>% 
  mutate(host_acceptance_rate_categ = fct_na_value_to_level(host_acceptance_rate_categ, "NR"))

#Détermination des variables actives et supplémentaires

vars_acm <- c("number_of_reviews_ltm_categ", "host_acceptance_rate_categ", "instant_bookable_categ", "availability_365_categ", "host_is_superhost_categ") 

d_acm <- d_acm %>% select(ID_URL, C_IR, J_L_CO, latitude, longitude, vars_acm)

vars_acti <- vars_acm

rm(vars_acm)

#-- Tableau modalités variables actives
modalites_vars_acti <- d_acm %>% select(all_of(vars_acti)) %>% 
  gather (variables, modalites) %>% count (variables, modalites)

modalites_vars_acti <- modalites_vars_acti %>%
  left_join(modalites_vars_acti %>%
              group_by(variables) %>% 
              summarise(nb_mod_ds_var=n()),by="variables") %>%
mutate(pourcentage = round(n / nrow(d_acm) * 100, 2),  # Limite à 2 décimales
  type_var = "actives"
)

#- Présentation ce tableau "modalites"
modalites <- modalites_vars_acti

tab <- modalites %>% kbl() %>%  kable_styling ("striped")

save_kable(tab, "~/Tab_modalite_ACM_hautefreq.png")

###- réalisation de l'ACM
names(d_acm)

res.acm <- MCA(d_acm[,6:10],graph=F)

#-- informations sur les variables actives
coord <- as.data.frame(round(res.acm$var$coord, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "coord", sep = "_")) %>% mutate(index = rownames(.))
contrib <- as.data.frame(round(res.acm$var$contrib, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "contrib", sep = "_")) %>% mutate(index = rownames(.))
cos2 <- as.data.frame(round(res.acm$var$cos2, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "cos2", sep = "_")) %>% mutate(index = rownames(.))
vtest <- as.data.frame(round(res.acm$var$v.test, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "vtest", sep = "_")) %>% mutate(index = rownames(.))
eta2 <- as.data.frame(round(res.acm$var$eta2, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "eta2", sep = "_")) %>% 
  mutate(variables = rownames(.))

modalites_vars_acti <- modalites %>% select(variables,modalites) %>%
  inner_join(coord, by = c("modalites" = "index")) %>%
  left_join(contrib, by = c("modalites" = "index")) %>%
  left_join(cos2, by = c("modalites" = "index")) %>%
  left_join(vtest, by = c("modalites" = "index")) %>%
  left_join(eta2, by="variables") %>% select(-variables)

#-- joindre ces informations au tableau "modalites"
modalites <- modalites %>%
  left_join(modalites_vars_acti, by="modalites")

#-- suppression des objets temporaires
rm(coord,contrib,cos2,vtest,modalites_vars_acti)

#-- visualisation de ce tableau "modalites", suite à ces ajouts
tab_res_acm <- modalites %>% kbl() %>% kable_styling ("striped")
  
write.table(modalites, "~/Tab_modalite_resultACM_hautefreq.csv", sep=";", row.names=F)

#Compléter le tableau d_acm pour visualiser les individus sur les plans factoriels avec ggplot
coord <- as.data.frame(round(res.acm$ind$coord, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "coord", sep = "_")) %>% mutate(id = rownames(.))
contrib <- as.data.frame(round(res.acm$ind$contrib, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "contrib", sep = "_")) %>% mutate(id = rownames(.))
cos2 <- as.data.frame(round(res.acm$ind$cos2, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "cos2", sep = "_")) %>% mutate(id = rownames(.))

d_acm <- d_acm %>% mutate(id = rownames(.)) %>%
  left_join(coord,by="id") %>% left_join(contrib,by="id") %>%
  left_join(cos2,by="id") %>% column_to_rownames("id")

#- je supprime les objets temporaires
rm(coord,contrib,cos2) 
  
#- nombre d'axes de notre ACM
nb_axes <- nrow(res.acm$eig)

#- visualisation des parts d'inertie expliquées par chaque axe
fviz_eig(res.acm, ncp=nb_axes,addlabels = TRUE)

nb_axes_kaiser <- nrow(res.acm$eig[res.acm$eig[,2]>=100/nb_axes,])
  
#
vars_dim <- colnames(modalites)[grepl("dim1",colnames(modalites))]
mt_d <- as.matrix(modalites[modalites$type_var=="actives",c(vars_dim)])
corrplot(cor(mt_d), method = "circle", addCoef.col="black", type="lower")


#Creation des objets : nb_mod_actives : pour extraire les seuls éléments aux contributions “significatives” (i.e. supérieures à la contribution moyenne);
#Et eig : pour labelliser les axes avec les parts d’inertie expliquée par chaque axe

nb_mod_actives <- nrow(res.acm$var$contrib)
eig <- round(res.acm$eig[,2],1)

#Interpretation premier axe : 
#ce qui est chiant c'esty qu'on a peut-être un effet guttman... Mais pas sur, en tout cas on a un axe qui est lié à l'activité de l'annonce : avec des annonces plus ou moins louées/mises en location
modalites %>% 
  filter(dim1_contrib >=100/nb_mod_actives) %>%
  ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(aes(colour=dim1_contrib, size=dim1_cos2)) +
  geom_text(aes(label=modalites,size=dim1_cos2),size=2, hjust=-.1) +
  scale_color_gradient(low="grey80",high="grey20") +
  labs(title="Eléments pour l'interprétation de l'Axe 1",
       subtitle="Modalités des variables actives \npour lesquelles la contribution est supérieure à la contribution moyenne",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 2 (",eig[2]," %)")) +
  theme_minimal()


modalites %>% ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_vline(xintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_point(aes(color=dim1_cos2, size=pourcentage)) +
  geom_text(aes(label=modalites),size=2, hjust=+.02, vjust=1.3) +
  scale_color_gradient2(low="yellow",mid="orange",high="darkred",midpoint = 0.4) +
  theme_minimal()


#Nuage des individus dim 1
d_acm %>%
  ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(aes(colour=number_of_reviews_ltm_categ),size=.5) +
  scale_color_manual(values=brewer.pal(length(levels(d_acm$number_of_reviews_ltm_categ)), "Reds")) +
  labs(title="Eléments pour l'interprétation de l'Axe 1",
       subtitle="Individus",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 2 (",eig[2]," %)")) +
  theme_minimal()


#10 individus aux contrib les plus fortes pour la dimension 1
d_acm %>% arrange(desc(dim1_contrib)) %>% slice(1:10) %>%
  mutate(id = rownames(.)) %>%
  ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(colour="red") +
  geom_text(aes(label=id),size=2)+
  labs(title="Eléments pour l'interprétation de l'Axe 1",
       subtitle="Les 10 individus aux contributions les plus fortes",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 2 (",eig[2]," %)")) +
  theme_minimal()

# Identifier les modalités ayant les plus grandes contributions sur la dimension 1
top_contributors_dim1 <- modalites %>%
  arrange(desc(dim1_contrib)) %>%
  slice_head(n = 5) %>%
  select(modalites, dim1_contrib)

dimdesc(res.acm, axes = 1) #ceci permet de calculer une Anova entre les variables et la dim 1, ainsi que d'autres choses


###Axe 2
modalites %>% 
  filter(dim2_contrib >=100/nb_mod_actives) %>%
  ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(aes(colour=dim2_contrib, size=dim2_cos2)) +
  geom_text(aes(label=modalites,size=dim2_cos2),size=2, hjust=-.1) +
  scale_color_gradient(low="grey80",high="grey20") +
  labs(title="Eléments pour l'interprétation de l'Axe 2",
       subtitle="Modalités des variables actives \npour lesquelles la contribution est supérieure à la contribution moyenne",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 2 (",eig[2]," %)")) +
  theme_minimal()

# Identifier les modalités ayant les plus grandes contributions sur la dimension 2
top_contributors_dim2 <- modalites %>%
  arrange(desc(dim2_contrib)) %>%
  slice_head(n = 5) %>%
  select(modalites, dim2_contrib)


##### ACM 2 / avec minstay -----
#tentative n.2 avec la variable "minstay" en plus
d_acm <- HF_202302

#Categ host_acceptance
freq(d_acm$host_acceptance_rateNETT)
summary(d_acm$host_acceptance_rateNETT)

d_acm %>% ggplot(aes(x=host_acceptance_rateNETT))+ geom_bar(stat="count")+
  theme_minimal() +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Histogramme du taux d'acceptation des réservations \nen février 2023", x ="Taux d'acceptation des réservations", y = "Nombre d'annonces")

d_acm$host_acceptance_rate_categ <- cut(d_acm$host_acceptance_rateNETT, c(0, 49.9999, 74.99999, 90.9999, 99.9999, 101.9999), right=FALSE, include.lowest = TRUE, labels = c("moins de 50%", "de 50% à 74%", "de 75% à 90%", "de 91% à 99%", "100%"))
freq(d_acm$host_acceptance_rate_categ)

#Categ availability

freq(d_acm$availability_365)

d_acm %>% ggplot(aes(x=availability_365))+ geom_bar(stat="count")+
  theme_minimal() +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Histogramme du nombre de nuitées ouvertes à la réservation dans le calendrier \ncollecte de février 2023", x ="Nombre de nuits disponibles à la réservation", y = "Nombre d'annonces")

d_acm$availability_365_categ <- cut(d_acm$availability_365, c(0, 0.9999, 11.9999, 30.9999, 60.9999, 180.9999, 240.9999, 365.9999), right=FALSE, include.lowest = TRUE, labels = c("Aucune disponibilité", "Moins de 12 nuits", "de 12 à 30 nuits", "de 31 à 60 nuits", "de 61 à 180 nuits", "de 181 à 340 nuits", "Plus de 340 nuits"))
summary(d_acm$availability_365)

freq(d_acm$availability_365_categ)

#Categ reviews_ltm
freq(d_acm$number_of_reviews_ltm)

d_acm %>% ggplot(aes(x=number_of_reviews_ltm))+ geom_bar(stat="count")+
  theme_minimal() +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Histogramme du nombre de commentaires reçus au cours des 12 mois précédents \ncollecte de février 2023", x ="Nombre de commentaires", y = "Nombre d'annonces")

d_acm$number_of_reviews_ltm_categ <- cut(d_acm$number_of_reviews_ltm, c(0, 3.9999, 10.9999, 30.9999, 200), right=FALSE, include.lowest = TRUE, labels = c("1 à 3 commentaires", "de 4 à 10 commentaires", "de 11 à 30 commentaires", "Plus de 30 commentaires"))
summary(d_acm$number_of_reviews_ltm)

freq(d_acm$number_of_reviews_ltm_categ)

#Categ Instant_Bookable
freq(d_acm$instant_bookable)
d_acm$instant_bookable <- as.factor(d_acm$instant_bookable)

d_acm <- d_acm %>%
  mutate (instant_bookable_categ = recode_factor (instant_bookable, f ="not_instantbook", t="is_instantbook"))

#Categ minstay
freq(d_acm$minimum_nights_avg_ntm)

d_acm %>% ggplot(aes(x=minimum_nights_avg_ntm))+ geom_bar(stat="count")+
  theme_minimal() +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Histogramme de la durée minimale moyenne sur les 12 mois suivants \ncollecte de février 2023", x ="Nombre minimal de nuitées d'un séjour", y = "Nombre d'annonces")

summary(d_acm$minimum_nights_avg_ntm)

d_acm$minimum_nights_avg_ntm_categ <- cut(d_acm$minimum_nights_avg_ntm, c(0, 1.1, 2.1, 3.1, 4.1, 5.9999, 28.1, 500), right=FALSE, include.lowest = TRUE, labels = c("Minimum moyen de 1,0 nuit", "Mini de 1,1 à 2,0 nuits", "Mini de 2,1 à 3,0 nuits", "Mini de 3,1 à 4,0 nuits", "Mini de 4,1 à 5,9 nuits", "Mini de 6,0 à 28,0 nuits", "Mini sup à 28,0 nuits"))

freq(d_acm$minimum_nights_avg_ntm_categ)

d_acm <- d_acm %>% filter(!is.na(minimum_nights_avg_ntm)) #on enlève l'annonce qui n'a pas de moy mini remplie
d_acm <- d_acm %>% filter(!minimum_nights_avg_ntm_categ =="Mini sup à 28,0 nuits")

#Transformation des NA
d_acm <- d_acm %>% 
  mutate(host_acceptance_rate_categ = fct_na_value_to_level(host_acceptance_rate_categ, "NR"))

#Détermination des variables actives et supplémentaires

vars_acm <- c("number_of_reviews_ltm_categ", "host_acceptance_rate_categ", "instant_bookable_categ", "availability_365_categ", "minimum_nights_avg_ntm_categ") 

d_acm <- d_acm %>% select(ID_URL, C_IR, J_L_CO, latitude, longitude, vars_acm)

vars_acti <- vars_acm

rm(vars_acm)

#-- Tableau modalités variables actives
modalites_vars_acti <- d_acm %>% select(all_of(vars_acti)) %>% 
  gather (variables, modalites) %>% count (variables, modalites)

modalites_vars_acti <- modalites_vars_acti %>%
  left_join(modalites_vars_acti %>%
              group_by(variables) %>% 
              summarise(nb_mod_ds_var=n()),by="variables") %>%
  mutate(pourcentage = round(n / nrow(d_acm) * 100, 2),  # Limite à 2 décimales
         type_var = "actives"
  )

#- Présentation ce tableau "modalites"
modalites <- modalites_vars_acti

tab <- modalites %>% kbl() %>%  kable_styling ("striped")
tab

save_kable(tab, "~/Tab_modalite_ACM2_hautefreq.png")

###- réalisation de l'ACM 2
names(d_acm)

res.acm <- MCA(d_acm[,6:10],graph=F)

#-- informations sur les variables actives
coord <- as.data.frame(round(res.acm$var$coord, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "coord", sep = "_")) %>% mutate(index = rownames(.))
contrib <- as.data.frame(round(res.acm$var$contrib, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "contrib", sep = "_")) %>% mutate(index = rownames(.))
cos2 <- as.data.frame(round(res.acm$var$cos2, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "cos2", sep = "_")) %>% mutate(index = rownames(.))
vtest <- as.data.frame(round(res.acm$var$v.test, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "vtest", sep = "_")) %>% mutate(index = rownames(.))
eta2 <- as.data.frame(round(res.acm$var$eta2, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "eta2", sep = "_")) %>% 
  mutate(variables = rownames(.))

modalites_vars_acti <- modalites %>% select(variables,modalites) %>%
  inner_join(coord, by = c("modalites" = "index")) %>%
  left_join(contrib, by = c("modalites" = "index")) %>%
  left_join(cos2, by = c("modalites" = "index")) %>%
  left_join(vtest, by = c("modalites" = "index")) %>%
  left_join(eta2, by="variables") %>% select(-variables)

#-- joindre ces informations au tableau "modalites"
modalites <- modalites %>%
  left_join(modalites_vars_acti, by="modalites")

#-- suppression des objets temporaires
rm(coord,contrib,cos2,vtest,modalites_vars_acti)

#-- visualisation de ce tableau "modalites", suite à ces ajouts
tab_res_acm <- modalites %>% kbl() %>% kable_styling ("striped")

write.table(modalites, "~/Tab_modalite_resultACM2_hautefreq.csv", sep=";", row.names=F)

#Compléter le tableau d_acm pour visualiser les individus sur les plans factoriels avec ggplot
coord <- as.data.frame(round(res.acm$ind$coord, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "coord", sep = "_")) %>% mutate(id = rownames(.))
contrib <- as.data.frame(round(res.acm$ind$contrib, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "contrib", sep = "_")) %>% mutate(id = rownames(.))
cos2 <- as.data.frame(round(res.acm$ind$cos2, 2))  %>% 
  rename_all(tolower) %>% rename_all(~ str_replace(., " ", "")) %>% 
  rename_all(~ str_c(., "cos2", sep = "_")) %>% mutate(id = rownames(.))

d_acm <- d_acm %>% mutate(id = rownames(.)) %>%
  left_join(coord,by="id") %>% left_join(contrib,by="id") %>%
  left_join(cos2,by="id") %>% column_to_rownames("id")

#- je supprime les objets temporaires
rm(coord,contrib,cos2) 

#- nombre d'axes de notre ACM
nb_axes <- nrow(res.acm$eig)

#- visualisation des parts d'inertie expliquées par chaque axe
fviz_eig(res.acm, ncp=nb_axes,addlabels = TRUE)

nb_axes_kaiser <- nrow(res.acm$eig[res.acm$eig[,2]>=100/nb_axes,])

#
vars_dim <- colnames(modalites)[grepl("dim1",colnames(modalites))]
mt_d <- as.matrix(modalites[modalites$type_var=="actives",c(vars_dim)])
corrplot(cor(mt_d), method = "circle", addCoef.col="black", type="lower")

#graph des variables
plot(res.acm, choix="var")

#Creation des objets : nb_mod_actives : pour extraire les seuls éléments aux contributions “significatives” (i.e. supérieures à la contribution moyenne);
#Et eig : pour labelliser les axes avec les parts d’inertie expliquée par chaque axe

nb_mod_actives <- nrow(res.acm$var$contrib)
eig <- round(res.acm$eig[,2],1)

#Interpretation premier axe : 
modalites %>% 
  filter(dim1_contrib >=100/nb_mod_actives) %>%
  ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(aes(colour=dim1_contrib, size=dim1_cos2)) +
  geom_text(aes(label=modalites,size=dim1_cos2),size=2, hjust=-.05) +
  scale_color_gradient(low="grey80",high="grey20") +
  labs(title="Eléments pour l'interprétation de l'Axe 1",
       subtitle="Modalités des variables actives \npour lesquelles la contribution est supérieure à la contribution moyenne",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 2 (",eig[2]," %)")) +
  theme_minimal()

modalites %>% ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_vline(xintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_point(aes(color=dim1_cos2, size=pourcentage)) +
  geom_text(aes(label=modalites),size=2, hjust=+.02, vjust=1.3) +
  scale_color_gradient2(low="yellow",mid="orange",high="darkred",midpoint = 0.4) +
  theme_minimal()

#Nuage des individus dim 1
d_acm %>%
  ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(aes(colour=number_of_reviews_ltm_categ),size=.5) +
  scale_color_manual(values=brewer.pal(length(levels(d_acm$number_of_reviews_ltm_categ)), "Reds")) +
  labs(title="Eléments pour l'interprétation de l'Axe 1",
       subtitle="Individus",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 2 (",eig[2]," %)")) +
  theme_minimal()

#10 individus aux contrib les plus fortes pour la dimension 1
d_acm %>% arrange(desc(dim1_contrib)) %>% slice(1:10) %>%
  mutate(id = rownames(.)) %>%
  ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(colour="red") +
  geom_text(aes(label=id),size=2)+
  labs(title="Eléments pour l'interprétation de l'Axe 1",
       subtitle="Les 10 individus aux contributions les plus fortes",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 2 (",eig[2]," %)")) +
  theme_minimal()

# Identifier les modalités ayant les plus grandes contributions sur la dimension 1
top_contributors_dim1 <- modalites %>%
  arrange(desc(dim1_contrib)) %>%
  slice_head(n = 5) %>%
  select(modalites, dim1_contrib)

dimdesc(res.acm, axes = 1) #ceci permet de calculer une Anova entre les variables et la dim 1, ainsi que d'autres choses


###Axe 2
modalites %>% 
  filter(dim2_contrib >=100/nb_mod_actives) %>%
  ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(aes(colour=dim2_contrib, size=dim2_cos2)) +
  geom_text(aes(label=modalites,size=dim2_cos2),size=2, hjust=-.1) +
  scale_color_gradient(low="grey80",high="grey20") +
  labs(title="Eléments pour l'interprétation de l'Axe 2",
       subtitle="Modalités des variables actives \npour lesquelles la contribution est supérieure à la contribution moyenne",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 2 (",eig[2]," %)")) +
  theme_minimal()

modalites %>% ggplot(aes(x=dim1_coord,y=dim2_coord)) +
  geom_hline(yintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_vline(xintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_point(aes(color=dim2_cos2, size=pourcentage)) +
  geom_text(aes(label=modalites),size=2, hjust=+.02, vjust=1.3) +
  scale_color_gradient2(low="yellow",mid="orange",high="darkred",midpoint = 0.4) +
  theme_minimal()

# Identifier les modalités ayant les plus grandes contributions sur la dimension 2
top_contributors_dim2 <- modalites %>%
  arrange(desc(dim2_contrib)) %>%
  slice_head(n = 5) %>%
  select(modalites, dim2_contrib)

dimdesc(res.acm, axes = 2) #ceci permet de calculer une Anova entre les variables et la dim 1, ainsi que d'autres choses

###Axe 3
modalites %>% 
  filter(dim3_contrib >=100/nb_mod_actives) %>%
  ggplot(aes(x=dim1_coord,y=dim3_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(aes(colour=dim2_contrib, size=dim2_cos2)) +
  geom_text(aes(label=modalites,size=dim3_cos2),size=2, hjust=-.1) +
  scale_color_gradient(low="grey80",high="grey20") +
  labs(title="Eléments pour l'interprétation de l'Axe 3",
       subtitle="Modalités des variables actives \npour lesquelles la contribution est supérieure à la contribution moyenne",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 3 (",eig[3]," %)")) +
  theme_minimal()

modalites %>% ggplot(aes(x=dim1_coord,y=dim3_coord)) +
  geom_hline(yintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_vline(xintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_point(aes(color=dim3_cos2, size=pourcentage)) +
  geom_text(aes(label=modalites),size=2, hjust=+.02, vjust=1.3) +
  scale_color_gradient2(low="yellow",mid="orange",high="darkred",midpoint = 0.4) +
  theme_minimal()

# Identifier les modalités ayant les plus grandes contributions sur la dimension 3
top_contributors_dim3 <- modalites %>%
  arrange(desc(dim3_contrib)) %>%
  slice_head(n = 5) %>%
  select(modalites, dim3_contrib)

###Axe 4
modalites %>% 
  filter(dim4_contrib >=100/nb_mod_actives) %>%
  ggplot(aes(x=dim1_coord,y=dim4_coord)) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_point(aes(colour=dim2_contrib, size=dim2_cos2)) +
  geom_text(aes(label=modalites,size=dim4_cos2),size=2, hjust=-.1) +
  scale_color_gradient(low="grey80",high="grey20") +
  labs(title="Eléments pour l'interprétation de l'Axe 4",
       subtitle="Modalités des variables actives \npour lesquelles la contribution est supérieure à la contribution moyenne",
       x=paste0("Axe 1 (",eig[1]," %)"),
       y=paste0("Axe 4 (",eig[4]," %)")) +
  theme_minimal()

modalites %>% ggplot(aes(x=dim1_coord,y=dim4_coord)) +
  geom_hline(yintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_vline(xintercept= 0, colour="darkgrey", linetype="longdash") +
  geom_point(aes(color=dim4_cos2, size=pourcentage)) +
  geom_text(aes(label=modalites),size=2, hjust=+.02, vjust=1.3) +
  scale_color_gradient2(low="yellow",mid="orange",high="darkred",midpoint = 0.4) +
  theme_minimal()

# Identifier les modalités ayant les plus grandes contributions sur la dimension 4
top_contributors_dim4 <- modalites %>%
  arrange(desc(dim4_contrib)) %>%
  slice_head(n = 5) %>%
  select(modalites, dim4_contrib)

top_contrib <- bind_cols (top_contributors_dim1,top_contributors_dim2, top_contributors_dim3, top_contributors_dim4)
write.table(top_contrib, "~/Tab_contrib_resultACM2_hautefreq.csv", sep=";", row.names=F)

Investigate(res.acm) # Ca ca a fait buguer fort R (crash) mais ça a permis d'avoir un html avec plein de graphs et infos sur l'acm pour aider à l'analyser


#### ACP pour voir si ça fonctitonne mieux que l'ACM ----
d_acp <- HF_202302

d_acp <- d_acp %>% 
  filter(!is.na(host_acceptance_rateNETT)) %>%
  filter(!is.na(minimum_nights_avg_ntm)) #On retire les individus qui ont des NA pour procéder à l'ACP

#On va d'abord afficher les corrélations entre toutes nos var quanti utiles
df_cor <- d_acp %>% select ("ID_URL", "number_of_reviews_ltm", "host_acceptance_rateNETT", "availability_365", "minimum_nights_avg_ntm", "review_scores_rating", "price", "instant_bookable") 

rownames(df_cor) <- df_cor$ID_URL
df_cor <- df_cor %>% select (-ID_URL) 

summary(df_cor)

#On regarde aussi ce que ça donne pour d'autres dates

#2018-04 - pas de variable "host_acceptance_rate en 2018"
IA_MGP_LE_2018_04 <- IA_MGP18_23 %>% 
  filter(last_scraped_YM=="2018-04") %>%
  filter(room_type=="Entire home/apt")

df_cor1804 <- IA_MGP_LE_2018_04 %>% 
  filter(calc_orig < "2017-04-23", na.rm = TRUE) %>%
  filter(!is.na(review_scores_rating)) %>%
  filter(number_of_reviews_ltm<200 & number_of_reviews_ltm>0) %>%
  select ("ID_URL", "number_of_reviews_ltm", "availability_365", "minimum_nights_avg_ntm", "review_scores_rating", "price")

rownames(df_cor1804) <- df_cor1804$ID_URL
df_cor1804 <- df_cor1804 %>% select (-ID_URL) 
summary(df_cor1804)

#2020-02
IA_MGP_LE_2020_02 <- IA_MGP18_23 %>% 
  filter(last_scraped_YM=="2020-02") %>%
  filter(room_type=="Entire home/apt")

df_cor2002 <- IA_MGP_LE_2020_02 %>% 
  filter(calc_orig < "2019-01-23", na.rm = TRUE) %>%
  filter(!is.na(host_acceptance_rateNETT)) %>%
  filter(!is.na(review_scores_rating)) %>%
  filter(number_of_reviews_ltm<200 & number_of_reviews_ltm>0) %>%
  select ("ID_URL", "number_of_reviews_ltm", "host_acceptance_rateNETT", "availability_365", "minimum_nights_avg_ntm", "review_scores_rating", "price") 

rownames(df_cor2002) <- df_cor2002$ID_URL
df_cor2002 <- df_cor2002 %>% select (-ID_URL) 
summary(df_cor2002)

#2022-05
IA_MGP_LE_2022_05 <- IA_MGP18_23 %>% 
  filter(last_scraped_YM=="2022-05") %>%
  filter(room_type=="Entire home/apt")

df_cor2205 <- IA_MGP_LE_2022_05 %>% 
  filter(calc_orig < "2021-05-22", na.rm = TRUE) %>%
  filter(!is.na(host_acceptance_rateNETT)) %>%
  select ("ID_URL", "number_of_reviews_ltm", "host_acceptance_rateNETT", "availability_365", "minimum_nights_avg_ntm", "review_scores_rating", "price") %>%
  filter(number_of_reviews_ltm<200 & number_of_reviews_ltm>0) #on enlève les annonces qui ont plus de 200 comm/an car ce sont des annonces qui aggrègent en fait plusieurs annonces : leur calendrier est toujours ouvert et ce doit être un logiciel interne qui gère les résa

rownames(df_cor2205) <- df_cor2205$ID_URL
df_cor2205 <- df_cor2205 %>% select (-ID_URL) 
summary(df_cor2205)

#2023-05
IA_MGP_LE_2023_05 <- IA_MGP18_23 %>% 
  filter(last_scraped_YM=="2023-05") %>%
  filter(room_type=="Entire home/apt")

df_cor2305 <- IA_MGP_LE_2023_05 %>%
  filter(calc_orig < "2022-05-24", na.rm = TRUE) %>%
  filter(!is.na(host_acceptance_rateNETT)) %>%
  filter(!is.na(review_scores_rating)) %>%
  filter(number_of_reviews_ltm<200 & number_of_reviews_ltm>0) %>%
  select ("ID_URL", "number_of_reviews_ltm", "host_acceptance_rateNETT", "availability_365", "minimum_nights_avg_ntm", "review_scores_rating", "price") 

rownames(df_cor2305) <- df_cor2305$ID_URL
df_cor2305 <- df_cor2305 %>% select (-ID_URL) 
summary(df_cor2305)


#On fait le graph cor_plot de corrélation entre ces var ----
pairs(df_cor[,1:6])
cor(df_cor[,1:6])

panel.cor_simple <- function(x, y, digits = 2, prefix = "", cex.cor = 1.5) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- cor(x, y) 
  txt <- format(c(r, 0.123456789), digits = digits)[1] 
  txt <- paste(prefix, txt, sep = "") 
  
  test <- cor.test(x, y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex.cor)  # La taille est fixe avec cex.cor
  text(0.8, 0.8, Signif, cex = cex.cor, col = 2)  # Même taille fixe pour les symboles
} #D'après : https://delladata.fr/correlation-deux-a-deux-correlation-des-paires-ou-pairewise-correlations/ et https://www.r-bloggers.com/2011/03/five-ways-to-visualize-your-pairwise-comparisons/ 

# Utilisation dans pairs
pairs(df_cor[,1:6], lower.panel = panel.smooth, upper.panel = panel.cor_simple)

#On regarde aussi la répartition des t/f de instant_book par rapport à number_of_reviews_ltm
df_IB <- HF_202302 %>%
  mutate(
    number_of_reviews_ltm_categ_deciles = cut(
      number_of_reviews_ltm, 
      breaks = quantile(number_of_reviews_ltm, probs = seq(0, 1, 0.1), na.rm = TRUE), 
      include.lowest = TRUE
    )
  )

df_IB$number_of_reviews_ltm_categ_deciles <- as.factor(df_IB$number_of_reviews_ltm_categ_deciles)
levels(df_IB$number_of_reviews_ltm_categ_deciles) <- c("[1, 2]","[3, 4]", "[5, 6]", "[7, 8]", "[9, 11]", "[12, 15]", "[16, 19]", "[20, 26]", "[27, 41]", "[42, 168]")

#-- Réalisation du tableau croisé 
names(df_IB)

df_IB$instant_bookable <- as.factor(df_IB$instant_bookable)
levels(df_IB$instant_bookable) <- c("Pas de résa instant.","Résa instant.")

tab_ib <- df_IB %>%
  filter(!is.na(instant_bookable) & !is.na(number_of_reviews_ltm_categ_deciles)) %>%
  group_by(instant_bookable, number_of_reviews_ltm_categ_deciles) %>%
  summarise(nb = n(), .groups = "drop") %>%
  pivot_wider(names_from = instant_bookable, values_from = nb) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  bind_rows(summarise(
    .,
    across(where(is.numeric), sum),
    across(where(is.factor), ~"Ensemble")
  )) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  mutate(Effectifs = Total) %>%
  mutate(across(c(where(is.numeric), -Effectifs), ~./Effectifs * 100)) %>%
  mutate(across(where(is.numeric), ~round(., 1))) # Arrondir à 1 chiffre après la virgule

tab_ib <- tab_ib %>%
  gt(rowname_col = "number_of_reviews_ltm_categ_deciles") %>%
  tab_header(title = md("**Activité des annonces et activation de la réservation instantannée**")) %>%
  tab_stubhead(label = "Nombre de commentaires reçus au cours\ndes 12 mois précédents, répartition par déciles ") %>%
  tab_source_note(source_note = "Source : InsideAirbnb, jeu du 23/02/2023, réalisation J. Richon sous R, avec l'aide d'Aliette Roux, 2025 ") %>%
  tab_footnote(
    footnote = "Lecture : parmi les annonces ayant reçu un à deux commentaires au cours des 12 mois précédents, 86,1% n'ont pas activé la réservation instantannée",
  ) %>%
  fmt_number(columns = 2:3, decimals = 1) %>% # Format des nombres avec 1 chiffre après la virgule
  tab_options(footnotes.font.size = 11)

tab_ib

freq(HF_202302$host_acceptance_rateNETT)

#Tentative de faire directement une CAH sur nos données ----
names(df_cor)

df_cah <- d_acp %>% select ("ID_URL", "number_of_reviews_ltm", "host_acceptance_rateNETT", "minimum_nights_avg_ntm") 
df_cah <- df_cor %>% select(ID_URL, number_of_reviews_ltm, host_acceptance_rateNETT, minimum_nights_avg_ntm)

identifiants <- df_cah$ID_URL

df_cah <- df_cah %>% select(-ID_URL)

#On realise la CAH avec hclust
#- pour centrer-réduire : 
df_cah.cr <- scale(df_cah) 

df_cah_F_cr <- as.data.frame(df_cah.cr)

#- on doit ensuite calculer la matrice des distances entre individus 
d.df_cah <- dist(df_cah.cr, method = "euclidean")

#- on réalise enfin la CAH, en utilisant ici le critère de Ward 
cah.ward <- hclust(d.df_cah, method="ward.D2")

#- visualisation du dendogramme 
plot(cah.ward)

inertie <- sort(cah.ward$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 4, 5), inertie[c(2, 4, 5)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

#- dendrogramme avec matérialisation des groupes 
rect.hclust(cah.ward,k=4) 

# Découpage en clusters
clusters <- cutree(cah.ward, k=4)

# Associer les clusters aux identifiants
groupes.cah <- data.frame(
  ID = identifiants,
  cluster = clusters
)

freq(groupes.cah$cluster)

df_cah <- bind_cols (df_cah,groupes.cah)
names(df_cah)

# Calculer les moyennes des variables par cluster
cluster_means <- df_cah %>%
  group_by(cluster...7) %>%
  summarise(across(where(is.numeric), mean))

# Pivot pour ggplot (au cas où vous avez plusieurs variables)
cluster_means_long <- cluster_means %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "mean")

#Nuage de points post-CAH avec hclust
df_cah$cluster...7 <- as.factor(df_cah$cluster...7)

ggplot(df_cah) +
  geom_point(aes(x=number_of_reviews_ltm, y= host_acceptance_rateNETT, color = cluster...7), , alpha=0.5) +
  theme_minimal() +
  scale_color_manual(
    values = c("#3399FF", "#ECA72C", "#82A9A1", "#EC674E", "#8F2D56") #, "#009F81", "blue", "purple") 
  ) +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Nombre de commentaires au cours des 12 mois derniers \nen fonction du taux d'acceptation des résa", y ="Taux d'acceptation\ndes réservations", x = "Nombre de commentaires reçus \nau cours des 12 derniers mois")

#Essai CAH n.2 sur nos données, avec plus de variables ----
names(df_cor)

df_cah <- HF_202302 %>% select ("ID_URL", "number_of_reviews_ltm", "host_acceptance_rateNETT", "minimum_nights_avg_ntm", "availability_365" ) 

df_cah <- df_cah %>%
  filter(!is.na(host_acceptance_rateNETT)) %>%
  filter(!is.na(minimum_nights_avg_ntm)) #On retire les individus qui ont des NA pour procéder à la CAH

identifiants <- df_cah$ID_URL

df_cah <- df_cah %>% select(-ID_URL)

#On realise la CAH avec hclust
#- pour centrer-réduire : 
df_cah.cr <- scale(df_cah) 

df_cah_F_cr <- as.data.frame(df_cah.cr)

#- on doit ensuite calculer la matrice des distances entre individus 
d.df_cah <- dist(df_cah.cr, method = "euclidean")

#- on réalise enfin la CAH, en utilisant ici le critère de Ward 
cah.ward <- hclust(d.df_cah, method="ward.D2")

#- visualisation du dendogramme 
plot(cah.ward)

inertie <- sort(cah.ward$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 4, 5), inertie[c(2, 4, 5)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

#- dendrogramme avec matérialisation des groupes 
rect.hclust(cah.ward,k=6) 

# Découpage en clusters
clusters <- cutree(cah.ward, k=6)

# Associer les clusters aux identifiants
groupes.cah_bis <- data.frame(
  ID = identifiants,
  cluster_bis = clusters
)

freq(groupes.cah_bis$cluster)

df_cah <- bind_cols (df_cah,groupes.cah_bis)
names(df_cah)

# Calculer les moyennes des variables par cluster
cluster_means <- df_cah %>%
  group_by(cluster_bis) %>%
  summarise(across(where(is.numeric), mean))

# Pivot pour ggplot (au cas où vous avez plusieurs variables)
cluster_means_long <- cluster_means %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "mean")

#Nuage de points post-CAH avec hclust
df_cah$cluster_bis <- as.factor(df_cah$cluster_bis)

ggplot(df_cah) +
  geom_point(aes(x=number_of_reviews_ltm, y= host_acceptance_rateNETT, color = cluster_bis), , alpha=0.5) +
  theme_minimal() +
  scale_color_manual(
    values = c("#3399FF", "#ECA72C", "#82A9A1", "#EC674E", "#8F2D56", "#009F81")
  ) +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Nombre de commentaires au cours des 12 mois derniers \nen fonction du taux d'acceptation des résa", y ="Taux d'acceptation\ndes réservations", x = "Nombre de commentaires reçus \nau cours des 12 derniers mois")

ggplot(df_cah) +
  geom_point(aes(x=number_of_reviews_ltm, y= minimum_nights_avg_ntm, color = cluster_bis), , alpha=0.5) +
  theme_minimal() +
  scale_color_manual(
    values = c("#3399FF", "#ECA72C", "#82A9A1", "#EC674E", "#8F2D56", "#009F81")
  ) +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Nombre de commentaires au cours des 12 mois derniers \nen fonction ", y ="XXX\nXXXX", x = "Nombre de commentaires reçus \nau cours des 12 derniers mois")

ggplot(df_cah) +
  geom_point(aes(x=number_of_reviews_ltm, y= availability_365, color = cluster_bis), , alpha=0.5) +
  theme_minimal() +
  scale_color_manual(
    values = c("#3399FF", "#ECA72C", "#82A9A1", "#EC674E", "#8F2D56", "#009F81")
  ) +
  theme(legend.title = element_text(size=18), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  labs(title="Nombre de commentaires au cours des 12 mois derniers \nen fonction ", y ="XXX\nXXXX", x = "Nombre de commentaires reçus \nau cours des 12 derniers mois")

##Ajouts autres variables / quali dans l'estimation de la haute frequence de location ----
#Instant_bookable (réservation instantannee) croisé avec availability (ouverture du calendrier)
HF_202302_ib <- HF_202302 %>%    #ib = instant_booking
  filter(instant_bookable=="t") %>%
  filter(availability_365>0)

HF_202302_ib %>%
  ggplot(aes(x = availability_365)) +
  geom_histogram(binwidth = 1, fill = "#009F81", color = "white") +
  labs(
    title = "Histogramme du nombre de jours disponibles chez les annonces en résa instantanée HF_202302",
    x = "Nombre de jours disponibles à la réservation sur les 365 prochains jours",
    y = "Nombre d'annonces"
  ) +
  theme_minimal()

HF_202302_ib_dispo45 <- HF_202302 %>%    
  filter(instant_bookable=="t") %>%
  filter(availability_365 >44)

freq(HF_202302_ib_dispo35$number_of_reviews_ltm)
summary(HF_202302_ib_dispo35$number_of_reviews_ltm)
summary(HF_202302$number_of_reviews_ltm)
rm(HF_202302_ib_dispo35)

#Demarche de faisceau d'indices ----

HF75_202302 <- HF_202302 %>%
  filter(number_of_reviews_ltm >12)

HF30_75_202302 <- HF_202302 %>%
  filter(number_of_reviews_ltm >5 & number_of_reviews_ltm <13)

sum(HF_202302$number_of_reviews_ltm <6)

HFib45_202302 <- HF_202302 %>%
  filter(instant_bookable =="t" & availability_365 > 44)

freq(HFib45_202302$number_of_reviews_ltm) #ça en fait 2403 récup en tout, dont 332 qui ont moins de 6 commentaires, et 454 qui ont entre 6 et 12 commentaires (inclus)

#HF30_75_202302

freq(HF30_75_202302$avail_365_logi)
freq(HF30_75_202302$host_acceptance_rateNETT)
sum(HF30_75_202302$host_acceptance_rateNETT<30, na.rm = TRUE)

sum(HF30_75_202302$number_of_reviews>50)

summary(HF30_75_202302$host_total_listings_count)
summary(HF30_75_202302$host_listings_count)
sum(HF30_75_202302$host_total_listings_count>1)

freq(HF30_75_202302)

sum(is.na(HF30_75_202302$license_Nett))
sum(HF30_75_202302$license_dupli_pltf_count>1, na.rm = TRUE)

sum(HF30_75_202302$license_dupli_air_count>1, na.rm = TRUE)

freq(HF30_75_202302$license_faux)
freq(HF30_75_202302$reviews_per_month)

summary(HF75_202302$number_of_reviews)

#Tableau recap faisceau
Tab_faisceau <- HF_202302 %>%
  summarise(
    n_tot = n(),
    n_inf6_rLTM_A = sum(number_of_reviews_ltm <6),
    n_6a12_rLTM_B = sum(number_of_reviews_ltm >5 & number_of_reviews_ltm <13),        
    n_sup12_rLTM_C = sum(number_of_reviews_ltm >12),
    A_ib45 = sum(number_of_reviews_ltm <6 & instant_bookable =="t" & availability_365 > 44),
    B_ib45 = sum(number_of_reviews_ltm >5 & number_of_reviews_ltm <13 & instant_bookable =="t" & availability_365 > 44),
    C_ib45 = sum(number_of_reviews_ltm >12 & instant_bookable =="t" & availability_365 > 44)
  )

B_Hib45 <- HF_202302 %>%
  filter(number_of_reviews_ltm >5 & number_of_reviews_ltm <13) %>%
  filter(!(instant_bookable =="t" & availability_365 >44)) #creation du groupe contenant les 4023 annonces à tester avec d'autres variables

C <- HF_202302 %>%
  filter(number_of_reviews_ltm >12) #annonces directement considérées comme HRP par leur nombre de commentaires ltm supérieur à 12

Tab_faisceau_hib45 <- B_Hib45 %>%
  summarise(
    n = n(),
    ib = sum(instant_bookable == "t"),
    nib = sum(instant_bookable == "f"),
    ib_multi = sum(instant_bookable == "t" & calculated_host_listings_count >1),
    ib_mono = sum(instant_bookable == "t" & calculated_host_listings_count ==1),
    ib_mono_dupinter = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count >1), na.rm=TRUE),
    ib_mono_dupintra = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 , na.rm = TRUE),
    ib_mono_faux = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & license_faux=="FAUX", na.rm = TRUE),
    ib_mono_sup64 = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews>64),
    ib_mono_txminst = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews<65 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm = TRUE),
    nib_multi = sum(instant_bookable == "f" & calculated_host_listings_count >1),
    nib_multi_dupinter = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count >1), na.rm=TRUE),
    nib_multi_dupintra = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 , na.rm = TRUE),
    nib_multi_faux = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & license_faux=="FAUX", na.rm = TRUE),
    nib_multi_sup64 = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews>64),
    nib_multi_txminst = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews<65 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm = TRUE),
    nib_mono = sum(instant_bookable == "f" & calculated_host_listings_count ==1),
    nib_mono_dupinter = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count >1), na.rm=TRUE),
    nib_mono_dupinter_intra = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & license_dupli_air_count >1 , na.rm=TRUE),
    nib_mono_dupinter_faux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & license_faux=="FAUX", na.rm=TRUE),
    nib_mono_dupinter_sup64 = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews>64, na.rm=TRUE),
    nib_mono_dupinter_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews<65 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm = TRUE),
    nib_mono_ndupter = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count))),
    nib_mono_ndupter_intra = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 , na.rm=TRUE),
    nib_mono_ndupter_intra_faux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 & license_faux=="FAUX" , na.rm=TRUE),
    nib_mono_ndupter_intra_sup64 = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 & is.na(license_faux) & number_of_reviews>64, na.rm=TRUE),
    nib_mono_ndupter_intra_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 & is.na(license_faux) & number_of_reviews<65 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE),
    nib_mono_ndupter_nintra = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count))),
    nib_mono_ndupter_nintra_faux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & license_faux=="FAUX" , na.rm=TRUE),
    nib_mono_ndupter_nintra_faux_sup64 = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & license_faux=="FAUX" & number_of_reviews>64, na.rm=TRUE),
    nib_mono_ndupter_nintra_faux_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & license_faux=="FAUX" & number_of_reviews<65 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE),
    nib_mono_ndupter_nintra_nfaux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & is.na(license_faux)),
    nib_mono_ndupter_nintra_nfaux_sup64 = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews>64),
    nib_mono_ndupter_nintra_nfaux_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews>64 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE),
    nib_mono_ndupter_nintra_nfaux_inf65 = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews<65),
    nib_mono_ndupter_nintra_nfaux_inf65_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & is.na(license_faux) & number_of_reviews<65 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE))

sum(is.na(B_Hib45$license_Nett))

summary(C$host_acceptance_rateNETT)

names(B_Hib45)
freq(B_Hib45$license_faux)

##Faisceau annonces moins de 1 an ----

#Creer une variable qui calcule le nombre de jours d'existence à partir de la date de scraping ----
IA_MGP18_23 <- IA_MGP18_23 %>%
  mutate(duree_activ_scrpg_day = time_length(interval(start = ymd(calc_orig), end = ymd(last_scraped)), unit = "days"))


HF_202302_m1_sup27 <- IA_MGP18_23 %>%
  filter(last_scraped_YM =="2023-02") %>%
  filter(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & sup1an_scrpg ==FALSE & hotelRdT==FALSE & minimum_nights <28) %>%
  filter(duree_activ_scrpg_day >27)

HF_202302_m1_inf28 <- IA_MGP18_23 %>%
  filter(last_scraped_YM =="2023-02") %>%
  filter(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & sup1an_scrpg ==FALSE & hotelRdT==FALSE & minimum_nights <28) %>%
  filter(duree_activ_scrpg_day <28)

freq(HF_202302_m1_inf28$number_of_reviews_ltm)

freq(HF_202302_m1$duree_activ_scrpg_day) #pour voir combien de jours d'ancienneté ont les annonces
summary(HF_202302_m1$duree_activ_scrpg_day)

#adapter le modele aux annonces de moins de 1 an ----
#annonces dee plus de 27 jours et moins de 1 an
Tab_faisceau_m1 <- HF_202302_m1_sup27 %>%
  summarise(
    n_tot = n(),
    n_inf6_rLTM_A = sum(number_of_reviews_ltm < (duree_activ_scrpg_day*30/365/4.07*0.72)),
    n_6a12_rLTM_B = sum(number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & number_of_reviews_ltm < (duree_activ_scrpg_day*75/365/4.07*0.72)),
    n_sup12_rLTM_C = sum(number_of_reviews_ltm > (duree_activ_scrpg_day*75/365/4.07*0.72)),
    A_ib45 = sum(number_of_reviews_ltm < (duree_activ_scrpg_day*30/365/4.07*0.72) & instant_bookable =="t" & availability_365 > 44),
    B_ib45 = sum(number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & number_of_reviews_ltm < (duree_activ_scrpg_day*75/365/4.07*0.72) & instant_bookable =="t" & availability_365 > 44),
    C_ib45 = sum(number_of_reviews_ltm > (duree_activ_scrpg_day*75/365/4.07*0.72) & instant_bookable =="t" & availability_365 > 44)
  )

B_Hib45_m1 <- HF_202302_m1_sup27 %>%
  filter(duree_activ_scrpg_day>27) %>%
  filter(number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & number_of_reviews_ltm < (duree_activ_scrpg_day*75/365/4.07*0.72)) %>%
  filter(!(instant_bookable =="t" & availability_365 >44)) #creation du groupe contenant les 3363 annonces à tester avec d'autres variables

C_m1 <- HF_202302_m1_sup27 %>%
  filter(number_of_reviews_ltm > (duree_activ_scrpg_day*75/365/4.07*0.72)) #annonces directement considérées comme HRP par leur nombre de commentaires ltm supérieur à 12

Tab_faisceau_hib45_m1 <- B_Hib45_m1 %>%
  summarise(
    n = n(),
    ib = sum(instant_bookable == "t"),
    nib = sum(instant_bookable == "f"),
    ib_multi = sum(instant_bookable == "t" & calculated_host_listings_count >1),
    ib_mono = sum(instant_bookable == "t" & calculated_host_listings_count ==1),
    ib_mono_dupinter = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count >1), na.rm=TRUE),
    ib_mono_dupintra = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 , na.rm = TRUE),
    ib_mono_faux = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & license_faux=="FAUX", na.rm = TRUE),
    ib_mono_txminst = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm = TRUE),
    nib_multi = sum(instant_bookable == "f" & calculated_host_listings_count >1),
    nib_multi_dupinter = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count >1), na.rm=TRUE),
    nib_multi_dupintra = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 , na.rm = TRUE),
    nib_multi_faux = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & license_faux=="FAUX", na.rm = TRUE),
    nib_multi_txminst = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm = TRUE),
    nib_mono = sum(instant_bookable == "f" & calculated_host_listings_count ==1),
    nib_mono_dupinter = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count >1), na.rm=TRUE),
    nib_mono_dupinter_intra = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & license_dupli_air_count >1 , na.rm=TRUE),
    nib_mono_dupinter_faux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & license_faux=="FAUX", na.rm=TRUE),
    nib_mono_dupinter_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm = TRUE),
    nib_mono_ndupter = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count))),
    nib_mono_ndupter_intra = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 , na.rm=TRUE),
    nib_mono_ndupter_intra_faux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 & license_faux=="FAUX" , na.rm=TRUE),
    nib_mono_ndupter_intra_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE),
    nib_mono_ndupter_nintra = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count))),
    nib_mono_ndupter_nintra_faux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & license_faux=="FAUX" , na.rm=TRUE),
    nib_mono_ndupter_nintra_faux_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & license_faux=="FAUX" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE),
    nib_mono_ndupter_nintra_nfaux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & is.na(license_faux)),
    nib_mono_ndupter_nintra_nfaux_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE)
  )


#annonces de moins de 28 jours
HF_202302_m1_inf28 <- IA_MGP18_23 %>%
  filter(last_scraped_YM =="2023-02") %>%
  filter(room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & sup1an_scrpg ==FALSE & hotelRdT==FALSE & minimum_nights <28) %>%
  filter(duree_activ_scrpg_day <28)

sum(HF_202302_m1_inf28$number_of_reviews_ltm>=2)

Tab_faisceau_m28j <- HF_202302_m1_inf28 %>%
  summarise(
    n_tot = n(),
    n_0_rLTM_A = sum(number_of_reviews_ltm ==0),
    n_1_rLTM_B = sum(number_of_reviews_ltm ==1),
    n_sup1_rLTM_C = sum(number_of_reviews_ltm >1),
    A_ib45 = sum(number_of_reviews_ltm ==0 & instant_bookable =="t" & availability_365 > 44),
    B_ib45 = sum(number_of_reviews_ltm ==1 & instant_bookable =="t" & availability_365 > 44),
    C_ib45 = sum(number_of_reviews_ltm >1 & instant_bookable =="t" & availability_365 > 44)
  )

AB_Hib45_m28j <- HF_202302_m1_inf28 %>%
  filter(number_of_reviews_ltm <2) %>%
  filter(!(instant_bookable =="t" & availability_365 >44)) #creation du groupe contenant les 2106 annonces à tester avec d'autres variables

C_m28j <- HF_202302_m1_inf28 %>%
  filter(number_of_reviews_ltm >1) #annonces directement considérées comme HRP par leur nombre de commentaires ltm supérieur à 12

Tab_faisceau_hib45_ABm28j <- AB_Hib45_m28j %>%
  summarise(
    n = n(),
    ib = sum(instant_bookable == "t"),
    nib = sum(instant_bookable == "f"),
    ib_multi = sum(instant_bookable == "t" & calculated_host_listings_count >1),
    ib_mono = sum(instant_bookable == "t" & calculated_host_listings_count ==1),
    ib_mono_dupinter = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count >1), na.rm=TRUE),
    ib_mono_dupintra = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 , na.rm = TRUE),
    ib_mono_faux = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & license_faux=="FAUX", na.rm = TRUE),
    ib_mono_txminst = sum(instant_bookable == "t" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm = TRUE),
    nib_multi = sum(instant_bookable == "f" & calculated_host_listings_count >1),
    nib_multi_dupinter = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count >1), na.rm=TRUE),
    nib_multi_dupintra = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 , na.rm = TRUE),
    nib_multi_faux = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & license_faux=="FAUX", na.rm = TRUE),
    nib_multi_txminst = sum(instant_bookable == "f" & calculated_host_listings_count >1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm = TRUE),
    nib_mono = sum(instant_bookable == "f" & calculated_host_listings_count ==1),
    nib_mono_dupinter = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count >1), na.rm=TRUE),
    nib_mono_dupinter_intra = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & license_dupli_air_count >1 , na.rm=TRUE),
    nib_mono_dupinter_faux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & license_faux=="FAUX", na.rm=TRUE),
    nib_mono_dupinter_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & license_dupli_pltf_count >1 & (license_dupli_air_count ==1 | is.na(license_dupli_air_count)) & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm = TRUE),
    nib_mono_ndupter = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count))),
    nib_mono_ndupter_intra = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 , na.rm=TRUE),
    nib_mono_ndupter_intra_faux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 & license_faux=="FAUX" , na.rm=TRUE),
    nib_mono_ndupter_intra_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & license_dupli_air_count >1 & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE),
    nib_mono_ndupter_nintra = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count))),
    nib_mono_ndupter_nintra_faux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & license_faux=="FAUX" , na.rm=TRUE),
    nib_mono_ndupter_nintra_faux_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & license_faux=="FAUX" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE),
    nib_mono_ndupter_nintra_nfaux = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & is.na(license_faux)),
    nib_mono_ndupter_nintra_nfaux_txminst = sum(instant_bookable == "f" & calculated_host_listings_count ==1 & (license_dupli_pltf_count ==1 | is.na(license_dupli_pltf_count)) & (license_dupli_air_count ==1 |is.na(license_dupli_air_count)) & is.na(license_faux) & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98, na.rm=TRUE)
  )

#Creation de la colonne d'annonces considérées comme louees non occasionnellement en 2023 pour les analyser----
#nommee "typo_HRP" (car a un moment on les a appelees "hors residence principale") 

IA_MGP18_23 <-  IA_MGP18_23 %>%
  mutate(
    typo_HRP = case_when(
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >12 ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & instant_bookable =="t" & availability_365 >44 ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & instant_bookable == "t" & calculated_host_listings_count >1  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & instant_bookable == "t" & license_dupli_pltf_count >1  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & instant_bookable == "t" & license_dupli_air_count >1   ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & instant_bookable == "t" & license_faux=="FAUX"  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & instant_bookable == "t" & number_of_reviews>64  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & instant_bookable == "t" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & calculated_host_listings_count >1 & license_dupli_pltf_count >1  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & calculated_host_listings_count >1 & license_dupli_air_count >1   ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & calculated_host_listings_count >1 & license_faux=="FAUX"  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & calculated_host_listings_count >1 & number_of_reviews>64  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & calculated_host_listings_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & license_dupli_pltf_count >1 & license_dupli_air_count >1   ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & license_dupli_pltf_count >1 & license_faux=="FAUX"  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & license_dupli_pltf_count >1 & number_of_reviews>64  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & license_dupli_pltf_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & license_dupli_air_count >1 & license_faux=="FAUX"  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & license_dupli_air_count >1 & number_of_reviews>64  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & license_dupli_air_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & license_faux=="FAUX" & number_of_reviews>64  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & license_faux=="FAUX" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_sup1",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >5 & number_of_reviews>64 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_sup1", #là on a toutes les annonces HRP_sup 1 an, maintenant faut faire de même avec les moins de 1 an + ajouter les baux mobilité etc// RQ surement une manière d'aller plus vite en utilisant (|)
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*75/365/4.07*0.72) ~ "HRP_inf1sup28", 
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & instant_bookable =="t" & availability_365 > 44 ~ "HRP_inf1sup28", 
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & instant_bookable == "t" & calculated_host_listings_count >1  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & instant_bookable == "t" & license_dupli_pltf_count >1  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & instant_bookable == "t" & license_dupli_air_count >1   ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & instant_bookable == "t" & license_faux=="FAUX"  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & instant_bookable == "t" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & calculated_host_listings_count >1 & license_dupli_pltf_count >1  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & calculated_host_listings_count >1 & license_dupli_air_count >1   ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & calculated_host_listings_count >1 & license_faux=="FAUX"  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & calculated_host_listings_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & license_dupli_pltf_count >1 & license_dupli_air_count >1   ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & license_dupli_pltf_count >1 & license_faux=="FAUX"  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & license_dupli_pltf_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & license_dupli_air_count >1 & license_faux=="FAUX"  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & license_dupli_air_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf1sup28",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day >27 & number_of_reviews_ltm > (duree_activ_scrpg_day*30/365/4.07*0.72) & license_faux=="FAUX" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf1sup28", #là on a toutes les annonces HRP_inf 1 an sup à 28j + faut ajouter les baux mobilité etc// RQ surement une manière d'aller plus vite en utilisant (|)
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & number_of_reviews_ltm >= 2 ~ "HRP_inf28j", 
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & instant_bookable =="t" & availability_365 > 44 ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & instant_bookable == "t" & calculated_host_listings_count >1  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & instant_bookable == "t" & license_dupli_pltf_count >1  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & instant_bookable == "t" & license_dupli_air_count >1   ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & instant_bookable == "t" & license_faux=="FAUX"  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & instant_bookable == "t" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & calculated_host_listings_count >1 & license_dupli_pltf_count >1  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & calculated_host_listings_count >1 & license_dupli_air_count >1   ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & calculated_host_listings_count >1 & license_faux=="FAUX"  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & calculated_host_listings_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & license_dupli_pltf_count >1 & license_dupli_air_count >1   ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & license_dupli_pltf_count >1 & license_faux=="FAUX"  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & license_dupli_pltf_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & license_dupli_air_count >1 & license_faux=="FAUX"  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & license_dupli_air_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf28j",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == FALSE & duree_activ_scrpg_day <28 & (number_of_reviews_ltm ==0 | number_of_reviews_ltm ==1) & license_faux=="FAUX" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_inf28j", #là on a toutes les annonces HRP_inf 1 an sup à 28j + faut ajouter les baux mobilité etc// RQ surement une manière d'aller plus vite en utilisant (|) ?
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == TRUE &  bloK_Air == FALSE & hotelRdT==FALSE ~ "HRP_bailmob", #là on a les baux mobilité
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & minimum_nights >27  ~ "HRP_moyHbmob", #là on a les 967 ann (pour fev 2023) qui sont des STR de moyenne durée
      ID_URL != "568093760836051568" & room_type=="Hotel room" & bail_mob == FALSE &  bloK_Air == FALSE & hotelRdT==FALSE & !is.na(license_Nett)  ~ "HRP_fhotel", #là on a les 18 ann (pour fev 2023) qui sont des STR pro planquées dans les hotels
      TRUE ~ NA_character_
    )
  )

freq(IA_MGP18_23$typo_HRP)

Tab_HRP <- IA_MGP18_23 %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annonces = n(),
    nb_annoncesHH = sum(!room_type == "Hotel room", na.rm = TRUE),
    nb_logt_ent = sum(room_type == "Entire home/apt", na.rm = TRUE),
    nb_pr_sr = sum(room_type == "Private room") +  sum(room_type == "Shared room"),
    nb_bloK = sum(bloK_Air == TRUE & room_type == "Entire home/apt"),
    nb_inact = sum(bloK_Air == FALSE & room_type == "Entire home/apt" & minimum_nights <28 & number_of_reviews_ltm == 0 & sup1an_scrpg == TRUE ),
    nb_HRP_bailmob = sum(typo_HRP == "HRP_bailmob", na.rm = TRUE),
    nb_HRP_fhotel = sum(typo_HRP == "HRP_fhotel", na.rm = TRUE),
    nb_HRP_moyHbmob = sum(typo_HRP == "HRP_moyHbmob", na.rm = TRUE),
    nb_HRP_inf28j = sum(typo_HRP == "HRP_inf28j", na.rm = TRUE),
    nb_HRP_inf1sup28 = sum(typo_HRP == "HRP_inf1sup28", na.rm = TRUE),
    nb_HRP_sup1 = sum(typo_HRP == "HRP_sup1", na.rm = TRUE),
    nb_HRP_LCD = nb_HRP_inf28j + nb_HRP_inf1sup28 + nb_HRP_sup1,
    nb_HRP_LCD_pct = nb_HRP_LCD/nb_annonces*100,
    nb_HRP_TOT = nb_HRP_bailmob + nb_HRP_fhotel + nb_HRP_moyHbmob + nb_HRP_LCD,
    nb_HRP_TOT_pct = nb_HRP_TOT/nb_annonces*100)

names(IA_MGP18_23)


#Test du recouvrement des critères du modele en faisceau----
#de sorte a voir quels criteres ramenent le plus d'ann

#Creation de la colonne HRP pour 2023
HF_202302 <-  HF_202302 %>%
  mutate(
    testC_HRP = case_when(
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >12 ~ "HRP_Comm",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable =="t" & availability_365 >44 ~ "HRP_IB45",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & calculated_host_listings_count >1  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & license_dupli_pltf_count >1  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & license_dupli_air_count >1   ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & license_faux=="FAUX"  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & license_dupli_pltf_count >1  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & license_dupli_air_count >1   ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & license_faux=="FAUX"  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_pltf_count >1 & license_dupli_air_count >1   ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_pltf_count >1 & license_faux=="FAUX"  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_pltf_count >1 & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_pltf_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_air_count >1 & license_faux=="FAUX"  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_air_count >1 & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_air_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_faux=="FAUX" & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_faux=="FAUX" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & number_of_reviews>64 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4", #là on a toutes les annonces HRP_sup 1 an, maintenant faut faire de même avec les moins de 1 an + ajouter les baux mobilité etc// RQ surement une manière d'aller plus vite en utilisant (|)
      TRUE ~ NA_character_
    )
  )

freq(HF_202302$testC_HRP)


HF_202302 <-  HF_202302 %>%
  mutate(
    testC_HRP_COM = case_when(
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE & number_of_reviews_ltm >12 ~ "HRP_Comm",
  TRUE ~ NA_character_
    )
  )

HF_202302 <-  HF_202302 %>%
  mutate(
    testC_HRP_IB45 = case_when(
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable =="t" & availability_365 >44 ~ "HRP_IB45",
     TRUE ~ NA_character_
    )
  )

HF_202302 <-  HF_202302 %>%
  mutate(
    testC_HRP_4 = case_when(
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & calculated_host_listings_count >1  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & license_dupli_pltf_count >1  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & license_dupli_air_count >1   ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & license_faux=="FAUX"  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & instant_bookable == "t" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & license_dupli_pltf_count >1  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & license_dupli_air_count >1   ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & license_faux=="FAUX"  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & calculated_host_listings_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_pltf_count >1 & license_dupli_air_count >1   ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_pltf_count >1 & license_faux=="FAUX"  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_pltf_count >1 & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_pltf_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_air_count >1 & license_faux=="FAUX"  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_air_count >1 & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_dupli_air_count >1 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_faux=="FAUX" & number_of_reviews>64  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & license_faux=="FAUX" & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4",
      ID_URL != "568093760836051568" & room_type=="Entire home/apt" & bail_mob == FALSE &  bloK_Air == FALSE & number_of_reviews_ltm > 0 & hotelRdT==FALSE & minimum_nights <28 & sup1an_scrpg == TRUE  & number_of_reviews>64 & minimum_nights_avg_ntm < 2 & host_acceptance_rateNETT >=98  ~ "HRP_4", #là on a toutes les annonces HRP_sup 1 an, maintenant faut faire de même avec les moins de 1 an + ajouter les baux mobilité etc// RQ surement une manière d'aller plus vite en utilisant (|)
      TRUE ~ NA_character_
    )
  )

freq(HF_202302$testC_HRP)
freq(HF_202302$testC_HRP_COM)
freq(HF_202302$testC_HRP_IB45)
freq(HF_202302$testC_HRP_4)


#Recouvrement
HF_202302 %>%
  mutate(
    HRP_Comm = !is.na(testC_HRP_COM),
    HRP_IB45 = !is.na(testC_HRP_IB45),
    HRP_4 = !is.na(testC_HRP_4)
  ) %>%
  summarise(
    n_total = n(),
    n_HRP_Comm = sum(HRP_Comm),
    n_HRP_IB45 = sum(HRP_IB45),
    n_HRP_4 = sum(HRP_4),
    Comm_and_IB45 = sum(HRP_Comm & HRP_IB45),
    Comm_and_4 = sum(HRP_Comm & HRP_4),
    IB45_and_4 = sum(HRP_IB45 & HRP_4),
    Comm_and_IB45_and_4 = sum(HRP_Comm & HRP_IB45 & HRP_4)
  ) %>%
  mutate(
    across(everything(), ~ format(.x, big.mark = " "))
  )

HF_202302 %>%
  mutate(
    HRP_Comm = !is.na(testC_HRP_COM),
    HRP_IB45 = !is.na(testC_HRP_IB45),
    HRP_4 = !is.na(testC_HRP_4)
  ) %>%
  select(HRP_Comm, HRP_IB45, HRP_4) %>%
  ggvenn(
    c("HRP_Comm", "HRP_IB45", "HRP_4"),
    fill_color = c("#0073C2FF", "#EFC000FF", "#CD534CFF"),
    stroke_size = 0.8,
    set_name_size = 5
  )

#Carto des ann HRP ----
#Prepa du fond de carte IRIS - communes : au final on l'a fait dans QGIS

#Prépa des données : mix iris / communes

names(IA_MGP18_23)
freq(IA_MGP18_23$typo_HRP)

Tab_cartoHRP_Paris <- IA_MGP18_23 %>%
  filter(last_scraped_YM == "2023-02") %>%
  filter(L_CO =="Paris") %>%
  group_by(C_IR) %>%
  summarise(
    nb_ann_tot23_02 = n(),
    HRP_bailmob = sum(typo_HRP == "HRP_bailmob", na.rm = TRUE),
    HRP_bailmob_pct = HRP_bailmob/nb_ann_tot23_02*100,
    HRP_moyHbmob = sum(typo_HRP == "HRP_moyHbmob", na.rm = TRUE),
    HRP_moyHbmob_pct = HRP_moyHbmob/nb_ann_tot23_02*100,
    HRP_bailmob_moy = HRP_bailmob + HRP_moyHbmob,
    HRP_bailmob_moy_pct = HRP_bailmob_moy/nb_ann_tot23_02*100,
    HRP_LCD = sum(typo_HRP == "HRP_inf28j" | typo_HRP == "HRP_inf1sup28" | typo_HRP == "HRP_sup1", na.rm = TRUE),
    HRP_LCD_pct = HRP_LCD/nb_ann_tot23_02*100)

Tab_cartoHRP_Paris <- Tab_cartoHRP_Paris %>%
  rename(C_IRCOM = C_IR)

Tab_cartoHRP_HParis <- IA_MGP18_23 %>%
  filter(last_scraped_YM == "2023-02") %>%
  filter(!L_CO =="Paris") %>%
  group_by(J_C_COINSE) %>%
  summarise(
    nb_ann_tot23_02 = n(),
    HRP_bailmob = sum(typo_HRP == "HRP_bailmob", na.rm = TRUE),
    HRP_bailmob_pct = HRP_bailmob/nb_ann_tot23_02*100,
    HRP_moyHbmob = sum(typo_HRP == "HRP_moyHbmob", na.rm = TRUE),
    HRP_moyHbmob_pct = HRP_moyHbmob/nb_ann_tot23_02*100,
    HRP_bailmob_moy = HRP_bailmob + HRP_moyHbmob,
    HRP_bailmob_moy_pct = HRP_bailmob_moy/nb_ann_tot23_02*100,
    HRP_LCD = sum(typo_HRP == "HRP_inf28j" | typo_HRP == "HRP_inf1sup28" | typo_HRP == "HRP_sup1", na.rm = TRUE),
    HRP_LCD_pct = HRP_LCD/nb_ann_tot23_02*100)

Tab_cartoHRP_HParis <- Tab_cartoHRP_HParis %>%
  rename(C_IRCOM = J_C_COINSE) %>%
  mutate(C_IRCOM = as.character(C_IRCOM))

Tab_cartoHRP <- bind_rows(Tab_cartoHRP_Paris, Tab_cartoHRP_HParis)

#idem 2020
Tab_cartoHRP_Paris20 <- IA_MGP18_23 %>%
  filter(last_scraped_YM == "2020-02") %>%
  filter(L_CO =="Paris") %>%
  group_by(C_IR) %>%
  summarise(
    nb_ann_tot20_01 = n(),
    HRP_bailmob = sum(typo_HRP == "HRP_bailmob", na.rm = TRUE),
    HRP_bailmob_pct = HRP_bailmob/nb_ann_tot20_01*100,
    HRP_moyHbmob = sum(typo_HRP == "HRP_moyHbmob", na.rm = TRUE),
    HRP_moyHbmob_pct = HRP_moyHbmob/nb_ann_tot20_01*100,
    HRP_bailmob_moy = HRP_bailmob + HRP_moyHbmob,
    HRP_bailmob_moy_pct = HRP_bailmob_moy/nb_ann_tot20_01*100,
    HRP_LCD20 = sum(typo_HRP == "HRP_inf28j" | typo_HRP == "HRP_inf1sup28" | typo_HRP == "HRP_sup1", na.rm = TRUE),
    HRP_LCD_pct20 = HRP_LCD20/nb_ann_tot20_01*100)

Tab_cartoHRP_Paris20 <- Tab_cartoHRP_Paris20 %>%
  rename(C_IRCOM = C_IR)

Tab_cartoHRP_HParis20 <- IA_MGP18_23 %>%
  filter(last_scraped_YM == "2020-02") %>%
  filter(!L_CO =="Paris") %>%
  group_by(J_C_COINSE) %>%
  summarise(
    nb_ann_tot20_01 = n(),
    HRP_bailmob = sum(typo_HRP == "HRP_bailmob", na.rm = TRUE),
    HRP_bailmob_pct = HRP_bailmob/nb_ann_tot20_01*100,
    HRP_moyHbmob = sum(typo_HRP == "HRP_moyHbmob", na.rm = TRUE),
    HRP_moyHbmob_pct = HRP_moyHbmob/nb_ann_tot20_01*100,
    HRP_bailmob_moy = HRP_bailmob + HRP_moyHbmob,
    HRP_bailmob_moy_pct = HRP_bailmob_moy/nb_ann_tot20_01*100,
    HRP_LCD20 = sum(typo_HRP == "HRP_inf28j" | typo_HRP == "HRP_inf1sup28" | typo_HRP == "HRP_sup1", na.rm = TRUE),
    HRP_LCD_pct20 = HRP_LCD20/nb_ann_tot20_01*100)

Tab_cartoHRP_HParis20 <- Tab_cartoHRP_HParis20 %>%
  rename(C_IRCOM = J_C_COINSE) %>%
  mutate(C_IRCOM = as.character(C_IRCOM))

Tab_cartoHRP20 <- bind_rows(Tab_cartoHRP_Paris20, Tab_cartoHRP_HParis20)

Tab_cartoHRP <- Tab_cartoHRP %>% left_join (Tab_cartoHRP20, by = "C_IRCOM")

Tab_cartoHRP <- Tab_cartoHRP %>%
  mutate(evolA_HRP = HRP_LCD-HRP_LCD20,
         evolpart_HRP = HRP_LCD_pct-HRP_LCD_pct20)
  
options(scipen=999) # pour éviter d'avoir de l'écriture scientifique
fwrite(Tab_cartoHRP, file = "~/Tab_cartoHRP.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)

rm(Tab_cartoHRP)

## #Prépa des données : communes/arr seulement

Tab_cartoHRP_comm <- IA_MGP18_23 %>%
  filter(last_scraped_YM == "2023-02") %>%
  group_by(C_CAINSEE) %>%
  summarise(
    nb_ann_tot23_02 = n(),
    HRP_bailmob = sum(typo_HRP == "HRP_bailmob", na.rm = TRUE),
    HRP_bailmob_pct = HRP_bailmob/nb_ann_tot23_02*100,
    HRP_moyHbmob = sum(typo_HRP == "HRP_moyHbmob", na.rm = TRUE),
    HRP_moyHbmob_pct = HRP_moyHbmob/nb_ann_tot23_02*100,
    HRP_bailmob_moy = HRP_bailmob + HRP_moyHbmob,
    HRP_bailmob_moy_pct = HRP_bailmob_moy/nb_ann_tot23_02*100,
    HRP_LCD = sum(typo_HRP == "HRP_inf28j" | typo_HRP == "HRP_inf1sup28" | typo_HRP == "HRP_sup1", na.rm = TRUE),
    HRP_LCD_pct = HRP_LCD/nb_ann_tot23_02*100,
    nb_pr_sr = sum(room_type == "Private room") +  sum(room_type == "Shared room"),
    nb_hotel = sum(room_type =="Hotel room"),
    nb_bloK = sum(bloK_Air == TRUE & room_type == "Entire home/apt"),
    nb_RdT = sum(hotelRdT==TRUE),
    RP_LCD_activ = sum(bloK_Air == FALSE & room_type == "Entire home/apt" & is.na(typo_HRP) & minimum_nights < 28 & !(number_of_reviews_ltm==0 & sup1an_scrpg == TRUE)),
    RP_LCD_inactiv = sum(bloK_Air == FALSE & room_type == "Entire home/apt" & is.na(typo_HRP) & minimum_nights < 28 & number_of_reviews_ltm ==0 & sup1an_scrpg == TRUE))
    #, total = nb_ann_tot23_02-HRP_bailmob_moy-HRP_LCD-nb_pr_sr-nb_bloK-RP_LCD_activ-RP_LCD_inactiv-nb_hotel-nb_RdT)

Tab_cartoHRP_comm20 <- IA_MGP18_23 %>%
  filter(last_scraped_YM == "2020-02") %>%
  group_by(C_CAINSEE) %>%
  summarise(
    nb_ann_tot20_01 = n(),
    HRP_bailmob = sum(typo_HRP == "HRP_bailmob", na.rm = TRUE),
    HRP_bailmob_pct = HRP_bailmob/nb_ann_tot20_01*100,
    HRP_moyHbmob = sum(typo_HRP == "HRP_moyHbmob", na.rm = TRUE),
    HRP_moyHbmob_pct = HRP_moyHbmob/nb_ann_tot20_01*100,
    HRP_bailmob_moy = HRP_bailmob + HRP_moyHbmob,
    HRP_bailmob_moy_pct = HRP_bailmob_moy/nb_ann_tot20_01*100,
    HRP_LCD20 = sum(typo_HRP == "HRP_inf28j" | typo_HRP == "HRP_inf1sup28" | typo_HRP == "HRP_sup1", na.rm = TRUE),
    HRP_LCD_pct20 = HRP_LCD20/nb_ann_tot20_01*100,
    nb_pr_sr = sum(room_type == "Private room") +  sum(room_type == "Shared room"),
    nb_hotel = sum(room_type =="Hotel room"),
    nb_bloK = sum(bloK_Air == TRUE & room_type == "Entire home/apt"),
    nb_RdT = sum(hotelRdT==TRUE),
    RP_LCD_activ20 = sum(bloK_Air == FALSE & room_type == "Entire home/apt" & is.na(typo_HRP) & minimum_nights < 28 & !(number_of_reviews_ltm==0 & sup1an_scrpg == TRUE)),
    RP_LCD_inactiv20 = sum(bloK_Air == FALSE & room_type == "Entire home/apt" & is.na(typo_HRP) & minimum_nights < 28 & number_of_reviews_ltm ==0 & sup1an_scrpg == TRUE))
#, total = nb_ann_tot23_02-HRP_bailmob_moy-HRP_LCD-nb_pr_sr-nb_bloK-RP_LCD_activ20-RP_LCD_inactiv20-nb_hotel-nb_RdT)

Tab_cartoHRP_comm <- Tab_cartoHRP_comm %>% left_join (Tab_cartoHRP_comm20, by = "C_CAINSEE")

Tab_cartoHRP_comm <- Tab_cartoHRP_comm %>%
  mutate(evolA_HRP = HRP_LCD-HRP_LCD20,
         evolpart_HRP = HRP_LCD_pct-HRP_LCD_pct20)

options(scipen=999) # pour éviter d'avoir de l'écriture scientifique
fwrite(Tab_cartoHRP_comm, file = "~/Tab_cartoHRP_comm_detailRP.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)

rm(Tab_cartoHRP_comm)

## #Prépa des données : iris Paris

Tab_cartoHRP_irisP <- IA_MGP18_23 %>%
  filter(last_scraped_YM == "2023-02") %>%
  filter(L_CO =="Paris") %>%
  group_by(C_IR) %>%
  summarise(
    nb_ann_tot23_02 = n(),
    HRP_bailmob = sum(typo_HRP == "HRP_bailmob", na.rm = TRUE),
    HRP_bailmob_pct = HRP_bailmob/nb_ann_tot23_02*100,
    HRP_moyHbmob = sum(typo_HRP == "HRP_moyHbmob", na.rm = TRUE),
    HRP_moyHbmob_pct = HRP_moyHbmob/nb_ann_tot23_02*100,
    HRP_bailmob_moy = HRP_bailmob + HRP_moyHbmob,
    HRP_bailmob_moy_pct = HRP_bailmob_moy/nb_ann_tot23_02*100,
    HRP_LCD = sum(typo_HRP == "HRP_inf28j" | typo_HRP == "HRP_inf1sup28" | typo_HRP == "HRP_sup1", na.rm = TRUE),
    HRP_LCD_pct = HRP_LCD/nb_ann_tot23_02*100)

Tab_cartoHRP_irisP20 <- IA_MGP18_23 %>%
  filter(last_scraped_YM == "2020-02") %>%
  filter(L_CO =="Paris") %>%
  group_by(C_IR) %>%
  summarise(
    nb_ann_tot20_01 = n(),
    HRP_bailmob = sum(typo_HRP == "HRP_bailmob", na.rm = TRUE),
    HRP_bailmob_pct = HRP_bailmob/nb_ann_tot20_01*100,
    HRP_moyHbmob = sum(typo_HRP == "HRP_moyHbmob", na.rm = TRUE),
    HRP_moyHbmob_pct = HRP_moyHbmob/nb_ann_tot20_01*100,
    HRP_bailmob_moy = HRP_bailmob + HRP_moyHbmob,
    HRP_bailmob_moy_pct = HRP_bailmob_moy/nb_ann_tot20_01*100,
    HRP_LCD20 = sum(typo_HRP == "HRP_inf28j" | typo_HRP == "HRP_inf1sup28" | typo_HRP == "HRP_sup1", na.rm = TRUE),
    HRP_LCD_pct20 = HRP_LCD20/nb_ann_tot20_01*100)

Tab_cartoHRP_irisP <- Tab_cartoHRP_irisP %>% left_join (Tab_cartoHRP_irisP20, by = "C_IR")

Tab_cartoHRP_irisP <- Tab_cartoHRP_irisP %>%
  mutate(evolA_HRP = HRP_LCD-HRP_LCD20,
         evolpart_HRP = HRP_LCD_pct-HRP_LCD_pct20)

options(scipen=999) # pour éviter d'avoir de l'écriture scientifique
fwrite(Tab_cartoHRP_irisP, file = "~/Tab_cartoHRP_iris_Paris.csv", sep = ";", dec = ".", encoding='UTF-8', row.names = FALSE)
rm(Tab_cartoHRP_irisP, Tab_cartoHRP_irisP20, Tab_cartoHRP_irisP)

#Loueurs de plus de 10 ann -----

summary(IA_MGP18_23$calculated_host_listings_count_entire_homes)

IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>% 
  filter(last_scraped_YM=="2023-02") %>%
  filter(calculated_host_listings_count_entire_homes >1 & calculated_host_listings_count_entire_homes <100) %>% 
  ggplot(aes(x = calculated_host_listings_count_entire_homes)) +
  geom_histogram(binwidth = 1, fill = "#009F81", color = "white") +
  labs(
    title = "Histogramme du nombre d'ann. de logements entiers detenu par loueur·se en fev. 2023",
    x = "Nombre d'ann. detenues",
    y = "Fréquence"
  ) +
  theme_minimal()

ggplot(d,aes(x=heures.tv)) +
  geom_histogram()

Tab_multi <- IA_MGP18_23 %>%
  filter(room_type == "Entire home/apt") %>%
#  filter(L_CO == "Paris") %>%
  group_by(last_scraped_YM) %>%
  summarise(n_LE = n(),
            mono_loueur = sum(calculated_host_listings_count_entire_homes == 1),
            multi = sum(calculated_host_listings_count_entire_homes>1),
            sup10 = sum(calculated_host_listings_count_entire_homes>10),
            mi2_5= sum(calculated_host_listings_count_entire_homes>1 & calculated_host_listings_count_entire_homes<6),
            mi6_10= sum(calculated_host_listings_count_entire_homes>5 & calculated_host_listings_count_entire_homes<11),
            mi11_50= sum(calculated_host_listings_count_entire_homes>10 & calculated_host_listings_count_entire_homes<51),
            misup_50= sum(calculated_host_listings_count_entire_homes>50),
            mono_pct = mono_loueur/n_LE*100,
            multi_pct = multi/n_LE*100,
            sup10_pct = sup10/n_LE*100,
            mi2_5_pct = mi2_5/n_LE*100,
            mi6_10_pct = mi6_10/n_LE*100,
            mi11_50_pct = mi11_50/n_LE*100,
            misup_50_pct = misup_50/n_LE*100)

write.table(Tab_multi, "~/Tab_multiloueur_dates_IA_MGP.csv", sep=",", dec=".", row.names=F)
rm(Tab_multi)

####Ecriture de la base fusionnée pour reprise MGP7 ICI SI CA PLANTE ----
options(scipen=999) # pour éviter d'avoir de l'écriture scientifique

fwrite(IA_MGP18_23, file = "~/Murray France/IA_listings_2018-202311_MGP7.csv", sep = ",", dec = ".", encoding='UTF-8', row.names = FALSE)

#Reouverture de la base : 
col_types <- c(
  ID_URL ="character",
  scrape_id = "character",
  listing_url = "character",
  host_id = "character",
  license = "character",
  license_Nett = "character",
  host_listings_count = "integer",
  host_total_listings_count= "integer",
  accommodates = "numeric",
  bathrooms = "integer",
  bedrooms = "integer",
  beds = "integer",
  minimum_nights = "integer",
  maximum_nights = "integer",
  minimum_minimum_nights  = "integer",
  maximum_minimum_nights  = "integer",
  maximum_nights_avg_ntm = "numeric",
  minimum_maximum_nights = "numeric",   
  maximum_maximum_nights = "numeric",
  minimum_nights_avg_ntm = "numeric",                  
  availability_30 = "integer",
  availability_60 = "integer",
  availability_90 = "integer",
  availability_365 = "integer",
  number_of_reviews = "numeric",
  number_of_reviews_ltm = "numeric",
  number_of_reviews_l30d = "numeric",
  review_scores_rating ="numeric",
  review_scores_value ="numeric",
  review_scores_cleanliness = "numeric",
  review_scores_checkin = "numeric",
  review_scores_communication= "numeric",
  review_scores_accuracy = "numeric",
  review_scores_location = "numeric",     
  calculated_host_listings_count_entire_homes = "numeric",
  calculated_host_listings_count_private_rooms = "numeric",
  calculated_host_listings_count_shared_rooms = "numeric",
  calculated_host_listings_count= "numeric",
  calendar_updated = "character",
  first_review = "character",
  last_review = "character",
  reviews_per_month = "numeric",
  latitude = "character",
  longitude ="character",
  OBJECTID= "character",
  N_SQ_CO= "character",
  J_C_COINSE = "character",
  C_COINSEE= "character",
  C_DEP= "character",
  C_IR= "character",
  L_CO= "character",
  C_POSTSEC= "character",
  C_AGGLO= "character",
  C_VILLENOU= "character",
  C_METROP= "character",
  B_LIMITRO= "character",
  B_UNITEURB= "character",
  N_SQ_DE= "character",
  N_SQ_EPCI= "character",
  NB_POP= "numeric",
  SHAPE_Leng= "character",
  SHAPE_Area= "character",
  zipcode= "character",
  square_feet= "character", 
  last_scraped_YM= "character",
  prem_obser_calc= "character",
  der_obser_calc= "character",
  license_Nett = "character",
  calc_orig = "character",
  monthly_price_logi = "logical", #ça je sais pas si ça fonctionne
  avail_365_logi = "logical"
)

IA_MGP18_23 <- fread("~/Murray France/IA_listings_2018-202311_MGP7.csv", 
                     encoding = "UTF-8",
                     na.strings = c("N/A", "", "NA"),
                     colClasses = col_types)

test <- IA_MGP18_23[grepl("e\\+", IA_MGP18_23$ID_URL), ] #repérer où il y a des id sous forme d'écriture scientif : a priori non

###Convertir les dates en date
IA_MGP18_23 <- IA_MGP18_23 %>%
  mutate(last_scraped = as.Date(last_scraped, format = "%Y-%m-%d"),
         last_searched = as.Date(last_searched, format = "%Y-%m-%d"),
         host_since = as.Date(host_since, format = "%Y-%m-%d"),
         calendar_last_scraped = as.Date(calendar_last_scraped, format = "%Y-%m-%d"),
         first_review = as.Date(first_review, format = "%Y-%m-%d"),
         last_review = as.Date(last_review, format = "%Y-%m-%d"),
         prem_obser_calc =as.Date(prem_obser_calc, format = "%Y-%m-%d"),
         der_obser_calc= as.Date(der_obser_calc, format = "%Y-%m-%d"),
         calc_orig=as.Date(calc_orig, format = "%Y-%m-%d"))

str(IA_MGP18_23)
str(IA_MGP18_23[,110:155])
names(IA_MGP18_23)

#Carte des prix pp ----
#preparation des donnees et carto effectuee avec QGIS

summary(IA_MGP18_23$price_pp)

freq(IA_MGP18_23$typo_HRP)
names(IA_MGP18_23)

Tab_evol_prix_pp <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>%
  filter(bloK_Air ==FALSE) %>%
#  filter(!L_CO=="Paris") %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE_nnbloK = n(),
    price_pp_mean = mean(price_pp, na.rm = TRUE),
    price_pp_min = min(price_pp, na.rm = TRUE),
    price_pp_Qr1 = quantile(price_pp, probs = 0.25, na.rm = TRUE),
    price_pp_med = median(price_pp, na.rm = TRUE),
    price_pp_Qr3 = quantile(price_pp, probs = 0.75, na.rm = TRUE),
    price_pp_max = max(price_pp, na.rm = TRUE),
    price_mean = mean(price, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE),
    price_Qr1 = quantile(price, probs = 0.25, na.rm = TRUE),
    price_med = median(price, na.rm = TRUE),
    price_Qr3 = quantile(price, probs = 0.75, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE))

write.table(Tab_evol_prix_pp, "~/Murray France/Tab_prix_IA_MGPHH_nonbloK.csv", sep=",", dec=".", row.names=F)


Carto_prix_pp_med_2023_02 <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>%
  filter(bloK_Air ==FALSE) %>%
  filter(last_scraped_YM=="2023-02") %>%
  group_by(C_CAINSEE) %>%
  summarise(
    nb_annLE_nnbloK_2023_02 = n(),
    price_pp_med = mean(price_pp, na.rm = TRUE),
    price_pp_mean = mean(price_pp, na.rm = TRUE),
    price_pp_min = min(price_pp, na.rm = TRUE),
    price_pp_Qr1 = quantile(price_pp, probs = 0.25, na.rm = TRUE),
    price_pp_med = median(price_pp, na.rm = TRUE),
    price_pp_Qr3 = quantile(price_pp, probs = 0.75, na.rm = TRUE),
    price_pp_max = max(price_pp, na.rm = TRUE),
    price_mean = mean(price, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE),
    price_Qr1 = quantile(price, probs = 0.25, na.rm = TRUE),
    price_med = median(price, na.rm = TRUE),
    price_Qr3 = quantile(price, probs = 0.75, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE))

write.table(Carto_prix_pp_med_2023_02, "~/Murray France/Carto_prix_IA_MGPHH_nonbloK_2023_02.csv", sep=",", dec=".", row.names=F)
rm(Carto_prix_pp_med_2023_02, Tab_evol_prix_pp)

# Prix nuitee Paris ----

freq(IA_Paris$last_scraped)
summary(IA_Paris$price)
IA_Paris$price

#D'abord on créé last_scraped_YM
IA_Paris	<- IA_Paris %>% mutate(Annee_extraction= str_sub(last_scraped,1,4))
IA_Paris	<- IA_Paris %>% mutate(Mois_extraction= str_sub(last_scraped,6,7))

IA_Paris <- IA_Paris %>% mutate (last_scraped_YM = str_c(Annee_extraction, Mois_extraction, sep="-"))

IA_Paris <- IA_Paris %>% mutate (price = str_replace (price, fixed("$"), "")) %>%
  mutate(price=str_replace(price, fixed(","), ""))
IA_Paris$price <- as.numeric(IA_Paris$price)

Tab_evol_prix_Paris <- IA_Paris %>%
  filter(room_type =="Entire home/apt") %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    price_mean = mean(price, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE),
    price_Qr1 = quantile(price, probs = 0.25, na.rm = TRUE),
    price_med = median(price, na.rm = TRUE),
    price_Qr3 = quantile(price, probs = 0.75, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE))


#Fréquence de location, minstay ----
#minstay = minimum_nights = durée minimum de séjour proposée à la location

summary(IA_MGP18_23$minimum_nights)
freq(IA_MGP18_23$minimum_nights)

Tab_minstay <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>%
  filter(!L_CO=="Paris") %>%
  filter(bloK_Air ==FALSE) %>%
  filter(bail_mob ==FALSE) %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annLE_nnbloK = n(),
    minstay_mean = mean(minimum_nights, na.rm = TRUE),
    minstay_med = median (minimum_nights, na.rm = TRUE),
    minstay_1 = sum(minimum_nights==1),
    minstay_1_pct = minstay_1 /nb_annLE_nnbloK*100)

Tab_minstay_Paris <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>%
  filter(L_CO=="Paris") %>%
  filter(bloK_Air ==FALSE) %>%
  filter(bail_mob ==FALSE) %>%
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annLE_nnbloK = n(),
    minstay_mean = mean(minimum_nights, na.rm = TRUE),
    minstay_med = median (minimum_nights, na.rm = TRUE),
    minstay_1 = sum(minimum_nights==1),
    minstay_1_pct = minstay_1 /nb_annLE_nnbloK*100)


####### Travail sur host_location (chap 4) ----
#host_location correspond à la localisation déclarée par les loueur.ses

freq(IA_MGP18_23$host_location)

#Mettre tout en majuscule
IA_MGP18_23$host_location2 <- str_to_upper(IA_MGP18_23$host_location) #ca met tout en majuscules

#Enlever les accents
IA_MGP18_23$host_location2 <- IA_MGP18_23$host_location2 %>% 
  gsub(pattern = "Œ", replacement = "OE") %>%
  gsub(pattern = "Æ", replacement = "AE") %>%
  gsub(pattern = "[`^~\"]", replacement = " ") %>%
  gsub(pattern = "/", replacement = " ") %>%
  gsub(pattern = "'", replacement = " ") %>%
  gsub(pattern = "-", replacement = " ") %>%
  str_trim(side="both") %>%
  chartr(old = "ÁÀÂÃÅÄÊËÉÈÏÎÌÔÖÕØÛÜÙÚŸÝÇÑ", new = "AAAAAAEEEEIIIOOOOUUUUYYCN")

##### création de 3 colonnes qui correspondent à "ville, région, pays"

#On nettoie les pays renseignés à 2 lettres seulement, en créant host_location3 pour vérifier au fur et à mesure l'étendue des transformations : 
IA_MGP18_23$host_location3 <- IA_MGP18_23$host_location2
IA_MGP18_23 <- IA_MGP18_23 %>% mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="US", ", , ETATS UNIS", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="FR", ", , FRANCE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="GB", ", , ROYAUME UNI", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="IT", ", , ITALIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="IE", ", , IRLANDE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="IN", ", , INDE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="ES", ", , ESPAGNE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="CA", ", , CANADA", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="CN", ", , CHINE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="MA", ", , MAROC", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="MY", ", , MALAISIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="NL", ", , PAYS BAS", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="DE", ", , ALLEMAGNE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="BE", ", , BELGIQUE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="IL", ", , ISRAEL", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="CH", ", , SUISSE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="TN", ", , TUNISIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="RU", ", , RUSSIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="AU", ", , AUSTRALIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="BR", ", , BRESIL", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="GR", ", , GRECE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="JP", ", , JAPON", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="EU", ", , UNION EUROPEENNE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="PT", ", , PORTUGAL", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="PH", ", , PHILIPPINES", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="LB", ", , LIBAN", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="HK", ", , HONG KONG", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="RE", ", , REUNION", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="MQ", ", , MARTINIQUE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="CZ", ", , REPUBLIQUE TCHEQUE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="RO", ", , ROUMANIE", host_location3))  %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="RS", ", , SERBIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="UK", ", , ROYAUME UNI", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="PF", ", , POLYNESIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="AR", ", , ARGENTINE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="TR", ", , TURQUIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="KR", ", , COREE DU SUD", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="CO", ", , COLOMBIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="SG", ", , SINGAPOUR", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="MX", ", , MEXIQUE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="GP", ", , GUADELOUPE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="TH", ", , THAILANDE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="PL", ", , POLOGNE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="LU", ", , LUXEMBOURG", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="MG", ", , MADAGASCAR", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="ID", ", , INDONESIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="VN", ", , VIETNAM", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="MU", ", , MAURICE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="DJ", ", , DJIBOUTI", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="TW", ", , TAIWAN", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="BJ", ", , BENIN", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="NC", ", , NOUVELLE CALEDONIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="LT", ", , LITUANIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="DK", ", , DANEMARK", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="UA", ", , UKRAINE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="AT", ", , AUTRICHE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="MC", ", , MONACO", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="NO", ", , NORVEGE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="QA", ", , QATAR", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="CM", ", , CAMEROUN", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="SE", ", , SUEDE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="A2", ", , INCONNU", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="KM", ", , COMORES", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="HR", ", , CROATIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="DO", ", , REPUBLIQUE DOMINICAINE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="KG", ", , KIRGHIZISTAN", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="CG", ", , CONGO", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="DZ", ", , ALGERIE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="EG", ", , EGYPTE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="ZA", ", , AFRIQUE DU SUD", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="GA", ", , GABON", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="SJ", ", , NORVEGE", host_location3)) %>%
  mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location2)==2 & IA_MGP18_23$host_location2=="CR", ", , COSTA RICA", host_location3))

## On ajoute quelques exceptions supplémentaires dans le nettoyage (notamment car des LONDON seuls il y en a plein)
IA_MGP18_23 <- IA_MGP18_23 %>% mutate (host_location3 = ifelse(grepl("UK",host_location3)," , , ROYAUME UNI", host_location3)) %>%
  mutate (host_location3 = ifelse(host_location3=="FRANCE"," , , FRANCE", host_location3)) %>%
  mutate (host_location3 = ifelse(host_location3=="PARIS","PARIS, ILE DE FRANCE, FRANCE", host_location3)) %>%
  mutate (host_location3 = ifelse(grepl("LONDON",host_location3),"LONDRES, , ROYAUME UNI", host_location3)) %>%
  mutate (host_location3 = ifelse(grepl("LONDRES",host_location3),"LONDRES, , ROYAUME UNI", host_location3)) %>%
  mutate (host_location3 = ifelse(grepl("UK",host_location3)," , , ROYAUME UNI", host_location3))

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (host_location3 = ifelse(str_length(IA_MGP18_23$host_location3)==2, ", , AUTRE ETRANGER", host_location3))

##Ensuite on créé les 3 colonnes : Ville / Region / Pays et on  nettoye à nouveau "Pays" pour enlever les éventuelles virgules et espaces en trop et autres symboles qui seraient présents
IA_MGP18_23$host_location4 <- IA_MGP18_23$host_location3
IA_MGP18_23 <- IA_MGP18_23 %>% separate(host_location4,c("Ville_HL3", "Region_HL3", "Pays_HL3"), sep=", ")

IA_MGP18_23$Pays_HL3 <- IA_MGP18_23$Pays_HL3 %>% 
  gsub(pattern = "['`^~\".,/0123456789]", replacement = "") %>%
  str_trim(side="both")

#regrouper encore des modalités du factor "PAYS_HL3" qu'on renomme en "Pays_HL3_2" : obligée de découper en 40 morceaux pour gérer la mémoire de l'ordi, trop bizarre... Mais ça a marché !
IA_MGP18_23$Pays_HL3_2 <- as.factor(IA_MGP18_23$Pays_HL3)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Pays_HL3_2 = recode_factor (Pays_HL3_2,ALGERIA ="ALGERIE", 
                                                     ARGENTINA="ARGENTINE", 
                                                     ARMENIA="ARMENIE", 
                                                     ATHENS="GRECE", 
                                                     AUSTRALIA="AUSTRALIE", 
                                                     AUSTRIA="AUTRICHE", 
                                                     BELGIUM="BELGIQUE",
                                                     BRAZIL="BRESIL", 
                                                     BUDAPEST="HONGRIE", 
                                                     CAMEROON="CAMEROUN", 
                                                     CASABLANCA="MAROC", 
                                                     CENTRE="FRANCE",
                                                     CHILE="CHILI", 
                                                     CHINA="CHINE", 
                                                     COLOMBIA="COLOMBIE", 
                                                     COMOROS="COMORES"))

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Pays_HL3_2 = recode_factor (Pays_HL3_2,CROATIA="CROATIE",
                                                     CROUTTES="FRANCE", 
                                                     "CZECH REPUBLIC"="REPUBLIQUE TCHEQUE", 
                                                     CZECHIA="REPUBLIQUE TCHEQUE", 
                                                     "DANS UN IMMEUBLE AVEC DE BELLES COURS EN CONTREBAS"="FRANCE", 
                                                     "DEMOCRATIC REPUBLIC OF THE CONGO"="REPUBLIQUE DEMOCRATIQUE DU CONGO", 
                                                     "EN TANT QUANTHROPOLOGUE ET SAGE FEMME"="", 
                                                     "DISTRICT OF COLUMBIA"="ETATS UNIS", 
                                                     "DOMINICAN REPUBLIC"="REPUBLIQUE DOMINICAINE", 
                                                     DENMARK="DANEMARK", 
                                                     EGYPT="EGYPTE", 
                                                     "EME ARRONDISSEMENT"="FRANCE", 
                                                     "FRENCH POLYNESIA"="POLYNESIE", 
                                                     GALICIA="ESPAGNE", 
                                                     "GANGWON DO"="COREE DU SUD",
                                                     GERMANY="ALLEMAGNE"))

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Pays_HL3_2 = recode_factor (Pays_HL3_2,GREECE="GRECE",  
                                                     HUNGARY="HONGRIE",  
                                                     "HYOGO PREFECTURE"="JAPON",  
                                                     "ILE DE FRANCE"="FRANCE",  
                                                     "IN ADDTIOTION TO NUMEROUS BUSESS"="",  
                                                     INDIA="INDE",  
                                                     INCONNU="",  
                                                     INDONESIA="INDONESIE",  
                                                     IRELAND="IRLANDE",  
                                                     ITALY="ITALIE",  
                                                     JAPAN="JAPON",  
                                                     "KANAGAWA PREFECTURE"="JAPON")) 

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Pays_HL3_2 = recode_factor (Pays_HL3_2,LONDRES="ROYAUME UNI", 
                                                     "LONDRES ET LA NORMANDIE :)"="ROYAUME UNI", 
                                                     "LOWER NORMANDY"="FRANCE", 
                                                     MALAYSIA="MALAISIE", 
                                                     MESSENIE="GRECE", 
                                                     MOROCCO="MAROC", 
                                                     MEXICO="MEXIQUE", 
                                                     MOLDOVA="MOLDAVIE",
                                                     MONTPELLIER="FRANCE", 
                                                     "NEW CALEDONIA"="NOUVELLE CALEDONIE")) 

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Pays_HL3_2 = recode_factor (Pays_HL3_2,"NORTH MACEDONIA"="MACEDOINE", 
                                                     NORWAY="NORVEGE", 
                                                     "NEW YORK"="ETATS UNIS", 
                                                     "NY  CHICAGO"="ETATS UNIS", 
                                                     ON="", 
                                                     "PAYS DE LA LOIRE"="FRANCE", 
                                                     "POITOU CHARENTES"="FRANCE",
                                                     "RHONE ALPES"="FRANCE",
                                                     POLAND="POLOGNE"))

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Pays_HL3_2 = recode_factor (Pays_HL3_2,PUNE="INDE", 
                                                     "REPUBLIC OF THE CONGO"="REPUBLIQUE DU CONGO", 
                                                     CONGO="REPUBLIQUE DU CONGO", 
                                                     ROMANIA="ROUMANIE",
                                                     RUSSIA="RUSSIE", 
                                                     "RUSSIAN FEDERATION"="RUSSIE",
                                                     "SAUDI ARABIA"="ARABIE SAOUDITE",
                                                     SERBIA="SERBIE",
                                                     "SINT MAARTEN"="PAYS BAS",
                                                     RUSSIA="RUSSIE",
                                                     "GYEONGGI DO"="COREE DU SUD", 
                                                     "GYEONGSANGBUK DO"="COREE DU SUD", 
                                                     "SODRA VAGEN B"="SUEDE",
                                                     "SOUTH AFRICA"="AFRIQUE DU SUD",
                                                     "SOUTH KOREA"="COREE DU SUD",
                                                     SPAIN="ESPAGNE"))

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Pays_HL3_2 = recode_factor (Pays_HL3_2,SWEDEN="SUEDE",
                                                     SWITZERLAND="SUISSE",
                                                     "TH DISTRICT"="ETATS UNIS",
                                                     THAILAND="THAILANDE",
                                                     "THE NETHERLANDS"="PAYS BAS",
                                                     TUNISIA="TUNISIE",
                                                     TURKEY="TURQUIE",
                                                     UK="ROYAUME UNI"))


IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Pays_HL3_2 = recode_factor (Pays_HL3_2,"UNITED KINGDOM"="ROYAUME UNI",
                                                     "UTRECHT (RIETVELD HOUSE)"="PAYS BAS",
                                                     VALENCIENNES="FRANCE",
                                                     WASHINGTON="ETATS UNIS",
                                                     "UNITED ARAB EMIRATES"="EMIRATS ARABES UNIS",
                                                     "UNITED STATES"="ETATS UNIS",
                                                     BERMUDA="BERMUDES",
                                                     "WJ AE"="",
                                                     KYRGYZSTAN="KIRGHIZISTAN",  
                                                     LEBANON="LIBAN",  
                                                     LATVIA="LETTONIE",   
                                                     LITHUANIA="LITUANIE", 
                                                     LONDON="ROYAUME UNI",
                                                     "FRANCE  RUE CUSINO VILLEPINTE"="FRANCE",
                                                     "THE NETHERLANDS"="NETHERLANDS",
                                                     "EN TANT QU ANTHROPOLOGUE ET SAGE FEMME"="NA",
                                                     "NETHERLANDS"="PAYS BAS"))

summary(IA_MGP18_23$Pays_HL3_2)
freq(IA_MGP18_23$Pays_HL3_2)
levels(IA_MGP18_23$Pays_HL3_2)

IA_MGP18_23$Pays_HL3_2_categ <- IA_MGP18_23$Pays_HL3_2

IA_MGP18_23 <- IA_MGP18_23 %>%   mutate (Pays_HL3_2_categ = ifelse(Pays_HL3_2 =="FRANCE", "France metropolitaine", "Hors France")) %>%
  mutate (Pays_HL3_2_categ = ifelse(Pays_HL3_2 =="NA"|is.na(Pays_HL3_2), NA, Pays_HL3_2_categ)) %>%
  mutate (Pays_HL3_2_categ = ifelse(grepl("GUADELOUPE",Pays_HL3_2)|
                                      grepl("FRENCH GUIANA",Pays_HL3_2)|
                                      grepl("MARTINIQUE",Pays_HL3_2)|
                                      grepl("MAYOTTE",Pays_HL3_2)|
                                      grepl("NOUVELLE CALEDONIE",Pays_HL3_2)|
                                      grepl("POLYNESIE",Pays_HL3_2)|
                                      grepl("REUNION",Pays_HL3_2)|
                                      grepl("MIQUELON",Pays_HL3_2)|
                                      grepl("WALLIS",Pays_HL3_2),"France DROM-COM", Pays_HL3_2_categ))

freq(IA_MGP18_23$Pays_HL3_2_categ)
IA_MGP18_23$Pays_HL3_2_categ <- as.factor(IA_MGP18_23$Pays_HL3_2_categ)

#On essaye de bosser sur la variable Region_HL3 maintenant, sachant qu'on veut parvenir à 5 levels: Region_categ : IDF / France métropolitaine hors IDF / France DROM-COM / hors France / NA
IA_MGP18_23$Region_HL3_2 <- IA_MGP18_23$Region_HL3

IA_MGP18_23 <- IA_MGP18_23 %>%   mutate (Region_HL3_2 = ifelse(Pays_HL3_2_categ =="Hors France", "Hors France", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(Pays_HL3_2_categ =="France DROM-COM", "France DROM-COM", Region_HL3_2))

freq(IA_MGP18_23$Region_HL3_2)  

IA_MGP18_23$Region_HL3_2 <- str_squish(IA_MGP18_23$Region_HL3_2)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Region_HL3_2 = ifelse(grepl("AUVERGNE",Region_HL3_2)|grepl("RHONE",Region_HL3_2)|grepl("GRENOBLE",Region_HL3_2)|grepl("LYON",Region_HL3_2)|grepl("ISERE",Region_HL3_2),"Auvergne-Rhone-Alpes", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("PROVENCE",Region_HL3_2),"Provence-Alpes-Cote d'Azur", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("BURGANDY",Region_HL3_2)|grepl("BURGUNDY",Region_HL3_2)|grepl("COMTE",Region_HL3_2)|grepl("BOURGOGNE",Region_HL3_2),"Bourgogne-Franche-Comté", Region_HL3_2))%>%
  mutate (Region_HL3_2 = ifelse(grepl("BRETAGNE",Region_HL3_2)|grepl("BRITTANY",Region_HL3_2)|grepl("LE BAS BOURG",Region_HL3_2),"Bretagne", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("CENTRE",Region_HL3_2)|grepl("LOIRET",Region_HL3_2)|grepl("CHER",Region_HL3_2),"Centre-Val de Loire", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("NORMANDY",Region_HL3_2)|grepl("NORMANDIE",Region_HL3_2)|grepl("MANCHE",Region_HL3_2),"Normandie", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("ALSACE",Region_HL3_2)|grepl("CHAMPAGNE",Region_HL3_2)|grepl("GRAND EST",Region_HL3_2)|grepl("LORRAINE",Region_HL3_2),"Grand Est", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("CORSICA",Region_HL3_2)|grepl("CORSE",Region_HL3_2),"Corse", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("HAUTS DE FRANCE",Region_HL3_2)|grepl("NORD",Region_HL3_2)|grepl("PICARDY",Region_HL3_2)|grepl("PICARDIE",Region_HL3_2),"Hauts-de-France", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("AQUITAINE",Region_HL3_2)|grepl("POITOU",Region_HL3_2)|grepl("LIMOUSIN",Region_HL3_2),"Nouvelle-Aquitaine", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("OCCITANIE",Region_HL3_2)|grepl("MIDI",Region_HL3_2)|grepl("LANGUEDOC",Region_HL3_2)|grepl("LA SEGUINIE 46600 MARTEL",Region_HL3_2),"Occitanie", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("PARIS",Region_HL3_2)|grepl("ILE DE FRANCE",Region_HL3_2)|grepl("VAL D OISE",Region_HL3_2)|grepl("YVELINES",Region_HL3_2)|grepl("NEUILLY SUR SEINE",Region_HL3_2),"Ile-de-France", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("PAYS DE LA LOIRE",Region_HL3_2)|grepl("VENDEE",Region_HL3_2)|grepl("MAINE ET LOIRE",Region_HL3_2),"Pays de la Loire", Region_HL3_2)) %>%
  mutate (Region_HL3_2 = ifelse(grepl("GUADELOUPE",Region_HL3_2),"France DROM-COM", Region_HL3_2)) %>% 
  mutate (Region_HL3_2 = ifelse(grepl("DANS UN QUARTIER VIVANT",Region_HL3_2)|grepl("FRANCE",Region_HL3_2),NA, Region_HL3_2))

IA_MGP18_23 <- IA_MGP18_23 %>%mutate (Region_HL3_2 = ifelse(Region_HL3_2==""|Region_HL3_2==" ", NA, Region_HL3_2))

#Creation de Region_HL3_2_categ
IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Region_HL3_2_categ = ifelse(grepl("Ile-de-France",Region_HL3_2), "Ile-de-France", "France metropolitaine hors IDF ")) %>%
  mutate (Region_HL3_2_categ = ifelse(is.na(Region_HL3_2), NA, Region_HL3_2_categ)) %>%
  mutate (Region_HL3_2_categ = ifelse(Region_HL3_2 =="Hors France", "Hors France", Region_HL3_2_categ))%>%
  mutate (Region_HL3_2_categ = ifelse(Region_HL3_2 =="France DROM-COM", "France DROM-COM", Region_HL3_2_categ))

freq(IA_MGP18_23$Region_HL3_2_categ)  

#Creation de Region_HL3_2_categR = categories resserrées pour l'AGD = "France DROM-COM" fusionné avec "Hors France"
IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Region_HL3_2_categR = ifelse(Region_HL3_2_categ =="France DROM-COM", "Hors France metropolitaine", Region_HL3_2_categ)) %>%
  mutate (Region_HL3_2_categR = ifelse(Region_HL3_2_categ =="Hors France", "Hors France metropolitaine", Region_HL3_2_categR))

freq(IA_MGP18_23$Region_HL3_2_categR)

##Création de Ville_categ en 7 catégories : France DROM-COM /France metropolitaine hors IDF/ Hors France (=étranger)/Ile-de-France hors MGP/ MGP/ Paris / NA
IA_MGP18_23$Ville_HL3_2 <- IA_MGP18_23$Ville_HL3

IA_MGP18_23 <- IA_MGP18_23 %>%   mutate (Ville_HL3_2 = ifelse(Region_HL3_2_categ =="Hors France", "Hors France", Ville_HL3_2)) %>%
  mutate (Ville_HL3_2 = ifelse(Region_HL3_2_categ =="France DROM-COM", "France DROM-COM", Ville_HL3_2)) %>%
  mutate (Ville_HL3_2 = ifelse(grepl("France metropolitaine hors IDF", Region_HL3_2_categ),"France metropolitaine hors IDF", Ville_HL3_2))

freq(IA_MGP18_23$Ville_HL3_2)

IA_MGP18_23$Ville_HL3_2 <- str_squish(IA_MGP18_23$Ville_HL3_2)

#On passe par une boucle pour différencier les communes de la MGP de celles hors MGP (et hors IDF qui seraient là par erreur)
liste_comm_IDF <- read.csv("~/Liste_communes_IDF_R.csv", encoding = "UTF-8")
liste_comm_MGP <- read.csv("~/Liste_communes_MGP_R.csv", encoding = "UTF-8")

liste_comm_IDF_2<- liste_comm_IDF$NOMS
liste_comm_MGP_2 <- liste_comm_MGP$NOMS
rm(liste_comm_IDF, liste_comm_MGP)

IA_MGP18_23$Ville_HL3_2_categ <- IA_MGP18_23$Ville_HL3_2

for (i in 1:length(liste_comm_IDF_2)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Ville_HL3_2_categ = ifelse(grepl(liste_comm_IDF_2[i], IA_MGP18_23$Ville_HL3_2), "Ile-de-France hors MGP", Ville_HL3_2_categ))
}

for (i in 1:length(liste_comm_MGP_2)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Ville_HL3_2_categ = ifelse(grepl(liste_comm_MGP_2[i], IA_MGP18_23$Ville_HL3_2), "MGP", Ville_HL3_2_categ))
}

freq(IA_MGP18_23$Ville_HL3_2_categ)

#finir de nettoyer les quelques exceptions
IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Ville_HL3_2_categ = ifelse(grepl("GENNEVILLERS",Ville_HL3_2_categ)|grepl("LA DEFENSE",Ville_HL3_2_categ)|grepl("MONTMARTRE",Ville_HL3_2_categ)|grepl("VAL DE MARNE",Ville_HL3_2_categ)|grepl("ILE DE LA JATTE",Ville_HL3_2_categ),"MGP", Ville_HL3_2_categ)) %>%
  mutate (Ville_HL3_2_categ = ifelse(grepl("EPONE",Ville_HL3_2_categ)|grepl("MORET SUR LOING",Ville_HL3_2_categ)|grepl("MARNE LA VALLEE",Ville_HL3_2_categ)|grepl("YVELINES",Ville_HL3_2_categ)|grepl("BREANCON",Ville_HL3_2_categ)|grepl("ECUELLES",Ville_HL3_2_categ),"Ile-de-France hors MGP", Ville_HL3_2_categ)) %>%
  mutate (Ville_HL3_2_categ = ifelse(grepl("EU",Ville_HL3_2_categ)|grepl("NORTH CAROLINA",Ville_HL3_2_categ)|grepl("PARADISE",Ville_HL3_2_categ),"Hors France", Ville_HL3_2_categ)) %>%
  mutate (Ville_HL3_2_categ = ifelse(grepl("FORT DE FRANCE",Ville_HL3_2_categ),"France DROM-COM", Ville_HL3_2_categ))

IA_MGP18_23$Ville_HL3_2_categ <- as.factor(IA_MGP18_23$Ville_HL3_2_categ)

freq(IA_MGP18_23$Ville_HL3_2_categ)

#On décide de mettre à part Paris car en fait c'est une énorme part dans la MGP, création de la variable Ville_HL3_2_categPR pour Paris et regroupée pour l'AGD
#ca créé donc 6 categ : Hors France metropolitaine / France metropolitaine hors IDF / Ile-de-France hors MGP / MGP / PARIS / NA 

IA_MGP18_23$Ville_HL3_2_categPR <- IA_MGP18_23$Ville_HL3_2_categ

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (Ville_HL3_2_categPR = ifelse(Ville_HL3_2 =="PARIS", "Paris", Ville_HL3_2_categ)) %>%
  mutate(Ville_HL3_2_categPR = ifelse(Ville_HL3_2_categPR == "France DROM-COM", "Hors France metropolitaine", Ville_HL3_2_categPR)) %>%
  mutate(Ville_HL3_2_categPR = ifelse(Ville_HL3_2_categPR == "Hors France", "Hors France metropolitaine", Ville_HL3_2_categPR))%>%
  mutate(Ville_HL3_2_categPR = ifelse(Ville_HL3_2_categPR == "MGP", "MGP hors Paris", Ville_HL3_2_categPR))

freq(IA_MGP18_23$Ville_HL3_2_categPR)

IA_MGP18_23$Ville_HL3_2_categPR <- as.factor(IA_MGP18_23$Ville_HL3_2_categPR)

## Pour terminer, on fait des regroupements géographiques dans host_location pour détailler les zones du monde

freq(IA_MGP18_23$Pays_HL3_2)
freq(IA_MGP18_23$Ville_HL3_2_categ)
freq(IA_MGP18_23$Ville_HL3_2_categPR) #Paris à part

IA_MGP18_23$PRV_location <- as.character(IA_MGP18_23$Pays_HL3_2)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl("Paris",Ville_HL3_2_categPR), "Paris", PRV_location)) %>%
  mutate(PRV_location = ifelse(grepl("France metropolitaine hors IDF",Ville_HL3_2_categPR), "France metropolitaine hors IDF", PRV_location)) %>%
  mutate(PRV_location = ifelse(grepl("Ile-de-France hors MGP",Ville_HL3_2_categPR), "Ile-de-France hors MGP", PRV_location)) %>%
  mutate(PRV_location = ifelse(grepl("MGP hors Paris",Ville_HL3_2_categPR), "MGP hors Paris", PRV_location)) %>%
  mutate(PRV_location = ifelse(grepl("France DROM-COM",Ville_HL3_2_categ), "France DROM-COM", PRV_location)) %>%
  mutate(PRV_location = ifelse(grepl("NA",PRV_location), NA, PRV_location)) %>%
  mutate(PRV_location = ifelse(PRV_location == "FRANCE", "France indéterminé", PRV_location))

freq(IA_MGP18_23$PRV_location)

liste_asie_sud_est <- c("AFGHANISTAN", "BALI", "CAMBODIA", "CHINE", "COREE DU SUD", "INDE", "PHILIPPINES", "INDONESIE", "HONG KONG", "JAPON", "SINGAPOUR", "SRI LANKA", "COREE DU SUD", "CAMBODIA", "TAIWAN", "PAKISTAN", "MALAISIE", "BALI", "PHILIPPINES", "THAILANDE")
liste_asie_ouest <- c("ARMENIE", "ARABIE SAOUDITE", "BAHRAIN", "EMIRATS ARABES UNIS", "GEORGIA", "ISRAEL", "JORDAN", "LIBAN", "OMAN", "QATAR", "TURQUIE" )
liste_russie_asie_centrale <- c("KAZAKHSTAN", "KIRGHIZISTAN", "TAJIKISTAN", "RUSSIE")
liste_am_nord <- c("ETATS UNIS", "CANADA", "NY   CHICAGO")
liste_am_lat_carab <- c("ARGENTINE", "BERMUDES", "THE BAHAMAS", "BOLIVIA", "BRESIL", "CAYMAN ISLANDS", "CHILI", "COLOMBIE", "COSTA RICA", "CUBA", "ECUADOR", "EL SALVADOR", "GUATEMALA", "HAITI", "JAMAICA", "MEXIQUE", "NICARAGUA", "PERU", "PUERTO RICO", "REPUBLIQUE DOMINICAINE", "SAINT MARTIN", "URUGUAY", "VENEZUELA")
liste_monde_austral_pacifique <- c("AUSTRALIE", "NEW ZEALAND", "VANUATU")
liste_af_nord <- c("MAROC", "ALGERIE", "EGYPTE", "LIBYA", "TUNISIE" )
liste_af_subsa <- c("AFRIQUE DU SUD", "ANGOLA", "BENIN", "BURUNDI", "CAMEROUN", "COMORES", "COTE D IVOIRE", "DJIBOUTI", "EQUATORIAL GUINEA", "ETHIOPIA", "GABON", "KENYA", "MADAGASCAR", "MALI", "MAURICE", "MAURITIUS", "MOZAMBIQUE", "NIGERIA", "REPUBLIQUE DEMOCRATIQUE DU CONGO", "REPUBLIQUE DU CONGO", "RWANDA", "SENEGAL", "SEYCHELLES", "TANZANIA", "TOGO", "UGANDA", "ZIMBABWE")
liste_europe <- c("ALBANIA", "ALLEMAGNE", "ANDORRA", "ARMENIE", "AUTRICHE", "BELARUS", "BELGIQUE", "BULGARIA", "CROATIE", "CYPRUS", "DANEMARK", "ESPAGNE", "ESTONIA", "FINLAND", "GRECE", "HONGRIE", "ICELAND", "IRLANDE", "ITALIE", "LETTONIE", "LITUANIE", "LUXEMBOURG", "MACEDOINE", "MOLDAVIE", "NORVEGE", "PAYS BAS", "POLOGNE", "PORTUGAL", "REPUBLIQUE TCHEQUE", "ROUMANIE", "ROYAUME UNI", "SERBIE", "SLOVAKIA", "SLOVENIA", "SUEDE", "SUISSE", "UNION EUROPEENNE" )


for (i in 1:length(liste_asie_sud_est)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl(liste_asie_sud_est[i], IA_MGP18_23$PRV_location), "Asie du Sud et de l'Est", PRV_location))
}
for (i in 1:length(liste_asie_ouest)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl(liste_asie_ouest[i], IA_MGP18_23$PRV_location), "Asie du Sud-Ouest", PRV_location))
}
for (i in 1:length(liste_russie_asie_centrale)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl(liste_russie_asie_centrale[i], IA_MGP18_23$PRV_location), "Asie centrale et Russie", PRV_location))
}
for (i in 1:length(liste_am_nord)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl(liste_am_nord[i], IA_MGP18_23$PRV_location), "Amérique du Nord", PRV_location))
}
for (i in 1:length(liste_am_lat_carab)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl(liste_am_lat_carab[i], IA_MGP18_23$PRV_location), "Amérique latine et Caraïbes", PRV_location))
}
for (i in 1:length(liste_monde_austral_pacifique)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl(liste_monde_austral_pacifique[i], IA_MGP18_23$PRV_location), "Monde austral et du Pacifique", PRV_location))
}
for (i in 1:length(liste_af_nord)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl(liste_af_nord[i], IA_MGP18_23$PRV_location), "Afrique du Nord", PRV_location))
}
for (i in 1:length(liste_af_subsa)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl(liste_af_subsa[i], IA_MGP18_23$PRV_location), "Afrique subsaharienne", PRV_location))
}
for (i in 1:length(liste_europe)){ 
  IA_MGP18_23 <- IA_MGP18_23 %>% mutate (PRV_location = ifelse(grepl(liste_europe[i], IA_MGP18_23$PRV_location), "Europe", PRV_location))
}

IA_MGP18_23$PRV_location2 <- as.character(IA_MGP18_23$PRV_location)

IA_MGP18_23 <- IA_MGP18_23 %>% mutate(PRV_location2 = ifelse(PRV_location2=="", NA, PRV_location2)) %>%
  mutate(PRV_location2 = ifelse(PRV_location2=="BRETAGNE", NA, PRV_location2)) %>%
  mutate(PRV_location2 = ifelse(PRV_location2=="AUTRE ETRANGER", "Autre", PRV_location2))

IA_MGP18_23$PRV_location <- IA_MGP18_23$PRV_location2

IA_MGP18_23 <- IA_MGP18_23 %>% select(-PRV_location2)

IA_MGP18_23$PRV_location <- as.factor(IA_MGP18_23$PRV_location)

#on supprime host_location2 et Pays_HL3
IA_MGP18_23 <- IA_MGP18_23 %>% select(-host_location2, -Pays_HL3)

freq(IA_MGP18_23$PRV_location) # France DROM-COM / France Metro / Hors France /NA

#Region_HL3_2_categR : France metropolitaine hors IDF / Hors France metropolitaine  / Ile-de-France / NA
freq(IA_MGP18_23$Region_HL3_2_categR)

#Ville_HL3_2_categ : France DROM-COM / France metropolitaine hors IDF /Hors France / Ile-de-France hors MGP / MGP / NA
freq(IA_MGP18_23$Ville_HL3_2_categ)
freq(IA_MGP18_23$Pays_HL3_2)

#Ville_HL3_2_categPR : Hors France metropolitaine / France metropolitaine hors IDF / Ile-de-France hors MGP / MGP / PARIS / NA
freq(IA_MGP18_23$Region_HL3_2_categR)

IA_MGP18_23$PRV_location <- as.character(IA_MGP18_23$PRV_location)

Tab_host_loc <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>% 
  group_by(last_scraped_YM) %>%
  summarise(
    nb_annoncesLE = n(),
    nb_HL_France = sum(PRV_location == "France DROM-COM"| PRV_location == "France indéterminé"| PRV_location == "Paris", na.rm = TRUE),
    nb_HL_Paris = sum(PRV_location == "Paris", na.rm = TRUE),
    nb_HL_AMN = sum(PRV_location == "Amérique du Nord", na.rm = TRUE),
    nb_HL_AFN = sum(PRV_location == "Afrique du Nord", na.rm = TRUE),
    nb_HL_AFS = sum(PRV_location == "Afrique subsaharienne", na.rm = TRUE),
    nb_HL_AMLC = sum(PRV_location == "Amérique latine et Caraïbes", na.rm = TRUE),
    nb_HL_ACRUS = sum(PRV_location == "Asie centrale et Russie", na.rm = TRUE),
    nb_HL_ASE = sum(PRV_location == "Asie du Sud et de l'Est", na.rm = TRUE),
    nb_HL_ASO = sum(PRV_location == "Asie du Sud-Ouest", na.rm = TRUE),
    nb_HL_EUR = sum(PRV_location == "Europe", na.rm = TRUE),
    nb_HL_AUSP = sum(PRV_location == "Monde austral et du Pacifique", na.rm = TRUE),
    nb_NA = sum(is.na(PRV_location)),
    nb_etrang = nb_annoncesLE - (nb_HL_France + nb_NA),
    nb_Italie = sum(Pays_HL3_2 == "ITALIE", na.rm = TRUE),
    nb_Esp = sum(Pays_HL3_2 == "ESPAGNE", na.rm = TRUE),
    nb_UK = sum(Pays_HL3_2 == "ROYAUME UNI", na.rm = TRUE),
    nb_Suisse = sum(Pays_HL3_2 == "SUISSE", na.rm = TRUE),
    nb_HL_France_pct = nb_HL_France/nb_annoncesLE*100,
    nb_HL_Paris_pct = nb_HL_Paris/nb_annoncesLE*100,
    nb_HL_AMN_pct = nb_HL_AMN/nb_annoncesLE*100,
    nb_HL_AFN_pct = nb_HL_AFN/nb_annoncesLE*100,
    nb_HL_AFS_pct = nb_HL_AFS/nb_annoncesLE*100,
    nb_HL_AMLC_pct = nb_HL_AMLC/nb_annoncesLE*100,
    nb_HL_ACRUS_pct = nb_HL_ACRUS/nb_annoncesLE*100,
    nb_HL_ASE_pct = nb_HL_ASE/nb_annoncesLE*100,
    nb_HL_ASO_pct = nb_HL_ASO/nb_annoncesLE*100,
    nb_HL_EUR_pct = nb_HL_EUR/nb_annoncesLE*100,
    nb_HL_AUSP_pct = nb_HL_AUSP/nb_annoncesLE*100,
    nb_NA_pct = nb_NA/nb_annoncesLE*100,
    nb_etrang_pct = nb_etrang/nb_annoncesLE*100,
    nb_Italie_pct =nb_Italie/nb_annoncesLE*100,
    nb_Esp_pct =nb_Esp/nb_annoncesLE*100,
    nb_UK_pct =nb_UK/nb_annoncesLE*100,
    nb_Suisse_pct =nb_Suisse/nb_annoncesLE*100)

Tab_host_loc_comm <- IA_MGP18_23 %>%
  filter(room_type =="Entire home/apt") %>% 
  filter(last_scraped_YM == "2022-05") %>%
  group_by(C_CAINSEE) %>%
  summarise(
    nb_annoncesLE = n(),
    nb_HL_France = sum(PRV_location == "France DROM-COM"| PRV_location == "France indéterminé"| PRV_location == "Paris", na.rm = TRUE),
    nb_HL_Paris = sum(PRV_location == "Paris", na.rm = TRUE),
    nb_HL_AMN = sum(PRV_location == "Amérique du Nord", na.rm = TRUE),
    nb_HL_AFN = sum(PRV_location == "Afrique du Nord", na.rm = TRUE),
    nb_HL_AFS = sum(PRV_location == "Afrique subsaharienne", na.rm = TRUE),
    nb_HL_AMLC = sum(PRV_location == "Amérique latine et Caraïbes", na.rm = TRUE),
    nb_HL_ACRUS = sum(PRV_location == "Asie centrale et Russie", na.rm = TRUE),
    nb_HL_ASE = sum(PRV_location == "Asie du Sud et de l'Est", na.rm = TRUE),
    nb_HL_ASO = sum(PRV_location == "Asie du Sud-Ouest", na.rm = TRUE),
    nb_HL_EUR = sum(PRV_location == "Europe", na.rm = TRUE),
    nb_HL_AUSP = sum(PRV_location == "Monde austral et du Pacifique", na.rm = TRUE),
    nb_NA = sum(is.na(PRV_location)),
    nb_etrang = nb_annoncesLE - (nb_HL_France + nb_NA),
    nb_Italie = sum(Pays_HL3_2 == "ITALIE", na.rm = TRUE),
    nb_Esp = sum(Pays_HL3_2 == "ESPAGNE", na.rm = TRUE),
    nb_UK = sum(Pays_HL3_2 == "ROYAUME UNI", na.rm = TRUE),
    nb_Suisse = sum(Pays_HL3_2 == "SUISSE", na.rm = TRUE),
    nb_HL_France_pct = nb_HL_France/nb_annoncesLE*100,
    nb_HL_Paris_pct = nb_HL_Paris/nb_annoncesLE*100,
    nb_HL_AMN_pct = nb_HL_AMN/nb_annoncesLE*100,
    nb_HL_AFN_pct = nb_HL_AFN/nb_annoncesLE*100,
    nb_HL_AFS_pct = nb_HL_AFS/nb_annoncesLE*100,
    nb_HL_AMLC_pct = nb_HL_AMLC/nb_annoncesLE*100,
    nb_HL_ACRUS_pct = nb_HL_ACRUS/nb_annoncesLE*100,
    nb_HL_ASE_pct = nb_HL_ASE/nb_annoncesLE*100,
    nb_HL_ASO_pct = nb_HL_ASO/nb_annoncesLE*100,
    nb_HL_EUR_pct = nb_HL_EUR/nb_annoncesLE*100,
    nb_HL_AUSP_pct = nb_HL_AUSP/nb_annoncesLE*100,
    nb_etrang_pct = nb_etrang/nb_annoncesLE*100,
    nb_NA_pct = nb_NA/nb_annoncesLE*100,
    nb_Italie_pct =nb_Italie/nb_annoncesLE*100,
    nb_Esp_pct =nb_Esp/nb_annoncesLE*100,
    nb_UK_pct =nb_UK/nb_annoncesLE*100,
    nb_Suisse_pct =nb_Suisse/nb_annoncesLE*100)

write.table(Tab_host_loc, "~/Murray France/Tab_host_location_LE.csv", sep=",", dec=".", row.names=F)

write.table(Tab_host_loc_comm, "~/Murray France/Tab_host_location_LE_202205_comm.csv", sep=",", dec=".", row.names=F)
