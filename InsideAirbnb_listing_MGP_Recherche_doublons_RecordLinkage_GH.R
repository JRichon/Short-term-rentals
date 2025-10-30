#### Recherche de doublons dans InsideAirbnb_listing

install.packages("textTinyR")
install.packages("Matrix")
install.packages("tm")
install.packages("slam")
install.packages("stringdist")
install.packages("RecordLinkage")

library(RecordLinkage)
library(tm)
library(proxy)
library(stringr)
library(dplyr)
library(stringdist)
library(geosphere)
library(tidyr)
library(tidyverse)
library(questionr)
library(lubridate)
library(tools)
library(readr)
library(stringr)
library(anytime)
library(sf)
library(gt)
library(ggplot2)
library(Matrix)
library(textTinyR)
library(igraph)

#je repars de la base InsideAirbnb du script "InsideAirbnb_listing_MGP_2018-2023_GH"

## Avec package RecordLinkage de recherche de doublons : ----

# Création d'un objet RecordLinkage
df <- IA_MGP18_23_LE %>%
  filter(last_scraped_YM  %in% c("2022-05", "2022-08", "2022-11", "2023-02")) #on ne peut pas prendre 2023-11 car il manque le champs description

freq(df$last_scraped_YM)
freq(df$room_type)

df_rl <- df[, c("ID_URL", "name", "description", "latitude", "longitude", "beds", "accommodates", "host_id", "host_acceptance_rateNETT", "number_of_reviews_ltm", "review_scores_rating", "duree_activ_mth")]

df_rl$latitude <- as.numeric(df_rl$latitude)
df_rl$longitude <- as.numeric(df_rl$longitude)

df_rl$latitude <- round(df_rl$latitude, 3) #arrondi des coordonnees geo
df_rl$longitude <- round(df_rl$longitude, 3)

df_rl$name <- toupper(df_rl$name)  # Convertir la colonne "name" en majuscules car Levenshtein est sensible a la casse
df_rl$description <- toupper(df_rl$description)

df_rl_uniq <- df_rl[!duplicated(df_rl$ID_URL, fromLast = TRUE), ]

df_rl_uniq <- df_rl_uniq %>%
  filter(!is.na(name) & !is.na(description))

rpairs <- compare.dedup(df_rl_uniq, blockfld = c("latitude", "longitude", "accommodates", "beds"),
                        strcmp = c("name", "description"),
                        strcmpfun = levenshteinSim,         #Ou sinon tester avec Jaro-Winkler, plus laxiste (?) 
                        phonetic = FALSE,
                        exclude = c("ID_URL", "host_id", "host_acceptance_rateNETT", "number_of_reviews_ltm", "review_scores_rating", "duree_activ_mth"))

# Utilisation d'une fonction de correspondance
rpairs <- epiWeights(rpairs) #ca vient peut-être des alphabets différents que la fonction gère mal
summary(rpairs$pairs)

# Spécifier un seuil pour détecter les doublons probables (facultatif)
potential_duplicates_rl <- getPairs(rpairs)
potential_duplicates_rl <- potential_duplicates_rl[!apply(potential_duplicates_rl, 1, function(row) all(trimws(row) == "")), ]

#Graph de la progression de Weight
potential_duplicates_rl$Weight <- as.numeric(potential_duplicates_rl$Weight)

ggplot(potential_duplicates_rl, aes(x = Weight)) +
  geom_histogram(binwidth = 0.01, fill = "#5B857C", color = "white", alpha = 0.7) +  # Ajustez binwidth si nécessaire
  labs(
    title = "Distribution des poids (Weight)",
    x = "Poids (Weight)",
    y = "Fréquence"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.10),  # Indications tous les 0.10
    limits = c(0, 1)                # Limiter l'axe X entre 0 et 1
  ) +
  theme_minimal()

#Graph zoomé sur les plus de 0,70
potential_duplicates_rl %>%
  filter(Weight>0.70) %>%
  ggplot(aes(x = Weight)) +
  geom_histogram(binwidth = 0.005, fill = "#5B857C", color = "white", alpha = 0.7) +  # Ajustez binwidth si nécessaire
  labs(
    title = "Distribution des poids (Weight)",
    x = "Poids (Weight)",
    y = "Fréquence"
  ) +
  scale_x_continuous(
    breaks = seq(0.70, 1, by = 0.10),  # Indications tous les 0.10
    limits = c(0.70, 1)                # Limiter l'axe X entre 0 et 1
  ) +
  theme_minimal()

# Spécifier un seuil pour détecter les doublons probables
threshold <- 0.825 #nous on choisi 0,825 sur 1 après étude du graph et des ann identifiees comme doublons
potential_duplicates_rl <- getPairs(rpairs, min.weight = threshold)
potential_duplicates_rl <- potential_duplicates_rl[!apply(potential_duplicates_rl, 1, function(row) all(trimws(row) == "")), ]

# Transformation de potential_duplicates_rl
# il faut que le nombre de lignes soit bien pair
if (nrow(potential_duplicates_rl) %% 2 != 0) {
  stop("Le nombre de lignes de df1 doit être pair pour cette transformation.")
}

# Création d'une fonction pour extraire les nombres d'une chaîne
extract_numbers <- function(text) {
  matches <- gregexpr("\\d+", text)
  as.numeric(regmatches(text, matches)[[1]])
}

# Creation d'un dataframe vide pour stocker les résultats
doublons_extend <- data.frame() #doublons_extend ne contient pas le nombre réel d'annonces doublonnées mais il contient des annonces comparées 2 à 2, ce qui implique que dans le cas de triplons on aura deux couples et donc une annonces qui apparait 2 fois dans cette base vu qu'elle matche avec 2 autres

# Parcourir les lignes deux par deux
for (i in seq(1, nrow(potential_duplicates_rl), by = 2)) {
  # Vérifier qu'il existe une paire (ligne i et ligne i+1)
  if (i + 1 <= nrow(potential_duplicates_rl)) {
    # Extraire les lignes correspondantes
    row1 <- potential_duplicates_rl[i, ]
    row2 <- potential_duplicates_rl[i + 1, ]
    
    # Extraire les nombres des champs "name" pour chaque ligne
    numbers1 <- extract_numbers2(row1$name)
    numbers2 <- extract_numbers2(row2$name)
    
    # Vérifier les conditions de filtrage
if ((length(numbers1) == 0 && length(numbers2) == 0) ||  # Aucun des deux n'a de nombre
    (length(numbers1) > 0 && length(numbers2) > 0 && all(numbers1 == numbers2)) ||
    (length(numbers1) == 0 && length(numbers2) > 0) ||
     (length(numbers1) > 0 && length(numbers2) == 0)) {  # Les nombres sont identiques
  # Ajouter la paire au DataFrame final
  doublons_extend <- rbind(doublons_extend, rbind(row1, row2))
}
# Si les tailles diffèrent ou qu'un seul a des nombres, on ignore la paire (aucune action nécessaire)
}}

write.table(doublons_extend, "~/Tab_doublons_extend_2022-2023_MGP_LE.csv", sep=";", row.names=F)

#Créer une table des doublons agrégés à partir de doublons_extend ----
# Créer une colonne qui regroupe les IDs associés dans chaque ligne

doublons_extend <- doublons_extend %>%
  mutate(associated_ids = if_else(row_number() %% 2 == 1,  # Si la ligne est impaire
                                  lead(ID_URL),          # Associer l'ID de la ligne suivante
                                  lag(ID_URL))) 

# Créer un graphe basé sur les relations entre ID_URL
edges <- doublons_extend %>%
  select(ID_URL, associated_ids) %>%
  as.matrix()  # Convertit en matrice le df doublons_extend pour créer un graph d'arêtes (edge list) qui consiste à lier entre elles les annonces qui sont des duplicatas

g <- graph_from_edgelist(edges, directed = FALSE) #Crée un graph non orienté (directed = FALSE) à partir de la matrice edges. Chaque ligne de la matrice correspond à une arête entre deux noeuds (=2 ID)

components <- components(g) #Identifie les groupes du graph. 

# Ajouter une colonne indiquant le groupe auquel chaque ID_URL appartient
doublons_extend$group <- components$membership[as.character(doublons_extend$ID_URL)] #Associe chaque ID à un numéro de groupe. Et pour chaque ID_URL dans le df doublons_extend, on récupère son numéro de groupe

# Analyse des résultats
doublons_extend$number_of_reviews_ltm <- as.numeric(doublons_extend$number_of_reviews_ltm)
doublons_extend$duree_activ_mth     <- as.numeric(doublons_extend$duree_activ_mth)
doublons_extend$host_acceptance_rateNETT     <- as.numeric(doublons_extend$host_acceptance_rateNETT)
doublons_extend$review_scores_rating     <- as.numeric(doublons_extend$review_scores_rating)

tab_doublons <- doublons_extend %>%
  group_by(group) %>%
  summarise(
    associated_ids = paste(unique(ID_URL), collapse = ";"),  # Liste des IDs associés
    duplicate_count = n_distinct(ID_URL),  # Nombre d'annonces uniques
    number_of_reviews_ltm_agreg = sum(number_of_reviews_ltm, na.rm = TRUE),  # Somme des reviews
    number_of_reviews_ltm_min = if_else(all(is.na(number_of_reviews_ltm)), NA_real_, min(number_of_reviews_ltm, na.rm = TRUE)), 
    number_of_reviews_ltm_max = if_else(all(is.na(number_of_reviews_ltm)), NA_real_, max(number_of_reviews_ltm, na.rm = TRUE)), 
    host_acceptance_rate_min = if_else(all(is.na(host_acceptance_rateNETT)), NA_real_, min(host_acceptance_rateNETT, na.rm = TRUE)),
    review_scores_rating_min = if_else(all(is.na(review_scores_rating)), NA_real_, min(review_scores_rating, na.rm = TRUE)),
    review_scores_rating_max = if_else(all(is.na(review_scores_rating)), NA_real_, max(review_scores_rating, na.rm = TRUE)),
    duree_activ_mth_min = if_else(all(is.na(duree_activ_mth)), NA_real_, min(duree_activ_mth, na.rm = TRUE)), 
    duree_activ_mth_max = if_else(all(is.na(duree_activ_mth)), NA_real_, max(duree_activ_mth, na.rm = TRUE)),
    host_id_agreg = paste(unique(host_id), collapse = ";"),
    primary_id = ID_URL[which.max(number_of_reviews_ltm)]  # ID conservées est celle avec max de reviewsltm
    #Donc tab_doublons c'est un df qui regroupe les doublons par groupe, sachant qu'un groupe c'est un doublon / triplon/quadruplon d'annonces
  )

tab_doublons <- tab_doublons %>%
  rename(group_id = primary_id) %>%
  select(group_id, everything())  # Réorganiser les colonnes pour mettre group_id en premier

doublons_unique <- doublons_extend[!duplicated(doublons_extend$ID_URL), ] #Ici ce df nous permet d'avoir à un endroit l'ensemble des annonces qui sont dupliquées

write.table(tab_doublons, "~/tab_doublons_2022-2023_MGP_LE.csv", sep=";", row.names=F)
write.table(doublons_unique, "~/tab_doublons_uniques_2022-2023_MGP_LE.csv", sep=";", row.names=F)

freq(tab_doublons$duplicate_count)
freq(tab_doublons$group)

rm(g, edges, components)

#Petite analyse des annonces doublonnées ----
names(IA_MGP18_23)

df_lght <- df %>%
  select(ID_URL, description, host_name, host_about, price, number_of_reviews, instant_bookable,  J_C_COINSE, C_CAINSEE, J_L_CO, L_CO, calculated_host_listings_count)

df_lght_unique <- df_lght[!duplicated(df_lght$ID_URL, fromLast = TRUE), ]

tab_doublons <- tab_doublons %>%
 left_join (df_lght_unique, by = c("group_id" = "ID_URL"))

freq(df_lght$J_L_CO)
freq(tab_doublons$J_L_CO)

J_L_CO_tab_doublons <- tab_doublons %>%
  group_by(J_L_CO) %>%
  summarise(Nb_doublons = n(), .groups = "drop")

J_L_CO_df_lght <- df_lght_unique %>%
  group_by(J_L_CO) %>%
  summarise(Nb_LE = n(), .groups = "drop")

Tab_doublons_JLCO <- J_L_CO_df_lght %>%
  left_join (J_L_CO_tab_doublons, by="J_L_CO")

Tab_doublons_JLCO <- Tab_doublons_JLCO %>%
  mutate(Prct_doubl_sur_LE = Nb_doublons/Nb_LE*100,
         Part_LE_comMGP = Nb_LE/sum(Tab_doublons_JLCO$Nb_LE, na.rm = TRUE)*100,
         Part_doubl_comMGP = Nb_doublons/sum(Tab_doublons_JLCO$Nb_doublons, na.rm = TRUE)*100
         )

doublons_unique <- doublons_unique %>%
  left_join (df_lght_unique, by = "ID_URL")

J_L_CO_doublons_unique <- doublons_unique %>%
  group_by(J_L_CO) %>%
  summarise(Nb_doublons_tot = n(), .groups = "drop")

Tab_doublons_JLCO <- Tab_doublons_JLCO %>%
  left_join (J_L_CO_doublons_unique, by="J_L_CO")

Tab_doublons_JLCO <- Tab_doublons_JLCO %>%
  mutate(Prct_doubl_tot_sur_LE = Nb_doublons_tot/Nb_LE*100,
         Part_doubl_com_tot_MGP = Nb_doublons_tot/sum(Tab_doublons_JLCO$Nb_doublons_tot, na.rm = TRUE)*100,
         Sur_ss_rpz = Part_doubl_com_tot_MGP-Part_LE_comMGP
  )

sum(Tab_doublons_JLCO$Nb_doublons, na.rm = TRUE)

#idem avec une ligne pour Paris seulement ----

L_CO_df_lght <- df_lght_unique %>%
  group_by(L_CO) %>%
  summarise(Nb_LE = n(), .groups = "drop")

L_CO_tab_doublons <- tab_doublons %>%
  group_by(L_CO) %>%
  summarise(Nb_doublons = n(), .groups = "drop")

Tab_doublons_LCO <- L_CO_df_lght %>%
  left_join (L_CO_tab_doublons, by="L_CO")

Tab_doublons_LCO <- Tab_doublons_LCO %>%
  mutate(Prct_doubl_sur_LE = Nb_doublons/Nb_LE*100,
         Part_LE_comMGP = Nb_LE/sum(Tab_doublons_JLCO$Nb_LE, na.rm = TRUE)*100,
         Part_doubl_comMGP = Nb_doublons/sum(Tab_doublons_JLCO$Nb_doublons, na.rm = TRUE)*100
  )

L_CO_doublons_unique <- doublons_unique %>%
  group_by(L_CO) %>%
  summarise(Nb_doublons_tot = n(), .groups = "drop")

Tab_doublons_LCO <- Tab_doublons_LCO %>%
  left_join (L_CO_doublons_unique, by="L_CO")

Tab_doublons_LCO <- Tab_doublons_LCO %>%
  mutate(Prct_doubl_tot_sur_LE = Nb_doublons_tot/Nb_LE*100,
         Part_doubl_com_tot_MGP = Nb_doublons_tot/sum(Tab_doublons_JLCO$Nb_doublons_tot, na.rm = TRUE)*100
  )

rm(df_lght, df_lght_unique)

#Mini analyse de tab_doublons pour voir quel type d'annonce a des doublons ----
summary(tab_doublons$number_of_reviews_ltm_agreg)
summary(doublons_unique$number_of_reviews_ltm)
summary(df_rl_uniq$number_of_reviews_ltm)

freq(doublons_unique$number_of_reviews_ltm)
freq(df_rl_uniq$number_of_reviews_ltm)

summary(doublons_unique$duree_activ_mth) #les doublons ont des durées de vie plus courtes que l'ensemble des annonces
summary(df_rl_uniq$duree_activ_mth)

summary(doublons_unique$review_scores_rating) 
summary(df_rl_uniq$review_scores_rating)

summary(doublons_unique$host_acceptance_rateNETT) 
summary(df_rl_uniq$host_acceptance_rateNETT)

doublons_unique %>%
  ggplot(aes(x = number_of_reviews_ltm)) +
  geom_histogram(binwidth = 1, fill = "#5B857C", color = "white", alpha = 0.7) +  # Ajustez binwidth si nécessaire
  labs(
    title = "Distribution du nombre de commentaire sur les douze derniers mois",
    x = "Nombre de commentaires (ltm)",
    y = "Nombre d'annonces"
  ) +
  scale_x_continuous(
  breaks = seq(0, 165, by = 20),  # Indications tous les 0.10
  limits = c(-1, 165)                # Limiter l'axe X entre 0 et 1
) +
  theme_minimal()
