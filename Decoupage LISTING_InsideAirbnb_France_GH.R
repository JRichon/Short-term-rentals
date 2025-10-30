#Script ouverture base listing InsideAirbnb_FRANCE et la découper en IDF et MGP

library(tidyverse)
library(questionr)
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(sf)

options(scipen=999) # pour éviter d'avoir de l'écriture scientifique

guess_encoding("~/Murray France/ORIG/IA_listings_2023-11-11_IDF_Apur.csv")

IA_FR <- read.csv("~/IA_listings_2023-11-11.csv",sep= ",", encoding='UTF-8', header=T, na.strings="")

#freq(InsideAirbnb_FRANCE$region_parent_name) # pas possible de simplement sélectionner avec cette colonne ni même une autre car les infos ne sont pas propres, donc on passe par l'analyse spatiale en se servant de la geolocalisation des points

IDF <- st_read("~/Couches fond/COMMUNE_IDF/COMMUNE.shp") #ce sont les couches qui vont nous permettre de faire la découpe des données InsideAirbnb, ici IDF
MGP <- st_read("~/Couches fond/JR_Communes MGP_dapresAPUR/Communes MGP_dapresAPUR.shp") #SHP issu de l'opendata de l'Atelier parisien d'urbanisme (apur)

#IA_ex <- st_read("C:/InsideAirbnb_2015-05-06/2015-05-06_listings_IA.shp") # utile simplement pour récupérer son SCR WGS84
#lambert_CRS <- st_crs(MGP)
#WGS84_CRS <- st_crs(IA_ex)

IA_FR_shp <- st_as_sf(IA_FR, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) # On créé une couche SHP à partir des latitudes et longitudes contenues dans le fichier csv

IA_FR_shp <- st_transform(IA_FR_shp, 2154) # ici on transforme le SCR de la couche SHP pour qu'il soit en lambert93 comme nos couches de formes MGP/IDF

######## Si besoin (car erreur dans R : st_as_sf.data.frame(IA_IDF, coords = c("longitude", "latitude"),missing values in coordinates not allowed) 
#Identifier les lignes avec des valeurs manquantes dans les colonnes de coordonnées
na_rows <- is.na(IA_IDF$longitude) | is.na(IA_IDF$latitude)

num_missing_longitude <- sum(is.na(IA_IDF$longitude)) # Compter les lignes avec des valeurs manquantes
print(num_missing_longitude)

IA_IDF_clean <- IA_IDF %>% filter(!is.na(longitude) & !is.na(latitude)) # Supprimer les lignes avec des valeurs manquantes

# Convertir en objet spatial
IA_IDF_shp <- st_as_sf(IA_IDF_clean, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
IA_IDF_shp <- st_transform(IA_IDF_shp, 2154) # ici on transforme le SCR de la couche SHP pour qu'il soit en lambert93 comme nos couches de formes MGP/IDF

#########fin de la parenthèse

plot(IA_FR_shp$geometry) #on voit qu'on a des points très éloignés de la France métropo, qui ne correspondent pas necessairement aux DOM TOM

IA_IDF <- st_intersection(IA_FR_shp, IDF) # ici on fait les intersections entre l'IDF et le fichier IA France pour ne garder que les annonces en IDF
IA_MGP <- st_intersection(IA_IDF, MGP) # idem ici pour la MGP

plot(IA_MGP$geometry) # ici pour vérifier que ça a fonctionné
plot(IA_IDF$geometry)

# On nettoye un peu le nombre de colonnes de MGP qui a bcp augmenté avec ces étapes, car les colonnes des communes se sont ajoutees, avec leurs infos
names(IA_MGP)
IA_MGP <- IA_MGP[,-c(98:114)] #ca enlève les colonnes en trop

# Visualiser le résultat
ggplot() +
  geom_sf(data = IA_MGP, fill = "lightblue", alpha = 0.5) +
  geom_sf(data = MGP, fill = "red", alpha = 0.5) +
  theme_minimal()

st_write(IA_MGP, "~/Murray France/IA_listings_2023-11_MGP.csv")
st_write(IA_IDF, "~/Murray France/IA_listings_2023-11_IDF.csv")
