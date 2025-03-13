library(readr)
library(dplyr)
library(tidyverse)

# Lire le fichier (remplace "data.csv" par ton fichier)
Food_pattern<- read_csv("servings.csv", col_names = FALSE)

# Définir la deuxième ligne comme noms de colonnes
colnames(Food_pattern) <- Food_pattern[2, ]  

# Supprimer les deux premières lignes devenues inutiles
Food_pattern <- Food_pattern[-c(1,2), ]  

# Définir la première ligne valide comme noms de colonnes
colnames(Food_pattern) <- Food_pattern[1, ]

# Supprimer les lignes inutiles
Food_pattern <- Food_pattern[-c(1:5), ]  # Suppression des lignes d'en-tête inutiles

Food_pattern<-Food_pattern[-c(45:66), ]

# Convertir les colonnes en numériques
df$Year <- as.numeric(df$Year)
df$`Meat, eggs, and nuts` <- as.numeric(df$`Meat, eggs, and nuts`)


ggplot(Food_pattern, aes(x =Year, y = `Meat, eggs, and nuts`)) +
  geom_line(color = "red", size = 2) +  # Ligne rouge
  geom_point(color = "black", size = 2) +  # Points noirs
  labs(title = "Évolution de la consommation de viande, œufs et noix",
       x = "Année",
       y = "Consommation (Ounces)") +
  theme_minimal()

ggplot(Food_pattern, aes(x =Year, y = `Added fats and oils and dairy fats`)) +
  geom_point(color = "black", size = 2) +  # Points noirs
  labs(title = "Évolution de la consommation de gras, huile et produit laitiers",
       x = "Année",
       y = "Consommation (gm)") +
  theme_minimal()

ggplot(Food_pattern, aes(x =Year, y = `Added sugars`)) +
  geom_point(color = "black", size = 2) +  # Points noirs
  labs(title = "Évolution de la consommation de sucre",
       x = "Année",
       y = "Consommation (gm)") +
  theme_minimal()

# Supprimer les deux premières lignes devenues inutiles
Race<- Obésité[ -c(20:29), ]  
Race<-Race [-c(1:13),] 
names(Race)[1] <- "Race/Ethnicity" 
names(Race)[2] <- "2023"

ggplot(Race, aes(x =`Race/Ethnicity`, y = `X2023`)) +
  geom_bar(color = "black", size = 2) +  # Points noirs
  labs(title = "Évolution de la consommation de sucre",
       x = "Race/Ethnicity",
       y = "Pourcentage de la population diabétique") +
  theme_minimal()

ggplot(Race, aes(x = `Race/Ethnicity`, y = `X2023`)) +
  geom_bar(stat = "identity", color = "black", fill = "skyblue") +  # Ajout de fill et stat="identity"
  labs(title = "Évolution de la consommation de sucre",
       x = "Race/Ethnicity",
       y = "Pourcentage de la population diabétique") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
