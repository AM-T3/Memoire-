library(readr)
library(dplyr)
library(tidyverse)

#EVOLUTION CONSOMATION SUCRE/GRAS


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

Food_pattern$Year[14] <- 2000

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

#SUCRE BIS
sugar_2<-read.csv("table-5-US-food-group-intakes-by-food-source.csv")


#ESSAI SUR LES RACES ET DIABETE 


# Supprimer les deux premières lignes devenues inutiles
Race<- Obésité[ -c(20:29), ]  
Race<-Race [-c(1:13),] 
names(Race)[1] <- "Race/Ethnicity" 
names(Race)[2] <- "2023"


ggplot(Race, aes(x = `Race/Ethnicity`, y = `2023`)) +
  geom_bar(stat = "identity", color = "black", fill = "skyblue") +  # Ajout de fill et stat="identity"
  labs(title = "Évolution de la consommation de sucre",
       x = "Race/Ethnicity",
       y = "Pourcentage de la population diabétique") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#EVOLUTION DE L OBESITE

#data base: obésité2
library(dplyr)
obesite_us <- obésité2[, -c(4:6)]  # Supprime la colonne class, topic, question
obesite_us <- obesite_us[, -c(11:24)]
obesite_us <- obesite_us[, -c(2)]

Overall<-obesite_us%>% filter(Break_Out == "Overall", Locationdesc == "All States and DC (median) **", Response == "Obese (BMI 30.0 - 99.8)")

ggplot(Overall, aes(x = Year, y = Data_value)) +
  geom_line(color = "blue", size = 1) +   # Ligne bleue
  geom_point(color = "red", size = 2) +   # Points rouges sur les années
  labs(title = "Évolution du taux d'obésité",
       x = "Année",
       y = "Pourcentage d'obèse dans la pop") +
  theme_minimal()  

#SAME GRAPH AVEC AUTRE BASE DE DONNEES
ggplot(overweight, aes(x = YEAR, y = ESTIMATE)) +
  geom_line(color = "blue", size = 1) +   # Ligne bleue
  geom_point(color = "red", size = 2) +   # Points rouges sur les années
  labs(title = "Évolution du taux d'obésité",
       x = "Année",
       y = "Taux d'obèse dans la pop en moyenne") +
  theme_minimal()  
