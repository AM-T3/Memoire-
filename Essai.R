getwd()
library(tidyverse)
library(ggplot2)
Consomation<-read.csv("nominal_expenditures.csv")
Population<-read.csv("pop.csv")
Food_pattern<-read.csv("servings.csv") #ce que mange les gens en quatité dans le temps
sucre<-read.csv("sugar.csv")
food_mondialisation<-read.csv("ctcsp.csv")

X<-read.csv("2016-Eating-and-Health-Module-Tables/Table1_2016.csv")
Fastfood_2023<-read.csv("fastfood_2023.csv")
Fastfood_2022<-read.csv("fastfood_2022.csv")
Fastfood_2006<-read.csv("2016-Eating-and-Health-Module-Tables/Table4_2006.csv")

Chronic_disease<-read.csv("Chronic_Disease_Indicators.csv")
Obésité<-read.csv("2023-Obesity-by-demographic.csv")
