overweight<-read.csv("overweight.csv")
overweight<- overweight[,-c(4)]  
overweight<- overweight[,-c(1:2)]



str(Food_pattern)
Food_pattern<-Food_pattern %>% 
  mutate ( Year = as.numeric(Year),
           Dairy = as.numeric(Dairy),
           `Added sugars` = as.numeric(`Added sugars`),
           `Added fats and oils and dairy fats` = as.numeric(`Added fats and oils and dairy fats`))


Food_pattern_dairy<- Food_pattern %>%
  mutate(Periode = case_when(
    Year >= 1988 & Year <= 1994 ~ "1988-1994",
    Year >= 1999 & Year <= 2002 ~ "1999-2003",
    Year >= 2001 & Year <= 2004 ~ "2001-2004",
    Year >= 2003 & Year <= 2006 ~ "2003-2006",
    Year >= 2005 & Year <= 2008 ~ "2005-2008",
    Year >= 2007 & Year <= 2010 ~ "2007-2010",
    Year >= 2009 & Year <= 2012 ~ "2009-2012",
    Year >= 2011 & Year <= 2013 ~ "2011-2014",
    TRUE ~ NA_character_  # Exclut les autres années
  )) %>%
  filter(!is.na(Periode)) 

print(Food_pattern_dairy)  # Vérifie les données avant la moyenne

FP_dairy<- Food_pattern_dairy %>%
  group_by(Periode) %>%
  summarise(Moyenne_Conso = mean(Dairy, na.rm = TRUE)) %>%
  print()



Food_pattern_SUGAR<- Food_pattern %>%
  mutate(Periode = case_when(
    Year >= 1988 & Year <= 1994 ~ "1988-1994",
    Year >= 1999 & Year <= 2002 ~ "1999-2003",
    Year >= 2001 & Year <= 2004 ~ "2001-2004",
    Year >= 2003 & Year <= 2006 ~ "2003-2006",
    Year >= 2005 & Year <= 2008 ~ "2005-2008",
    Year >= 2007 & Year <= 2010 ~ "2007-2010",
    Year >= 2009 & Year <= 2012 ~ "2009-2012",
    Year >= 2011 & Year <= 2013 ~ "2011-2014",
    TRUE ~ NA_character_  # Exclut les autres années
  )) %>%
  filter(!is.na(Periode)) 

FP_sugar<- Food_pattern_SUGAR %>%
  group_by(Periode) %>%
  summarise(Moyenne_Conso = mean(`Added sugars`, na.rm = TRUE)) %>%
  print()

Food_pattern_FAT<- Food_pattern %>%
  mutate(Periode = case_when(
    Year >= 1988 & Year <= 1994 ~ "1988-1994",
    Year >= 1999 & Year <= 2002 ~ "1999-2003",
    Year >= 2001 & Year <= 2004 ~ "2001-2004",
    Year >= 2003 & Year <= 2006 ~ "2003-2006",
    Year >= 2005 & Year <= 2008 ~ "2005-2008",
    Year >= 2007 & Year <= 2010 ~ "2007-2010",
    Year >= 2009 & Year <= 2012 ~ "2009-2012",
    Year >= 2011 & Year <= 2013 ~ "2011-2014",
    TRUE ~ NA_character_  # Exclut les autres années
  )) %>%
  filter(!is.na(Periode)) 

print(Food_pattern_FAT)  # Vérifie les données avant la moyenne

FP_fat<- Food_pattern_FAT %>%
  group_by(Periode) %>%
  summarise(Moyenne_Conso = mean(`Added fats and oils and dairy fats`, na.rm = TRUE)) %>%
  print()



overweightbis<-overweight[-c(1,3,5,7,9,11,13,15,17,18,19,20),]


lm(FP_sugar$Moyenne_Conso ~ overweightbis$ESTIMATE)
summary(lm(FP_sugar$Moyenne_Conso ~ overweightbis$ESTIMATE))

lm(FP_fat$Moyenne_Conso ~ overweightbis$ESTIMATE)
summary(lm(FP_fat$Moyenne_Conso ~ overweightbis$ESTIMATE))

lm(overweightbis$ESTIMATE ~ FP_fat$Moyenne_Conso)
summary(lm(overweightbis$ESTIMATE ~ FP_fat$Moyenne_Conso))

################################
# MODELES
################################

# Modèle 1
model1 <- lm(FP_sugar$Moyenne_Conso ~ overweightbis$ESTIMATE)
summary(model1)

# Modèle 2
model2 <- lm(FP_fat$Moyenne_Conso ~ overweightbis$ESTIMATE)
summary(model2)

# Modèle 3
model3 <- lm(overweightbis$ESTIMATE ~ FP_fat$Moyenne_Conso)
summary(model3)

################################
# DIAGNOSTICS POUR CHAQUE MODELE
################################

# ----- MODELE 1 -----
# 1) QQ-plot
qqnorm(model1$residuals, main = "QQ-plot - Model 1 (Sugar vs Overweight)")
qqline(model1$residuals, col = "red")

# 2) Kolmogorov-Smirnov test
ks.test(model1$residuals, 
        "pnorm", 
         mean = mean(model1$residuals), 
         sd   = sd(model1$residuals))

# ----- MODELE 2 -----
qqnorm(model2$residuals, main = "QQ-plot - Model 2 (Fat vs Overweight)")
qqline(model2$residuals, col = "red")

ks.test(model2$residuals, 
        "pnorm", 
         mean = mean(model2$residuals), 
         sd   = sd(model2$residuals))

# ----- MODELE 3 -----
qqnorm(model3$residuals, main = "QQ-plot - Model 3 (Overweight vs Fat)")
qqline(model3$residuals, col = "red")

ks.test(model3$residuals, 
        "pnorm", 
         mean = mean(model3$residuals), 
         sd   = sd(model3$residuals))



