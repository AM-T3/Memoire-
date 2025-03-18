overweight<-read.csv("overweight.csv")
overweight<- overweight[,-c(4)]  
overweight<- overweight[,-c(1:2)]



str(Food_pattern)
Food_pattern<-Food_pattern %>% 
  mutate ( Year = as.numeric(Year))

Food_pattern_mean <- Food_pattern %>% 
  mutate(annees_moy = (Year + lead(Year))/ 2,
         valeur_moy = (Fruit)+lead(Fruit)/2)
