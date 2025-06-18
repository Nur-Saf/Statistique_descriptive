#----------------------------------- Importation ------------------------

Student_Depression <- read.csv("C:/Users/DELL/Desktop/First Project Stat/Student_Depression.csv", sep = ";",
                               stringsAsFactors=TRUE,
                               row.names = 1)

Student_Depression <- subset(Student_Depression, select = -id)
Stu_Depression_Update <- write.csv(Student_Depression, "Stu_Depression_Update.csv",
                                   row.names = FALSE)
#Exporter le fichier en local
write.csv(Student_Depression, "C:/Users/DELL/Desktop/INSSEDS/Student_Depression.csv",
          row.names = FALSE)

Student_Depression$depression <- factor(Student_Depression$depression,
                                        levels = c(0, 1),
                                        labels = c("No", "Yes"))
Student_Depression$pression_liee_au_travail <- factor(Student_Depression$pression_liee_au_travail,
                                                      levels = c(0, 1),
                                                      labels = c("Pas de pression", "Pression"))
Student_Depression$satisfaction_travail <- factor(Student_Depression$satisfaction_travail,
                                                  levels = c(0, 1),
                                                  labels = c("Pas Satisfait", "Satisfait"))
Student_Depression$pression_academique <- cut(Student_Depression$pression_academique,
                                              breaks = c(-Inf, 2, 3, 5),  # Définir les intervalles
                                              labels = c("Faible", "Modérée", "Élevée"),
                                              right = TRUE,  # Inclure la limite supérieure
                                              ordered_result = TRUE)
Student_Depression$satisfaction_etudes <- cut(Student_Depression$satisfaction_etudes,
                                              breaks = c(-Inf, 2, 3, 5),  # Définir les intervalles
                                              labels = c("Très Insatisfait", "Satisfait", "Très Satisfait"),
                                              right = TRUE,  # Inclure la limite supérieure
                                              ordered_result = TRUE)
Student_Depression$stress_financier <- cut(Student_Depression$stress_financier,
                                           breaks = c(-Inf, 2, 3, 5),  # Définir les intervalles
                                           labels = c("Faible", "Modéré", "Élevé"),
                                           right = TRUE,  # Inclure la limite supérieure
                                           ordered_result = TRUE)

print(Student_Depression)
str(Student_Depression)

#----------------------------------- Prétraitement -------------------------------

#----------------------------------Traitement de doublons
traitement_doublons(Student_Depression)

#-----------------------------------Traitement des valeurs manquantes
install.packages("visdat")
library(visdat)
vis_dat(Student_Depression)
vis_miss(Student_Depression)
Student_Depression[!complete.cases(Student_Depression),]
nrow(Student_Depression[!complete.cases(Student_Depression),])
nrow(Student_Depression[!complete.cases(Student_Depression),]) / nrow(Student_Depression)
Student_Depression <- na.omit(Student_Depression)
nrow(Student_Depression[!complete.cases(Student_Depression),])

#---------------------------------------Traitement des valeurs extremes
#---------Visualisation
install.packages('rpart')
library(rpart)
boxplot(Student_Depression$age, col = "red")


#----------Traitement
install.packages('DescTools')
library(DescTools)
Student_Depression$age <- Winsorize(Student_Depression$age)
boxplot(Student_Depression$age, col = "red")



#--------------------------------- Analyse Univariée -------------------------
# -----Variable âge
library(Dagnogo)
safia.qt.tableau(Student_Depression$age)
safia.qt.graph(Student_Depression$age)

install.packages('RVAideMemoire')
library(RVAideMemoire)

install.packages("moments")
library(moments)

safia.qt.resume(Student_Depression$age)

# -----variable nombre_heure_travail_etude
safia.qt.tableau(Student_Depression$nombre_heure_travail_etude)
safia.qt.graph(Student_Depression$nombre_heure_travail_etude)
safia.qt.resume(Student_Depression$nombre_heure_travail_etude)

#---- variable sexe
safia.ql.graph(Student_Depression$sexe)

#---- variable pression_adademique
safia.ql.graph(Student_Depression$pression_academique)

#----------stress_financier
safia.ql.graph(Student_Depression$stress_financier)

#------- pensée_suicidaire
safia.ql.graph(Student_Depression$pensees_suicidaire)

#------------ habitudes_alimentaires
safia.ql.graph(Student_Depression$habitudes_alimentaires)


#--------------------------Analyse bivariée ----------------------
# Hypothèse 1: est-ce que la depression chez les étudiants est liée au âge ?

install.packages('BioStatR')
library('BioStatR')

# (Tableau de contingence)
table(Student_Depression$age, Student_Depression$depression)

# Graphique: (Boîte par niveau de facteur et horizontalement)
ggplot(Student_Depression, aes(x = depression, y = age)) +
                                 geom_boxplot(fill = "blue") +
                                 labs(title = "Diagramme en boîte des depression par âge",
                                  x = "depression",
                                  y = "age")
# (Résumé numérique)
safia.qtql.liaison(Student_Depression$age, Student_Depression$depression)


# Hypothèse 2: est-ce que la depression chez les étudiants est liée  au sexe ?
#Tableau
safia.2ql.tableau(Student_Depression$depression, Student_Depression$sexe)
install.packages('questionr')
safia.2ql.graph(Student_Depression$sexe, Student_Depression$depression)

# (Création d'un DataFrame à partir de la table de contingence)
library(ggplot2)
contingency_table <- table(Student_Depression$depression, Student_Depression$sexe)
contingency_df <- as.data.frame(contingency_table)

# (Création du diagramme en bâtons groupés à l'aide de...)
ggplot(contingency_df, aes(x = Var2, y = Freq,
                           fill = Var1)) +
                           geom_bar(stat = "identity", position = "dodge") +
                           theme_minimal() +
                           xlab("sexe") +
                           ylab("Fréquence") +
                           ggtitle("Diagramme en bâtons groupés") +
                           guides(fill = guide_legend(title = "depression"))

# (Résumé numérique)
safia.2ql.liaison(Student_Depression$sexe, Student_Depression$depression)

# Hypothèse 3: est-ce que la depression chez les étudiants est liée à la pression academique ?
safia.2ql.tableau(Student_Depression$depression, Student_Depression$pression_academique)

# Graphique
# (Création d'un DataFrame à partir de la table de contingence)
library(ggplot2)
contingency_table <- table(Student_Depression$depression, Student_Depression$pression_academique)
contingency_df <- as.data.frame(contingency_table)

# (Création du diagramme en bâtons groupés à l'aide de...)
ggplot(contingency_df, aes(x = Var2, y = Freq,
                           fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("pression_academique") +
  ylab("Fréquence") +
  ggtitle("Diagramme en bâtons groupés") +
  guides(fill = guide_legend(title = "depression"))

# Résumé
safia.2ql.liaison(Student_Depression$depression, Student_Depression$pression_academique)
library(questionr)
cramer.v(table(Student_Depression$depression, Student_Depression$pression_academique))


# Hypothèse 4: Est-ce que la dépression chez les étudiants est liée au nombre d’heure de travail ou d’étude ?
# Graphique
library(ggplot2)
contingency_table <- table(Student_Depression$depression, Student_Depression$nombre_heure_travail_etude)
contingency_df <- as.data.frame(contingency_table)

# (Création du diagramme en bâtons groupés à l'aide de...)
ggplot(contingency_df, aes(x = Var2, y = Freq,
                           fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("nombre_heure_travail_etude") +
  ylab("Fréquence") +
  ggtitle("Diagramme en bâtons groupés") +
  guides(fill = guide_legend(title = "depression"))

# Résumé
cramer.v(table(Student_Depression$depression, Student_Depression$nombre_heure_travail_etude))


# Hypothèse 5: Est-ce que la dépression chez les étudiants est causée par la durée de sommeil ?
# Graphique
library(ggplot2)
contingency_table <- table(Student_Depression$depression, Student_Depression$duree_sommeil)
contingency_df <- as.data.frame(contingency_table)

# (Création du diagramme en bâtons groupés à l'aide de...)
ggplot(contingency_df, aes(x = Var2, y = Freq,
                           fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("durée_sommeil") +
  ylab("Effectif") +
  ggtitle("Diagramme en bâtons groupés") +
  guides(fill = guide_legend(title = "depression"))

# Résumé
cramer.v(table(Student_Depression$depression, Student_Depression$duree_sommeil))


# Hypothèse 6: Est-ce que la dépression chez les étudiants est liée au stress financier ?
# Graphique
contingency_table <- table(Student_Depression$depression, Student_Depression$stress_financier)
contingency_df <- as.data.frame(contingency_table)

# (Création du diagramme en bâtons groupés à l'aide de...)
ggplot(contingency_df, aes(x = Var2, y = Freq,
                           fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("stress_financier") +
  ylab("Effectif") +
  ggtitle("Diagramme en bâtons groupés") +
  guides(fill = guide_legend(title = "depression"))

# Résumé
cramer.v(table(Student_Depression$depression, Student_Depression$duree_sommeil))


# Hypothèse 7:	Est-ce que la dépression chez les étudiants est liée aux habitudes alimentaires ?
# Graphique
contingency_table <- table(Student_Depression$depression, Student_Depression$habitudes_alimentaires)
contingency_df <- as.data.frame(contingency_table)

# (Création du diagramme en bâtons groupés à l'aide de...)
ggplot(contingency_df, aes(x = Var2, y = Freq,
                           fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("habitudes_alimentaires") +
  ylab("Effectif") +
  ggtitle("Diagramme en bâtons groupés") +
  guides(fill = guide_legend(title = "depression"))

# Résumé
cramer.v(table(Student_Depression$depression, Student_Depression$habitudes_alimentaires))


# Hypothèse 8:	Est-ce que la dépression chez les étudiants est liée aux antécédants familiaux de maladie mentale ?
contingency_table <- table(Student_Depression$depression, Student_Depression$antecedants_familiaux_maladie_mentale)
contingency_df <- as.data.frame(contingency_table)

# (Création du diagramme en bâtons groupés à l'aide de...)
ggplot(contingency_df, aes(x = Var2, y = Freq,
                           fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("antecedants_familiaux_maladie_mentale") +
  ylab("Effectif") +
  ggtitle("Diagramme en bâtons groupés") +
  guides(fill = guide_legend(title = "depression"))

# Résumé
cramer.v(table(Student_Depression$depression, Student_Depression$antecedants_familiaux_maladie_mentale))


# Hypothèse 9: Est-ce que la dépression chez les étudiants est liée à la satisfaction d'étude ?
contingency_table <- table(Student_Depression$depression, Student_Depression$satisfaction_etudes)
contingency_df <- as.data.frame(contingency_table)

# (Création du diagramme en bâtons groupés à l'aide de...)
ggplot(contingency_df, aes(x = Var2, y = Freq,
                           fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("satisfaction_etudes") +
  ylab("Effectif") +
  ggtitle("Diagramme en bâtons groupés") +
  guides(fill = guide_legend(title = "depression"))

# Résumé
cramer.v(table(Student_Depression$depression, Student_Depression$satisfaction_etudes))
