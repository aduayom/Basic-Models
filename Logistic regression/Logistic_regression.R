# Chargement des bibliothèques nécessaires
library(ggplot2)
library(caret)

# ÉTAPE 1: LECTURE ET COMPRÉHENSION DES DONNÉES
df <- read.csv("GitHub/Desktop_Mac_Daniel/Basic-Models/other_files/images_analyzed_productivity1.csv")
print(head(df))

# Tracer les valeurs de productivité pour voir la répartition entre Bon et Mauvais
sizes <- table(df$Productivity)
labels <- paste(names(sizes), "\n", round(prop.table(sizes) * 100, 1), "%", sep = "")
pie(sizes, main="Productivity Distribution", col=c("red", "blue"), labels=labels)


# ÉTAPE 2: SUPPRESSION DES DONNÉES IRRELEVANTES
df <- df[ , !(names(df) %in% c("Images_Analyzed", "User"))]

# ÉTAPE 3: Gérer les valeurs manquantes, si nécessaire
# df <- na.omit(df)  # Supprime toutes les lignes avec au moins une valeur nulle

# ÉTAPE 4: Convertir les données non numériques en numériques, si nécessaire
df$Productivity <- ifelse(df$Productivity == 'Good', 1, 0)
print(head(df))

# ÉTAPE 5: PRÉPARATION DES DONNÉES
Y <- as.integer(df$Productivity)
X <- df[ , !(names(df) %in% "Productivity")]

# ÉTAPE 6: DIVISER LES DONNÉES EN DONNÉES D'ENTRAÎNEMENT ET DE TEST
set.seed(20)
trainIndex <- createDataPartition(Y, p = .6, 
                                  list = FALSE, 
                                  times = 1)
X_train <- X[ trainIndex,]
X_test <- X[-trainIndex,]
y_train <- Y[ trainIndex]
y_test <- Y[-trainIndex]

# ÉTAPE 7: Définition du modèle et entraînement
# Utilisation de glm() pour la régression logistique
model <- glm(Productivity ~ ., data = data.frame(Productivity = y_train, X_train), family = binomial)

# ÉTAPE 8: TEST DU MODÈLE EN FAISANT DES PRÉDICTIONS SUR LES DONNÉES DE TEST
# ET CALCUL DE L'INDICE DE PRÉCISION
prediction_test <- predict(model, data.frame(X_test), type = "response")
prediction_test <- ifelse(prediction_test > 0.5, 1, 2)

# Imprimer la précision de la prédiction
accuracy <- sum(prediction_test == y_test) / length(y_test)
print(paste("Accuracy = ", accuracy))

# COMPRENDRE LES VARIABLES QUI ONT LE PLUS D'INFLUENCE SUR LE RÉSULTAT
# Pour obtenir les poids de toutes les variables
coefficients <- summary(model)$coefficients
print(coefficients)
