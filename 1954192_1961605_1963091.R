#importer fichier
donnees <- read.csv("1954192_1961605_1963091.csv", header = TRUE)

#pour s'assurer que les donnees ont bien ete importees
donnees

#pour omettre les rangees ayant une valeur NA
donneesFinales <- na.omit(donnees)
donneesFinales

# QUESTION 1:
# ------------------------------
# La relation entre le nombre de votes et le revenu est-elle linéaire? 

#nuage de points simple des données X et Y
plot(x=donneesFinales$Votes,y=donneesFinales$Revenu, xlab="Score", ylab="Revenu", main="Relation entre le nombre de votes et le revenu")

# QUESTION 3:
# ------------------------------
# Quel(s) genre(s) de films est le plus populaire au fil des années (par décennies)?
library(ggplot2)

ggplot2.histogram(data=donneesFinales$Genre)