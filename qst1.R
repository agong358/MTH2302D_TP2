# QUESTION 1:
# ------------------------------
# La relation entre le nombre de votes et le revenu est-elle linéaire? 
#

#importer fichier
donnees <- read.csv2("donnees_a_analyser.csv")

#pour s'assurer que les donnees ont bien ete importees
donnees

#pour omettre les rangees ayant une valeur NA
donneesFinal <- na.omit(donnees)
donneesFinal

#nuage de points simple des données X et Y
plot(x=donneesFinal$Votes,y=donneesFinal$Revenu,xlab = 'Votes',ylab = 'Revenu',main = 'Relation entre le nombre de votes et le revenu')
