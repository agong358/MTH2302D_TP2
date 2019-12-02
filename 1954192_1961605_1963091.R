# %% [code] {"_execution_state":"idle"}
# Importing packages
library(ggplot2)
library(reshape2)
library(fitdistrplus)
library(logspline)
library(EnvStats)
library(MASS)


#Lire le fichier
data <- read.csv("1954192_1961605_1963091.csv", header = TRUE)

#Grouper les differents genres
groupe1 <- subset(data, Genre == 1 | Genre == 2 | Genre == 9 | Genre == 15 | Genre == 16,
                  select=c(Annee, Revenu, Genre))
groupe2 <- subset(data, Genre == 6 | Genre == 7 | Genre == 10 | Genre == 11 | Genre == 13,
                  select=c(Annee, Revenu, Genre))
groupe3 <- subset(data, Genre == 3 | Genre == 5 | Genre == 8,
                  select=c(Annee, Revenu, Genre))
groupe4 <- subset(data, Genre == 12 | Genre == 14 | Genre == 4,
                  select=c(Annee, Revenu, Genre))

##############################################
#####-----STATISTIQUE DESCRIPTIVE------#######
##############################################


###--- Heatmap
mydata <- data[, c(1,2,3,4,5,6)]
head(mydata)
cormat <- round(cor(mydata),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


###--- Relation entre le nombre de votes et le revenu---###

plot(x=data$Votes, y=data$Revenu, 
     xlab = "Votes", ylab = "Revenu", 
     main = "Relation entre le nombre de votes et le revenu")


###--- Popularite d'un genre par rapport à l'annee de sortie---###

hist(groupe1$Annee, main = "Répartition des années pour le groupe 1 de genres",
     xlab = "Années", ylab = "Fréquence")
hist(groupe2$Annee, main = "Répartition des années pour le groupe 2 de genres",
     xlab = "Années", ylab = "Fréquence")
hist(groupe3$Annee, main = "Répartition des années pour le groupe 3 de genres",
     xlab = "Années", ylab = "Fréquence")
hist(groupe4$Annee, main = "Répartition des années pour le groupe 4 de genres",
     xlab = "Années", ylab = "Fréquence")


###--- Relation entre le genre du film et le revenu perçu

boxplot(groupe1$Revenu, groupe2$Revenu, groupe3$Revenu, groupe4$Revenu, 
        main = "Boites à moustache du revenu pour chaque groupe de genre",
        xlab ="Différents groupes de genres", ylab = "Revenu",
        names = c("Groupe 1", "Groupe 2", "Groupe 3", "Groupe 4"))
boxplot(groupe1$Revenu, groupe2$Revenu, groupe3$Revenu, groupe4$Revenu, 
        main = "Boites à moustache du revenu pour chaque groupe de genre",
        xlab ="Différents groupes de genres", ylab = "Revenu", outline = FALSE, 
        names = c("Groupe 1", "Groupe 2", "Groupe 3", "Groupe 4"))


###--- Relation entre le score du film et le Metascore
plot(data$Score, data$Metascore, 
     xlab = "Score", ylab = "Metascore", 
     main = "Corrélation entre le score et le Metascore")
abline(lm(data$Metascore~data$Score), col="red")
reg3 <- lm(data$Metascore~data$Score, data)
summary(reg3)


################################################
#####-----ESTIMATION DES PARAMETRES------#######
################################################

#Histogrammes pour déterminer la distribution
#Annee
hist(data$Annee, 
     freq = FALSE, 
     main = "Histogramme des années", 
     xlab = "Années", ylab = "Densité")
lines(density(data$Annee), col="red", lwd=2)
#Score
hist(data$Score, 
     freq = FALSE, 
     main = "Histogramme des scores", 
     xlab = "Score", ylab = "Densité")
lines(density(data$Score), col="red", lwd=2)
#Metascore
hist(data$Metascore, 
     freq = FALSE, 
     main = "Histogramme des Metascores", 
     xlab = "Metascore", ylab = "Densité")
lines(density(data$Metascore), col="red", lwd=2)
#Revenu
hist(data$Revenu, 
     xlab = "Revenu", ylab = "Frequence", main = "Histogramme du revenu")
lines(density(data$Revenu), col="red", lwd=2)
#Nombre de votes
hist(data$Votes, 
     xlab = "Nombre de votes", ylab = "FrÃ©quence", main = "Histogramme du nombre de votes", 
     freq = FALSE)
lines(density(data$Votes), col="red", lwd=2)

#Shapiro Wilk pour la normalite
shapiro.test(data$Annee)
shapiro.test(data$Score)
shapiro.test(data$Metascore)
shapiro.test(data$Revenu)
shapiro.test(data$Votes)


###--- Annee

#Determination de la distribution pour Annee
descdist(data$Annee, discrete = FALSE)

#Test distribution pour Annee
fit.gamma <- fitdist(data$Annee, "gamma")
plot(fit.gamma)
fit.exp <- fitdist(data$Annee, "exp", method = "mme")
plot(fit.exp)
fit.gamma$aic
fit.exp$aic

#Estimation des parametres de Annee, forme et echelle
fitdistr(data$Annee, "gamma")
estimAnnee <- eqgamma(data$Annee, ci = TRUE)
estimAnnee$interval

#Test d'hypothese pour la moyenne de Annee
df.new <- data[-sample(1:nrow(data), 1500), ]
annee.mean <- mean(df.new$Annee)
t.test(data$Annee, mu=annee.mean)


###--- Score

#Estimation des parametres de Score
fitdistr(data$Score, "normal")

#Intervalle de confiance pour score
Deg = sd(data$Metascore)
qt(1-0.025, (length(data$Score))-1)
Llim = mean(data$Score) - qt(1-0.025, (length(data$Score))-1) * Deg / sqrt(length(data$Score))
Ulim = mean(data$Score) + qt(1-0.025, (length(data$Score))-1) * Deg / sqrt(length(data$Score))
Llim
Ulim

#Test d'hypothese pour la moyenne de Score
score.mean <- mean(df.new$Score)
t.test(data$Score, mu=score.mean)


###--- Metascore

#QQ plots pour les variables ayant un W de shapiro assez grand
qqnorm(data$Metascore, main = "Q-Q Plot des Metascores")
qqline(data$Metascore)

#Estimation pontuelle du metascore avec le maximum de vraisemblance
fitdistr(data$Metascore, "normal")

#Intervalle de confiance pour metascore
S = sd(data$Metascore)
qt(1-0.025, (length(data$Metascore))-1)
L = mean(data$Metascore) - qt(1-0.025, (length(data$Metascore))-1) * S / sqrt(length(data$Metascore))
U = mean(data$Metascore) + qt(1-0.025, (length(data$Metascore))-1) * S / sqrt(length(data$Metascore))
L
U

#Test d'hypothese pour la moyenne de Metascore
metascore.mean <- mean(df.new$Metascore)
t.test(data$Metascore, mu=metascore.mean)


###--- Genre

#Determination de la distribution pour Genre
descdist(data$Genre, discrete=FALSE, boot=500)

#Estimation pontuelle du genre avec la méthode des moments
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
mu <- mean(data$Genre)
var <- var(data$Genre)
estBetaParams(mu, var)

#Intervalle de confiance et test d'hypothèse pour Genre
df.new <- data[-sample(1:nrow(data), 1500), ]
Genre.mean <- mean(df.new$Genre)
t.test(data$Genre, mu=Genre.mean)

# %% [code]
###--- Revenu

#Determination de la distribution pour Revenu
descdist(data$Revenu, discrete=FALSE, boot=500)

#Estimation ponctuelle du revenu avec le maximum de vraisemblance
eqgamma(data$Revenu)

#Intervalle de confiance et test d'hypothèse pour revenu
df.new <- data[-sample(1:nrow(data), 1500), ]
Revenu.mean <- mean(df.new$Revenu)
t.test(data$Revenu, mu=Revenu.mean)

# %% [code]
###--- Nombre de votes

#Determination de la distribution pour nombre de votes
descdist(data$Votes, discrete=FALSE, boot=500)

#Estimation ponctuelle du nombre de votes avec le maximum de vraisemblance
eqgamma(data$Votes)

#Intervalle de confiance et test d'hypothèse pour revenu
df.new <- data[-sample(1:nrow(data), 1500), ]
Votes.mean <- mean(df.new$Vote)
t.test(data$Votes, mu=Votes.mean)

# %% [code]
#################################
#####-----REGRESSION------#######
#################################
#Relation entre le nombre de votes et le revenu
plot(data$Votes,data$Revenu, xlab="Votes", ylab="Revenu", main="Relation entre le nombre de votes et le revenu")
abline(lm(data$Revenu ~ data$Votes), col="red")

#Diagramme de residus du nombre de votes
plot.lm = lm(data$Revenu ~ data$Votes)
plot.res = resid(plot.lm)
plot(data$Votes,plot.res,ylab="Résidus standardisés",xlab="Votes",main="Résidus") 
abline(0, 0)                  

#Relation entre le score et le metascore
x = data$Score
y = data$Metascore

plot(x,y, xlab="Score", ylab="Metascore", main="Relation entre le metascore et le score")
abline(lm(y~x), col="red")