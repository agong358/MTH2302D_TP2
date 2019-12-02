 {"_execution_state":"idle"}
#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## Importing packages
library(ggplot2)
library(reshape2)
library(fitdistrplus)
library(logspline)
library(EnvStats)
library(MASS)

list.files(path = "../input")


#Lire le fichier
data <- read.csv("../input/donnees_a_analyser.csv")


#A GARDER!!!!!!!!!!!!!!!!!!!11
#Pour faire le heatmap
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


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Grouper les différents genres
groupe1 <- subset(data, Genre == 1 | Genre == 2 | Genre == 9 | Genre == 15 | Genre == 16,
                  select=c(Annee, Revenu, Genre))
groupe2 <- subset(data, Genre == 6 | Genre == 7 | Genre == 10 | Genre == 11 | Genre == 13,
                  select=c(Annee, Revenu, Genre))
groupe3 <- subset(data, Genre == 3 | Genre == 5 | Genre == 8,
                  select=c(Annee, Revenu, Genre))
groupe4 <- subset(data, Genre == 12 | Genre == 14 | Genre == 4,
                  select=c(Annee, Revenu, Genre))


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Question 2: Quel(s) genre(s) de films est le plus populaire au fil des années (par décennies)?
hist(groupe1$Annee, main = "Répartition des années pour le groupe 1 de genres",
     xlab = "Années", ylab = "Fréquence")
hist(groupe2$Annee, main = "Répartition des années pour le groupe 2 de genres",
     xlab = "Années", ylab = "Fréquence")
hist(groupe3$Annee, main = "Répartition des années pour le groupe 3 de genres",
     xlab = "Années", ylab = "Fréquence")
hist(groupe4$Annee, main = "Répartition des années pour le groupe 4 de genres",
     xlab = "Années", ylab = "Fréquence")


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Question 3: Y-a-t-il une corrélation entre le genre de film et le revenu perçu?

boxplot(groupe1$Revenu, groupe2$Revenu, groupe3$Revenu, groupe4$Revenu, 
        main = "Boites à moustache du revenu pour chaque groupe de genre",
        xlab ="Différents groupes de genres", ylab = "Revenu",
        names = c("Groupe 1", "Groupe 2", "Groupe 3", "Groupe 4"))
boxplot(groupe1$Revenu, groupe2$Revenu, groupe3$Revenu, groupe4$Revenu, 
        main = "Boites à moustache du revenu pour chaque groupe de genre",
        xlab ="Différents groupes de genres", ylab = "Revenu", outline = FALSE, 
        names = c("Groupe 1", "Groupe 2", "Groupe 3", "Groupe 4"))



#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Question 4: Est-ce que le score a une corrélation avec le Metascore?
plot(data$Score, data$Metascore, xlab = "Score", ylab = "Metascore", main = "Corrélation entre le score et le Metascore")
abline(lm(data$Metascore~data$Score), col="red")
reg3 <- lm(data$Metascore~data$Score, data)
summary(reg3)


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Histogrammes pour déterminer la distribution
hist(data$Annee, 
     freq = FALSE, 
     main = "Histogramme des années", 
     xlab = "Années", ylab = "Densité")
lines(density(data$Annee), col="red", lwd=2)
hist(data$Score, 
     freq = FALSE, 
     main = "Histogramme des scores", 
     xlab = "Score", ylab = "Densité")
lines(density(data$Score), col="red", lwd=2)
hist(data$Metascore, 
     freq = FALSE, 
     main = "Histogramme des Metascores", 
     xlab = "Metascore", ylab = "Densité")
lines(density(data$Metascore), col="red", lwd=2)


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Shapiro Wilk pour la normalité
shapiro.test(data$Annee)
shapiro.test(data$Score)
shapiro.test(data$Metascore)


#Determination de la distribution pour Annee
descdist(data$Annee, discrete = FALSE)


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Test distribution pour Annee
fit.gamma <- fitdist(data$Annee, "gamma")
plot(fit.gamma)
fit.exp <- fitdist(data$Annee, "exp", method = "mme")
plot(fit.exp)
fit.gamma$aic
fit.exp$aic


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Estimation des paramètres de Annee, forme et echelle
fitdistr(data$Annee, "gamma")
estimAnnee <- eqgamma(data$Annee, ci = TRUE)
estimAnnee$interval


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Test d'hypothese pour la moyenne de Annee
df.new <- data[-sample(1:nrow(data), 1500), ]
annee.mean <- mean(df.new$Annee)
t.test(data$Annee, mu=annee.mean)


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Determination de la distribution pour Score
descdist(data$Score, discrete = FALSE)


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Test distribution pour score
fit.lnorm <- fitdist(data$Score, "lnorm")
plot(fit.lnorm)
fit.logis <- fitdist(data$Score, "logis")
plot(fit.logis)
fit.norm <- fitdist(data$Score, "norm")
plot(fit.norm)
fit.lnorm$aic
fit.logis$aic
fit.norm$aic


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Estimation des paramètres de Score
fitdistr(data$Score, "normal")
#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Intervalle de confiance pour score
Deg = sd(data$Metascore)
qt(1-0.025, (length(data$Score))-1)
Llim = mean(data$Score) - qt(1-0.025, (length(data$Score))-1) * Deg / sqrt(length(data$Score))
Ulim = mean(data$Score) + qt(1-0.025, (length(data$Score))-1) * Deg / sqrt(length(data$Score))
Llim
Ulim


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Test d'hypothese pour la moyenne de Score
score.mean <- mean(df.new$Score)
t.test(data$Score, mu=score.mean)


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#QQ plots pour les variables ayant un W de shapiro assez grand
qqnorm(data$Metascore, main = "Q-Q Plot des Metascores")
qqline(data$Metascore)
#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Estimation pontuelle du metascore avec le maximum de vraisemblance
fitdistr(data$Metascore, "normal")
#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Intervalle de confiance pour metascore
S = sd(data$Metascore)
qt(1-0.025, (length(data$Metascore))-1)
L = mean(data$Metascore) - qt(1-0.025, (length(data$Metascore))-1) * S / sqrt(length(data$Metascore))
U = mean(data$Metascore) + qt(1-0.025, (length(data$Metascore))-1) * S / sqrt(length(data$Metascore))
L
U


#A GARDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Test d'hypothese pour la moyenne de Metascore
metascore.mean <- mean(df.new$Metascore)
t.test(data$Metascore, mu=metascore.mean)