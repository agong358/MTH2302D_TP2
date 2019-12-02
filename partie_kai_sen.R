#---- Lecture de donnees

donnees <- read.csv('C:/Users/kai_k/Documents/Polytechnique/Aut19/MTH2302D/Projet/MTH2302D_TP2/donnees_a_analyser.csv')

data <- na.omit(donnees)
data

#---- La relation entre le nombre de votes et le revenu est-elle lineaire?

#Nuage de points

#plot(x=donneesFinales$Votes, y=donneesFinales$Revenu)

#---- Quel(s) genre(s) de films est le plus populaire au fil des années (par décennies).

groupe1 <- subset(data, Genre == 1 | Genre == 2 | Genre == 9 | Genre == 15 | Genre == 16)
groupe2 <- subset(data, Genre == 6 | Genre == 7 | Genre == 10| Genre == 11 | Genre == 13)
groupe3 <- subset(data, Genre == 12| Genre == 14)
groupe4 <- subset(data, Genre == 3 | Genre == 5 | Genre == 8 )
groupe5 <- subset(data, Genre == 4 )


## Normalité Revenu
hist(data$Revenu, xlab = "Revenu", ylab = "Fréquence", main = "Histogramme du revenu")
lines(density(data$Revenu), col="red", lwd=2)
shapiro.test(data$Revenu)

##Normalité votes
hist(data$Votes, xlab = "Nombre de votes", ylab = "Fréquence", main = "Histogramme du nombre de votes", freq = FALSE)
lines(density(data$Votes), col="red", lwd=2)
shapiro.test(data$Votes)


##Distribution genre
library(fitdistrplus)
data("data", package = "fitdistrplus")
genre <- data$Genre
descdist(genre, discrete=FALSE, boot=500)

##Distribution beta de genre  ?????????????????????????????

fit <- fitdist(data$Genre, "beta", start=list(shape1 = 1, shape2 = 1, rate = 1))
plot(fit, las = 1)

##Distribution revenu
revenu <- data$Revenu
descdist(revenu, discrete=FALSE, boot=500)

##Distribution gamma revenu ??????????????????
fit.gamma <- fitdist(data$Revenu, "gamma")
plot(fit.gamma)
fit.exp <- fitdist(data$Revenu, "exp", method = "mme")
fit.gamma$aic

fitdistr(data$Revenu, "gamma")
estimGamma <- eqgamma(data$Gamma)
estimGamma$interval

##Distribution nombre de votes
votes <- data$Votes
descdist(votes, discrete=FALSE, boot=500)


#---- Y-a-t-il une correlation entre le genre de film et le revenu percu?

#Boite a moustache


#---- Est-ce que le score a une corrélation avec le Metascore?

plot(x=data$Score, y=data$Metascore)

