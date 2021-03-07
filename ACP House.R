
#********** Procedure ACP ********

#librairie lecture fichier excel
library(xlsx)  
#package permettant l'exploration interactive des résultats d'analyses multivariées
library(explor)

#Définir le chemin d'accès vers le répertoire contenant la dataset
setwd("C:\\Users\\bos\\Desktop\\analyse de données\\partie 2\\rendu")
getwd()

#Importation des données "House_complet2.xlsx" dans la première feuille de calcul
#première colonne = label des observations
#les données sont dans la première feuille
autos <- read.xlsx("House_complet2.xlsx", row.names = "Maisons", header = TRUE, sheetIndex = 1, 
                   stringsAsFactors = TRUE)
#autos <- read.xlsx(file.choose(), row.names = "Modele", header = TRUE, sheetIndex = 1, 
#                      stringsAsFactors = TRUE)

#afficher les données
View(autos)

#structure des données
str(autos)

#quelques vérifications
##affichage
print(autos)
##statistiques descriptives de toutes les variables
summary(autos)

#définition des variables quantitatives
autos.quant <- autos[,c(1,2,3,4,6,10,11)]
#définition des variables qualitatives
autos.quali <- autos[,c(7,8,9,14,15,16)]

##statistiques descriptives de toutes les variables quantitatives
summary(autos.quant)
##effectifs des modalité pour les variables qualitatives 
summary(autos.quali)

#nuages de points
pairs(autos)
#partition des données (var. actives et illustratives)
#On vas selectionner les variables actives et les var illustrative (supplementaires)

#définition des variables actives
autos.actifs <- autos[,c(2,3,4,5,6,10,11,12)]
#définition des variables illustratives
autos.illus <- autos[,c(1,7,8,9,14,15,16)]

#sélection de la variable quantitative "price" et certaines variables qualitative
autos.price_quali <- autos[,c(1,7,9,15)]

#nuages de points entre variables quantitatives
pairs(autos.quant)
#nuages de points entre la variable quantitative "price" et certaines variables qualitative
pairs(autos.price_quali)

#nombre d'observations
n <- nrow(autos.actifs)
print(n)

#*******************************
# ACP avec la procédure princomp
#*******************************

#centrage et réduction des données --> cor = TRUE(normaliser les données)
#calcul des coordonnées factorielles --> scores = TRUE(on demande a la fct de sauvegarder les vecteurs propre)
#X = autos.actifs : tableau de données des variables actives
acp.autos <- princomp(x = autos.actifs, cor = TRUE, scores = TRUE)

#print : sa donne les composantes principales(8 vue qu'on utilise 8 variables) avec leurs écart types.
print(acp.autos)

#summary : résumer statistique, on a les composantes principale, leurs écart types, les variances et les variances cumulé
print(summary(acp.autos))


#quelles sont les propriétés associées à l'objet
#afficher les diffirents composants qu'on a créer avec la fct princomp
#sdev : composantes qui contient les écarts types
#loadings : contient les composantes principales
#scores : contient les vect propres ,n.obs : nbres d'observations
print(attributes(acp.autos))
acp.autos$scores

#*************************
#**** valeurs propres *******
#*************************
#obtenir les variances(écart-types au carré) associées aux axes, c-Ã -d, les valeurs propres
#les valeurs propres = variances 
val.propres <- acp.autos$sdev^2
print(val.propres)

#scree plot (graphique des éboulis des valeurs propres) Afficher les valeurs propres
plot(1:8, val.propres, type = "b", ylab = "Valeurs propres", xlab = "Composante", main = "Scree plot")

#*************************************************
#coordonnées des variables sur les axes factoriels
#*************************************************

#coordonnées des variables = corrélations entres les var et les composantes principales
#acp.autos$loadings[,1] : composante principales 1 
# produit scalaire avec la var et on normalise par l'écart type
#**** corrélation variables-facteurs ****
#corrélation entres les variables et les 3 premières composantes principales
#Avec la 1er Comp principale
C1 <- acp.autos$loadings[,1]*acp.autos$sdev[1]
#Avec la 2ème Comp principale
C2 <- acp.autos$loadings[,2]*acp.autos$sdev[2]
#Avec la 3ème Comp principale
C3 <- acp.autos$loadings[,3]*acp.autos$sdev[3]

#affichage des corrélations entres les variables et les 3 premières comp principales(organise cela dans une matrice)
#coordoné des variables sur les trois axes(c1, c2, c3) 
correlation <- cbind(C1,C2,C3)
print(correlation, digits = 2)

correlation1 <- cbind(C1,C2)
print(correlation1, digits = 2)

correlation2 <- cbind(C1,C3)
print(correlation2, digits = 2)

#carrés de la corrélation (calcul des cos² = correlation au carré)
#plan(c1, c2, c3) 
print(correlation^2, digits = 2)
#plan(c1, c2) 
print(correlation1^2, digits = 2)
#plan(c1, c2) 
print(correlation2^2, digits = 2)

#cumul carrés de la corrélation(cumulé des cos²)
#taux d'informations expliquer par le plan(c1, c2) pour chacune des variables
print(t(apply(correlation1^2, 1, cumsum)), digits = 2)
#taux d'informations expliquer par le plan(c1, c3) pour chacune des variables
print(t(apply(correlation2^2, 1, cumsum)), digits = 2)

#*** cercle des corrélations - variables actives ***
# Traçage du cercle de corrélation 
#plan (c1. c2)
plot(C1, C2, xlim = c(-1,+1), ylim = c(-1,+1), type = "n")
abline(h = 0, v = 0)
text(C1, C2, labels = colnames(autos.actifs), cex = 0.5)
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)
#plan (c1. c3)
plot(C1, C3, xlim = c(-1,+1), ylim = c(-1,+1), type = "n")
abline(h = 0, v = 0)
text(C1, C3, labels = colnames(autos.actifs), cex = 0.5)
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)



#***************************************************************
#*** projection des individus dans le premier plan factoriel ***
#***************************************************************

#Representation des individus dans le plan principale | Score sont les Composantes principales
#l'option "scores" demandée dans princomp est trés importante ici
plot(acp.autos$scores[,1], acp.autos$scores[,2], type = "n", xlab = "Comp.1 - 74%", ylab = "Comp.2 - 14%")
abline(h = 0, v = 0)
text(acp.autos$scores[,1], acp.autos$scores[,2], labels = rownames(autos.actifs), cex = 0.75)

#**************************************
#*** Cette partie est utilisé afin de calculer la qlt de représentation des individus
#*** afin de supprimer ceux qui sont mal représenter
#**************************************
d2 <- function(x){return(sum(((x-acp.autos$center)/acp.autos$scale)^2))}
#appliquer Ã  l'ensemble des observations
all.d2 <- apply(autos.actifs, 1, d2)
cos2 <- function(x){return(x^2/all.d2)}
#cosinus^2 pour une composante
all.cos2 <- apply(acp.autos$scores[, 1:2], 2, cos2)
print(all.cos2)
#cosinus^2 pour l'ensemble des composantes retenues (les 2 premières)
all.cos2 = data.frame(all.cos2)
cumul = t(apply(all.cos2, 1, cumsum))
cumul
cumul = data.frame(cumul)
print(t(apply(all.cos2, 1, cumsum)), digits = 2)
#selectionner les individus qui ont une qlt inferieur a 90%
row = row.names(cumul)[which(cumul$Comp.2<0.90)]
row = row[row!= "m_2968"]
row = row[row!= "m_1183"]
#**************************************
test1 = data.frame(acp.autos$scores[,1])
test2 = data.frame(acp.autos$scores[,2])
test3 = data.frame(acp.autos$scores[,3])

#*Suppression des individus qui on une qlt inferieur à 90%
test1 = test1[!(rownames(test1) %in% row),]
test2 = test2[!(rownames(test2) %in% row),]
test3 = test3[!(rownames(test3) %in% row),]

#Representation des individus dans le plan(c1. c2) 
#après suppresion des individus mal representer
plot(test1, test2, type = "n", xlab = "Comp.1", ylab = "Comp.2")
abline(h = 0, v = 0)
text(test1, test2, labels = rownames(autos.actifs), cex = 0.75)

#**************************************************************************************
#*** représentation simultanée : individus x variables - cf. Lebart et al., pages 46-48
# Sur le mème graphique representé à la fois les variables et les individus
#**************************************************************************************

#suppression des individus qui on une qlt de representation inferieur à 90%
acp.autos$scores =acp.autos$scores[!(rownames(acp.autos$scores) %in% row),]
explor(acp.autos)

#representation simultaner des individus et des variables actives
biplot(acp.autos, cex = 0.75)
#affiche les axes
abline(h = 0, v = 0, lty = 2)

#Pour chaque individus, on a ses contributions pour la construction de chacun des 2 axes
#contributions Ã  une composante - calcul pour les 2 premières composantes
all.ctr <- NULL
for (k in 1:2){all.ctr <- cbind(all.ctr, 100.0*(1.0/n)*(acp.autos$scores[,k]^2)/(acp.autos$sdev[k]^2))}
print(all.ctr)

print(t(apply(all.ctr, 1, cumsum)), digits = 2)

#***************************************
# traitement des variables illustratives(Supplémentaires)
#***************************************

# Representé la variable supplémentaire quantitative étudié (price)
#*******************************************************************************************
#calcul des coordonnées de la variable illustrative étudié (price) sur le cercle des corrélations

#premier axe
#corrélation avec le 1er axe

#suppresion des individus qui on une qlt inferieurs a 90%
autos.illus = autos.illus[!(rownames(autos.illus) %in% row),]
test1 = data.frame(acp.autos$scores)
test1 = test1[!(rownames(test1) %in% row),]

ma_cor_1 <- function(x){return(cor(x,test1[,1]))}
s1 <- sapply(autos.illus[1], ma_cor_1)
#second axe
#corrélation avec le 2eme axe
ma_cor_2 <- function(x){return(cor(x,test1[,2]))}
s2 <- sapply(autos.illus[1], ma_cor_2)

#Traçage du cercle de corrélation pour la var supplementaire étudié (Prix)
plot(s1,s2, xlim = c(-1,+1), ylim = c(-1,+1), type = "n", main = "Variables illustratives",
     xlab = "Comp.1", ylab = "Comp.2")
abline(h = 0, v = 0, lty = 2)
text(s1,s2, labels = colnames(autos.illus[1]), cex = 1.0)
symbols(0,0, circles = 1, inches = FALSE, add = TRUE)

#représentation simultanée des var actives et la var supplémentaire étudié (prix)
plot(c(C1,s1), c(C2,s2), xlim = c(-1,+1), ylim = c(-1,+1), type = "n", 
     main = "Variables illustratives", xlab = "Comp.1", ylab = "Comp.2")
text(C1,C2, labels = colnames(autos.actifs), cex = 0.5)
text(s1,s2, labels = colnames(autos.illus[1]), cex = 0.75, col = "red")
abline(h = 0, v = 0, lty = 2)
symbols(0,0, circles = 1, inches = FALSE, add = TRUE)



#*******************************************************************************
#Representation des Variables supplémentaires qualitatives 
#Representation à l'aide de leurs modalitées 

#positionner les modalités de la variable illustrative + (calcul des valeurs test : n'est pas fait ici)
#on calcule les coord par a l'axe 1 et l'axe 2

#***********City****************
#extraire les modalitées de la var qualitatives
K <- nlevels(autos.illus[,"city"])
print(K)
var.illus <- unclass(autos.illus[,"city"])

autos.illus[,"city"]
#Calcule des coordonées des modalitées
m1 <- c()
m2 <- c()
for (i in 1:K){m1[i] <- mean(test1[var.illus==i,1])}
for (i in 1:K){m2[i] <- mean(test1[var.illus==i,2])}
cond.moyenne <- cbind(m1,m2)
rownames(cond.moyenne) <- levels(autos.illus[,"city"])

#graphique
plot(c(test1[,1],m1), c(test1[,2],m2), xlab = "Comp.1",
     ylab = "Comp.2", main = "Positionnement var.illus catégorielle", type = "n")
abline(h = 0, v = 0, lty = 2)

autos.actifs = data.frame(autos.actifs)
autos.actifs = autos.actifs[!(rownames(autos.actifs) %in% row),]

text(test1[,1],test1[,2], rownames(autos.actifs), cex = 0.5)
text(m1,m2, rownames(cond.moyenne), cex = 0.95, col = "red")

#*************View**********************
#Representation des modalité de la variable View avec ses coord dans le plan principale
K <- nlevels(autos.illus[,"view"])
print(K)
var.illus <- unclass(autos.illus[,"view"])

autos.illus[,"view"]
m1 <- c()
m2 <- c()
for (i in 1:K){m1[i] <- mean(test1[var.illus==i,1])}
for (i in 1:K){m2[i] <- mean(test1[var.illus==i,2])}
cond.moyenne <- cbind(m1,m2)
rownames(cond.moyenne) <- levels(autos.illus[,"view"])
#graphique
plot(c(test1[,1],m1), c(test1[,2],m2), xlab = "Comp.1",
     ylab = "Comp.2", main = "Positionnement var.illus catégorielle", type = "n")
abline(h = 0, v = 0, lty = 2)

autos.actifs = data.frame(autos.actifs)
autos.actifs = autos.actifs[!(rownames(autos.actifs) %in% row),]

text(test1[,1],test1[,2], rownames(autos.actifs), cex = 0.5)
text(m1,m2, rownames(cond.moyenne), cex = 0.95, col = "red")

#*******Condition*******
K <- nlevels(autos.illus[,"condition"])
print(K)
var.illus <- unclass(autos.illus[,"condition"])

autos.illus[,"condition"]
m1 <- c()
m2 <- c()
for (i in 1:K){m1[i] <- mean(test1[var.illus==i,1])}
for (i in 1:K){m2[i] <- mean(test1[var.illus==i,2])}
cond.moyenne <- cbind(m1,m2)
rownames(cond.moyenne) <- levels(autos.illus[,"condition"])
#graphique
plot(c(test1[,1],m1), c(test1[,2],m2), xlab = "Comp.1",
     ylab = "Comp.2", main = "Positionnement var.illus catégorielle", type = "n")
abline(h = 0, v = 0, lty = 2)

autos.actifs = data.frame(autos.actifs)
autos.actifs = autos.actifs[!(rownames(autos.actifs) %in% row),]

text(test1[,1],test1[,2], rownames(autos.actifs), cex = 0.5)
text(m1,m2, rownames(cond.moyenne), cex = 0.95, col = "red")

#**************************************
# traitement des individus illustratifs
#**************************************
#chargement de la seconde feuille de calcul + vérification "House_complet2.xlsx"


#Representation des individus supplémentaire(la deuxième fiche du fichier excel)
ind.illus <- read.xlsx("House_complet2.xlsx", row.names = "Maisons", header = TRUE, sheetIndex = 2, 
                       stringsAsFactors = TRUE)
#ind.illus <- read.xlsx(file.choose(), row.names = "Modele", header = TRUE, sheetIndex = 2, 
#                         stringsAsFactors = TRUE)

#selection des indivudus avec leurs variables actives
ind.illus <- ind.illus[,c(2,3,4,5,6,10,11,12)]
#Less individus supplementaires qu'on vas representer sur le plan principale
#ils non pas participer à la construction des bases(du plan)
ind.illus
summary(ind.illus)

#centrage et réduction en utilisant les moyennes et écarts-type de l'ACP
ind.scaled <- NULL
for (i in 1:nrow(ind.illus)){ind.scaled <- rbind(ind.scaled,(ind.illus[i,] - acp.autos$center)/acp.autos$scale)}
print(ind.scaled)

#calcul des coordonnées factorielles(en utilisant les valeurs propres cf. loadings)
produit.scal <- function(x,k){return(sum(x*acp.autos$loadings[,k]))}
ind.coord <- NULL
for (k in 1:2){ind.coord <- cbind(ind.coord,apply(ind.scaled,1,produit.scal,k))}
print(ind.coord)

#calcul des cos² avec des fct
#calcul du carré de la distance d'un individu au center de gravité
d2 <- function(x){return(sum(((x-acp.autos$center)/acp.autos$scale)^2))}
#appliquer Ã  l'ensemble des observations
all.d2 <- apply(ind.illus, 1, d2)
ind.illus
#cosinus^2 pour une composante
cos2 <- function(x){return(x^2/all.d2)}
#cosinus^2 pour l'ensemble des composantes retenues (les 2 premières)
all.cos2 <- apply(test1[, 1:2], 2, cos2)
print(all.cos2)
all.cos2 = data.frame(all.cos2)


#cumul des cos2 (cos² cumulé)
cumul = t(apply(all.cos2, 1, cumsum))
cumul
cumul = data.frame(cumul)

row = row.names(cumul)[which(cumul$Comp.2<0.90)]

#*** projection des individus actifs ET illustratifs dans le premier plan factoriel ***
# projection des individus supplementaire sur le plan 
plot(c(test1[,1],ind.coord[,1]), c(test1[,2],ind.coord[,2]),
     xlim = c(-6,6), type = "n", xlab = "Comp.1", ylab = "Comp.2")
abline(h = 0, v = 0, lty = 2)
text(test1[,1],test1[,2], labels = rownames(autos.actifs), cex = 0.5)
text(ind.coord[,1],ind.coord[,2], labels = rownames(ind.illus), cex = 0.5, col = "red")

#fin
