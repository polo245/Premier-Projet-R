#EXERCICE 1 
#On suppose que la taille d’un homme (notée X) suit une loi normale d’esperance 175 et d’écart-type 10
# 1. Calculer a) P(X=<170) b) P(X>190)
pnorm(170,175,10)
1-pnorm(190,175,10)

#2 Calculer P(170<=X<=190)
pnorm(190,175,10)-pnorm(170,175,10)

#3  on cherche x tel que P(X ≤ x) = 0.99
x <- qnorm(0.99,175,10)
print("La probabilité qu’un homme mesure moins de")
x
print("centimètre est de 0.99")
 
#4 Générer un vecteur “simulation” de 500 observations. Les observations suivront la loi de X

simulation <- rnorm(500,175,10)
plot(density(simulation)) #pour voir la simulation graphiquement

#5 A l’aide de la fonction quantile(), donner la valeur de la mediane sur ce vecteur

quantile(simulation)

#EXERCICE 2
#1 Importer la table illeetvilaine. Utilisez l’option StringsAsfactors=FALSE. Faites ensuite un résumé
#du contenu de la table

illeetvilaine <- read.csv2("TP2/illeetvilaine.csv")
summary(illeetvilaine)

#2 Convertir les variables CODGEO et DEP en character

illeetvilaine$CODGEO <- as.character(illeetvilaine$CODGEO)
illeetvilaine$DEP <- as.character(illeetvilaine$DEP)

#3 Créer une variable DENSITE (Population / superficie). Arrondir cette densité à une décimale. A l’aide
#d’une fonction bien choisie, faites un résumé de la distribution de cette variable.

DENSITE <- cbind(illeetvilaine$CODGEO,(illeetvilaine$P15_POPH + illeetvilaine$P15_POPH)/illeetvilaine$SUPERFICIE)
summary(DENSITE)
head(DENSITE)
illeetvilaine2<-merge(illeetvilaine,DENSITE,bx=CODGEO,by=1)

colnames(illeetvilaine2$V2,do.NULL = TRUE, prefix = "col") 
colnames(illeetvilaine2)[2]<- "DEN"
head
#4 On considère qu’une commune est dense si sa densité est supérieure ou égale à 1500 hab/km2, de
#densité intermédiaire si sa densité est comprise dans [300;1500[ hab/km2, peu dense si comprise dans [25;300[ hab/km2 et 
#très peu dense si < 25 hab/km2. Créer une variable CATEGORIECOMMUNE
#de type factor ayant les quatre modalités suivantes : “Commune dense”, “Commune de densité
#intermédiaire”, “Commune peu dense” et “Commune très peu dense”. Indice : utiliser la fonction ifelse 
#combinée avec la fonction factor.


illeetvilaine$categoriedecommune <- factor(ifelse(illeetvilaine$DEN>1500,"tres dense"))



