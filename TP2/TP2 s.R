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
colnames(illeetvilaine2)[8]<- "DEN"
head(illeetvilaine2)
illeetvilaine2$DEN<- as.numeric(illeetvilaine2$DEN)
#4 On considère qu’une commune est dense si sa densité est supérieure ou égale à 1500 hab/km2, de
#densité intermédiaire si sa densité est comprise dans [300;1500[ hab/km2, peu dense si comprise dans [25;300[ hab/km2 et 
#très peu dense si < 25 hab/km2. Créer une variable CATEGORIECOMMUNE
#de type factor ayant les quatre modalités suivantes : “Commune dense”, “Commune de densité
#intermédiaire”, “Commune peu dense” et “Commune très peu dense”. Indice : utiliser la fonction ifelse 
#combinée avec la fonction factor.


illeetvilaine2$categoriedecommune <- factor(ifelse(illeetvilaine2$DEN>=1500,"dense",
                                                  ifelse(illeetvilaine2$DEN>=300,"densite intermediaire",
                                                         ifelse(illeetvilaine2$DEN>=25,"faible densite",
                                                                ifelse(illeetvilaine2$DEN>=0,"tres peu dense")))))

head(illeetvilaine2)

#5 La fonction within() permet d’éviter de toujours répéter le nom d’une table pour accéder aux variables.
#Vous avez pu constater que le nom de la table était souvent appelé dans le code, dès lors que l’on créait
#une nouvelle variable. Essayer de faire la question précédente, en utilisant la fonction within(). La
#logique reste la même (utilisation de la fonction factor avec ifelse).Vous effectuerez ce traitement dans
#une nouvelle table qui s’appellera illetvilaine2.


illeetvilaine3 <- within(illeetvilaine2, factor(ifelse(DEN>=1500,"dense",
                                                         ifelse(DEN>=300,"densite intermediaire",
                                                                ifelse(DEN>=25,"faible densite",
                                                                       ifelse(DEN>=0,"tres peu dense"))))))
summary(illeetvilaine3)
head(illeetvilaine3)
# 6 A l’aide de la fonction setdiff(), assurez-vous que les tables illeetvilaine et illeetvilaine2 sont les
#mêmes
setdiff(illeetvilaine3,illeetvilaine2) #nornalement n'affiche rien probleme elle affiche quelque chose dc la table 2 et 3 sont differentes je ne comprend pas 2 OK

#7 Combien y a t’il de communes pour chacune des modalités de cette nouvelle variable ? Faites un
#diagramme en baton ayant en abscisse les modalités et en ordonnée les effectifs

table(illeetvilaine2$categoriedecommune)
plot(table(illeetvilaine2$categoriedecommune))

#8 Afficher maintenant non pas les effectifs mais les pourcentages pour chacune des modalités. On utilisera
#pour cela la fonction prop.table(). Cette fonction prend en paramètre d’entrée un tableau d’effectif
#tel que construit dans la question précédente.

prop.table(table(illeetvilaine2$categoriedecommune))
