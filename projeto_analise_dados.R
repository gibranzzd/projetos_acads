#Gibran Deon e Gabriel Fávaro

install.packages('titanic')
install.packages('tidyverse')
install.packages(c('class', 'mlbench', 'chemometrics'))
install.packages('tree')
library(tree)
library(class)
library(mlbench)
library(chemometrics)
library(tidyverse)
library(titanic)

treinamento = titanic_train
teste = titanic_test
head(treinamento)


treinamento_df <- data.frame(treinamento)

NaoMorreram = treinamento_df %>% filter(Survived == 1)
NaoViveram = treinamento_df %>% filter(Survived == 0)

#1
barplot(NaoMorreram$Age,  xlab="pessoas", ylab="idade", main="vasco")

#2
barplot(NaoViveram$Age,  xlab="pessoas", ylab="idade", main="vasco")

#3 e 4

NaoMorreram$Sex <- as.factor(NaoMorreram$Sex)

plot(NaoMorreram$Sex)

NaoViveram$Sex <- as.factor(NaoViveram$Sex)

plot(NaoViveram$Sex)

#Sobreviveram mais mulheres e crianças do que homens

#5

#PassengerId, Name , SibSp, Parch, Ticket, Fare, Cabin, pois não vai ser util na
# NOSSA analise de dados

treinamento_df$Name = NULL
treinamento_df$PassengerId = NULL
treinamento_df$SibSp = NULL
treinamento_df$Parch = NULL
treinamento_df$Ticket = NULL
treinamento_df$Fare = NULL
treinamento_df$Cabin = NULL
treinamento_df$Embarked = NULL

treinamento_df

#6

treinamento_df <-treinamento_df[!is.na(treinamento_df$Age),]
treinamento_df


#7
# Vamos usar o Genero, Idade e Classe

#8

TitanicLog2 = glm(Survived ~ . -Pclass - Sex - Age, data = treinamento_df, family = binomial)
TitanicLog2

summary(TitanicLog2)


predictTrain = predict(TitanicLog2, type = "response")
table(treinamento_df$Survived, predictTrain >= 0.5)


predictTest = predict(TitanicLog2, type = "response", newdata = teste)


teste$Survived = as.numeric(predictTest >= 0.5)
table(teste$Survived)
Predictions = data.frame(teste[c("Age", "Survived")])
write.csv(file = "TitanicPred", x = Predictions)

#9
#Pessimo, morreu todo mundo

#10

data("PimaIndiansDiabetes")

our.big.tree = tree(Survived ~., data=treinamento_df)
summary(our.big.tree)

plot(our.big.tree)
text(our.big.tree)

set.seed(3)
cv.results = cv.tree(our.big.tree, FUN=prune.misclass)
plot(cv.results$size, cv.results$dev, type="b")

pruned.tree = prune.misclass(our.big.tree, best=3)
plot(pruned.tree)
text(pruned.tree)
