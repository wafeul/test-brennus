library(FactoMineR)

donnees = read.csv("anon_offer_lines.csv")
description = read.csv("description_anonymous_data.csv", sep =";", colClasses = c("character","character"))

# pour afficher les descriptions des variables
for(i in 1:ncol(donnees)){
	if(class(donnees[,i]) == "integer" || class(donnees[,i]) == "numeric"){
		if(description[i,2] == "null"){
			print(names(donnees)[i])
		}else{
			print(description[i,2] )
		}
	}
}


# Analyse en Composante Principale
library("FactoMineR")
donnees.quant = donnees[,c(2,3,14,16,17,22,23)]
res.pca = PCA(donnees.quant, quali.sup=1, graph = F)
res.pca$var$contrib
barplot(res.pca$eig[,2])
x11()
plot(res.pca, axes = c(1,2), choix = "var")
x11()
plot(res.pca, axes = c(1,3), choix = "var")
x11()
plot(res.pca, axes = c(2,3), choix = "var")
x11()
plot(res.pca, axes = c(1,2), habillage = 1, col.hab=c("green","red"),label = "none", xlim = c(-50,50), ylim = c(0,50))
x11()
plot(res.pca, axes = c(1,3), habillage = 1, col.hab=c("green","red"),label = "none")
x11()
plot(res.pca, axes = c(2,3), habillage = 1, col.hab=c("green","red"),label = "none")

#barplot : règle de Kaiser : 2 voir 3 composantes. 70 -> 3 donc plutot 3
#dimension 1 : très forte corr entre lineic weight et ca_p_std_per_km -> dans ttes les dims -> surapprentissage
#dimension 2 : prix de la transaction : les gros clients achètent quasi-toujours
#dimension 3 : idem 2 mais sur les quantités de materiel acheté


#SVM
library(caret)
#UNE SUITE DE DIMINUTION DES LIGNES AVEC TESTS POUR FAIRE TOURNER LE SVM (trop long sinon)
for (i in 1:nrow(donnees.quant)){
	if(i %% 10 == 0) {
		donnees.quant2 = rbind(donnees.quant2,donnees.quant[i,])
	}
}
for (i in 1:nrow(donnees.quant2)){
	if(i %% 10 == 0) {
		donnees.quant3 = rbind(donnees.quant3,donnees.quant2[i,])
	}
}
for (i in 1:nrow(donnees.quant3)){
	if(i %% 10 == 0) {
		donnees.quant4 = rbind(donnees.quant4,donnees.quant3[i,])
	}
}
for (i in 1:nrow(donnees.quant4)){
	if(i %% 10 == 0) {
		donnees.quant5 = rbind(donnees.quant5,donnees.quant4[i,])
	}
}
donnees.quant6 = donnees.quant5[1:50,]

set.seed(1000)
#donnees.quant6 sont 50 premières lignes de donnees.quant
intrain <- createDataPartition(y = donnees.quant6[,1], p = 0.7, list = F)
training = donnees.quant6[intrain,]
testing = donnees.quant6[-intrain,]
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svmPoly = train(accepted~., data = training, method = "svmPoly", trControl = trctrl, preProcess = c("center","scale"), tuneLength = 10, na.action = na.omit)
testing = testing[c(2:10,13,14),]
test_pred = predict(svmPoly, newdata=testing)
svmPoly
plot(svmPoly)
confusionMatrix(test_pred, testing$accepted)










#Question 2
mat.prem = read.table("Brennus_data_avril_2016_matieres_premieres.txt", sep="\t", head = T)
aluminium = mat.prem[57:84,]
cuivre = mat.prem[1:56,]
lims=c()
y_dates=dates$creation_date[1]
for(i in 2:nrow(dates)){
	if(dates$creation_date[i-1] != dates$creation_date[i]){
		lims = c(lims, i)
		y_dates = c(y_dates,dates$creation_date[i-1])
	}
}
lims=c(1,lims)

accept.al= c(rep(0,41))
refuse.al = c(rep(0,41))
accept.cu= c(rep(0,41))
refuse.cu = c(rep(0,41))
for(i in 1:40){
	for(j in lims[i]:lims[i+1]){
		if(dates$accepted[j] == "True"){
			if(dates$metal[j] == "Al")
				{accept.al[i] = accept.al[i]+1
				}else{
				accept.cu[i] = accept.cu[i]+1
				}
			}else{
			if(dates$metal[j] == "Al")
				{refuse.al[i] = refuse.al[i]+1
				} else{
				refuse.cu[i] = refuse.cu[i]+1
			}
		}
	}
}

taux.al = accept.al / (refuse.al + accept.al)
taux.cu = accept.cu / (refuse.cu + accept.cu)

cor(taux.cu[1:28], cuivre$cours[1:28])
cor(taux.al[1:28], aluminium$cours)
names(cuivre) = c("id_matiere", "id_cours",   "mois",       "cours cuivre")
names(aluminium) = c("id_matiere", "id_cours",   "mois",       "cours aluminium")
cuivre[c(1:28),4] = cuivre[c(1:28),4] / max(cuivre[c(1:28),4])
plot(y_dates[1:28],taux.cu[1:28], ylim = 0:1)
lines(cuivreLME[,c(3,4)])



