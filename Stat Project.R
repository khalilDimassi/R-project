# Tache 1: Importation des données

library(readr)                              # pour read_csv
Raisin <- read_csv("Raisin.csv")        # consider les cellules vides comme NA
View(Raisin)

# Tache 2: Pretraitement de données

# 1/ Valeurs aberrantes
summary(Raisin)                             # analyse des variables de la base
boxplot(Raisin[, names(Raisin) != "Class"]) # analyse graphic des valeurs aberrantes

# conclusion: 
# Tous les variables ne sont pas normalement distribuées 


# localiser les valeurs aberrantes
outliers_Area <- which(Raisin$Area %in% boxplot.stats(Raisin$Area)$out)
outliers_MajorAxisLength <- which(Raisin$MajorAxisLength %in% boxplot.stats(Raisin$MajorAxisLength)$out)
outliers_MinorAxisLength <- which(Raisin$MinorAxisLength %in% boxplot.stats(Raisin$MinorAxisLength)$out)
outliers_Eccentricity <- which(Raisin$Eccentricity %in% boxplot.stats(Raisin$Eccentricity)$out)
outliers_ConvexArea <- which(Raisin$ConvexArea %in% boxplot.stats(Raisin$ConvexArea)$out)
outliers_Extent <- which(Raisin$Extent %in% boxplot.stats(Raisin$Extent)$out)
outliers_Perimeter <- which(Raisin$Perimeter %in% boxplot.stats(Raisin$Perimeter)$out)

# imputation:
Raisin$Area[outliers_Area] <- median(Raisin$Area)
Raisin$Area[outliers_MajorAxisLength] <- median(Raisin$MajorAxisLength)
Raisin$Area[outliers_MinorAxisLength] <- median(Raisin$MinorAxisLength)
Raisin$Area[outliers_Eccentricity] <- median(Raisin$Eccentricity)
Raisin$Area[outliers_ConvexArea] <- median(Raisin$ConvexArea)
Raisin$Area[outliers_Extent] <- median(Raisin$Extent)
Raisin$Area[outliers_Perimeter] <- median(Raisin$Perimeter)

# 2/ Valeurs manquantes
sum(is.na(Raisin)) * 100 / prod(dim(Raisin)) # 2.40%

# drop na
Raisin <- na.omit(Raisin) # Taux: 2.40% < 5% alors on les suprime


# Tache 3 : Analyse univaríee
shap_test <- lapply(Raisin[, names(Raisin) != "Class"], shapiro.test)
shap_test                                   # tous les p-values << 0.05


# Label encoding of Class column to (0 - 1)
factors <- factor(Raisin$Class) # factor assign (1 - 2)
Raisin$Class <- as.numeric(factors) - 1 # assign new data as (0 - 1)

# test de modalité

table(Raisin$Class) # 0: 313, 1: 360
plot(table(Raisin$Class)) # plot

Besni = Raisin$Class[Raisin$Class == 0]
Kecimen = Raisin$Class[Raisin$Class == 1]

wilcox.test(Besni, Kecimen) # on Accept H1: Les Deux variables sont pas homogene

# Tache 4 : Analyse bivaríee:

# pair plot
plot(Raisin, col= 'blue')

sp1=cor(Raisin$Area,Raisin$MajorAxisLength,method="spearman")
sp1 #tres forte correlation

sp2=cor(Raisin$Area,Raisin$MinorAxisLength,method="spearman")
sp2#tres forte correlation

sp3=cor(Raisin$Area,Raisin$Eccentricity,method="spearman")
sp3#relation faible

sp4=cor(Raisin$Area,Raisin$ConvexArea,method="spearman")
sp4#tes forte 0,99

sp5=cor(Raisin$Area,Raisin$Extent,method="spearman")
sp5# faible presque nulle

sp6=cor(Raisin$Area,Raisin$Perimeter,method="spearman")
sp6#forte

sp7=cor(Raisin$MajorAxisLength,Raisin$MinorAxisLength,method="spearman")
sp7#forte

sp8=cor(Raisin$MajorAxisLength,Raisin$Eccentricity,method="spearman")
sp8#forte

sp9=cor(Raisin$MajorAxisLength,Raisin$ConvexArea,method="spearman")
sp9# tres forte

sp10=cor(Raisin$MajorAxisLength,Raisin$Extent,method="spearman")
sp10#faible negative presque nulle 

sp11=cor(Raisin$MajorAxisLength,Raisin$Perimeter,method="spearman")
sp11#tres forte

sp12=cor(Raisin$MinorAxisLength,Raisin$Perimeter,method="spearman")
sp12# forte

sp13=cor(Raisin$MinorAxisLength,Raisin$Eccentricity,method="spearman")
sp13#faible

sp14=cor(Raisin$MinorAxisLength,Raisin$ConvexArea,method="spearman")
sp14# tres forte

sp15=cor(Raisin$MinorAxisLength,Raisin$Extent,method="spearman")
sp15#faible

sp16=cor(Raisin$Eccentricity,Raisin$Extent,method="spearman")
sp16#faible negative 

sp17=cor(Raisin$Eccentricity,Raisin$ConvexArea,method="spearman")
sp17#faible

sp18=cor(Raisin$Eccentricity,Raisin$Perimeter,method="spearman")
sp18#forte

sp19=cor(Raisin$ConvexArea,Raisin$Perimeter,method="spearman")
sp19#tres forte

sp20=cor(Raisin$ConvexArea,Raisin$Extent,method="spearman")
sp20#null(-0.004)

sp21=cor(Raisin$Extent,Raisin$Perimeter,method="spearman")
sp21#null(-0.07)


# Tache 5 : Regression lińeaire :

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
library(broom)

set.seed(42) # for consistant testing

# splitting data to training set / testing set
split = sample.split(Raisin$MinorAxisLength, SplitRatio = 0.7)

training_set = subset(Raisin, split == TRUE)
test_set = subset(Raisin, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = MinorAxisLength ~ .,
               data = training_set)

# Predicting the Test set results
y_preds = predict(regressor, newdata = test_set)

# assessing model with basic R summery() method
summary(regressor) # Adjusted R-squared:  0.9704 ;; p-value: < 2.2e-16

# assessing model with Broom's glance() method
glance(regressor) # AIC = 3385;  BIC = 3422

# the 4 plots
# install.packages("ggfortify")
library(ggfortify)
autoplot(regressor) # d'apres le plot Scale-Location: la nuage des points n'est 
                    # pas bien répartie de maniere symétrique.
                    # on conclue que l'hypothese d'homogénité n'est pas accepté 


shapiro.test(residuals(regressor)) # p-value = 7.251e-15; normalité non accepté

library(caret)
library(tictoc)

# Repeated CV: hyperparameter tuning.
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 5)

tic()
set.seed(42)
lm_model <- train(MinorAxisLength ~ ., data = training_set, method = "lm", trControl = fitControl, verbose = FALSE)
toc()

summary(lm_model) # F-statistic:  2203 on 7 and 463 DF,  p-value: < 2.2e-16


# PCA selection des variables
# 1- échelonnement des données

Raisin[, names(Raisin) != "Class"] <- scale(Raisin[, names(Raisin) != "Class"])

# 2- créer un objet pca — prcomp
pca <- prcomp(Raisin, center = TRUE, scale.= TRUE)

summary(pca) # variance
pca$sdev ^ 2 # choix sur les parametre > 1

print(pca$rotation) # cercle de corellation
pca %>% biplot(cex = .5)

components <- cbind(MAL = Raisin$MinorAxisLength, pca$x[, 1:2]) %>% as.data.frame() # créer nouvel base de donnée
View(components)

lm_pca <- lm(MAL ~ ., data = components) # entrainer nouveaux model
summary(lm_pca) # R² = 0.9743,  p-value: < 2.2e-16; donc avec deux variable on a trouver R² +- egaux


autoplot(lm_pca)  # d'apres le plot Scale-Location: la nuage des points n'est 
                  # pas bien répartie de maniere symétrique.
                  # on conclue que l'hypothese d'homogénité n'est pas accepté


######################### Modeling - Classification - GLM - Logistic Regression ######################### 

# install.packages("caTools")    # For Logistic regression
# install.packages("ROCR")       # For ROC curve to evaluate model
library(caTools)
library(ROCR)

# Splitting data set
split <- sample.split(Raisin$Class, SplitRatio = 0.7)
split

X_y_train <- subset(Raisin, split == "TRUE")
X_y_test <- subset(Raisin, split == "FALSE")

# Training model
logistic_model <- glm(Class ~ ., 
                      data = X_y_train, 
                      family = "binomial",
                      method = "glm.fit")
logistic_model

# Summary
summary(logistic_model)


# Predict test data based on model
lr_preds <- predict(logistic_model, X_y_test, type = "response")

# Changing probabilities
lr_preds <- ifelse(lr_preds > 0.5, 1, 0) # if pred > 0.5 then assign 1. else 0

# Evaluating model accuracy using confusion matrix
table(X_y_test$Class, lr_preds)

missing_classerr <- mean(lr_preds != X_y_test$Class)
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC Curve
ROCPred <- prediction(lr_preds, X_y_test$Class) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)











