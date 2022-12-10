library(readr)
Raisin <- read_csv("Raisin.csv")

# checking  for null values in data set
apply(Raisin, 2, function(col)sum(is.na(col))) # null values in Eccentricity and Class only

# checking for normality of continuous data and skewness
shapiro.test(Raisin$Eccentricity)$p.value # p value << 0.05, not normally distributed
boxplot(Raisin$Eccentricity) # too many outlines

# conclusion: impute with median
Raisin$Eccentricity[is.na(Raisin$Eccentricity)] <- median(Raisin$Eccentricity, na.rm = T)

# checking the Class data
library(dplyr)
print(glimpse(Raisin$Class)) # categorical string typed binary data

# Label encoding to (0 - 1)
factors <- factor(Raisin$Class) # factor assign (1 - 2)
Raisin$Class <- as.numeric(factors) - 1 # assign new data as (0 - 1)

# imputing with vim's KNN
library(VIM)
Raisin = kNN(Raisin)[1:8]
View(Raisin)

# correlation matrix
cormat <- round(cor(Raisin),2)
head(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

lower_tri <- get_lower_tri(cormat)

# Heat map
library(reshape2)
melted_cormat <- melt(lower_tri, na.rm = TRUE) # reshape corr matrix

# plot
library(ggplot2)
# install.packages("ggheatmap")
library(ggheatmap) # gg version of the heat map to add annotations

# generate heat map
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# add annotation and plot heat map
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(1, 0.75),
    legend.direction = "vertical")+
  guides(fill = guide_colorbar(barwidth = 1, barheight = 8,
                               title.position = "top", title.hjust = 0.5))

# extract highly correlated variables
threshold <- 0.7
cc <- round(cor(Raisin),2)
w <- which(abs(cc)> threshold & row(cc) < col(cc), arr.ind=TRUE)

# reconstruct names from positions
high_cor <- matrix(colnames(cc)[w],ncol=2)
high_cor

# unique variables
cor_vars <- unique(high_cor[,1])
cor_vars

# new, clean data set
clean_set <- Raisin[, !names(Raisin) %in% cor_vars]

# split data
X <- clean_set[, names(clean_set) != "Class"]
y <- clean_set$Class

# normalize with min max method
# install.packages('caret')
library(caret)
process <- preProcess(as.data.frame(X), method=c("range"))
X <- predict(process, as.data.frame(X))

# merging data
clean_set <- X
clean_set$Class <- y

# pair plot
# install.packages("GGally")
library(GGally)
ggpairs(X, mapping = ggplot2::aes(color = as.character(y)))

######################### Modeling ######################### 

# install.packages("caTools")    # For Logistic regression
# install.packages("ROCR")       # For ROC curve to evaluate model
library(caTools)
library(ROCR)

# Splitting data set
split <- sample.split(clean_set, SplitRatio = 0.7)
split

X_y_train <- subset(clean_set, split == "TRUE")
X_y_test <- subset(clean_set, split == "FALSE")

# Training model
logistic_model <- glm(Class ~ Eccentricity + Extent + Perimeter, 
                      data = X_y_train, 
                      family = "binomial",
                      method = "glm.fit")
logistic_model

# Summary
summary(logistic_model)


# Predict test data based on model
y_pred <- predict(logistic_model, X_y_test, type = "response")

# Changing probabilities
y_pred <- ifelse(y_pred >0.5, 1, 0)

# Evaluating model accuracy using confusion matrix
table(X_y_test$Class, y_pred)

missing_classerr <- mean(y_pred != X_y_test$Class)
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC Curve
ROCPred <- prediction(y_pred, X_y_test$Class) 
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











