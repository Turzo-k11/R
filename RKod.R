# Läs in data
data <- read.csv("Bilar.csv")

# Översikt av data
str(data)
summary(data)
head(data)


# Histogram
par(mfrow=c(1,2))  # Arrangera plottar i en rad med två kolumner
hist(data$Pris, main="Histogram av Pris", xlab="Pris")
hist(data$Miltal, main="Histogram av Miltal", xlab="Miltal")

# Samband mellan variabler med scatterplots
plot(data$Miltal, data$Pris, main="Samband mellan Miltal och Pris"
     , xlab="Miltal", ylab="Pris")

# Skapa linjär regressionsmodell
lm_model <- lm(Pris ~ Miltal + Modell, data=data)

# Sammanfattning av modellen
summary(lm_model)

# Plot av outliers och icke-linjära samband
plot(lm_model)

# Beräkna RMSE och R-squared
predicted <- predict(lm_model, newdata=data)
rmse <- sqrt(mean((data$Pris - predicted)^2))
rsquared <- summary(lm_model)$r.squared

# Resultat RMSE och R-squared
print(paste("RMSE:", rmse))
print(paste("R-squared:", rsquared))

# Skapa random forest-modell
rf_model <- randomForest(Pris ~ Miltal + Modell, data=data)

# Visa sammanfattning av modellen
print(rf_model)

# Plotta viktigheten av variablerna i modellen
varImpPlot(rf_model)

# Beräkna RMSE för random forest-modellen
predictions <- predict(rf_model, newdata = data)
rmse_rf <- sqrt(mean((data$Pris - predictions)^2))

# Visa RMSE för random forest-modellen
print(paste("RMSE för random forest-modellen är:", rmse_rf))







# Läs in nödvändiga bibliotek
library(randomForest)
library(caret)

data <- read.csv("Bilar.csv")

# Dela upp datan i tain och test
set.seed(123) # För att få reproducibla resultat
train_index <- createDataPartition(data$Pris, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

#Skapa och utvärdera linjär regressionsmodell med korsvalidering
cv_lm <- train(Pris ~ Miltal + Modell, data = train_data, method = "lm", trControl = trainControl(method = "cv", number = 5))
print(cv_lm)

#linjär regressionsmodell
model <- lm(Pris ~ Miltal + Modell, data = data)
summary(model)


#Skapa och träna en random forest-modell
rf_model <- randomForest(Pris ~ Miltal + Modell, data = train_data)
print(rf_model)

# Random forest-modellen
cv_rf <- train(Pris ~ Miltal + Modell, data = train_data, method = "rf", trControl = trainControl(method = "cv", number = 5))
print(cv_rf)

#Mdellernas prestanda på test
lm_prediktion <- predict(lm_model, newdata = test_data)
lm_rmse <- sqrt(mean((test_data$Pris - lm_prediktion)^2))

re_prediktion<- predict(rf_model, newdata = test_data)
rf_rmse <- sqrt(mean((test_data$Pris - re_prediktion)^2))

print(paste("RMSE för linjär regression:", lm_rmse))
print(paste("RMSE för random forest:", rf_rmse))

#Utforska feature importance för random forest-modellen
varImpPlot(rf_model)

















