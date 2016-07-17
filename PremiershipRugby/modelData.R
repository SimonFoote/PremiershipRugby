#Title: Script to load and model data

## Load Packages
if (!require("xlsx"))
    install.packages("xlsx")
if (!require("caret"))
    install.packages("caret")
if (!require("dplyr"))
    install.packages("dplyr")

if (!require("randomForest"))
    install.packages("randomForest")
if (!require("e1071"))
    install.packages("e1071", dependencies = TRUE)

if (!require("Boruta"))
    install.packages("Boruta")

library(xlsx)
library(caret)
library(dplyr)

library(randomForest)
library(Boruta)

## Load Data
trainData <- read.xlsx("PastFixtures.xls", 1)

## Feature Selection
set.seed(123)

boruta.train <- Boruta(Winner ~ Venue + Location + Date + Time + Home + Home1 + Home2 + Home3 + Home4
+ Home5 + Home6 + Home7 + Home8 + Home9 + Home10 + Home11 + Home12 + Home13 + Home14 + Home15
+ Home16 + Home17 + Home18 + Home19 + Home20 + Home21 + Home22 + Home23 + Away + Away1 + Away2
+ Away3 + Away4 + Away5 + Away6 + Away7 + Away8 + Away9 + Away10 + Away11 + Away12 + Away13 +
Away14 + Away15 + Away16 + Away17 + Away18 + Away19 + Away20 + Away21 + Away22 + Away23,
    data = trainData, doTrace = 2)

print(boruta.train)


### Plot Features
plot(boruta.train, xlab = "", xaxt = "n")
lz <- lapply(1:ncol(boruta.train$ImpHistory), function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[, i]), i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz, median))
axis(side = 1, las = 2, labels = names(Labels),
 at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

### Tentative Attributes
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

plot(final.boruta, xlab = "", xaxt = "n")
lz <- lapply(1:ncol(final.boruta$ImpHistory), function(i)
final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[, i]), i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz, median))
axis(side = 1, las = 2, labels = names(Labels),
  at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)

selectedVariables <- getSelectedAttributes(final.boruta, withTentative = F)
selectedVariables <- gsub("`", "", selectedVariables)

trainDataSubset <- trainData[, selectedVariables]
trainData <- cbind(trainDataSubset, trainData$Winner)

names(trainData)[names(trainData) == "trainData$Winner"] <- "Winner"

### Split data
indexes <- sample(1:nrow(trainData), size = 0.2 * nrow(trainData))

# Split data
test <- trainData[indexes,]
###dim(test)
train <- trainData[ - indexes,]
###dim(train)

## Train model
set.seed(378)
trnControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

modelTrain <- train(Winner ~ ., data = train, method = "rf", trControl = trnControl)

plot(modelTrain)

modelPredict <- predict(modelTrain, newdata = test)

confusionMatrix(modelPredict, test$Winner)

mean(modelPredict == test$Winner)

