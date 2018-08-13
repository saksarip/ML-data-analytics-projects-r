uFilipinoFamilyData <- FilipinoFamilyData
colnames <- FilipinoFamilyData[1, ]
uFilipinoFamilyData <- uFilipinoFamilyData[-1, ]
colnames(uFilipinoFamilyData) <- colnames
View(uFilipinoFamilyData)
View(uFilipinoFamilyData)
FilipinoFamily
binaryIncome <- as.numeric(uFilipinoFamilyData[,1] >= 450000)
uFilipinoFamilyData[,1] <- as.numeric(uFilipinoFamilyData[, 2] >= 600000)

usableFilipinoData <- cbind(usableFilipinoData, as.numeric(usableFilipinoData$MedicalCareExpenditure >= 15288))

summary(l)

View(usableFilipinoData)

quantile(usableFilipinoData$`Medical Care Expenditure`, c(0.9))

usableFilipinoData <- uFilipinoFamilyData[, -c(2,4)]
View(usableFilipinoData)
usableFilipinoData <- usableFilipinoData[, 1:23]
usableFilipinoData[, 1]
class(usableFilipinoData)
usableFilipinoData[, 26] <- as.numeric(usableFilipinoData[, 26] == "Male")


usableFilipinoData <- sapply(usableFilipinoData, as.numeric)

class(usableFilipinoData)
usableFilipinoData <- usableFilipinoData[, 1:30]

usableFilipinoData <- as.data.frame(usableFilipinoData)

sum(is.na(usableFilipinoData))

split <- sample.split(usableFilipinoData[, 2], SplitRatio = 0.65)
fTrainSet <- subset(usableFilipinoData, split == TRUE)
fTestSet <- subset(usableFilipinoData, split == FALSE)

fLabels <- fTrainSet$




length(fTrainSet$`Total Household Income`)
View(fTestSet)

class(usableFilipinoData)

length(fTrainSet)

class(uFilipinoFamilyData$`Total Food Expenditure`)

ex <- gsub(" ", "", colnames(usableFilipinoData))

colnames(usableFilipinoData) <- ex

length(fTrainSet$`Total Household Income`)

colnames(usableFilipinoData) <- gsub(",", "", colnames(usableFilipinoData))
colnames(usableFilipinoData)[24] <- "binaryMedicalCosts"

sum(uFilipinoFamilyData$`Total Household Income` >= 600000)
usableFilipinoData$

usableFilipinoData$binaryIncome <- usableFilipinoData$binaryIncome - 1
colnames(usableFilipinoData)

class(usableFilipinoData$TotalFoodExpenditure)

usableFilipinoData[, 1] <- as.factor(usableFilipinoData$binaryIncome)
View(usableFilipinoData)
usableFilipinoData$binaryIncome
usableFilipinoData <- usableFilipinoData[, -3]

View(uFilipinoFamilyData)




plot(nbtable)
summary(fModel)[3]

glm
fTrainSet$binaryIncome
colnames(usableFilipinoData)
usableFilipinoData$binaryIncome <- as.numeric(usableFilipinoData$binaryIncome)
class(fTrainSet$`Agricultural Household indicator`)
fModel <- glm(binaryMedicalCosts ~ TotalHouseholdIncome + TotalFoodExpenditure + RestaurantandhotelsExpenditure + ClothingFootwearandOtherWearExpenditure + HousingandwaterExpenditure + TransportationExpenditure + EducationExpenditure, family = binomial(), fTrainSet)


quantile(usableFilipinoData$TotalHouseholdIncome, 0.5)

binaryFilipinoFrame <- data.frame(1:41544)

colnames(binaryFilipinoFrame) <- rev(colnames(usableFilipinoData))

View(binaryFilipinoFrame)

View(binaryFilipinoFrame)

binaryFilipinoFrame <- cbind(binaryFilipinoFrame, as.numeric(uFilipinoFamilyData[, 46]))
View(binaryFilipinoFrame)

colnames(binaryFilipinoFrame)[24:25] <- c("AgriculturalIndicator", "Electricity")
max(binaryFilipinoFrame$AgriculturalIndicator)
binaryFilipinoFrame$AgriculturalIndicator <- as.numeric(binaryFilipinoFrame$AgriculturalIndicator >= 1)
max(binaryFilipinoFrame$Electricity)

binaryFilipinoFrame <- binaryFilipinoFrame[, -length(binaryFilipinoFrame)]
length(binaryFilipinoFrame)
sum(binaryFilipinoFrame[, 1])
print(colSums(binaryFilipinoFrame))
length()

nbpr <- prediction(nbPredictions, nTestSet$binaryMedicalCosts)
nbprf <- performance(nbpr, measure = "tpr", x.measure = "fpr")
plot(nbprf, col = "blue", main = "Receiver Operating Curve")


library(pROC)

nbroc <- roc(nbPredictions, nTestSet$binaryMedicalCosts)
auc(nbroc)



colnames(binaryFilipinoFrame) <- rev(colnames(usableFilipinoData))

print
sum(usableFilipinoData[, 23] > quantile(usableFilipinoData[, 23], 0.5))
usableFilipinoData
View(usableFilipinoData)
binaryFilipinoFrame <- binaryFilipinoFrame[, -length(binaryFilipinoFrame)]
binaryFilipinoFrame <- binaryFilipinoFrame[, -length(binaryFilipinoFrame)]
for(i in 1 : length(usableFilipinoData)){
  
  q <- quantile(usableFilipinoData[, i], 0.5)
  
  f <- as.numeric(usableFilipinoData[, i] > q)
  print(sum(f))
  
  binaryFilipinoFrame <- cbind(f, binaryFilipinoFrame)
  
  

  
}


nsplit <- sample.split(binaryFilipinoFrame$binaryMedicalCosts, SplitRatio = 0.75)
nTrainSet <- subset(binaryFilipinoFrame, nsplit == TRUE)
nTestSet <- subset(binaryFilipinoFrame, nsplit == FALSE)

nTrainSet[, 1] <- as.factor(nTrainSet$binaryMedicalCosts)
nTestSet[, 1] <- as.factor(nTestSet$binaryMedicalCosts)

library(e1071)
colnames(binaryFilipinoFrame)
binaryFilipinoFrame[, 1] <- usableFilipinoData$binaryMedicalCosts

nb <- naiveBayes(binaryMedicalCosts ~ ., data = nTrainSet)
nbprediction <- predict(nb, nTestSet, type = "raw")
class(nbprediction)
nbtable <- table(nbPredictions, nTestSet$binaryMedicalCosts)
View(nbprediction)

nbaccuracy <- (nbtable[1,1] + nbtable[2,2]) / (nbtable[1,1] + nbtable[1,2] + nbtable[2,1] + nbtable[2,2])
nbspecificity <- (nbtable[1,1]) / (nbtable[1,1] + nbtable[1,2])
nbsensitivity <- (nbtable[2,2]) / (nbtable[2,2] + nbtable[1,2])




length(nbPredictions)
length(nTestSet$binaryMedicalCosts)
class(formatted$`0`)
View(formatted)
formatted <- as.data.frame(nbprediction)
nbPredictions <- 1:length(formatted$`0`)
for(i in 1:length(formatted$`0`)){
  
  if(formatted[i, 1] > formatted[i, 2]){
    nbPredictions[i] <- 0
    
    
  }else{
    
    nbPredictions[i] <- 1
    
  }
  
  
  
}
View(nbPredictions)

View(nbprediction)
length(binaryFilipinoFrame$binaryMedicalCosts)
length(nbprediction)
nb$tables
class(nb$tables)

fPrediction <- predict(fModel, fTestSet, type = "response")
binaryMedicalCosts <- as.numeric(usableFilipinoData$MedicalCareExpenditure >= 15288)
usableFilipinoData <- cbind(usableFilipinoData, binaryMedicalCosts)

logTable <- table(fTestSet$binaryMedicalCosts, fPrediction >= 0.5)

plot(log10(usableFilipinoData$TotalHouseholdIncome), log10(usableFilipinoData$TotalFoodExpenditure))
cor(log10(usableFilipinoData$TotalHouseholdIncome), log10(usableFilipinoData$TotalFoodExpenditure))
cor(x, y)
quantile(usableFilipinoData$MedicalCareExpenditure, prob = seq(0, 1, length = 11), type = 5)
summary(fModel)

x <- log10(usableFilipinoData$TotalHouseholdIncome)[-sg]
y <- log10(usableFilipinoData$MedicalCareExpenditure)[-sg]

sg <- which(log10(usableFilipinoData$MedicalCareExpenditure) == -Inf)

sum(log10(usableFilipinoData$MedicalCareExpenditure) == -Inf)

sum(is.na(usableFilipinoData))


prediction <- predict()
fTrainSet$binaryIncome
class(fTrainSet$binaryIncome)
fPrediction <- predict(fModel, lTestSet, type = "response")
predicted <- as.numeric(fPrediction >= 0.5)

length(predicted) == length(fTestSet$binaryMedicalCosts)

library(ggplot2)

logisticModel <- data.frame(fTestSet$binaryMedicalCosts, predicted)
colnames(logisticModel) <- c("Actual", "Predicted")

library(plotROC)

ggplot(logisticModel, aes(d = Predicted, m = Actual)) + geom_roc()


accuracy <- (logTable[1,1] + logTable[2,2]) / (logTable[1,1] + logTable[1,2] + logTable[2,1] + logTable[2,2])
sensitivity <- (logTable[1,1]) / (logTable[1,1] + logTable[2, 1])
specificity <- (logTable[2,2]) / (logTable[2,2] + logTable[1,2])




library(caret)


logCm <- confusionMatrix()


sensitivity(fTestSet$binaryMedicalCosts, fPrediction >= 0.5)





performance(pr, "f")

sum(uFilipinoFamilyData[,1] >= 450000)


sum(is.na(uFilipinoFamilyData$`Total Household Income`))


uFilipinoFamilyData[,1] <- as.numeric(uFilipinoFamilyData[,1])



class(uFilipinoFamilyData$`Total Household Income`)





ncol(FilipinoFamilyData)

sum(is.na(FilipinoFamilyData))

mexample <- mean(1,5,9)

summary


finalPrediction <- (nbprediction[, 2] * 0.2) + (fPrediction * 0.1) + (kprediction * 0.7)
binaryFinalPrediction <- as.numeric(finalPrediction >= 0.5)

maxAccuracy <- 0
accuracyScales <- c()


finalTable <- table(binaryFinalPrediction, fkTestSet$V23)
finalAccuracy <- (finalTable[1,1] + finalTable[2,2]) / (finalTable[1,1] + finalTable[1,2] + finalTable[2,1] + finalTable[2,2]) 

class(nbprediction[, 2])
class(fPrediction)
class(kprediction)

for(i in 1:10){
  for(k in 1:10){
    for(g in 1:10){
      
      if(((0.1 * i) + (0.1 * k) + (0.1 * g)) == 1){
      
      weightedPrediction <- (nbprediction[, 2] * 0.1 * i) + (fPrediction * 0.1 * k) + (kprediction * 0.1 * g)
      weightedTable <- table(as.numeric(weightedPrediction >= 0.5), fkTestSet$V23)
      weightedAccuracy <- sensitivy(weightedTable)
      
      if(weightedAccuracy > maxAccuracy){
        maxAccuracy <- weightedAccuracy
        accuracyScales <- c(i,k,g)
        
        
        
        
      }
      
      
      }
      
      
      
      
    }
    
    
    
    
    
  }
  
  
  
  
  
}
logmo

summary(logisticModel)

accuracy <- function(table){
  return((table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]))
  
  
  
}

sensitivy <- function(table){
  
  return((table[2,2]) / (table[1,2] + table[2,2]))
  
  
  
}
sensitivy(nbtable)

specificity <- function(table){
  
  
  return((table[1,1]) / (table[1,1] + table[1,2]))
  
  
}

F1 <- function(table){
  
  return(((2 * sensitivy(table) * specificity(table)) / (sensitivy(table) + specificity(table))))
  
  
  
}

class(temp[9])
class(summary(temp)[9])


class(summary(temp)[9])
lm

ex <- data.frame(1:10, 10:19)
colnames(ex) <- c("One", "Two")
ex$
temp <- lm(Two ~ One , data = ex)
as.numeric(summary(temp)[9])
c(1:2, 2:3)

class(log10(1:5)[-which(log10(1:5) == -Inf)])

ld <- lm(log10(4:8) ~ log10(1:5))
summary(ld)[9]


logR2(1:5, 4:8)
log10(1:5)






logR2 <- function(x,y){
  
  
  logYI <- log10(y)
  logXI <- log10(x)
  
  tempInfY <- which(logYI == -Inf | logYI == Inf)
  tempInfX <- which(logXI == -Inf | logXI == Inf)
  
  
  
  
  tempCombined <- unique(c(tempInfX, tempInfY))
  
  finalY <- logYI[-tempCombined]
  finalX <- logXI[-tempCombined]
  
  tempFrame <- data.frame(finalX, finalY)
  colnames(tempFrame) <- c("X", "Y")
  
  
  tempLinearModel <- lm(finalY ~ finalX)
  
  return(as.numeric(summary(tempLinearModel)[9]))
  
  
  
  
  
  
  
}





