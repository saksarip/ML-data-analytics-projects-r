
kFinancialData <- Financial_Fraud_Data[1:10000, c(3,5,6)]
kFinancialData <- scale(kFinancialData)
kFinancialData <- cbind(kFinancialData, Financial_Fraud_Data[, 10])

cusableFilipinoData <- usableFilipinoData[, -length(usableFilipinoData)]
cusableFilipinoData <- cusableFilipinoData[, 1:23]
cusableFilipinoData <- cusableFilipinoData[, -3]
cusableFilipinoData <- scale(cusableFilipinoData)
cusableFilipinoData <- cbind(cusableFilipinoData, usableFilipinoData[, length(usableFilipinoData)])
cusableFilipinoData <- as.data.frame(cusableFilipinoData)


max(as.numeric(uFilipinoFamilyData$Electricity))
usableFilipinoData$binaryMedicalCosts
View(cusableFilipinoData)

fEx <- 

  mydata <- cusableFilipinoData
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
  
  
class(kFinancialData)
kFinancialData$V4 <- factor(kFinancialData$V4)

# Elbow method
fviz_nbclust(usableFilipinoData, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

class(Financial_Fraud_Data)


class(kFinancialData)


View(kFinancialData)

sum(is.na(kFinancialData))

0.7 * length(kFinancialData$step)

kTrain <- kFinancialData[1:6500, ]
kTest <- kFinancialData[6501 : 10000, ]
kLabels <- kFinancialData[1 : 6500, 4]
kLabels2 <- kFinancialData[6501 : 10000, 4]


splitf <- sample.split(cusableFilipinoData$V23, SplitRatio = 0.75)
fkTrainSet <- subset(cusableFilipinoData, split == TRUE)
fkTestSet <- subset(cusableFilipinoData, split == FALSE)

fkLabels <- fkTrainSet$V23
fkLabels2 <- fkTestSet$V23


kFinancialData[, 4] <- factor(kFinancialData[, 4])

class(kTrain)


knn == kTest[, 4]


cl = factor(kTrain[, 4])


  
View(data.frame(knn, kTest[, 4]))  
  sum((knn == kTest[, 4]) == FALSE)
  
  class(kTest[ , 4])
  
confusionMatrix(as.factor(knn), kTest[, 4])
sqrt(length(kFinancialData[, 1]))

length(knn)
View(fkTrainSet)


library(class)
library(caret)
library(KODAMA)
library()
fkccnn <- knn1(fkTrainSet, fkTestSet, fkLabels)
.
colnames(fkTrainSet)



fknn <- knn(train = fkTrainSet, test = fkTestSet, cl = fkLabels, k = 10)
fkprobabilities <- knn.probability(train = fkTrainSet, test = fkTestSet, cl = fkLabels, k = 10)

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
colnames(fkTrainSet)

fkcnn <- train(V23 ~., data = fkTrainSet, method = "knn")
kprediction <- predict(fkcnn, fkTestSet)

formatte




levels(fknn)

library(MLmetrics)

logTable


logisticF1 <- F1_Score()



length(knn) == length(kTest[, 4])

length(kLabels)

confusionMatrix(as.factor(knn), as.factor(kLabels2))

library(ROCR)
library(MLmetrics)

pr <- prediction(knd, as.vector(kLabels2))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col = "green")
class(knn)

F1 <- performance(pr, "f")


knd <- as.vector(knn, mode = "numeric")

F1 <- F1_Score(as.vector(kLabels2), knd)

F
pred_knn<-prediction(knn, cl)
kFinancialData[7001:10000, 4] == kTest[, 4]


length(knn)
length(kTest[, 4])

class(kTest)
length(knn) == length(kTest)
