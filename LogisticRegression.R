
adult <- Adult_Data
View(Financial_Fraud_Data)
library(caTools)
eFinancialFraudData <- Financial_Fraud_Data
length(Financial_Fraud_Data$amount)
eFinancialFraudData <- subset(eFinancialFraudData, !(eFinancialFraudData$newbalanceOrig == 0))
length(eFinancialFraudData$amount)

split <- sample.split(eFinancialFraudData$amount, SplitRatio = 0.7)
train <- subset(eFinancialFraudData, split == TRUE)
test <- subset(eFinancialFraudData, split == FALSE)

model <- glm(isFraud ~ amount + oldbalanceOrg + newbalanceOrig, family = binomial(), train)
logPrediction <- predict(model, test, type = "response")
length(test$step)
length(logPrediction)
table(test, logPrediction)



adult$workclass <- as.character(adult$workclass)

adult$workclass[adult$workclass == "Without-pay" | 
                  adult$workclass == "Never-worked"] <- "Unemployed"

adult$workclass[adult$workclass == "State-gov" |
                  adult$workclass == "Local-gov"] <- "SL-gov"

adult$workclass[adult$workclass == "Self-emp-inc" |
                  adult$workclass == "Self-emp-not-inc"] <- "Self-employed"


adult$marital.status <- as.character(adult$marital.status)

adult$marital.status[adult$marital.status == "Married-AF-spouse" |
                       adult$marital.status == "Married-civ-spouse" |
                       adult$marital.status == "Married-spouse-absent"] <- "Married"

adult$marital.status[adult$marital.status == "Divorced" |
                       adult$marital.status == "Separated" |
                       adult$marital.status == "Widowed"] <- "Not-Married"





adult$native.country <- as.character(adult$native.country)

north.america <- c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                   "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                   "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago",
                   "United-States")
asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
          "Philippines", "Taiwan", "Thailand", "Vietnam")
south.america <- c("Columbia", "Ecuador", "Peru")
europe <- c("England", "France", "Germany", "Greece", "Holand-Netherlands",
            "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
            "Yugoslavia")
other <- c("South", "?")

adult$native.country[adult$native.country %in% north.america] <- "North America"
adult$native.country[adult$native.country %in% asia] <- "Asia"
adult$native.country[adult$native.country %in% south.america] <- "South America"
adult$native.country[adult$native.country %in% europe] <- "Europe"
adult$native.country[adult$native.country %in% other] <- "Other"

adult$native.country <- as.factor(adult$native.country)
adult$marital.status <- as.factor(adult$marital.status)
adult$workclass <- as.factor(adult$workclass)

adult[adult == "?"] <- NA


colnames(adult)

library(Amelia)
library(caTools)

missmap(adult, y.at = 1, y.labels = "", col = c("yellow", "black"), legend = FALSE)

library(ggplot2)
ggplot(adult, aes(age)) + geom_histogram(aes(fill = income), color = "black",
                                         binwidth = 1)

split <- sample.split(adult$income, SplitRatio = 0.7)
train <- subset(adult, split == TRUE)
test <- subset(adult, split == FALSE)





log.model <- glm(income ~ ., family = binomial(), train)
prediction <- predict(log.model, test, type = "response")
table(test$income, prediction >= 0.5)

sum(is.factor(Financial_Fraud_Data))
sum(!(Financial_Fraud_Data == 0))

cFinancialData <- Financial_Fraud_Data[, c(3,5,6, length(Financial_Fraud_Data))]




View(cFinancialData)

cFinanci
eSet <- Financial_Fraud_Data[1:10000, ]

splitted <- sample.split(eSet$amount, SplitRatio = 0.7)
lTrainSet <- subset(eSet, splitted = TRUE)
lTestSet <- subset(eSet, splitted = FALSE)

summary(fModel)

fModel <- glm(isFraud ~ amount + oldbalanceOrg + newbalanceOrig, family = binomial(), lTrainSet)
fPrediction <- predict(fModel, lTestSet, type = "response")

coefficients(fPrediction)

library(ISLR)
library(caret)



summary(fModel)


length(adult$workclass)

logTable <- table(lTestSet$isFraud, fPrediction >= 0.5)

sum(usableFilipinoData$binaryMedicalCosts ==  1) / length(usableFilipinoData$binaryMedicalCosts)

library(pROC)
length(fkTestSet$V23) == length(fknn)
ftable <- table(fkTestSet$V23, fknn)

faccuracy <- (ftable[1,1] + ftable[2,2]) / (ftable[1,1] + ftable[1,2] + ftable[2,1] + ftable[2,2])
fsensitivity <- (ftable[2,2]) / (ftable[2,1] + ftable[2,2])
fspecifity <- ftable[1,1] / (ftable[1,1] + ftable[1,2])

knnmodel <- data.frame(fknn, fkTestSet$V23)
colnames(knnmodel) <- c("Predicted", "Actual")
View(knnmodel)
View(knnmodel)
ggplot(knnmodel, aes(d = Predicted, m = Actual)) + geom_roc()

plot(fknn, print.thres = 0.5, type = "S")


library(e1071)

View(cusableFilipinoData)

library(pROC)
roc <- roc(fTestSet$binaryMedicalCosts, fknn)
auc(roc)

auc(fknn, fTestSet$binaryMedicalCosts)

auc



class(logTable)
logTable[2,2]

library(ROCR)

library(pROC)

library(caTools)

library(measures)

accuracy <- (logTable[1,1] + logTable[2,2]) / (logTable[1,1] + logTable[1,2] + logTable[2,1] + logTable[2,2])
sensitivity <- (logTable[1,1]) / (logTable[1,1] + logTable[1,2])
specificity <- (logTable[2,2]) / (logTable[2,2] + logTable[1,2])


pr <- prediction(fPrediction, fTestSet$binaryMedicalCosts)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col = "blue", main = "Receiver Operating Curve", sub = "Area under the curve: 0.7204")



library(hmeasure)
fkn
h <- HMeasure(fknn, fkTestSet$V23)
plotROC(h, which = 1, bw="nrd0")
pr2 <- prediction(fknn, fkTestSet$binaryMedicalCosts)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col = "blue", main = "Receiver Operating Curve", sub = "Area under the curve: 0.7204")





fF1 <- F1(fTestSet$binaryMedicalCosts, as.numeric(fPrediction >= 0.5))
fPrediction

summary(fPrediction)




auc(fknn, fkTestSet$V23)




auc(as.numeric(fPrediction >= 0.5), fTestSet$binaryMedicalCosts)


auc(fknn, fkTestSet$binaryMedicalCosts)


library(AUC)
auc()

pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)

library(factoextra)
library(NbClust)

kFinancialData <- scale(kFinancialData)


f <- auc(specificity, sensitivity)

auc(lTestSet$isFraud, fPrediction)
0.7 * length(Financial_Fraud_Data$amount)

f_train <- Financial_Fraud_Data[1:4453834, ]
f_test <- Financial_Fraud_Data[4453835 : length(Financial_Fraud_Data[, 1]), ]

length(f_train$amount) == length(f_train_labels$amount)

f_train_labels <- Financial_Fraud_Data[1:4453834, 1]
f_test_labels <- Financial_Fraud_Data[4453835 : length(Financial_Fraud_Data), 1]

f_prediction <- knn(train = f_train, test = f_test,cl = f_train_labels, k=25)



sum(is.na(f_train))


sqrt(length(Financial_Fraud_Data[, 1]))
library(gmodels)


library(class)




