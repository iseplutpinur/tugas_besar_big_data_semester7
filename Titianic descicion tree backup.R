library(dplyr)
library(party)
# clear console
cat("\014")

titanic = read.csv('D:\\Kampus\\big data titanic\\titanic.csv')
titanic$Survived = factor(titanic$Survived, labels=c("died", "survived"))
titanic$Embarked = factor(titanic$Embarked, labels=c("unkown", "Cherbourg", "Queenstown", "Southampton"))

male = titanic[titanic$Sex=="male",]
female = titanic[titanic$Sex=="female",]
par(mfrow = c(1, 2), mar = c(0, 0, 2, 0), oma = c(0, 1, 0, 1))
pie(table(male$Survived),labels=c("Dead","Survived"), main="Survival Portion Among Men")
pie(table(female$Survived),labels=c("Dead","Survived"), main="Survival Portion Among Women")

# Preprocessing ================================================================
# mengatasi missing value dengan mean value
for(i in 1:ncol(titanic)){
  titanic[is.na(titanic[,i]),i]<- mean(titanic[,i],na.rm = TRUE)
}

# ganti tipe data yang character menjadi factor
clean_titanic <- titanic %>% 
  mutate(across(where(is.character), as.factor))


# melatih model ================================================================
# set random
set.seed(54321)
training <- sample(2, nrow(clean_titanic), replace=TRUE, prob = c(0.7,0.3))

trainData <- clean_titanic[training==1,]
testData <- clean_titanic[training==2,]


# buat model ===================================================================
predictor <-Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked
tree <- ctree(predictor, data=trainData)
# plot(tree)

# predict on test data
tree <- ctree(predictor, data=testData)
testPred <- predict(tree, newData=testData)
table(testPred, testData$Survived)

# plot menggunakan rpart
library(rpart)
library(rpart.plot)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = trainData, method = 'class')
rpart.plot(fit, extra = 106)



cm <- table(predict(tree), trainData$Survived)
result_accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
result_precision <- cm[4] / sum(cm[4], cm[2])
result_sensitivity <- cm[4] / sum(cm[4], cm[3])
result_fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
result_specificity <- cm[1] / sum(cm[1], cm[2])
