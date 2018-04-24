library(caret); library(ggplot2)
library(e1071)

## Extracting features
# download training data
data <- read.csv(file = 'pml-training.csv')
data_test <- read.csv(file='pml-testing.csv')


# near zero variance
nzv <- nearZeroVar(data,saveMetrics = F)
nzv

data1 <- data[,-nzv]

# there are still many NAs
na_count <-sapply(data1, function(y) sum(sum(is.na(y))))
unique(na_count)
nav <- which(sapply(data1, function(x) any(is.na(x))))

# features
features <- names(data1)[-nav]

data2=data[,features]
str(data2)


data2 <- data2[,-(1:6)]
str(data2)

features <- names(data2)

inTrain <- createDataPartition(y=data2$classe,p=0.7,list = F)
training <- data2[inTrain,]
testing <- data2[-inTrain,]


preObj <- preProcess(training[,-53],method = c('center','scale','pca'),thresh = 0.9)

training2 <- predict(preObj,training[,-53])
testing2 <- predict(preObj,testing[,-53])

training2 <- data.frame(training2,classe = training$classe)
testing2 <- data.frame(testing2,classe = testing$classe)

mod1 <- train(classe~., method='rf',data = training2)
mod2 <- svm(classe~.,data = training2)


## on testing data
pred1 <- predict(mod1,testing2)
pred2 <- predict(mod2,testing2)
#pred3 <- predict(mod3,testing2)

sum(pred1==testing2$classe)/length(pred1)
sum(pred2==testing2$classe)/length(pred2)
#sum(pred3==testing2$classe)/length(pred3)

predDF <- data.frame(pred1,pred2,classe=testing2$classe)
combModFit <- train(classe~.,method='rf',data=predDF)
combPred <- predict(combModFit,predDF)
sum(combPred==testing2$classe)/length(combPred)

table(combPred,testing2$classe)
confusionMatrix(combPred,testing2$classe)

## on training date

pred1 <- predict(mod1,training2)
pred2 <- predict(mod2,training2)

predDF <- data.frame(pred1,pred2,classe=training2$classe)
combModFit <- train(classe~.,method='rf',data=predDF)
combPred <- predict(combModFit,predDF)

##### FIANLE #####

data_test <- data_test[,features[-53]]
data_test2 <- predict(preObj,data_test)

pred1V <- predict(mod1,data_test2)
pred2V <- predict(mod2,data_test2)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)



