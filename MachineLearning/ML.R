# Simple model
library(kernlab)
data("spam")
head(spam)

plot(density(spam$your[spam$type=='spam']),ylim=c(0,4),col='red',
     xlab='Frequency of Your',main='')
lines(density(spam$your[spam$type=='nonspam']),col='blue')
legend(x=6,y=3,legend = c('NonSpam','Spam'),col=c('blue','red'),lty = c(1,1))
abline(v=0.5)

pred <- ifelse(spam$your >0.5,'spam','nonspam')
table(pred,spam$type)/length(spam$type)

# take random 10 massegaes 
set.seed(333)
smallSpam <- spam[sample(dim(spam)[1],size = 10),]
spamLabel <- (smallSpam$type == 'spam')*1 +1
plot(smallSpam$capitalAve,col=spamLabel)

rule1 <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x>2.7] <- 'spam'
  prediction[x<2.4] <- 'nonspam'
  prediction[x>=2.4 & x<= 2.45] <- 'spam'
  prediction[x>2.45 & x<2.7] <- 'nonspam'
  return(prediction)
}

table(rule1(smallSpam$capitalAve),smallSpam$type)

rule2 <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x>2.8] <- 'spam'
  prediction[x<=2.8] <- 'nonspam'
  return(prediction)
}
table(rule2(smallSpam$capitalAve),smallSpam$type)

# apply for all data
T1<-table(rule1(spam$capitalAve),spam$type)
T2<-table(rule2(spam$capitalAve),spam$type)

#precision
T1[1,1]+T1[2,2]
T2[1,1]+T2[2,2]

#####################################
## CARET ##
# simple full cyrcle

library(caret)
library(kernlab)
data("spam")

inTrain <- createDataPartition(y=spam$type,p=0.75,list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
set.seed(32343)
modelFit <- train(type~.,data = training,method='glm')

modelFit
modelFit$finalModel

predictions <- predict(modelFit,newdata=testing)
predictions

confusionMatrix(predictions,testing$type)

## data slicing
# train-test split
library(caret)
library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,p=0.75,list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# K-folds
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=T,returnTrain = T)
sapply(folds,length)

# Resample
folds <- createResample(y=spam$type,times = 10,list = T)
sapply(folds, length)


folds <- createResample(y=spam$type)
sapply(folds, length)

# time slices
tme <- 1:1000
folds <- createTimeSlices(tme,initialWindow = 20,horizon = 10)
names(folds)

folds$train[[1]]
folds$test[[1]]

## Training options

library(caret)
library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,p=0.75,list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

args(train.default)
args(trainControl)

set.seed(1235)
modelFit2 <- train(type~.,data=training,method='glm')
modelFit2

# plotting predictors

library(ISLR); library(ggplot2); library(caret)
data("Wage")
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,p = 0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c('age','education','jobclass')],
            y=training$wage,
            plot = 'pairs')

qplot(age,wage,data = training,colour = jobclass)

qq<-qplot(age,wage,data = training,colour = education)
qq + geom_smooth(method='lm',formula = y~x)

library(Hmisc)

cutWage <- cut2(training$wage,g = 3) 
cutWage
table(cutWage)

p1 <- qplot(cutWage,age,data = training,fill=cutWage,geom = 'boxplot')
p1

p2 <- qplot(cutWage,age,data = training,fill=cutWage,geom = c('boxplot','jitter'))
p2

library(gridExtra)
library(grid)

grid.arrange(p1,p2,ncol=2)


t1 <- table(cutWage,training$jobclass)
t1

prop.table(t1,1)

qplot(wage,colour=education,data=training,geom = 'density')

## Preprocessing

# Standart Scaler
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type, p = 0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve,main='',xlab='ave')
preObj <- preProcess(training[,-58],method = c('center','scale'))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

set.seed(32343)
modelFit <- train(type~.,data = training,
                  preProcess=c('center','scale'),method = 'glm')

# Box-Cox

preObj <- preProcess(training[,-58],method = c('BoxCox'))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

# Imputing Data

set.seed(13343)
training$capAve <- training$capitalAve
selectNA <- rbinom(n=dim(training)[1],size = 1,prob = 0.05)==1
training$capAve[selectNA] <- NA

training$capAve

preObj <- preProcess(training[,-58],method = 'knnImpute')
capAve <- predict(preObj,training[,-58])$capAve

capAveTruth <- training$capitalAve
capAveTruth <-(capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve-capAveTruth)

# covariate (feature) creation

library(kernlab); data(spam)
spam$capitalAveSq <- spam$capitalAve^2

# from qualitative to quantitative
library(ISLR); library(caret); data("Wage")
inTrain <- createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
table(training$jobclass)

dumies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dumies,newdata=training))

# which features with near zero variability
nsv <- nearZeroVar(training,saveMetrics = T)
nsv

# polinomial fiting
library(splines)
bsBasis <- bs(training$age,df=3)
head(bsBasis)

lm1 <- lm(training$wage~bsBasis)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=data.frame(x=training$age)))


head(predict(bsBasis,newdata = data.frame(x=testing$age)))
head(bsBasis)


## PCA

library(caret)
library(kernlab); data(spam)

inTrain <- createDataPartition(y=spam$type,p=0.75,list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0

which(M > 0.8,arr.ind = T)

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

# rotate the plot

X <- sqrt(2)*training$num415 + sqrt(2)*training$num857
Y <- sqrt(2)*training$num415 - sqrt(2)*training$num857

# X contains most variability
plot(X,Y)

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

typeColor <- ((spam$type=='spam')*1 + 1)

prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab='PC1',ylab='PC2')
hist(prComp$sdev)

preProc <- preProcess(log10(spam[,-58]+1),method = 'pca',pcaComp = 2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC$PC1,spamPC$PC2,col=typeColor)

preProc <- preProcess(log10(training[,-58]+1),method = 'pca',pcaComp = 2)
trainPC <- predict(preProc,log10(training[,-58]+1))

newDF <- cbind(training$type,trainPC)
names(newDF) <- c('type','PC1','PC2')
modelFit <- train(type~.,method='glm',data = newDF)

testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

#alternative method
modelFit <- train(type~.,method = 'glm', preProcess='pca',data=training)
confusionMatrix(testing$type,predict(modelFit,testing))


## Regression

library(caret); data("faithful"); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,p=0.5,list = F)

trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]
head(trainFaith)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col='blue')

lm1 <- lm(eruptions ~ waiting,data = trainFaith)
summary(lm1)

lines(trainFaith$waiting,lm1$fitted.values,lwd=3)

lm1$coefficients[1] + lm1$coefficients[2]*80

newdata <- data.frame(waiting=80)
predict(lm1,newdata)


# RMSE

# on train data
sqrt(sum((lm1$fitted.values - trainFaith$eruptions)^2))

# on test data
sqrt(sum((predict(lm1,testFaith) - testFaith$eruptions)^2))


pred1 <- predict(lm1,newdata=testFaith,interval = 'prediction')
ord <- order(testFaith$waiting)

plot(testFaith$waiting,testFaith$eruptions,pch=19,col='blue')
matlines(testFaith$waiting[ord],pred1[ord,])

# the same in carte package
modelFit <- train(eruptions ~ waiting, data = trainFaith,method = 'lm')
summary(modelFit)


## multiple covariates

library(ISLR); library(ggplot2); library(caret)
data("Wage")
Wage <- subset(Wage,select = -logwage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,p=0.7,list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

qplot(age,wage,data = Wage, colour = jobclass)
qplot(age,wage,colour = education,data=Wage)

modFit <- train(wage ~ age + jobclass + education,method = 'lm',data=training)
fitMod <- modFit$finalModel
summary(fitMod)


plot(fitMod,1,pch=19,cex = 0.5)
qplot(fitMod$fitted.values,fitMod$residuals,colour=race,data=training)
plot(fitMod$residuals,pch=19)

pred <- predict(modFit, testing)
qplot(wage,pred,colour = year, data=testing)

### QUIZ 

library(AppliedPredictiveModeling)
data(AlzheimerDisease)

data("concrete")
library(caret)
set.seed(1000)

inTrain = createDataPartition(mixtures$CompressiveStrength,p=3/4)[[1]]
training <- mixtures[inTrain,]
testing <- mixtures[-inTrain,]

library(Hmisc)

qplot(seq_along(CompressiveStrength), CompressiveStrength,data = training,
      colour = cut2(x = FlyAsh,g=2))


set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength,p=3/4)[[1]]
training <- mixtures[inTrain,]
testing <- mixtures[-inTrain,]


hist(log(training$Superplasticizer+1))

set.seed(3433)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis,p=3/4)[[1]]
training <- adData[inTrain,]
testing <- adData[-inTrain,]

namesL <- sapply(names(training), function(x) grepl('^IL',x))
pred_vec <- names(training)[namesL]

procPCA <- preProcess(training[,pred_vec],method = 'pca',thresh = 0.8)
prComp <- predict(procPCA,training[,pred_vec])

var_vec <- apply(prComp,FUN = var, MARGIN = 2)


set.seed(3433)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis,p=3/4)[[1]]
training <- adData[inTrain,]
testing <- adData[-inTrain,]

namesL <- sapply(names(training), function(x) grepl('^IL',x))
pred_vec <- names(training)[namesL]
newDF <- data.frame(diagnosis = training$diagnosis,training[,pred_vec])
    
model1 <- train(diagnosis~.,method='glm',data = newDF)

procPCA <- preProcess(training[,pred_vec],method = 'pca',thresh = 0.8)
prComp <- predict(procPCA,training[,pred_vec])
newDF_pca <- data.frame(diagnosis = training$diagnosis,prComp)
model2 <- train(diagnosis~.,method='glm',data = newDF_pca)    

mod1_pred <- predict(model1,testing)
mod2_pred <- predict(model2,predict(procPCA,testing[,pred_vec]))

confusionMatrix(testing$diagnosis,mod1_pred)
confusionMatrix(testing$diagnosis,mod2_pred)


#### TREES

data("iris")
library(ggplot2); library(caret)
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species,p=0.7,list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]


qplot(Petal.Width,Sepal.Width,colour = Species,data=training)

modFit <- train(Species~.,method = 'rpart',data=training)
modFit$finalModel

plot(modFit$finalModel,uniform=TRUE)
text(modFit$finalModel,use.n = T,all = T,cex=.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)

predict(modFit,newdata = testing)


## Bagging (Bootstrap aggregation)

# our own caret
library(ElemStatLearn)
data("ozone",package = 'ElemStatLearn')
ozone <- ozone[order(ozone$ozone),]
head(ozone)

ll <- matrix(NA,nrow = 10,ncol = 155)

for (i in 1:10) {
    ss <- sample(1:dim(ozone)[1],replace = T)
    ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature~ozone,data = ozone0,span = 0.2)
    ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}


plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for (i in 1:10) {
    lines(1:155,ll[i,],col='grey',lwd=2)
}
lines(1:155,apply(ll,2,mean),col='red',lwd=2)

# bagging in caret

library(caret)
predictors <- data.frame(ozone=ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors,temperature,B=10,
               bagControl=bagControl(fit = ctreeBag$fit,
                                     predict = ctreeBag$pred,
                                     aggregate = ctreeBag$aggregate))


plot(ozone$ozone,ozone$temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col='red')
points(ozone$ozone,predict(treebag,predictors),pch=19,col='blue')

ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate


## Random Forests

data("iris"); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,p=0.7,list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

library(caret)

modFit <- train(Species~.,data = training,method='rf',prox=T)

getTree(modFit$finalModel,k=2)

irisP <- classCenter(training[,c(3,4)],training$Species,modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)

p<-qplot(Petal.Width,Petal.Length, col=Species,data = training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data = irisP)

pred <- predict(modFit,testing)
testing$predRight <- pred == testing$Species
table(pred,testing$Species)

## Boosting

library(ISLR); data(Wage)
library(ggplot2); library(caret)

Wage <- subset(Wage,select = -logwage)
inTrain <- createDataPartition(y=Wage$wage,p=0.7,list = F)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

modFit <- train(wage~.,method = 'gbm',data = training,verbose = F)
modFit

qplot(predict(modFit,testing),wage,data=testing)


## model based prediction

data("iris"); library(ggplot2)
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species,p=0.7,list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]


modlda <- train(Species~.,data=training,method='lda')
modnb <- train(Species~.,data=training,method='nb')

plda <- predict(modlda,testing)
pnb <- predict(modnb,testing)
table(plda,pnb)

equalPredictions <- plda==pnb
qplot(Petal.Width, Sepal.Width,data = testing,col=equalPredictions)

## QUIZ 

library(AppliedPredictiveModeling)
data("segmentationOriginal")
library(caret)
library(rattle)

inTrain <- createDataPartition(y=segmentationOriginal$Case,p=0.5,list = F)
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

set.seed(125)
cartmod <- train(Class~.,data = training,method='rpart')

fancyRpartPlot(cartmod$finalModel)

library(pgmm)
data("olive")
olive = olive[,-1]

cartmod <- train(Area~.,data = olive,method='rpart')
fancyRpartPlot(cartmod$finalModel)

predict(cartmod, newdata = as.data.frame(t(colMeans(olive))))


library(ElemStatLearn)
data("SAheart")
set.seed(8484)
train = sample(1:dim(SAheart)[1],size = dim(SAheart)[1]/2,replace = F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

logmod <-train(chd~age+alcohol+obesity+tobacco+typea+ldl,
               method='glm',family='binomial',data=trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(testSA$chd,predict(logmod,newdata = testSA))
missClass(trainSA$chd,predict(logmod,newdata = trainSA))

library(ElemStatLearn)
data("vowel.train")
data("vowel.test")

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

rfmod <- randomForest(y~.,data=vowel.train)
df<-varImp(rfmod)
order(df$Overall,decreasing = T)

## Ensemble modelling

library(ISLR); data(Wage)
library(ggplot2); library(caret)
Wage <- subset(Wage,select = -logwage)

inbuild <- createDataPartition(y=Wage$wage,p=0.7,list = F)
buildData <- Wage[inbuild,]
validation <- Wage[-inbuild,]

inTrain <- createDataPartition(y=buildData$wage,p=0.7,list = F)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]

mod1 <- train(wage~.,method='glm',data=training)
mod2 <- train(wage~.,method='rf',data=training,
              trControl=trainControl(method='cv'),number=3)

pred1 <- predict(mod1,testing)
pred2 <- predict(mod2,testing)
qplot(pred1,pred2,col=wage,data=testing)

predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage~.,method='gam',data=predDF)
combPred <- predict(combModFit,predDF)

sqrt(sum((pred1 - testing$wage)^2))
sqrt(sum((pred2 - testing$wage)^2))
sqrt(sum((combPred - testing$wage)^2))

pred1V <- predict(mod1,validation)
pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)

sqrt(sum((pred1V - validation$wage)^2))
sqrt(sum((pred2V - validation$wage)^2))
sqrt(sum((combPredV - validation$wage)^2))

### Forecasting

library(quantmod)
from.dat <- as.Date('01/01/08',format='%m/%d/%y')
to.dat <- as.Date('12/31/16',format='%m/%d/%y')
getSymbols('GOOG',src = 'google',from = from.dat,to=to.dat,verbose = T)

head(GOOG)
mGoog <- to.monthly(GOOG)
googOP <- Op(mGoog)
ts1 <- ts(googOP,frequency = 12)
plot(googOP)
plot(decompose(ts1))


## Unsuperwise learning
data("iris"); library(ggplot2)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species,p=0.7,list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

kMeans <- kmeans(subset(training,select = -Species),centers = 3)
training$clusters <- as.factor(kMeans$cluster)
qplot(Petal.Length,Petal.Width,data=training,col=clusters)
table(kMeans$cluster,training$Species)

modFit <- train(clusters~.,data=subset(training,select = -Species),method='rpart')
table(predict(modFit,training),training$Species)


## QUIZ

library(ElemStatLearn)
data("vowel.test")
data("vowel.train")
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
mod1 <- train(y~., method='rf',data = vowel.train)
mod2 <- train(y~., method='gbm',data = vowel.train,verbose=F)

pred1 <- predict(mod1,vowel.test)
pred2 <- predict(mod2,vowel.test)


predDF <- data.frame(pred1,pred2,y=vowel.test$y)
combModFit <- train(y~.,method='gam',data=predDF)
combPred <- predict(combModFit,predDF)

sum(pred1==vowel.test$y)/length(pred1)
sum(pred2==vowel.test$y)/length(pred2)
sum(pred1==pred2 & pred2==vowel.test$y)/sum(pred1==pred2)



library(caret);library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

mod1 <- train(diagnosis~., method='rf',data = training)
mod2 <- train(diagnosis~., method='gbm',data = training,verbose=F)
mod3 <- train(diagnosis~., method='lda',data = training)

pred1 <- predict(mod1,testing)
pred2 <- predict(mod2,testing)
pred3 <- predict(mod3,testing)

predDF <- data.frame(pred1,pred2,pred3,diagnosis=testing$diagnosis)
combModFit <- train(diagnosis~.,method='rf',data=predDF)
combPred <- predict(combModFit,predDF)

sum(pred1==testing$diagnosis)/length(pred1)
sum(pred2==testing$diagnosis)/length(pred2)
sum(pred3==testing$diagnosis)/length(pred3)
sum(combPred==testing$diagnosis)/length(combPred)


set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

mod <- train(CompressiveStrength~.,method='lasso',data=training)
plot.enet(mod$finalModel,xvar = 'penalty')


library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)


library(forecast)
fit <- bats(tstrain)
forec <- forecast(fit,235)

uper95 <- forec$upper[,2]
lower95 <- forec$lower[,2]

sum(testing$visitsTumblr<uper95 & testing$visitsTumblr > lower95)/length(uper95)


set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
mod <- svm(CompressiveStrength~.,data = training)
pred1 <- predict(mod,testing)

sqrt(sum((pred1-testing$CompressiveStrength)^2)/length(pred1))

