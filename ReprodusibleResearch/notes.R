library(kernlab)
data("spam")
set.seed(3435)
trainIndicator = rbinom(4601,size = 1,prob = 0.5)
table(trainIndicator)
trainSpam = spam[trainIndicator==1,]
testSpam = spam[trainIndicator==0,]
names(trainSpam)
head(trainSpam)

boxplot(log10(trainSpam$capitalAve+1)~trainSpam$type)
pairs(log10(trainSpam[,1:4]+1))

hCluster = hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hCluster)

trainSpam$numtype = as.numeric(trainSpam$type) - 1
costFunction = function(x,y) sum(x != (y>0.5))
cvError = rep(NA,55)
library(boot)
for (i in 1:55) {
    LmFormula = reformulate(names(trainSpam)[i],response = 'numtype')
    glmFit = glm(LmFormula,family = 'binomial',data = trainSpam)
    cvError[i] = cv.glm(trainSpam,glmFit,costFunction,2)$delta[2]
}

## which predictor has minimum cross-validation error ?
names(trainSpam)[which.min(cvError)]

predictionModel = glm(numtype~charDollar,family = 'binomial',data=trainSpam)
predictionTest = predict(predictionModel,testSpam)
predictedSpam = rep('nonspam',dim(testSpam)[1])
predictedSpam[predictionModel$fitted.values>0.5] ='spam'

table(predictedSpam,testSpam$type)
