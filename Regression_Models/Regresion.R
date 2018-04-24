library(UsingR)
library(dplyr)
library(ggplot2)
data("galton")

freqData<-as.data.frame(table(galton$child,galton$parent))
names(freqData)<-c('child','parent','freq')
freqData$child<-as.numeric(as.character(freqData$child))
freqData$parent<-as.numeric(as.character(freqData$parent))

g<-ggplot(filter(freqData,freq>0),aes(x=parent,y=child))
g<-g+scale_size(range = c(2,20),guide = 'none')
g<-g+geom_point(colour='grey50',aes(size=freq+20,show_guide=F))
g<-g+geom_point(aes(colour=freq, size=freq))
g<-g+scale_colour_gradient(low='lightblue',high='white')
g<-g+geom_smooth(method = 'lm')
g

lm(child~parent,data = galton)
lm(I(child-mean(child))~I(parent-mean(parent)),data = galton)


y<-galton$child
x<-galton$parent

beta1<-cor(x,y)*sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)
rbind(c(beta0,beta1),coef(lm(y~x)))

# Regression to the mean

library(UsingR)
data("father.son")
y<-(father.son$sheight-mean(father.son$sheight))/sd(father.son$sheight)
x<-(father.son$fheight-mean(father.son$fheight))/sd(father.son$fheight)
rho<-cor(x,y)

library(ggplot2)
library(plotly)
g=ggplot(data.frame(x=x,y=y),aes(x=x,y=y))
g=g+geom_point(size=6,colour='black',alpha=0.2)
g=g+geom_point(size=4,colour='salmon',alpha=0.2)
g=g+xlim(-4,4)+ylim(-4,4)
g=g+geom_abline(intercept = 0,slope = 1,size=1)
g=g+geom_vline(xintercept = 0)+geom_hline(yintercept = 0)
g=g+geom_abline(intercept = 0,slope = rho,size=2)
g=g+geom_abline(intercept = 0,slope=1/rho,size=2)
g

# Prediction

library(UsingR)
library(ggplot2)
data(diamond)

g <- ggplot(diamond,aes(x=carat,y=price))
g<-g+xlab('Mass (carats)') + ylab('Price ($)')
g <- g+ geom_point(size=6,col='black',alpha=0.2)
g <- g+ geom_point(size=5,col='blue',alpha=0.2)
g<-g+geom_smooth(method='lm',col='black')
g

fit <- lm(price~carat,data = diamond)
fit
fit$coefficients
newx <- c(.16,.27,.34)
predict(fit,data.frame(carat=newx))
newy <- predict(fit)

g+geom_point(aes(x=carat,y=newy),size=2,col='red')

# Resiaduals

library(UsingR)
library(ggplot2)
data(diamond)
y<-diamond$price; x<-diamond$carat; n<- length(y)
fit <- lm(y~x)
e<- fit$residuals
y-newy

yhat <- fit$fitted.values
max(abs(e-(y-yhat)))
max(abs(e-(y-fit$coefficients[1]-fit$coefficients[2]*x)))
plot(x,y,xlab = 'Mass',ylab='Price',bg='lightblue',col='black',cex=1.1,pch=21,
     frame=FALSE)
abline(fit,lwd=2)
for (i in 1:n){
    lines(x=c(x[i],x[i]),y=c(y[i],yhat[i]),col='red',lwd=2)
}

plot(x,e,xlab = 'Mass',ylab = 'Residuals',bg='lightblue',col='black',
     cex=2,pch=21, frame=F)
abline(h=0,lwd=2)
for (i in 1:n){
    lines(x=c(x[i],x[i]),y=c(0,e[i]),col='red',lwd=2)
}
hist(e,breaks = 10)

x<-runif(100,-3,3)
y<- x+sin(x)+rnorm(100,sd=0.2)
plot(x,y)
library(ggplot2)
g=ggplot(data.frame(x=x,y=y),aes(x=x,y=y))
g=g+geom_smooth(method = 'lm',col='black')
g=g+geom_point(size=7,col='black',alpha=0.4)
g=g+geom_point(size=5,col='red',alpha=0.4)
g

e=lm(y~x)$residuals
g=ggplot(data.frame(x=x,y=e),aes(x=x,y=e))
g=g+geom_hline(yintercept = 0,lwd=2)
g=g+geom_point(size=7,col='black',alpha=0.4)
g=g+geom_point(size=5,col='red',alpha=0.4)
g


#

y <- diamond$price; x <- diamond$carat; n<-length(y)
fit <- lm(y~x)
names(summary(fit))
summary(fit)$sigma
summary(fit)$r.squared


# Inference in Regression

library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat
n<-length(y)
beta1 <- cor(x,y)*sd(y)/sd(x)
beta0 <- mean(y) - beta1*mean(x)
e <- y-(beta0 + beta1*x)
sigma <- sqrt(sum(e^2)/(n-2))
ssx <- sum((x-mean(x))^2)
seBeta0 <- sigma*sqrt(1/n+mean(x)^2/ssx) 
seBeta1 <- sigma/sqrt(ssx)

tBeta0 <- beta0/seBeta0; tBeta1 <- beta1/seBeta1
pBeta0 <- 2*pt(abs(tBeta0),df=n-2,lower.tail = F)
pBeta1 <- 2*pt(abs(tBeta1),df=n-2,lower.tail = F)
coefTable <- rbind(c(beta0,seBeta0,tBeta0,pBeta0),c(beta1,seBeta1,tBeta1,pBeta1))
colnames(coefTable) <- c("Estimate","Std. Error","t value", "P(>|t|)")
row.names(coefTable)<- c("Intercept","x")

coefTable

fit <- lm(y~x)
sumCoef<- summary(fit)$coefficients

sumCoef[1,1] + c(-1,1)*qt(p=0.975,df=fit$df.residual)*sumCoef[1,2]
sumCoef[2,1] + c(-1,1)*qt(p=0.975,df=fit$df.residual)*sumCoef[2,2]

# Prediction

library(ggplot2)
library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat
n<-length(y)
fit <- lm(y~x)

newx <- data.frame(x=seq(min(x), max(x),length=100))
p1 = data.frame(predict(fit,newdata = newx,interval = 'confidence'))
p2 = data.frame(predict(fit,newdata = newx,interval = 'prediction'))

p1$interval = 'confidence'
p2$interval = 'prediction'

p1$x = newx$x
p2$x = newx$x

dat = rbind(p1,p2)
names(dat)[1] = 'y'


g=ggplot(dat,aes(x=x,y=y))
g=g+geom_ribbon(aes(ymin=lwr, ymax=upr, fill = interval),alpha=0.2)
g = g+geom_line() + geom_point(data = data.frame(x=x,y=y),size=4)

g


# multivariate regression

n=100; x=rnorm(n); x2 = rnorm(n); x3=rnorm(n)
y = 1+x+x2+x3+rnorm(n,sd = .1)
fit=lm(y~x+x2+x3)



# Ex1: swiss data
library(datasets)
library(ggplot2)
data("swiss")

summary(lm(Fertility~.,data=swiss))$coef
summary(lm(Fertility~Agriculture,data=swiss))$coef
summary(lm(Fertility~.-Agriculture-Examination,data=swiss))$coef


# Ex2: Factor variable

library(ggplot2)
library(datasets)
data("InsectSprays")
library(stats)

g=ggplot(data = InsectSprays,aes(x=spray,y=count))
g=g+geom_point(size=4,colour='blue',alpha=0.5)
g

g=ggplot(data=InsectSprays,aes(y=count,x=spray,fill=spray))
g=g+geom_violin(colour='black',size=2)
g = g+xlab("Type of spray") + ylab("Insect count")
g

summary(lm(count~spray,data=InsectSprays))$coef

summary(lm(count~I(spray=='B')+I(spray=='C')+I(spray=='D')+I(spray=='E')+
               I(spray=='F'),
           data=InsectSprays))$coef

summary(lm(count~spray-1,data = InsectSprays))$coef

spray2 <- relevel(InsectSprays$spray,"C")
summary(lm(count ~ spray2,data = InsectSprays))$coef

#Ex. 3

library(datasets)
data("swiss")
library(dplyr)

head(swiss)
swiss <- mutate(swiss,CatholicBin = as.integer(Catholic>50))

g=ggplot(data = swiss,aes(Agriculture,y=Fertility))
g=g+geom_point(size=6)
g=g+geom_point(size=4,aes(colour=as.factor(CatholicBin)))
g


fit1<-lm(Fertility~Agriculture, data = swiss)
g1=g
g1=g1+geom_abline(intercept = fit1$coefficients[1],slope = fit1$coefficients[2],size=2)
g1

fit2 <- lm(Fertility~Agriculture+factor(CatholicBin),data=swiss)
g1=g
g1=g1+geom_abline(intercept = fit2$coefficients[1],slope = fit2$coefficients[2],size=2)
g1=g1+geom_abline(intercept = fit2$coefficients[1] + fit2$coefficients[3],
                  slope=fit2$coefficients[2],size=2)
g1
    
fit3 <- lm(Fertility~Agriculture * factor(CatholicBin),data=swiss)
summary(fit3)$coef
g1=g
g1=g1+geom_abline(intercept = coef(fit3)[1],slope = coef(fit3)[2],size=2,color='red')
g1=g1+geom_abline(intercept = coef(fit3)[1]+coef(fit3)[3],
                  slope=coef(fit3)[2]+coef(fit3)[4],size=2,color='green')
g1

# Adjustmenets

n <- 100; 
t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)



n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), .9 + runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


p <- 1
n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2 
beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)


# Residuals again

library(datasets)
data(swiss)
par(mfrow=c(2,2))
fit<-lm(Fertility~.,data = swiss)
plot(fit)


n<-100; x<-c(10,rnorm(n)); y<-c(10,rnorm(n))
plot(x,y,frame=F,cex=2,pch=21,bg='lightblue',col='black')
abline(lm(y~x))

fit<-lm(y~x)



# model selection

n<- 100; nosim<-1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
betas<-sapply(1:nosim, function(i){
    y=x1 + rnorm(n,sd = .3)
    c(coef(lm(y~x1))[2], coef(lm(y~x1+x2))[2],coef(lm(y~x1+x2+x3))[2])
})

round(apply(betas, 1, sd),5)

n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

# swiss data
data(swiss); 
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- lm(Fertility ~ Agriculture + Examination, data = swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education, data = swiss)
c(summary(fit2)$cov.unscaled[2,2],
  summary(fit3)$cov.unscaled[2,2]) / a    


fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education, data = swiss)
fit5 <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality,data = swiss)
anova(fit1, fit3, fit5)




# Logistic regression

if (!dir.exists('data')) {
    dir.create('data')
}

download.file('https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda',
              destfile = 'data/ravensData.rda')

load('data/ravensData.rda')
head(ravensData)

lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef
pred_lm <- predict(lmRavens,newdata = ravensData)
ravensData$pred <- 0
ravensData$pred[pred_lm>0.5] <- 1
table(ravensData$ravenWinNum,ravensData$pred)


LogRegRavens <- glm(ravensData$ravenWinNum~ravensData$ravenScore,family = 'binomial')
summary(LogRegRavens)$coef
pred_glm <- predict(LogRegRavens,newdata = ravensData)
ravensData$glm_pred <- 0
ravensData$glm_pred[pred_glm>0.5] <- 1
table(ravensData$ravenWinNum,ravensData$glm_pred)
s<- summary(LogRegRavens)
confint(LogRegRavens)

plot(ravensData$ravenScore,LogRegRavens$fitted.values)

# Poisson regression

download.file('https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda',
              destfile = 'data/gaData.rda')

load('data/gaData.rda')
gaData$JD <- unclass(gaData$date)

plot(gaData$JD,gaData$visits)
lm1 <- lm(gaData$visits~gaData$JD)
abline(lm1)
glm1 <- glm(gaData$visits~gaData$JD, family = 'poisson')
lines(gaData$JD,glm1$fitted.values,col='red',lwd=3)

# hodgepodge

library(MASS)
data(shuttle)

fit1 <- glm(use~wind,data = shuttle,family = 'binomial')
fit1


fit2 <- glm(use~wind+magn,data = shuttle,family = 'binomial')
fit2

fit3 <- glm(1-use~wind,data = shuttle,family = 'binomial')
fit3

data("InsectSprays")

fit <- glm(count~factor(spray)+offset(10),data = InsectSprays,family = 'poisson')
fit

fit <- glm(count~factor(spray)+offset(10+log(10)),data = InsectSprays,family = 'poisson')
fit
