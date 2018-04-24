library(ggplot2)
data("mtcars")
head(mtcars)

g<- ggplot(data = mtcars,aes(x=factor(am),y=mpg,fill=factor(am)))
g<- g   + geom_boxplot()+ geom_point(size=5,alpha=0.5)
g<- g + xlab('Transmission') + ylab('Miles/(US) gallon')
g<- g + scale_x_discrete(labels=c('automatic','manual'))
g<- g + theme(legend.position='none')
g

# T-test
data_auto <- subset(mtcars,am==0)$mpg
data_manu <- subset(mtcars,am==1)$mpg
t<- t.test(data_manu,data_auto)
t$p.value
t$estimate


# Weight related to number of cylinders
h<- ggplot(data = mtcars,aes(x=factor(cyl),y=wt,fill=factor(cyl)))
h<- h   + geom_boxplot()+ geom_point()
h<- h + xlab('Number of cylinders') + ylab('Weight')
h<- h + theme(legend.position='none')
h

# Weight related to transmission
h<- ggplot(data = mtcars,aes(x=factor(am),y=wt,fill=factor(am)))
h<- h   + geom_boxplot()+ geom_point()
h<- h + xlab('Transmission') + ylab('Weight')
h<- h + theme(legend.position='none')
h


h<- ggplot(data = mtcars,aes(x=wt,y=mpg))
h + geom_point(size=10, aes(col=factor(cyl)))



fit1 <- lm(mpg~factor(am),data=mtcars)
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)

fit2 <- lm(mpg~factor(am)+factor(cyl),data=mtcars)
s<-summary(fit2)
s$sigma
s$coefficients


fit3 <- lm(mpg~factor(am)+wt,data=mtcars)
summary(fit3)$coef

fit4 <- lm(mpg~factor(am)+factor(cyl)+ wt,data=mtcars)
summary(fit4)$coef

fit2_int <- lm(mpg~factor(am)*factor(cyl),data=mtcars)
s<-summary(fit2_int)
s$sigma


anova(fit1,fit2,fit3,fit4)
anova(fit2,fit4)
anova(fit2_int,fit4)


fit5 <- lm(mpg~factor(am)+wt*factor(cyl),data=mtcars)
s<-summary(fit5)
s$sigma

anova(fit4,fit5)

fit6 <- lm(mpg~factor(am)*wt+factor(cyl),data=mtcars)
s<-summary(fit6)
s$sigma
s$coefficients

anova(fit4,fit6)
anova(fit2,fit6)

fit7 <- lm(mpg~factor(am)*wt,data=mtcars)
s<-summary(fit7)
s$sigma
s$coefficients

anova(fit6,fit7)
anova(fit2_int,fit7)

par(mfrow=c(2,2))
plot(fit7)

par(mfrow=c(2,2))
plot(fit2_int)

summary(fit2_int)$coef

h<- ggplot(data = mtcars,aes(x=wt,y=mpg))
h + geom_point(size=10, aes(col=factor(am)))


