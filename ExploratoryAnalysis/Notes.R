if (!file.exists('data')){
    dir.create('data')
}

pollution <- read.csv('data/avgpm25.csv',
                      colClasses = c('numeric','character','factor','numeric','numeric'))


summary(pollution$pm25)
boxplot(pollution$pm25,col='blue')

hist(pollution$pm25,col='green',breaks = 50)
rug(pollution$pm25)

boxplot(pollution$pm25,col='blue')
abline(h=12)

hist(pollution$pm25,col='green')
abline(v=12,lwd=2)
abline(v=median(pollution$pm25),col='magenta',lwd=4)

table(pollution$region)
barplot(table(pollution$region),col='wheat')

boxplot(pm25 ~ region,data = pollution,col='red')

par(mfrow = c(2,1),mar=c(4,4,2,1))
hist(subset(pollution,region=='east')$pm25,col='green')
hist(subset(pollution,region=='west')$pm25,col='green')


with(pollution, plot(latitude,pm25,col=region))
abline(h = 12,lwd=2,lty=2)

par(mfrow=c(1,2),mar=c(5,4,2,1))
with(subset(pollution,region=='west'), plot(latitude,pm25,main='West'))
with(subset(pollution,region=='east'), plot(latitude,pm25,main='East'))

# base plotting system
library(datasets)
data("cars")
with(cars,plot(speed,dist))

# lattice system
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout=c(4,1))

# ggplot2 system
library(ggplot2)
data("mpg")
qplot(displ,hwy, data = mpg)


##################################3
library(datasets)
hist(airquality$Ozone)
with(airquality,plot(Wind,Ozone,pch='*'))
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality,xlab = "Month", ylab="Ozone (ppb)")

with(airquality, plot(Wind,Ozone))
title(main="Blah blah")

with(airquality, plot(Wind,Ozone,main='blh blahj'))
with(subset(airquality,Month==5), points(Wind,Ozone,col='blue'))


with(airquality, plot(Wind, Ozone, main = 'Title goes here',type='n'))
with(subset(airquality, Month == 5), points(Wind, Ozone,col='blue'))
with(subset(airquality, Month != 5), points(Wind, Ozone,col='red'))
legend("topright",pch=1,col=c('blue','red'),legend=c('May','Other Months'))


with(airquality, plot(Wind,Ozone,main='Ozone and Wind',pch=20))
model <- lm(Ozone ~ Wind, airquality)
abline(model,lwd=2)


par(mfrow = c(1,2))
with(airquality, {
    plot(Wind,Ozone)
    plot(Solar.R,Ozone)
})


# Simulation
par(mfrow=c(1,1))
x<- rnorm(100)
hist(x)
y<-rnorm(100)
plot(x,y)
par('mar')

example(points)

plot(x,y,pch=15)
title("scatter plot")
text(-2,-2,"label")
legend("topleft",legend = "Data",pch=15)
fit <- lm(y~x)
abline(fit,lwd=3,col='blue')

plot(x,y,xlab="weight",ylab="Height",main="Scatter plot")

z<- rpois(100,2)
par(mfrow=c(2,1))
par(mar=c(2,2,1,0))
plot(x,y)
plot(x,z)

par(mfrow=c(1,1))
x<-rnorm(100)
y <- x+rnorm(100)
g<-gl(2,50,labels = c('Male','Female'))
plot(x,y,type='n')
par(mar=c(4,4,1,1))
points(x[g=='Male'],y[g=="Male"],col='green')
points(x[g=='Female'],y[g=="Female"],col='blue',pch=19)
legend('topleft',legend = c('male','female'),pch = 19,col = c('green','blue'))

# Graphic devices

pdf(file="myplot.pdf")
with(faithful,plot(eruptions,waiting))
title(main='Old Faithful Geyser data')
dev.off()


library(datasets)
with(faithful,plot(eruptions,waiting))
title(main='Old data')
dev.copy(device = png,file='geyserplot.png')
dev.off()


## Lattice system

library(lattice)
library(datasets)

p<-xyplot(Ozone~Wind,data=airquality)
p

airquality <- transform(airquality,Month=factor(Month))
xyplot(Ozone~Wind | Month, data=airquality,layout=c(5,1))

x<-rnorm(100)
f<-rep(0:1,each=50)
y<-x+f-f*x +rnorm(100,sd=0.5)
f<-factor(f,labels=c("G1","G2"))
xyplot(y~x|f,layout=c(2,1))

xyplot(y~x|f,panel = function(x,y,...){
    panel.xyplot(x,y,...)
    panel.lmline(x,y,col=2)
})

## GGPLOT

# qplot = quick plot
library(ggplot2)
data(mpg)
str(mpg)
qplot(displ,hwy,data=mpg,color=drv)+geom_smooth()
qplot(hwy,data=mpg,fill=drv)
qplot(displ,hwy,data=mpg,facets = .~drv)
qplot(hwy,data=mpg,facets = drv~.,binwidth=2)

# ggplot
load('maacs.Rda')
str(maacs)
qplot(log(pm25),duBedMusM,data=maacs,facets=.~mopos) + geom_smooth(method = 'lm')

g<-ggplot(maacs,aes(pm25,duBedMusM))
summary(g)

g+geom_point(size=3,shape=1)+geom_smooth()+facet_grid(.~mopos)+xlab('pm 25')
g+geom_point(aes(colour=mopos),size=4,alpha=1/2)+labs(title='MAACS Cohort')+
    theme_bw(base_size = 14,base_family = 'Times')+
    coord_cartesian(xlim=c(0,150),ylim = c(0,70000),expand = T)

# QUIZ
library(nlme)
library(lattice)
data("BodyWeight")
xyplot(weight ~ Time | Diet,data=BodyWeight)

library(datasets)
data(airquality)

airquality$Month <- as.factor(airquality$Month)
qplot(Wind,Ozone,data=airquality,facets = .~Month )

library(ggplot2)
library(ggplot2movies)
data(movies)
qplot(votes,rating,data=movies) + geom_smooth()

### Hierarchical clustering
# dist -> hclust -> plot
par(mar=c(4.1,4,2,2))
hc = hclust(dist(mtcars))
# very simple dendrogram
plot(hc)
plot(hc, hang = -1)

# using dendrogram objects
hcd = as.dendrogram(hc)
# alternative way to get a dendrogram
plot(hcd)
plot(hcd, type = "triangle")

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(hc, k = 3, boxes = FALSE, col.up = "gray50", 
        col.down = c("#FF6B6B",  "#4ECDC4", "#556270"))
plot(as.phylo(hc), type = "unrooted")


### K-MEANS Clustering
# kmeans
x<-rep(c(1,2,3),each=4) + rnorm(12,sd = 0.2)
y<-rep(c(1,3,2),each=4) + rnorm(12,sd = 0.2)
Kmeanobj <- kmeans(data.frame(x,y),centers = 3)
plot(x,y,pch=19,cex=3,col=Kmeanobj$cluster)
points(Kmeanobj$centers,pch=3,cex=3,lwd=3,col=1:3)

set.seed(12345)
op=par(mar=rep(0.2,4))
dataMatrix <- matrix(rnorm(400),nrow = 40)
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)
par(op)

### PCA & SVD
# pca and svd
load('face.rda')
image(t(faceData)[,nrow(faceData):1])

svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2),xlab='Singular vecor',ylab='variance explained')

approx1 <- svd1$u[,1:5] %*% diag(svd1$d[1:5])%*%t(svd1$v[,1:5])
approx2 <- svd1$u[,1:10] %*% diag(svd1$d[1:10])%*%t(svd1$v[,1:10])

par(mfrow=c(1,3))
image(t(approx1)[,nrow(approx1):1],main='a')
image(t(approx2)[,nrow(approx2):1],main='b')
image(t(faceData)[,nrow(faceData):1],main='c')

### COLORS
library(grDevices)
pal <- colorRamp(c('red','blue'))
pal(0)
pal(1)
pal(0.5)
pal(seq(0,1,len=10))

pal <- colorRampPalette(c('red','blue'))
pal(2)
pal(10)

library(RColorBrewer)
cols <- brewer.pal(3,name = 'BuGn')
pal <- colorRampPalette(cols)
image(volcano,col=pal(20))


x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x,y)
# vs
plot(x,y,pch=19,col=rgb(0,0,0,0.2))

## CASE STUDY
# 1
load('samsungData.rda')
names(samsungData)[1:12]
table(samsungData$activity)
samsungData <- transform(samsungData,activity = factor(activity))

sub1 <- subset(samsungData,subject==1)
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg = "#EFEFEF")
hc <- hclust(dist(sub1[,10:12]))

pal <- colorRampPalette(c('red','blue'))

k=2
A2Rplot(hc, k = 2, boxes = FALSE, col.up = "gray50", col.down = pal(2))

#2

pm0 <- read.table('RD_501_88101_1999-0.txt', comment.char = '#',header = F,
                  sep='|',na.strings = "")

dim(pm0)
head(pm0)

cnames <- readLines('RD_501_88101_1999-0.txt',n = 1)
cnames
cnames <- strsplit(cnames,"|",fixed = T)
cnames
names(pm0) <- make.names(cnames[[1]])
head(pm0)

x0 <- pm0$Sample.Value
str(x0)
summary(x0)
# how many NA ?
mean(is.na(x0)) # 11%

pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char = '#',header = F,
                  sep="|",na.strings = "")
dim(pm1)

names(pm1) <- make.names(cnames[[1]])
names(pm1)

x1 <- pm1$Sample.Value

summary(x1)
summary(x0)
mean(is.na(x1))
boxplot(x0,x1)

boxplot(log10(x0),log10(x1))

negative <- x1<0
str(negative)
mean(negative,na.rm = T)

dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates),"%Y%m%d")
str(dates)
hist(dates,'month')
hist(dates[negative],'month')


site0 <- unique(subset(pm0,State.Code == 36,c(County.Code,Site.ID)))
site1 <- unique(subset(pm1,State.Code == 36,c(County.Code,Site.ID)))
head(site0)
site0 <- paste(site0[,1],site0[,2],sep='.')
site1 <- paste(site1[,1],site1[,2],sep='.')

str(site0)
str(site1)


both <- intersect(site0,site1)
both
pm0$County.Site <- with(pm0,paste(County.Code,Site.ID,sep='.'))
pm1$County.Site <- with(pm1,paste(County.Code,Site.ID,sep='.'))

cnt0 <- subset(pm0,State.Code == 36 & County.Site %in% both)
cnt1 <- subset(pm1,State.Code == 36 & County.Site %in% both)

sapply(split(cnt0,cnt0$County.Site),nrow)
sapply(split(cnt1,cnt1$County.Site),nrow)


pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

dates1 <- pm1sub$Date
dates1 <- as.Date(as.character(dates1),"%Y%m%d")
x1sub <- pm1sub$Sample.Value
dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0),"%Y%m%d")
x0sub <- pm0sub$Sample.Value

rng<-range(x0sub,x1sub,na.rm = T)
par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(dates0,x0sub,pch=20,ylim=rng)
abline(h =  median(x0sub,na.rm = T))
plot(dates1,x1sub,pch=20,ylim=rng)
abline(h =  median(x1sub,na.rm = T))

mn0 <- tapply(pm0$Sample.Value, pm0$State.Code, mean,na.rm=T)
mn1 <- tapply(pm1$Sample.Value, pm1$State.Code, mean,na.rm=T)

summary(mn0)
summary(mn1)
d0 <- data.frame(state = names(mn0),mean=mn0)
d1 <- data.frame(state = names(mn1),mean=mn1)

mrg <- merge(d0,d1,by = 'state')
mrg

par(mfrow=c(1,1))
with(mrg,plot(rep(1999,52),mrg[,2],xlim=c(1998,2013)))
with(mrg,points(rep(2012,52),mrg[,3]))
segments(rep(1999,52),mrg[,2],rep(2012,52),mrg[,3])

