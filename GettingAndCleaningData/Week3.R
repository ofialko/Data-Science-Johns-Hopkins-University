set.seed(13435)
X<- data.frame("var1"=sample(1:5),
               "var2"=sample(6:10),
               "var3"=sample(11:15))

X<-X[sample(1:5),]; X$var2[c(1,3)]=NA
X[1:2,"var2"]

X[which(X$var2>8),]

sort(X$var1,decreasing = T,na.last = T)

X[order(X$var1),]

library(plyr)

arrange(X,var1)
arrange(X,desc(var1))

X$var4 <- rnorm(5)
X

Y <- cbind(X,rnorm(5))
names(Y)

# Summarazing data

fileURL <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileURL,destfile = 'data/restaurants.csv')
restData <- read.csv('data/restaurants.csv')

head(restData,n=3)
tail(restData,n=3)
summary(restData)
str(restData)

quantile(restData$councilDistrict,na.rm = T)

table(restData$zipCode,useNA = 'ifany')
table(restData$councilDistrict,restData$zipCode)

sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode>0)


colSums(is.na(restData))
all(colSums(is.na(restData))==0)


table(restData$zipCode %in% c('21212'))

indx = restData$zipCode %in% c('21212','21213')
restData[indx,]


data("UCBAdmissions")
DF = as.data.frame(UCBAdmissions)
summary(DF)


xt <- xtabs(Freq ~ Gender + Admit, data=DF)
xt

warpbreaks$replicate <- rep(1:9,len=54)
xt = xtabs(breaks ~., data = warpbreaks)
ftable(xt)


fakeData = rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData),units = "Mb")


#

restData <- read.csv("data/restaurants.csv")

s1 <- seq(1,10,by = 2); s1
s2 <- seq(1,10,length.out = 3);s2
x <- c(1,3,8,25,100); seq(along = x)

restData$nearMe = restData$neighborhood %in% c("Roland Park","Homeland")
table(restData$nearMe)

restData$zipWrong <- ifelse(restData$zipCode <0, TRUE, FALSE)
table(restData$zipWrong, restData$zipCode <0)

restData$zipGroups = cut(restData$zipCode,breaks = quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups,restData$zipCode)

library(Hmisc)
restData$zipGroups = cut2(restData$zipCode,g = 4)
table(restData$zipGroups)


restData$zcf <- factor(restData$zipCode)

yesno <- sample(c("yes","no"),size = 10,replace = TRUE)
yesno
yesnofac = factor(yesno,levels = c("yes","no"))
relevel(x = yesnofac,ref = "yes")

as.numeric(yesnofac)

library(Hmisc); library(plyr)

restData2 = mutate(restData,zipGroups = cut2(zipCode,g=4))
table(restData2$zipGroups)

#

library(reshape2)
head(mtcars)
mtcars$carname <- rownames(mtcars)

carMelt <- melt(mtcars,id.vars = c("carname","gear","cyl"),measure.vars = c("mpg","hp"))

cylData <- dcast(carMelt, cyl ~ variable,mean)

head(InsectSprays)

tapply(InsectSprays$count, InsectSprays$spray, sum)

spIns = split(InsectSprays$count,InsectSprays$spray)
sprCount = lapply(spIns, sum)
unlist(sprCount)

sapply(spIns, sum)


library(plyr)
ddply(InsectSprays,"spray",summarize,sum=sum(count))

spraySums <- ddply(InsectSprays,"spray",summarize,sum=ave(count,FUN=sum))


### dplyr

library(dplyr)
library(plyr)
options(width = 100)


chicago <- readRDS("data/chicago.rds")
dim(chicago)
str(chicago)
names(chicago)
# select
head(select(chicago,city:dptp))
head(select(chicago,-(city:dptp)))

# filter

chic.f <- filter(chicago,pm25tmean2 > 30 & tmpd > 80)
chic.f

# arrange

chicago <- arrange(chicago,date)
head(chicago)

# rename


chicago <- rename(chicago,replace = c(""))
chicago <- mutate(chicago,pm25detrent = pm25 - mean(pm25,na.rm = T))

# groupby

chicago <- mutate(chicago,tempcat = factor((tmpd>80),labels =c("cold","hot")))
hotcold = group_by(chicago,tempcat)
summarise(hotcold,pm25 = mean(pm25,na.rm = T),o3 = max(o3tmean2),no2 = median(no2tmean2))
    
chicago %>% mutate(month=as.POSIXlt(date)$mon + 1) %>% group_by(month) %>% 
    summarize(pm25 = mean(pm25))


#

fileURL1 <- 'https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv'
fileURL2 <- 'https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv'

download.file(fileURL1,destfile = 'data/reviews.csv')
download.file(fileURL2,destfile = 'data/solutions.csv')

reviews <- read.csv('data/reviews.csv')
solutions <- read.csv('data/solutions.csv')

head(reviews,2)
head(solutions,2)
names(reviews)
names(solutions)

mergedData <- merge(reviews,solutions,by.x = 'solution_id',by.y = 'id',all = T)


