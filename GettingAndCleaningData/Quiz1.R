#1-2
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",
              destfile = "./data/CommunitySurvey.csv")

properties <- read.csv(file = 'data/CommunitySurvey.csv')

table(properties$VAL)[24]

str(properties$FES)

#3
library(xlsx)
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx",
                         destfile = "data/NaturalGas.xlsx")

dat <- read.xlsx(file = "data/NaturalGas.xlsx",sheetIndex = 1, rowIndex = 18:23,colIndex = 7:15)
sum(dat$Zip*dat$Ext,na.rm = T)

#4

library(XML)
library(RCurl)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
xData <- getURL(fileUrl)
doc <-xmlTreeParse(xData,useInternalNodes = TRUE)
vals <- xpathSApply(doc,"//zipcode",xmlValue)
table(vals[vals=='21231'])

#5

download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv",
              destfile = "data/Communities.csv")

DT <- fread(input = "data/Communities.csv")

t1 <-as.numeric(Sys.time())
sapply(split(DT$pwgtp15,DT$SEX),mean)
as.numeric(Sys.time()) - t1

DT <- fread(input = "data/Communities.csv")

t1 <-as.numeric(Sys.time())
mean(DT$pwgtp15,by=DT$SEX)
as.numeric(Sys.time()) - t1

DT <- fread(input = "data/Communities.csv")

t1 <-as.numeric(Sys.time())
DT[,mean(pwgtp15),by=SEX]
as.numeric(Sys.time()) - t1

DT <- fread(input = "data/Communities.csv")

t1 <-as.numeric(Sys.time())
tapply(DT$pwgtp15,DT$SEX,mean)
as.numeric(Sys.time()) - t1

DT <- fread(input = "data/Communities.csv")

t1 <-as.numeric(Sys.time())
rowMeans(DT)[DT$SEX==1] 
rowMeans(DT)[DT$SEX==2]
as.numeric(Sys.time()) - t1

DT <- fread(input = "data/Communities.csv")

t1 <-as.numeric(Sys.time())
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
as.numeric(Sys.time()) - t1

