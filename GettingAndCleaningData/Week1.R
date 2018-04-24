# CSV file

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"

download.file(url = fileUrl,destfile = "./data/cameras.csv")
list.files("./data")
dateDownloaded <- date()

cameraData <- read.table("data/cameras.csv",sep = ",", header = TRUE)
head(cameraData)


# XML

library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl,useInternalNodes = TRUE)

rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
xmlValue(rootNode)

rootNode[[1]]
rootNode[[1]][[1]]
xmlValue(rootNode[[1]][[2]])

# sapply on every subelement
xmlSApply(rootNode,xmlValue)


# XPath
# /node Top level node
# //node any level nodes
# node[@attr-name]
# node[@attr-name='bob']

xpathSApply(rootNode,"//name",xmlValue)
xpathSApply(rootNode,"//price",xmlValue)
xpathSApply(doc,"//price",xmlValue)

fileUrl <- "http://www.espn.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl,useInternalNodes = TRUE)
teams <- xpathSApply(doc,"//li[@class='team-name']",xmlValue)

# JSON
library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)

myjson <- toJSON(iris,pretty = T)

# data.table

library(data.table)
DT = data.table(x=rnorm(9),y=rep(c('a','b','c'),each=3),z=rnorm(9))
head(DT,3)

tables()

DT[2,]
DT[DT$y=='a',]

DT[c(2,3)]
DT[,c(2,3)]


k={print(10); 5} # expression
print(k)

DT[,list(mean(x),sum(z))]
DT[,table(y)]

DT[,w:=z^2]
DT

DT[,m:={tmp<-(x+z);log2(tmp+5)}]
DT
DT[,a:=x>0]
DT

DT[,b:=mean(x+w),by=a]
DT

set.seed(123)
DT<-data.table(x=sample(letters[1:3],1E5,TRUE))
DT[,.N,by=x]

DT <- data.table(x=rep(c('a','b','c'),each=100),y=rnorm(300))
setkey(DT,x)
DT['a']
