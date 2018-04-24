### MYSQL

library(RMySQL)
# list all databases
ucscDB <- dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDB,"show databases;")
dbDisconnect(ucscDB);
result

hg19 <- dbConnect(MySQL(),user="genome",db='hg19',host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables)
allTables[1:5]

dbListFields(conn = hg19,name = 'affyU133Plus2')
dbGetQuery(hg19,"select count(*) from affyU133Plus2")
# reading tables from db
affyData <- dbReadTable(hg19,"affyU133Plus2")
head(affyData)

query <- dbSendQuery(hg19,"select * from affyU133Plus2 where 
                     misMatches between 1 and 3")

affyMis <- fetch(query)
quantile(affyMis$misMatches)

affyMisSmall <- fetch(query,n = 10)
dbClearResult(query)
dim(affyMisSmall)

dbDisconnect(hg19)

###  HDF5
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)


created = h5createFile("example.h5")
# groups

created = h5createGroup("example.h5","foo")
created = h5createGroup("example.h5","bea")
created = h5createGroup("example.h5","foo/foobea")
h5ls("example.h5")

A=matrix(1:10,nrow = 5,ncol = 2)
h5write(A,"example.h5","foo/A")
B=array(seq(0.1,2.0,by = 0.1),dim=c(5,2,2))
attr(B,"scale") <- "liter"
h5write(B,"example.h5","foo/foobea/B")
h5ls("example.h5")

df = data.frame(1L:5L,seq(0,1,length.out = 5),
                c("ab","cde","fghi","a","s"),stringsAsFactors = FALSE)

h5write(df,"example.h5","df")
h5ls("example.h5")

readA = h5read("example.h5","foo/A")

h5write(c(12,13,14),"example.h5","foo/A",index=list(1:3,1))

h5read("example.h5","foo/A")

### Scrapping the web

library(XML)

# Method 1
# first step: readinh html into txt
con <- url("http://www.uefa.com/index.html")
htmlCode <- readLines(con)
close(con)

# Parsing html
html <- htmlTreeParse(htmlCode,useInternalNodes = TRUE)


xpathSApply(html,"//title",xmlValue)
xpathSApply(html,"//td[@id='col-citeby']",xmlValue)

# Method 2 More powerfull: httr allows to GET, POST, PUT, DELETE
library(httr)
htmlCode2 = GET("http://www.uefa.com/index.html")
content2 = content(htmlCode2,as="text")
parsedHTML = htmlParse(content2,asText = TRUE)
xpathSApply(parsedHTML,"//title",xmlValue)

### API Twitter: 

library(httr)
myapp = oauth_app("Mining Twitter OF",key = "Zba2dlLdOVKDi03NjMje9UqB8",
                  secret = "UapRStoiNiufXlht2BKHotxl1ZDvaGLCkrNjebxQjCUyTcMJc5")

sig = sign_oauth1.0(myapp, token = "4919141599-RYUkTG1vLYcHU62dDUgODYnL48xQzXT05tV4M3z",
                    token_secret = "eun0n1em96zTfT0QF4tQ2Vpu6AFxrVYGZr7D09CiRhrD9")

homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)
json1 = content(homeTL)
library(jsonlite)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[1,1:4]


# Other sources
# file - open connection to acess files
# url - open connection to a url
# gzfile - open to acess .gz
# ? conneciton
# read.foo



