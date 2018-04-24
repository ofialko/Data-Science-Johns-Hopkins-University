library(httr)
oauth_endpoints("github")

myapp <- oauth_app("Getting and Cleaning Data",
                    key = "7a00f935e5d6d7828afc",
                   secret = "ba71a0d3dc065426f3b6ae61633db06a1eff79cf")

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

json1 = content(req)
library(jsonlite)
json2 = jsonlite::fromJSON(toJSON(json1))

#2

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv",
              destfile = "data/acs.csv")

acs <- read.csv("data/acs.csv")
library(sqldf)
sqldf("select pwgtp1 from acs where AGEP < 50")


con = url("http://biostat.jhsph.edu/~jleek/contact.html")
content1 = readLines(con)
close(con)
nchar(content1[10])
nchar(content1[20])
nchar(content1[30])
nchar(content1[100])


#5
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for",
              destfile = "data/data.for")

data <- read.fwf("data/data.for",skip = 4, widths = c(15,4,4,5,4,4,5,4,4,5,4,4))
sum(data[,5])


