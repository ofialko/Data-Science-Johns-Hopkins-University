fileURL <- 'https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD'
download.file(fileURL,destfile = 'data/camera.csv')
cameraData <- read.csv('data/camera.csv')

names(cameraData)
tolower(names(cameraData))

splitNames <- strsplit(names(cameraData),split = '\\.')
splitNames[[6]][2]

firstElement <- function(x){x[1]}

sapply(splitNames, firstElement)


reviews <- read.csv('data/reviews.csv')
solutions <- read.csv('data/solutions.csv')

head(reviews,2)
head(solutions,2)

names(reviews)

# sub, qsub
sub("_","",names(reviews))
#qsub

# grep, grepl
grep("Alameda",cameraData$intersection)
table(grepl("Alameda",cameraData$intersection))

cameraData2 <- cameraData[!grepl('Alameda',cameraData$intersection),]

# 
library(stringi)
nchar("Sasha")

substr("Sasha",1,3)

paste("Sasha","Fialko")
paste0("Sasha","Fialko")

stri_trim("     Sasha      ")

# Regular expressions

# Dates

d1=date()
d1
class(d1)

d2 = Sys.Date()
d2
class(d2)

format(d2,"%a %b %d")

x<-c("1jan1960","2jan1960"); z=as.Date(x,"%d%b%Y")
z

weekdays(d2)
months(d2)
julian(d2)

library(lubridate)
ymd('20140108')
mdy('08/04/2013')
dmy('05/05/2065')
ymd_hms(20160405205634)


#



