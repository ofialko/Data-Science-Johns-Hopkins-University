# 1
fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
download.file(fileURL,destfile = 'data/Idaho.csv')
idaho_data <- read.csv('data/Idaho.csv',sep = ',')
names_Idaho <- names(idaho_data)
lst <- strsplit(names_Idaho,split = 'wgtp')
lst[[123]]


# 2
library(stringi)
fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
download.file(fileURL,destfile = 'GDP.csv')
GDP_data <- read.csv('data/GDP.csv',skip = 4,nrows = 190)
ang_GDP <-as.numeric(stri_trim(gsub(x=GDP_data$X.4,',','')))
mean(ang_GDP)

# 3

countryNames <- GDP_data$X.3
unites <- grep(pattern = '^United',x = countryNames)
GDP_data$X.3[unites]

# 4

fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
download.file(fileURL,destfile = 'data/stats_country.csv')
STATS_data <- read.csv('data/stats_country.csv')

mergedData <- merge(STATS_data,GDP_data, by.x = 'CountryCode',by.y = 'X')
notes <- grep(x=mergedData$Special.Notes,pattern = '^Fiscal.*June')

mergedData$Special.Notes[notes]

# 5
library(lubridate)
library(quantmod)
amzn <- getSymbols('AMZN',auto.assign = FALSE)
sampleTimes <- index(amzn)
sampleTimes <- ymd(sampleTimes)
sample_2012 <- sum(year(sampleTimes)==2012)
