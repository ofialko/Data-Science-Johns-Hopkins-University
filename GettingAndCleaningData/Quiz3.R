# 1
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv',
              destfile = 'data/housing06.csv')

housing06 <- read.csv('data/housing06.csv')

agricultureLogical <- ((housing06$ACR == 3) & (housing06$AGS == 6))
which(agricultureLogical)

# 2

library(jpeg)
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg',
              destfile = 'data/photo.jpg')
photo <- readJPEG('data/photo.jpg',native = TRUE)
quantile(photo,probs = c(0.3,0.8))

# 3
library(dplyr)

download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv',
              destfile = 'data/GDP.csv')
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv',
              destfile = 'data/stats_country.csv')

gdp_data <- read.csv('data/GDP.csv',skip = 4,nrows = 190)
country_data <- read.csv('data/stats_country.csv')

gdp_data2 <-select(gdp_data, X,X.1,X.4)
country_data2 <- select(country_data, CountryCode,Long.Name,Income.Group)

mergedData <- merge(country_data2,gdp_data2, by.x = 'CountryCode',by.y = 'X')

mergedData$GDP <-gsub(" ","",as.character(mergedData$X.4))  
mergedData$GDP <-gsub(",","",mergedData$GDP) 

mergedData$GDP <-as.numeric(mergedData$GDP) 

select(arrange(mergedData,GDP),GDP,Long.Name)

#4

grouped_df <- group_by(mergedData,Income.Group)
summarise(grouped_df, aveGDP= mean(X.1))

# 5

library(Hmisc)
mergedData$GDPgroups = cut2(mergedData$X.1,g = 5)
table(mergedData$Income.Group,mergedData$GDPgroups)

