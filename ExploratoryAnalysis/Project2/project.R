NEI <- readRDS('exdata-data-NEI_data/summarySCC_PM25.rds')
SCC <- readRDS('exdata-data-NEI_data/Source_Classification_Code.rds')

x = as.character(unique(NEI$year))
total_emission <- with(NEI,tapply(Emissions, year, sum,na.rm=T))
plot(x,total_emission,xlab='Year', ylab='Total Emission',
     type='b',pch=21,cex=2,lty='dashed',col='#000099',bg='#FF6666')
