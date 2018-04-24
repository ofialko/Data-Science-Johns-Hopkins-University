NEI <- readRDS('exdata-data-NEI_data/summarySCC_PM25.rds')
SCC <- readRDS('exdata-data-NEI_data/Source_Classification_Code.rds')

x = as.character(unique(NEI$year))

baltimore <- subset(NEI,fips=='24510')

total_emission <- with(baltimore,tapply(Emissions, year, sum,na.rm=T))
plot(x,total_emission,xlab='Year', ylab='Emission (tons)',
     type='b',pch=21,cex=2,lty='dashed',col='#000099',bg='#FF6666',
     main='Total PM2.5 emission in Baltimore City')

dev.copy(png,file='plot2.png')
dev.off()
