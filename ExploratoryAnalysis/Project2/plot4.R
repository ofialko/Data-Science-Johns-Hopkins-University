NEI <- readRDS('exdata-data-NEI_data/summarySCC_PM25.rds')
SCC <- readRDS('exdata-data-NEI_data/Source_Classification_Code.rds')

x = as.character(unique(NEI$year))

# grep "Coal" indices from SCC
coal_indx <- grep('Coal',SCC$EI.Sector,fixed = T)
# convert them to scc source
scc <- SCC$SCC[coal_indx]

coal_data = subset(NEI, SCC %in% scc)
total_emission <- with(coal_data,tapply(Emissions, year, sum,na.rm=T))

plot(x,total_emission,xlab='Year', ylab='Emission (tons)',
     type='b',pch=21,cex=2,lty='dashed',col='#000099',bg='#FF6666',
     main='Total PM2.5 emission from coal combustion-related sources')

dev.copy(png,file='plot4.png')
dev.off()
