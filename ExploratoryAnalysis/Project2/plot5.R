NEI <- readRDS('exdata-data-NEI_data/summarySCC_PM25.rds')
SCC <- readRDS('exdata-data-NEI_data/Source_Classification_Code.rds')

x = as.character(unique(NEI$year))
# grep "Mobile Sources" indices from SCC
mobile_indx <- grep('Mobile Sources',SCC$SCC.Level.One,fixed = T)
# converte them to scc sources
scc <- SCC$SCC[mobile_indx]

baltimore <- subset(NEI,fips=='24510')
baltimore_mobile = subset(baltimore, SCC %in% scc)



total_emission <- with(baltimore_mobile,tapply(Emissions, year, sum,na.rm=T))
plot(x,total_emission,xlab='Year', ylab='Emission (tons)',
     type='b',pch=21,cex=2,lty='dashed',col='#000099',bg='#FF6666',
     main='Total PM2.5 emission from motor vehicle sources \n in Baltimore City ')

dev.copy(png,file='plot5.png')
dev.off()
