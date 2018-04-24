NEI <- readRDS('exdata-data-NEI_data/summarySCC_PM25.rds')
SCC <- readRDS('exdata-data-NEI_data/Source_Classification_Code.rds')

x = as.character(unique(NEI$year))
# grep "Mobile Sources" indices from SCC
mobile_indx <- grep('Mobile Sources',SCC$SCC.Level.One,fixed = T)
# convert them to scc sources
scc <- SCC$SCC[mobile_indx]

baltimore <- subset(NEI,fips=='24510')
baltimore_mobile <- subset(baltimore, SCC %in% scc)

losangeles <- subset(NEI,fips=='06037')
losangeles_mobile <- subset(losangeles,SCC %in% scc)

total_emission_b <- with(baltimore_mobile,tapply(Emissions, year, sum,na.rm=T))
total_emission_l <- with(losangeles_mobile,tapply(Emissions, year, sum,na.rm=T))


# Making plot
par(mfrow=c(1,2),mar=c(4,4,1,2),oma=c(0,0,3,0))
plot(x,total_emission_b,xlab='Year', ylab='Emission (tons)',
     type='b',pch=21,cex=2,lty='dashed',col='#000099',bg='#FF6666',
     main='',ylim=c(400,17000))
points(x,total_emission_l,type='b', pch = 21,cex=2,lty='dashed',col='#000099',bg='green')
legend('right',legend = c('Los Angeles ',
                          'Baltimore '),
       pch=19,col=c('green','#FF6666'),y.intersp = 1.5)

plot(x,total_emission_b/total_emission_b[1],xlab='Year', ylab='Scaled emission (tons)',
     type='b',pch=21,cex=2,lty='dashed',col='#000099',bg='#FF6666',
     main='',ylim=c(0,1.1))
points(x,total_emission_l/total_emission_l[1],type='b', pch = 21,cex=2,lty='dashed',col='#000099',bg='green')
legend('right',legend = c('Los Angeles ',
                          'Baltimore '),
       pch=19,col=c('green','#FF6666'),y.intersp = 1.5)
title('Total PM2.5 emission from motor vehicle sources
      in Baltimore City and Los Angeles County',outer=T)


dev.copy(png,width=800,height=400,file='plot6.png')
dev.off()
