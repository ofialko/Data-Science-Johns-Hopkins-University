NEI <- readRDS('exdata-data-NEI_data/summarySCC_PM25.rds')
SCC <- readRDS('exdata-data-NEI_data/Source_Classification_Code.rds')

x = as.character(unique(NEI$year))
baltimore <- subset(NEI,fips=='24510')
baltimore <- transform(baltimore,type=factor(type))
emission_type <- split(baltimore,baltimore$type)

year_type <- sapply(emission_type, function(x) with(x,tapply(Emissions, year, sum,na.rm=T)))
year_type <- as.data.frame(year_type)
year_type$year <- as.numeric(rownames(year_type))

library(reshape2)
year_type <- melt(data = year_type,id.vars = "year")

library(ggplot2)
g=ggplot(data=year_type,aes(x=year,y=value)) + facet_grid(facets = .~variable)
g=g +geom_point(shape=21,size=4,fill='red') +geom_line(linetype='dashed')
g=g+ labs(title='Total PM2.5 emission in Baltimore City for each type of source',y='Emission (tons)',x='Year')
g


dev.copy(png,width=1000,height=300,file='plot3.png')
dev.off()
