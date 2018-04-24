library(lubridate)

lines1 <- grep("^1/2/2007", readLines("household_power_consumption.txt"))
lines2 <- grep("^2/2/2007", readLines("household_power_consumption.txt"))
lines0 <- c(lines1,lines2)


data <- read.table('household_power_consumption.txt',
                   skip = lines0[1]-1,nrows = length(lines0),sep=';',na.strings = '?')
colnames(data) <-c('Date','Time','Global_active_power','Global_reactive_power',
                   'Voltage','Global_intensity','Sub_metering_1','Sub_metering_2','Sub_metering_3')

par(mfrow=c(1,1))
#par(bg='white')
data$datetime <- paste(data$Date,data$Time)
data$datetime <- strptime(data$datetime,format='%e/%M/%Y %H:%M:%S')

hist(data$Global_active_power,
     xlab = 'Global Active Power (kilowatts)',
     col='red',main = 'Global Active Power')
dev.copy(device=png,file='plot1.png',width=480,height=480)
dev.off()
