library(lubridate)

lines1 <- grep("^1/2/2007", readLines("household_power_consumption.txt"))
lines2 <- grep("^2/2/2007", readLines("household_power_consumption.txt"))
lines0 <- c(lines1,lines2)


data <- read.table('household_power_consumption.txt',
                   skip = lines0[1]-1,nrows = length(lines0),sep=';',na.strings = '?')
colnames(data) <-c('Date','Time','Global_active_power','Global_reactive_power',
                   'Voltage','Global_intensity','Sub_metering_1','Sub_metering_2','Sub_metering_3')


data$datetime <- paste(data$Date,data$Time)
data$datetime <- strptime(data$datetime,format='%e/%M/%Y %H:%M:%S')


par(mfrow=c(2,2))
par(mar=c(4,4,3,1))
#par(bg='grey87')
plot(data$datetime,data$Global_active_power,type='l',
     xlab='',ylab='Global Active Power')
plot(data$datetime,data$Voltage,type='l',
     xlab='datetime',ylab='Voltage')
plot(data$datetime,data$Sub_metering_1,type='l',col='black',
     xlab='',ylab='Energy sub metering')
lines(data$datetime,data$Sub_metering_2,type='l',col='red')
lines(data$datetime,data$Sub_metering_3,type='l',col='blue')
legend('topright',
       legend = c('Sub_metering_1              ',
                  'Sub_metering_2              ',
                  'Sub_metering_3              '),box.lwd = 0,
       col=c('black','red','blue'),lty = 1,y.intersp = 1.3)

plot(data$datetime,data$Global_reactive_power,type='l',
     xlab='datetime',ylab='Global_reactive_power')


dev.copy(device=png,file='plot4.png',width=480,height=480)
dev.off()
