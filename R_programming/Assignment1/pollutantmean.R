pollutantmean <- function(directory,pollutant,id = 1:332){
  data <- numeric()
  for (i in id) {
    if (i<10) {
      data_i <- read.csv(paste(directory,'/00',i,'.csv',sep = ''))
    }
    else if (i>9 && i<100) {
      data_i <- read.csv(paste(directory,'/0',i,'.csv',sep = ''))
    }
    else{
      data_i <- read.csv(paste(directory,'/',i,'.csv',sep = ''))
    }
    data <- c(data,data_i[,pollutant])
  }
  mean(data,na.rm = T)
}