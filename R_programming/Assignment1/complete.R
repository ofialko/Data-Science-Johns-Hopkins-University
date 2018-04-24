complete <- function(directory, id=1:332){
  df <- data.frame()
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
    df<-rbind(df,c(i,sum(!is.na(data_i$sulfate) & !is.na(data_i$nitrate))))
  }
  colnames(df)<-c('id','nobs')
  return(df)
}
