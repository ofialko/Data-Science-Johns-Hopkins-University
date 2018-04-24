corr <- function(directory, threshold=0){
  vec <- numeric()
  df <- complete(directory=directory)
  for (i in df$id) {
    if (df[i,'nobs']>threshold) {
      if (i<10) {
        data_i <- read.csv(paste(directory,'/00',i,'.csv',sep = ''))
      }
      else if (i>9 && i<100) {
        data_i <- read.csv(paste(directory,'/0',i,'.csv',sep = ''))
      }
      else{
        data_i <- read.csv(paste(directory,'/',i,'.csv',sep = ''))
      }
      corr <- cor(data_i$nitrate,data_i$sulfate,use="pairwise.complete.obs")
      vec <- c(vec,corr)     
    }
  }
  return(vec)
}