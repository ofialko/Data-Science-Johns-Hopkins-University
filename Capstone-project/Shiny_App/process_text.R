library(tm)

grams2 <- readRDS("data/2grams.RData")
grams3 <- readRDS("data/3grams.RData")
grams4 <- readRDS('data/4grams.RData')

predict_next <- function(text){
  Text <- tolower(text)
  Text <- removePunctuation(Text)
  Text <- removeNumbers(Text)
  Text <- stripWhitespace(Text)
  Text <- MC_tokenizer(Text)
  Count <- length(Text)
  
  if (Count>=3) {
    Input <- Text[(Count-2):Count]
  }
  else if (Count ==2)  {
    Input <- c(NA,Text)   
  }
  else {
    Input <- c(NA,NA,Text)
  }
  
  Pred <- as.character(grams4[grams4$unigram==Input[1] & 
                              grams4$bigram ==Input[2] &
                              grams4$trigram==Input[3],]$quadgram)
  
  if(length(Pred)) {
    Pred <- as.character(grams3[grams3$unigram==Input[2] & 
                                grams3$bigram ==Input[3],]$trigram)
    
    if(length(Pred)) {
      Pred <- as.character(grams2[grams2$unigram==Input[3],]$bigram)
    }
  }
  return(paste0(Pred,collapse = ', '))
  # len <- length(Pred)
  # if (len>=3){
  #   print(Pred[1:3])
  # }else print(Pred)
}

