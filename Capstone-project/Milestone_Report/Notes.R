# usefull libraries
library(tm)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
library(RWeka)
library(plotly)

# download files

if (!dir.exists('data')) {
    dir.create('data')
}

link = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
download.file(url = link,destfile = 'data/capstonedataset.zip')
rm(link)
unzip('data/capstonedataset.zip',exdir = 'data')


# loading data
#set.seed(123)

#loading blogs
blogs <- readLines('data/final/en_US/en_US.blogs.txt',skipNul = T)
indx <- rbinom(length(blogs)*.1,length(blogs),0.1)
blogs <- blogs[indx]
rm(indx)

#loading twitters
tweets <- readLines('data/final/en_US/en_US.twitter.txt',skipNul = T)
indx <- rbinom(length(tweets)*.1,length(tweets),0.1)
tweets <- tweets[indx]
rm(indx)

#loading news
news <- readLines('data/final/en_US/en_US.news.txt')
indx <- rbinom(length(news)*.1,length(news),0.1)
news <- news[indx]
rm(indx)

## QUIZ
# max(nchar(as.array(tweets)))
# file.size('final/en_US/en_US.blogs.txt')/1024^2
# sum(grepl('love', as.array(tweets),ignore.case = F))/sum(grepl('hate', as.array(tweets),ignore.case = F))
# tweets[grepl('biostats',as.array(tweets))]
# pattern = "A computer once beat me at chess, but it was no match for me at kickboxing"
# sum(grepl(pattern,as.array(tweets)))


if (!dir.exists('data/samples')) {
    dir.create('data/samples')
}


writeLines(blogs,'data/samples/blogs.txt')
writeLines(tweets,'data/samples/tweets.txt')
writeLines(news,'data/samples/news.txt')

rm(blogs,news,tweets)
# Data Preprocessing

data_corpus <- Corpus(DirSource('data/samples/'), readerControl = list(language='en-US'))
summary(data_corpus)
length(data_corpus[[1]]$content) == length(blogs)
length(data_corpus[[2]]$content) == length(news)
length(data_corpus[[3]]$content) == length(tweets)

pattern = '(http|ftp|https)://[[:alnum:][:punct:]]*'
removeURL <- function(x) gsub(pattern,"",x)
removeTags <- function(x) gsub("(#|@)\\S+","",x)

data_corpus <- tm_map(data_corpus,content_transformer(removeURL))
data_corpus <- tm_map(data_corpus,content_transformer(removeTags))
data_corpus <- tm_map(data_corpus,removeNumbers)
data_corpus <- tm_map(data_corpus,removePunctuation)
data_corpus <- tm_map(data_corpus,stripWhitespace)
data_corpus <- tm_map(data_corpus,content_transformer(tolower))
data_corpus0 <- data_corpus
data_corpus <- tm_map(data_corpus,removeWords,stopwords("english"))
data_corpus <- tm_map(data_corpus,stripWhitespace)


# we need to remove offensive words
link = 'https://www.cs.cmu.edu/~biglou/resources/bad-words.txt'
download.file(url = link,destfile = 'data/bad-words.txt')
rm(link)

bad_words <- readLines('data/bad-words.txt')
bad_words <- bad_words[2:length(bad_words)]
data_corpus  <- tm_map(data_corpus,removeWords,bad_words)
data_corpus0 <- tm_map(data_corpus0,removeWords,bad_words)
rm(bad_words)

data_corpus  <- tm_map(data_corpus,stripWhitespace)
data_corpus0 <- tm_map(data_corpus0,stripWhitespace)

data_corpus[[1]]$content[1]
data_corpus0[[1]]$content[1]


tdm.blogs  <- TermDocumentMatrix(data_corpus[1])
tdm.news   <- TermDocumentMatrix(data_corpus[2])
tdm.tweets <- TermDocumentMatrix(data_corpus[3])

tdm.blogs0  <- TermDocumentMatrix(data_corpus0[1])
tdm.news0   <- TermDocumentMatrix(data_corpus0[2])
tdm.tweets0 <- TermDocumentMatrix(data_corpus0[3])


#data_corpus <- tm_map(data_corpus,PlainTextDocument)

#####
tokens.blogs  <- MC_tokenizer(data_corpus[[1]])
tokens.news   <- MC_tokenizer(data_corpus[[2]])
tokens.tweets <- MC_tokenizer(data_corpus[[3]])

tokens.blogs0  <- MC_tokenizer(data_corpus0[[1]])
tokens.news0   <- MC_tokenizer(data_corpus0[[2]])
tokens.tweets0 <- MC_tokenizer(data_corpus0[[3]])


# Text mining


# avg number of words per line
words_per_line.blogs <- length(tokens.blogs)/length(data_corpus[[1]]$content)
words_per_line.news <- length(tokens.news)/length(data_corpus[[2]]$content)
words_per_line.tweets <- length(tokens.tweets)/length(data_corpus[[3]]$content)

# number of unique words per all words in a line

uni_words <- unique(c(tdm.blogs$dimnames$Terms,
                      tdm.news$dimnames$Terms,
                      tdm.tweets$dimnames$Terms))

uni_words.blogs   <- length(tdm.blogs$dimnames$Terms)/length(uni_words)
uni_words.news    <- length(tdm.news$dimnames$Terms)/length(uni_words)
uni_words.tweets  <- length(tdm.tweets$dimnames$Terms)/length(uni_words)


# content: avg (num_words - stopwords)/num_words

#cont_blogs <- apply(as.array(data_corpus[[1]]$content), 1, MC_tokenizer)
#cont_blogs <- sapply(cont_blogs, length)

#cont_blogs_sw <- apply(as.array(data_corpus_stopw[[1]]$content), 1, MC_tokenizer)
#cont_blogs_sw <- sapply(cont_blogs_sw, length)

#blogs_cont <- mean(cont_blogs_sw/cont_blogs,na.rm = T)


#cont_news <- apply(as.array(data_corpus[[2]]$content), 1, MC_tokenizer)
#cont_news <- sapply(cont_news, length)

#cont_news_sw <- apply(as.array(data_corpus_stopw[[2]]$content), 1, MC_tokenizer)
#cont_news_sw <- sapply(cont_news_sw, length)

#news_cont <- mean(cont_news_sw/cont_news,na.rm = T)

#cont_tweets <- apply(as.array(data_corpus[[3]]$content), 1, MC_tokenizer)
#cont_tweets <- sapply(cont_tweets, length)

#tweets_cont <-mean(cont_tweets_sw/cont_tweets,na.rm = T)


df <- data.frame('Source' = c('Blogs','News','Tweets'), 
                 'Uni_words' = c(uni_words.blogs,uni_words.news,uni_words.tweets),
                 'Words_line' = c(words_per_line.blogs,words_per_line.news,words_per_line.tweets))
                 

df_g <- gather(df,c(2,3),key='Stats',value='Count')
df_g$Stats <- as.factor(df_g$Stats)
levels(df_g$Stats) <- c('Fraction of unique words','Avg # words per line')

g <- ggplot(df_g,aes(x=Source,y=Count)) + 
    geom_bar(stat='Identity',fill='blue',color='black',alpha=0.5)
g <- g+facet_grid(Stats~.,scales = 'free') + theme_bw() + ylab('')
g <- g+ theme(text = element_text(size=12),axis.text = element_text(size=12))
g


save.image(file='Preprocess_DATA.RData')

### Word Frequency

freq.blogs  <- data.frame(word = tdm.blogs$dimnames$Terms, frequency = tdm.blogs$v)
ord <- order(freq.blogs$frequency,decreasing = T)
freq.blogs <- freq.blogs[ord,]

freq.blogs0  <- data.frame(word = tdm.blogs0$dimnames$Terms, frequency = tdm.blogs0$v)
ord <- order(freq.blogs0$frequency,decreasing = T)
freq.blogs0 <- freq.blogs0[ord,]


freq.news   <- data.frame(word = tdm.news$dimnames$Terms, frequency = tdm.news$v)
ord <- order(freq.news$frequency,decreasing = T)
freq.news <- freq.news[ord,]

freq.news0   <- data.frame(word = tdm.news0$dimnames$Terms, frequency = tdm.news0$v)
ord <- order(freq.news0$frequency,decreasing = T)
freq.news0 <- freq.news0[ord,]



freq.tweets <- data.frame(word = tdm.tweets$dimnames$Terms, frequency = tdm.tweets$v)
ord <- order(freq.tweets$frequency,decreasing = T)
freq.tweets <- freq.tweets[ord,]

freq.tweets0 <- data.frame(word = tdm.tweets0$dimnames$Terms, frequency = tdm.tweets0$v)
ord <- order(freq.tweets0$frequency,decreasing = T)
freq.tweets0 <- freq.tweets0[ord,]


freq.blogs$word <- reorder(factor(freq.blogs$word,levels = freq.blogs$word),freq.blogs$frequency)
freq.news$word <- reorder(factor(freq.news$word,levels = freq.news$word),freq.news$frequency)
freq.tweets$word <- reorder(factor(freq.tweets$word,levels = freq.tweets$word),freq.tweets$frequency)


lim <- max(freq.blogs[1:20,2]); lim = 1.1*lim
g.freq.blogs <- ggplot(freq.blogs[1:20,],aes(y=frequency,x=word)) +
    geom_bar(stat = 'Identity',width = 0.1,alpha=0.8) +
    geom_point(size=7) + geom_point(size=5,color='red') +
    coord_flip()  + xlab('') + ylab('') +
    scale_y_continuous(expand=c(0,0),limits = c(0,lim))+ 
    labs(title='Word Frequency: Blogs') +
    theme_bw() + theme(axis.text = element_text(size=12))

lim <- max(freq.news[1:20,2]); lim = 1.1*lim
g.freq.news <- ggplot(freq.news[1:20,],aes(y=frequency,x=word)) +
    geom_bar(stat = 'Identity',width = 0.1,alpha=0.8) +
    geom_point(size=7) + geom_point(size=5,color='red') +
    coord_flip()  + xlab('') + ylab('') +
    scale_y_continuous(expand=c(0,0),limits = c(0,lim))+ 
    labs(title='Word Frequency: News') +
    theme_bw() + theme(axis.text = element_text(size=12))

lim <- max(freq.tweets[1:20,2]); lim = 1.1*lim
g.freq.tweets <- ggplot(freq.tweets[1:20,],aes(y=frequency,x=word)) +
    geom_bar(stat = 'Identity',width = 0.1,alpha=0.8) +
    geom_point(size=7) + geom_point(size=5,color='red') +
    coord_flip()  + xlab('') + ylab('') +
    scale_y_continuous(expand=c(0,0),limits = c(0,lim))+ 
    labs(title='Word Frequency: Tweets') +
    theme_bw() + theme(axis.text = element_text(size=12))


grid.arrange(g.freq.blogs,g.freq.news,g.freq.tweets,ncol=3)

save.image('Frequency_DATA.RData')

### 2-GRAMS

Tokenizer <- function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
tdm2.blogs  <- TermDocumentMatrix(data_corpus[1],control = list(tokenize=Tokenizer))
tdm2.news   <- TermDocumentMatrix(data_corpus[2],control = list(tokenize=Tokenizer))
tdm2.tweets <- TermDocumentMatrix(data_corpus[3],control = list(tokenize=Tokenizer))

tdm2.blogs0  <- TermDocumentMatrix(data_corpus0[1],control = list(tokenize=Tokenizer))
tdm2.news0   <- TermDocumentMatrix(data_corpus0[2],control = list(tokenize=Tokenizer))
tdm2.tweets0 <- TermDocumentMatrix(data_corpus0[3],control = list(tokenize=Tokenizer))


#freq.blogs_2  <- data.frame(word = tdm2.blogs$dimnames$Terms, frequency = tdm2.blogs$v)
#ord <- order(freq.blogs_2$frequency,decreasing = T)
#freq.blogs_2 <- freq.blogs_2[ord,]

freq.blogs0_2  <- data.frame(word = tdm2.blogs0$dimnames$Terms, frequency = tdm2.blogs0$v)
ord <- order(freq.blogs0_2$frequency,decreasing = T)
freq.blogs0_2 <- freq.blogs0_2[ord,]

freq.news0_2  <- data.frame(word = tdm2.news0$dimnames$Terms, frequency = tdm2.news0$v)
ord <- order(freq.news0_2$frequency,decreasing = T)
freq.news0_2 <- freq.news0_2[ord,]

freq.tweets0_2  <- data.frame(word = tdm2.tweets0$dimnames$Terms, frequency = tdm2.tweets0$v)
ord <- order(freq.tweets0_2$frequency,decreasing = T)
freq.tweets0_2 <- freq.tweets0_2[ord,]


Tokenizer <- function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
tdm3.blogs  <- TermDocumentMatrix(data_corpus[1],control = list(tokenize=Tokenizer))
tdm3.news   <- TermDocumentMatrix(data_corpus[2],control = list(tokenize=Tokenizer))
tdm3.tweets <- TermDocumentMatrix(data_corpus[3],control = list(tokenize=Tokenizer))

tdm3.blogs0  <- TermDocumentMatrix(data_corpus0[1],control = list(tokenize=Tokenizer))
tdm3.news0   <- TermDocumentMatrix(data_corpus0[2],control = list(tokenize=Tokenizer))
tdm3.tweets0 <- TermDocumentMatrix(data_corpus0[3],control = list(tokenize=Tokenizer))


freq.blogs0_3  <- data.frame(word = tdm3.blogs0$dimnames$Terms, frequency = tdm3.blogs0$v)
ord <- order(freq.blogs0_3$frequency,decreasing = T)
freq.blogs0_3 <- freq.blogs0_3[ord,]

freq.news0_3  <- data.frame(word = tdm3.news0$dimnames$Terms, frequency = tdm3.news0$v)
ord <- order(freq.news0_3$frequency,decreasing = T)
freq.news0_3 <- freq.news0_3[ord,]

freq.tweets0_3  <- data.frame(word = tdm3.tweets0$dimnames$Terms, frequency = tdm3.tweets0$v)
ord <- order(freq.tweets0_3$frequency,decreasing = T)
freq.tweets0_3 <- freq.tweets0_3[ord,]


freq.blogs0_3$word <- reorder(factor(freq.blogs0_3$word,levels = freq.blogs0_3$word),freq.blogs0_3$frequency)
freq.news0_3$word <- reorder(factor(freq.news0_3$word,levels = freq.news0_3$word),freq.news0_3$frequency)
freq.tweets0_3$word <- reorder(factor(freq.tweets0_3$word,levels = freq.tweets0_3$word),freq.tweets0_3$frequency)

lim <- max(freq.blogs0_3[1:20,2]); lim = 1.1*lim
g.freq.blogs0_3 <- ggplot(freq.blogs0_3[1:20,],aes(y=frequency,x=word)) +
    geom_bar(stat = 'Identity',width = 0.1,alpha=0.8) +
    geom_point(size=7) + geom_point(size=5,color='green') +
    coord_flip()  + xlab('') + ylab('') +
    scale_y_continuous(expand=c(0,0),limits = c(0,lim))+ 
    labs(title='Word Frequency: Blogs') +
    theme_bw() + theme(axis.text = element_text(size=14))

lim <- max(freq.news0_3[1:20,2]); lim = 1.1*lim
g.freq.news0_3 <- ggplot(freq.news0_3[1:20,],aes(y=frequency,x=word)) +
    geom_bar(stat = 'Identity',width = 0.1,alpha=0.8) +
    geom_point(size=7) + geom_point(size=5,color='green') +
    coord_flip()  + xlab('') + ylab('') +
    scale_y_continuous(expand=c(0,0),limits = c(0,lim))+ 
    labs(title='Word Frequency: News') +
    theme_bw() + theme(axis.text = element_text(size=14))


lim <- max(freq.tweets0_3[1:20,2]); lim = 1.1*lim
g.freq.tweets0_3 <- ggplot(freq.tweets0_3[1:20,],aes(y=frequency,x=word)) +
    geom_bar(stat = 'Identity',width = 0.1,alpha=0.8) +
    geom_point(size=7) + geom_point(size=5,color='green') +
    coord_flip()  + xlab('') + ylab('') +
    scale_y_continuous(expand=c(0,0),limits = c(0,lim))+ 
    labs(title='Word Frequency: Tweets') +
    theme_bw() + theme(axis.text = element_text(size=14))


grid.arrange(g.freq.blogs0_3,g.freq.news0_3,g.freq.tweets0_3,ncol=3)

## Vocablurary coverage


coverage <- function(tdm,cov=0.99){
    #sorting
    m.tdm <- as.matrix(tdm)
    m.tdm <- rowSums(m.tdm)
    ord <- order(m.tdm,decreasing = T)
    m.tdm <- m.tdm[ord]
    
    m.sum <- sum(m.tdm)
    m.tdm2 <- m.tdm/m.sum
    m.cumsum <- cumsum(m.tdm2)
    
    for (i in  m.cumsum) {
        if (i>= cov) {
            val = i
            break()
        }
    }
    
    # 8143 words are needed for 90% of coverage
    indx<-which(m.cumsum == val,arr.ind = T)[[1]]
    m.tdm[1:indx]
    #indx
}

coverage_ind <- function(tdm,cov){
    #sorting
    m.tdm <- as.matrix(tdm)
    m.tdm <- rowSums(m.tdm)
    ord <- order(m.tdm,decreasing = T)
    m.tdm <- m.tdm[ord]
    
    m.sum <- sum(m.tdm)
    m.tdm2 <- m.tdm/m.sum
    m.cumsum <- cumsum(m.tdm2)
    
    for (i in  m.cumsum) {
        if (i>= cov) {
            val = i
            break()
        }
    }
    
    # 8143 words are needed for 90% of coverage
    indx<-which(m.cumsum == val,arr.ind = T)[[1]]
    #m.tdm[1:indx]
    indx
}



tdm.combined <- c(tdm.blogs,tdm.news,tdm.tweets)
m.tdm <- coverage(tdm.combined)

tdm2.combined <- c(tdm2.blogs0,tdm2.news0,tdm2.tweets0)
m.tdm2 <- coverage(tdm2.combined)

tdm3.combined <- c(tdm3.blogs0,tdm3.news0,tdm3.tweets0)
m.tdm3 <- coverage(tdm3.combined)


cover <- seq(0,1,0.01)
cover_ind <- cover
indx <- seq_along(cover)
for (ind in indx) {
    cover_ind[ind] <- coverage_ind(tdm.combined,cover[ind])    
}
coverage_df <- data.frame('number of words'=cover_ind,'coverage'=cover)

coverage_plot = plot_ly(coverage_df,y=~number.of.words,x=~cover,mode='lines') %>%
    layout(xaxis = list(title = "Coverage"),
           yaxis = list (title = "# of words"))
coverage_plot

### PREDICTING 

load('DATA.RData')

try_predict_next_word <- function(text){
    text <- tolower(text)
    txt  <- MC_tokenizer(text)
    txt_len <- length(txt)
    #if (txt[txt_len] %in% names(m.tdm)){
    if (txt_len==1) {
        pattern2 <- paste('^',txt[1],'[[:space:]]',sep = '')
        indx2 <-  which(sapply(as.array(names(m.tdm2)), 
                               function(x) grepl(pattern2,x,ignore.case = T)))
        df2 <-  as.data.frame(m.tdm2[indx2])
        nlen <- dim(df2)[1]
        return(row.names(df2)[1:min(nlen,3)])
    }else{
    
    pattern3 <- paste('^',txt[txt_len-1],sep = '')
    pattern3 <- paste(pattern3,txt[txt_len],sep = ' ')
    pattern3 <- paste(pattern3,'[[:space:]]',sep='')
    indx3 <-  which(sapply(as.array(names(m.tdm3)), 
                                   function(x) grepl(pattern3,x,ignore.case = T)))
    df3 <- as.data.frame(m.tdm3[indx3])
    nlen <- dim(df3)[1]
    if (nlen>0) {
        return(row.names(df3)[1:min(nlen,3)])
    }else{   
    
        pattern2 <- paste('^',txt[txt_len],'[[:space:]]',sep = '')
        indx2 <-  which(sapply(as.array(names(m.tdm2)), 
                                   function(x) grepl(pattern2,x,ignore.case = T)))
        df2 <-  as.data.frame(m.tdm2[indx2])
        nlen <- dim(df2)[1]
        nlen <- dim(df2)[1]
        return(row.names(df2)[1:min(nlen,3)])
        }
    }
    #}else{
    #    return('The word appear to be uncommon...try another combination')
    #}
}

predict_next_word <- function(text){
    suggestion = try_predict_next_word(text)
    if (is.na(suggestion)[1]) {
        return('Try another combination please')
    }else{
        return(suggestion)
    }
}

text <- 'physics'
predict_next_word('a case of')

# saving data for the report

save(data_corpus,g,g.freq.blogs,g.freq.news,g.freq.tweets,
       g.freq.blogs0_3,g.freq.news0_3,g.freq.tweets0_3,
       coverage_plot,predict_next_word,try_predict_next_word,cover,m.tdm2,m.tdm3,
     file = 'Report_Data.RData')


