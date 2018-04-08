library(tm)
library("SnowballC")
#install.packages("tm", dependencies = T)
#install.packages("SnowballC", dependencies = T)


prepareTextForAnalizing <- function(text) {
  result <- tm_map(text, iconv, 'cp1251', 'UTF-8')
  result <- tm_map(result, stripWhitespace)
  result <- tm_map(result, tolower)
  result <- tm_map(result, removeNumbers)
  result <- tm_map(result, removeWords, stopwords("english"))
  result <- tm_map(result, removePunctuation)
  return(result)
}

sortWordsByFreq <- function(dict) {
  v <- sort(rowSums(dict),decreasing=TRUE)
  sortedDict <- data.frame(word = names(v),freq=v)
  return(sortedDict)
}

formDictionary <- function(preparedText) {
  docs <- tm_map(preparedText, stemDocument)
  dtm <- TermDocumentMatrix(docs)
  dict <- as.matrix(dtm)
  return(dict)
} 

getMostPopularWords <- function(dirPath, amountOfWords){
  articles <- Corpus(DirSource(dirPath),
                     readerControl = list(reader = readPlain,
                                          language = "en",
                                          load = T))
  
  articles <- prepareTextForAnalizing(articles)
  dict <- formDictionary(articles)
  h <- head(sortWordsByFreq(dict), amountOfWords)
  return(h)
}

getMapFromDictionary <- function(dict) {
  keys <- unlist(dict$word)
  map <- c(dict$freq)
  names(map) <- keys
  
  return(map)
}


# Файлы хранятся в каталоге articles
dirpath <- "C:\\lab4\\SA_texts\\teach"
h <- getMostPopularWords(dirpath, 20)
foo <- getMapFromDictionary(h)

foo

