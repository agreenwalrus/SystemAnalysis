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

getDictionary <- function(dirPath, amountOfWords = -1){
  articles <- Corpus(DirSource(dirPath),
                     readerControl = list(reader = readPlain,
                                          language = "en",
                                          load = T))
  
  articles <- prepareTextForAnalizing(articles)
  dict <- formDictionary(articles)
  h <- sortWordsByFreq(dict)
  
  if(amountOfWords != -1) {
    h <- head(h, amountOfWords)
  }
  
  return(h)
}

getMapFromDictionary <- function(dict) {
  keys <- unlist(dict$word)
  map <- c(dict$freq)
  names(map) <- keys
  
  return(map)
}

getCoordinatesOfDict <- function(vectors, mappedDict){
  result <- c(0, 0, 0)
  keys <- names(mappedDict)
  print(result)
  print(vectors)
  print(length(vectors))
  
  for (key in keys) {
    for(i in 1:length(vectors)) {
      if (key %in% names(vectors[[i]])){
        result[i] <- result[i] + mappedDict[key]
      }
    }
  }
  
  return(result)
}

dirPaths <- c("C:\\SA_texts\\lab4\\teach\\crypto", 
              "C:\\SA_texts\\lab4\\teach\\space", 
              "C:\\SA_texts\\lab4\\teach\\cloning")

testDir <- "C:\\SA_texts\\lab4\\test\\test"

dicts <- list()
i <- 1
for(path in dirPaths) {
  h <- getDictionary(path, 60)
  dicts[[i]] <- getMapFromDictionary(h)
  i <- i + 1
}

h <- getDictionary(testDir)
testDict <- getMapFromDictionary(h)

a <- getCoordinatesOfDict(vectors = dicts, mappedDict = testDict)
a


