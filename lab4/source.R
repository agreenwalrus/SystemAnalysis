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
  v <- sort(rowSums(dict), decreasing = TRUE)
  sortedDict <- data.frame(word = names(v), freq = v)
  
  return(sortedDict)
}

formDictionary <- function(preparedText) {
  docs <- tm_map(preparedText, stemDocument)
  dtm <- TermDocumentMatrix(docs)
  dict <- as.matrix(dtm)
  
  return(dict)
} 

getCorpusSingleFile <- function(path) {
  conn <- file(path, "r")
  fulltext <- readLines(conn)
  close(conn)
  
  vs <- VectorSource(fulltext)
  return(Corpus(vs, readerControl=list(readPlain, language="en", load=TRUE)))
}


getCorpustMultipleFiles <- function(path) {
  return(Corpus(DirSource(path), 
         readerControl = list(reader = readPlain,
                              language = "en",
                              load = T)
         ))
}

getDictionary <- function(corpus, amountOfWords = -1){
  articles <- prepareTextForAnalizing(corpus)
  dict <- formDictionary(articles)
  h <- sortWordsByFreq(dict)
  
  if(amountOfWords != -1) {
    h <- head(h, amountOfWords)
  }
  
  s <- sum(c(h$freq))
  freq <- c()

  for(item in h$freq) {
    freq <- c(freq, item / s)
    #print(item)
  }
  
  
  h$freq <- freq

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
  #print(result)
  #print(vectors)
  #print(length(vectors))
  
  for (key in keys) {
    for(i in 1:length(vectors)) {
      if (key %in% names(vectors[[i]])){
        result[i] <- result[i] + mappedDict[key] * vectors[[i]][key] * 10000 
      }
    }
  }
  
  return(result)
}

#install.packages("scatterplot3d")
library(scatterplot3d) 
attach(mtcars)

build3dPlot <- function(data) {
  
  crypto <- unlist(lapply(data, function(x) x[[2]][1]))
  space <- unlist(lapply(data, function(x) x[[2]][2]))
  cloning <- unlist(lapply(data, function(x) x[[2]][3]))
  print(crypto)
  print(space)
  print(cloning)
  
  scatterplot3d(crypto, space, cloning, main="Text plot")
}

getListOfFilesCoordinates <- function(filesPath, dicts) {
  i <- 1
  for(fileName in filesPath) {
    h <- getDictionary(getCorpusSingleFile(fileName))
    testDict <- getMapFromDictionary(h)
    a <- getCoordinatesOfDict(vectors = dicts, mappedDict = testDict)
    testFilesCoordinates[[i]] <- list(fileName, a)
    i <- i + 1
  }
  
  return(testFilesCoordinates)
}

getListOfDictsForTexts <- function(dictPaths, sizeOfDict) {
  dicts <- list()
  i <- 1
  for(path in dictPaths) {
    h <- getDictionary(getCorpustMultipleFiles(path), sizeOfDict)
    dicts[[i]] <- getMapFromDictionary(h)
    i <- i + 1
  }
  
  return(dicts)
}


dictPaths <- c("C:\\SA_texts\\lab4\\dicts\\crypto", 
              "C:\\SA_texts\\lab4\\dicts\\space", 
              "C:\\SA_texts\\lab4\\dicts\\cloning")

filesDir <- "C:\\SA_texts\\lab4\\files\\"
testFiles <- c("cloning4.txt", "cloning5.txt", 
               "space4.txt", "space5.txt", 
               "crypto4.txt", "crypto5.txt")

kMeanFiles <- c("cloning1.txt", "cloning2.txt", "cloning3.txt", 
               "space1.txt", "space2.txt", "space3.txt",
               "crypto1.txt", "crypto2.txt", "crypto3.txt")



#dicts
kMeanFiles <- unlist(lapply(kMeanFiles, function(fname, dir){
  paste(dir, fname, sep="")
}, dir = filesDir))

testFiles <- unlist(lapply(testFiles, function(fname, dir){
  paste(dir, fname, sep="")
}, dir = filesDir))

kMeanFiles <- c (kMeanFiles, testFiles)


dicts <- getListOfDictsForTexts(dictPaths = dictPaths, sizeOfDict = 60)

fc <- getListOfFilesCoordinates(filesPath = kMeanFiles, 
                               dicts = dicts)

build3dPlot(fc)

fc


l <- lapply(d, function(el) el[[2]])
m <- matrix(unlist(l), ncol = 3, byrow = TRUE)

res <- kmeans(x = m, centers = 3)
res

clasters <- matrix(res$centers, ncol=3)



res$centers[[1]]






