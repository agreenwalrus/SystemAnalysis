library(tm)
library("SnowballC")
library(rgl)
#install.packages("tm", dependencies = T)
#install.packages("SnowballC", dependencies = T)
#install.packages("rgl")

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

addPoints <- function(coord, toClust){
  #newdf <- data.frame(data, K=clustVec)#Include the number of cluster in the data set
  #pcdf <- princomp(data)#,cor=T,score=T)
  #summary(pcdf)#Compute the validity of each component/dimension
  plot3d(x = coord[1],
         y = coord[2],
         z = coord[3],
         col=toClust, 
         add = T)#Create a 3D plot
}

clusterPlot3D <- function(data, kmRes){
  newdf <- data.frame(data, K=kmRes$cluster)#Include the number of cluster in the data set
  pcdf <- princomp(data)#,cor=T,score=T)
  summary(pcdf)#Compute the validity of each component/dimension
  plot3d(pcdf$scores, 
         col=newdf$K, 
         xlab = "crypto", 
         ylab = "space", 
         zlab = "cloning")#Create a 3D plot
}

build3dPlot <- function(data) {
  
  crypto <- unlist(lapply(data, function(x) x[[2]][1]))
  space <- unlist(lapply(data, function(x) x[[2]][2]))
  cloning <- unlist(lapply(data, function(x) x[[2]][3]))
  
  scatterplot3d(crypto, space, cloning, main="Text plot")
}

getListOfFilesCoordinates <- function(filesPath, dicts) {
  i <- 1
  testFilesCoordinates <- list()
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

getPrediction <- function(coord, clusters) {
  
  predict <- c(sqrt((clusters[1,][1] - coord[1])**2 + 
                     (clusters[1,][2] - coord[2])**2 +
                     (clusters[1,][3] - coord[3])**2), 1)
  len <- 1
  clustNum <- 2
  
  i <- 1
  for(i in 2:3) {
  
    current <- sqrt((clusters[i,][1] - coord[1])**2 + 
                      (clusters[i,][2] - coord[2])**2 +
                      (clusters[i,][3] - coord[3])**2)

    #print(predict)
    #print(i)
    #print(current)
    if(current < predict[len]){
      predict <- c(current, i)
    }
    
    i <- i + 1
  }
  
  return(predict)
}



#######################################
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
######################################


#form files' names
kMeanFiles <- unlist(lapply(kMeanFiles, function(fname, dir){
  paste(dir, fname, sep="")
}, dir = filesDir))

testFiles <- unlist(lapply(testFiles, function(fname, dir){
  paste(dir, fname, sep="")
}, dir = filesDir))



#form dictionary
dicts <- getListOfDictsForTexts(dictPaths = dictPaths, sizeOfDict = 60)

#form coordinates of files according to dictionaries
#for k-meanes files
kmfc <- getListOfFilesCoordinates(filesPath = kMeanFiles, 
                               dicts = dicts)

build3dPlot(kmfc)

#form coordinates of files according to dictionaries
#for test files
tfc <- getListOfFilesCoordinates(filesPath = testFiles, 
                                 dicts = dicts)

#form matrix of coordinates
l <- lapply(kmfc, function(el) el[[2]])
m <- matrix(unlist(l), ncol = 3, byrow = TRUE)


amountOfClusters <- 3
res <- kmeans(x = m, centers = amountOfClusters)
print("K-MEANS RESULT")
res

clusterVector <- res$cluster
for(i in 1:amountOfClusters) {
  clNum <- 1
  print(paste("CLUSTER GROUP ¹", toString(i), sep=""))
  print(paste("Centers:", toString(res$centers[i,], sep = "")))
  for(cl in clusterVector){
    if(cl == i){
      print(kMeanFiles[clNum])
    }
      clNum <- clNum + 1
  }
}
  
clusters <- matrix(res$centers, ncol=3)

print("-----------------------------------------------")

clusterPlot3D(data = m, kmRes = res)


print("CLUSTERS")

for(fil in tfc){
  pred <- getPrediction(coord = fil[[2]], clusters = clusters)
  print(paste(fil[[1]], "to group ¹", pred[2], sep=" "))
  addPoints(fil[[2]], pred[2])
  print("")        
}


print("-----------------------------------------------")













