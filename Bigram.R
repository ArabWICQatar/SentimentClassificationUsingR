
#create a vector of all the packages you want to load
packs <- c("tm","tau","qdap","RWeka","e1071","caret")
lapply(packs, require, character.only = TRUE)


#load the dataset as a matrix
inputText = read.csv(file = "pathToFile/AirlineTweets.csv", header = TRUE, sep = ",")
input=as.matrix(inputText)


# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", inputText[,1])

# remove at people e.g. @ArabWic
some_txt = gsub("@\\w+", "", some_txt)

# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)                            

# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)

# remove html links
some_txt = gsub("http\\w+", "", some_txt)

# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)



#convert some_txt into a corpus and remove stopwords and punctuations
corp=Corpus(VectorSource(some_txt)) 
as.VCorpus(corp)
corp=tm_map(corp, removeWords, stopwords('english'))
inspect(corp)
corp <- tm_map(corp, removePunctuation)                              
inspect(corp)                                                                                          

#create the bigrams and Term document matrix
BigramTokenizer <- function(y) NGramTokenizer(y, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
m= inspect(tdm)




library(wordcloud)
set.seed(1234)
v = sort(rowSums(m), decreasing = TRUE)
#you can change the frequency of the terms to a number of your choice.
wordcloud(names(v), v, min.freq = 10)


DF <- as.data.frame(m, stringsAsFactors = FALSE)
nrow(DF)
DF=as.matrix(DF)
tdf=as.data.frame(t(DF))
tdf=cbind(tdf,input[,2])
#continued on next page
len=ncol(tdf)
header=c(1:(len-1))
header=c(header,"Class")
colnames(tdf)=header

trainset <- tdf[1:1800,]
testset <- tdf[1801:2000,]

svm.model <- svm(Class ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-len])

pred <- factor(svm.pred)
xtab <- table(pred, testset[,len])
confusionMatrix(xtab)
