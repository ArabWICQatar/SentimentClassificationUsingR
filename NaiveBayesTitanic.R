
library(e1071)
library(caret) 
data(Titanic)

#convert the Titanic dataset into a dataframe and remove the unwanted frequency attribute
input=as.data.frame(Titanic)
input=input[,-5]

#split into train and test sets
index <- 1:nrow(input)
testindex <- sample(index, trunc(length(index)/2))
testset <- input[testindex,]
trainset <- input[-testindex,]

#create the naivebayes model
nb.model <- naiveBayes(Survived ~ ., data = trainset, cost = 100, gamma = 1)
nb.pred <- predict(nb.model, testset[,-4])

#create the confusion matrix
pred <- factor(nb.pred)
xtab <- table(pred, testset[,4])
confusionMatrix(xtab)
