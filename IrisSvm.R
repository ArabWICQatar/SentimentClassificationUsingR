
library(e1071)
library(caret) 
data("iris")
input=iris;

# split the dataset by half into train and test set
index <- 1:nrow(input)
testindex <- sample(index, trunc(length(index)/2))
testset <- input[testindex,]
trainset <- input[-testindex,]

# create the SVM model 
svm.model <- svm(Species ~ ., data = trainset)
svm.pred <- predict(svm.model, testset[,-5])


#create the confusion matrix
pred <- factor(svm.pred)
xtab <- table(pred, testset[,5])
confusionMatrix(xtab)

#visualize the model
plot(svm.model, iris, Petal.Width ~ Petal.Length)
