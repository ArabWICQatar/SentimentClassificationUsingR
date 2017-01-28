#install party package and the iris dataset
library(party)
data("iris")
input = iris

#check the class of the input variable
class(input)

#create the decision tree
output.tree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)

#plot the decision tree
plot(output.tree,type="simple")

#create a verbose output of the tree
print(output.tree)