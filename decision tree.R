## Removing observation column and converting 1/0 to characters 
German_Credit <- German.Credit[ c(-1)]
For (i in 1: nrow(German_Credit))
{if (German_Credit$RESPONSE[i] == 1)
{German_Credit$RESPONSE[i] = 'POSITIVE'}
  Else {German_Credit$RESPONSE[i] = 'NEGATIVE'}
}
## Making target variable "RESPONSE" factor
German_Credit$RESPONSE = factor(German_Credit$RESPONSE)
str(German_Credit)
#installing package to run tree 
install.packages ("party")
library(party)
credit_tree = ctree (RESPONSE ~., data = German_Credit)
summary(credit_tree)
table(predict(credit_tree), German_Credit$RESPONSE)
## building matrix 
#NEGATIVE POSITIVE
#NEGATIVE      153       88
#POSITIVE      147      612

## error and accuracy of the model built 

#Error = 147+88 / (1000) = 23.5%
#Accuracy =1- error = 76.5%
#Accuracy of positive class = 612/700 = 87%
#Accuracy of negative class = 153/300 = 50% 

# put the print of the plot here

plot(credit_tree, type='simple',inner_panel=node_inner(credit_tree,abbreviate = TRUE, pval = FALSE, id = FALSE), terminal_panel=node_terminal(credit_tree,abbreviate = TRUE,digits = 1, fill = c('white'), id = FALSE))

#Next consider developing a model for prediction. For this, we should divide the data into Training and Validation sets.  Consider a partition of the data into 50% for Training and 50% for Test. What model performance do you obtain? Is the model reliable (why or why not)?  Consider partitions of the data into 70% for Training and 30% for Test, and 80% for Training and 20% for Test and report on model and performance comparisons. Feel free to experiment with other size partitions on the data. 
##Used 70% 30% with rpart function using "Gini"
library(rpart)
credit_tree1 = rpart(RESPONSE ~., data =trainData,ontrol = rpart.control(minsplit=10),parms = list(split="gini"))
plot(credit_tree1)
printcp(credit_tree1)
##output
#Root node error: 192/698 = 0.27507
## Confusion Matrix
library(caret)
result = confusionMatrix(predict(credit_tree1, type = 'class', newdata = testData), testData$RESPONSE)

#Reference
#Prediction NEGATIVE POSITIVE
#NEGATIVE       39       20
#POSITIVE       69      174

#Accuracy: 0.7053 

