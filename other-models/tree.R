df <- read_excel("E:/Courses/5604-Predictive/project/Complete.xlsx")

library(rpart)
tree= rpart(df$spam~.,data=df,method="class")
tree= rpart(df$spam~.,data=df,method="class",control = rpart.control(minsplit = 5,cp=0.00001))
tree
printcp(tree)
plotcp(tree)
print(tree)
plot(tree)
text(tree)
summary(tree)


total.df=df
df=total.df
set.seed(99)
split <- sample.split(df, SplitRatio = 0.75)
train_set <- subset(df, split == TRUE) 
validation_set <- subset(df, split == FALSE) 
df = train_set

tree= rpart(df$spam~.,data=df,method="class")
plot(tree)
text(tree,pretty = 1)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#rattle()
fancyRpartPlot(tree)
printcp(tree)
plotcp(tree)
ptree<- prune(tree,cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]) #0.0052951
fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")

validation_set$class  <- predict(ptree, validation_set, type="class")
v_prob  <- predict(ptree, validation_set, type="prob")
validation_set$prob=v_prob[,2]


train_set$class  <- predict(ptree, train_set, type="class")
t_prob  <- predict(ptree, train_set, type="prob")
train_set$prob=t_prob[,2]

table(validation_set$spam,validation_set$class)
pred <- prediction(validation_set$prob , validation_set$spam)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="Validation ROC curve", colorize=T)



getTreeROC(validation_set$prob,validation_set$spam)

getTreeROC(train_set$prob,train_set$spam)


getMissclassificationRate(validation_set$prob,validation_set$spam,0.5)
getMissclassificationRate(train_set$prob,train_set$spam,0.5)

getMissclassificationRate(validation_set$prob,validation_set$spam,0.8)
getMissclassificationRate(train_set$prob,train_set$spam,0.8)

getAsymmetricMissclassificationRate(validation_set$prob,validation_set$spam,0.5)
getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,0.5)
a= rbind(train_set,validation_set)
getAsymmetricMissclassificationRate(a$prob,a$spam,0.5)
nrow(a)


getAsymmetricMissclassificationRate(validation_set$prob,validation_set$spam,0.8)
getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,0.8)
