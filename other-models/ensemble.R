## try creaing asymmetric error function for model creation


require(xgboost)
#install.packages("gbm")



df <- read_excel("E:/Courses/5604-Predictive/project/Complete.xlsx")
set.seed(99)
split <- sample.split(df, SplitRatio = 0.7)
train_set <- subset(df, split == TRUE) 
validation_set <- subset(df, split == FALSE) 

t_data=train_set[,-which(names(train_set) %in% c("spam"))]
boost_tree = xgboost(data = data.matrix(t_data),label = train_set$spam,eta = 0.1,max_depth = 15,nround=25,subsample = 0.5,
                     colsample_bytree = 0.5,eval_metric = "error",seed = 1,objective = "reg:logistic",nthread = 3)

train_set$prob = predict(boost_tree,data.matrix(train_set[,-length(colnames(train_set))]))

validation_set$prob = predict(boost_tree,data.matrix(validation_set[,-length(colnames(validation_set))]))


getTreeROC(validation_set$prob,validation_set$spam)

getTreeROC(train_set$prob,train_set$spam)


getMissclassificationRate(validation_set$prob,validation_set$spam,0.5)
getMissclassificationRate(train_set$prob,train_set$spam,0.5)

getAsymmetricMissclassificationRate(validation_set$prob,validation_set$spam,0.5)
getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,0.5)
a= rbind(train_set,validation_set)
getAsymmetricMissclassificationRate(a$prob,a$spam,0.5)
nrow(a)

getFalsePositiveRate(validation_set$prob,validation_set$spam,0.5)
getFalsePositiveRate(train_set$prob,train_set$spam,0.5)

getFalsePositiveRate(validation_set$prob,validation_set$spam,0.6)
getFalsePositiveRate(train_set$prob,train_set$spam,0.6)

getFalsePositiveRate(validation_set$prob,validation_set$spam,0.7)
getFalsePositiveRate(train_set$prob,train_set$spam,0.7)


model <- xgb.dump(boost_tree, with_stats = TRUE)
names <- dimnames(data.matrix(t_data))[[2]]
names
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = boost_tree)
# Nice graph
xgb.plot.importance(importance_matrix[1:20,])

library(readr)
test_op <- read_csv("E:/test/sample_op.csv")
colnames(test_op) <- colnames(df)
test_op$prob = predict(boost_tree,data.matrix(test_op[,-length(colnames(test_op))]))
test_op$op=ifelse(test_op$prob >= 0.5,1,0)

getMissclassificationRate(test_op$prob,test_op$spam,0.5)
getTreeROC(test_op$prob,test_op$spam)

getAsymmetricMissclassificationRate(test_op$prob,test_op$spam,0.5)

getFalsePositiveRate(test_op$prob,test_op$spam,0.5)

getAsymmetricMissclassificationRate(test_op$prob,test_op$spam,0.6)

getFalsePositiveRate(test_op$prob,test_op$spam,0.6)

getAsymmetricMissclassificationRate(test_op$prob,test_op$spam,0.7)

getFalsePositiveRate(test_op$prob,test_op$spam,0.7)


k = 100
n=floor(nrow(df)/k)
roc=rep(NA,k)
amr=rep(NA,k)

for (i in 1:k){
  s1=((i-1)*n+1)
  s2=(n*i)
  sub=s1:s2
  train=df[-sub,]
  test=df[sub,]
  t_data=train[,-which(names(train) %in% c("spam"))]
  model = xgboost(data = data.matrix(t_data),label = train$spam,eta = 0.1,max_depth = 15,nround=25,subsample = 0.5,
                  colsample_bytree = 0.5,eval_metric = "error",seed = 1,objective = "reg:logistic",nthread = 3)
  test$prediction = predict(model,data.matrix(test[,-length(colnames(test))]))
  #roc[i]= getTreeROC( test$prediction,test$spam)
  amr[i]=getAsymmetricMissclassificationRate( test$prediction,test$spam,0.5)[2]
}
plot(amr)
CV_AMR = n*sum(amr)/nrow(df)
CV_AMR
# getAsymmetricMissclassificationRate1 <- function(prob.column,calssification.column,cutoff){
#   results=data.frame("prob"=prob.column,"classification"=calssification.column)
#   results$new=ifelse(results$prob >= cutoff,1,0)
#   results$match=ifelse(results$new==results$classification,0,ifelse(results$classification==0,1,0.1))
#   x=sum(results$match)/nrow(results)
#   0
# }
# 
# 
# 
# df <- read_excel("E:/Courses/5604-Predictive/project/Complete.xlsx")
# set.seed(99)
# split <- sample.split(df, SplitRatio = 0.7)
# train_set <- subset(df, split == TRUE) 
# validation_set <- subset(df, split == FALSE) 
# 
# t_data=train_set[,-which(names(train_set) %in% c("spam"))]
# evalerror <- function(preds, dtrain) {
#   labels <- getinfo(dtrain, "label")
#   err <- getAsymmetricMissclassificationRate1(preds,labels,0.5)
#   # labels <- getinfo(dtrain, "label")
#   # err <- as.numeric(sum(labels != (preds > 0)))/length(labels)
#   return(list(metric = "error", value = err))
# }
# 
# param <- list(max_depth=15, eta=0.1, nthread = 3,subsample = 0.5,colsample_bytree = 0.5,seed = 1,silent=0,
#               objective = "reg:logistic")
# dtrain = xgb.DMatrix(data.matrix(t_data), label = train_set$spam)
# bst <- xgb.train(param, dtrain,nrounds=25,feval = evalerror)
# 
# train_set$prob = predict(bst,data.matrix(train_set[,-length(colnames(train_set))]))
# 
# validation_set$prob = predict(bst,data.matrix(validation_set[,-length(colnames(validation_set))]))
# 
# 
# getTreeROC(validation_set$prob,validation_set$spam)
# getTreeROC(train_set$prob,train_set$spam)
# 
# 
# 
# getMissclassificationRate(validation_set$prob,validation_set$spam,0.5)
# getMissclassificationRate(train_set$prob,train_set$spam,0.5)
# 
# getAsymmetricMissclassificationRate(validation_set$prob,validation_set$spam,0.5)
# getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,0.5)
# 
# getFalsePositiveRate(validation_set$prob,validation_set$spam,0.5)
# getFalsePositiveRate(train_set$prob,train_set$spam,0.5)
# 
# getFalsePositiveRate(validation_set$prob,validation_set$spam,0.6)
# getFalsePositiveRate(train_set$prob,train_set$spam,0.6)
# 
# getFalsePositiveRate(validation_set$prob,validation_set$spam,0.7)
# getFalsePositiveRate(train_set$prob,train_set$spam,0.7)
