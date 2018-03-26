library(readxl)
library(ROCR)
library(caTools)

getTreeROC <- function(prob.column,classification.columnn){
  pr <- prediction(prob.column, classification.columnn)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  auc <- performance(pr, measure = "auc")
  auc@y.values[[1]]
}


getMissclassificationRate <- function(prob.column,calssification.column,cutoff){
  results=data.frame("prob"=prob.column,"classification"=calssification.column)
  results$new=ifelse(results$prob >= cutoff,1,0)
  results$match=ifelse(results$new==results$classification,0,1)
  sum(results$match)/nrow(results)
}

getAsymmetricMissclassificationRate <- function(prob.column,calssification.column,cutoff){
  results=data.frame("prob"=prob.column,"classification"=calssification.column)
  results$new=ifelse(results$prob >= cutoff,1,0)
  results$match1=ifelse(results$new==results$classification,0,1)
  results$match=ifelse(results$new==results$classification,0,ifelse(results$classification==0,1,0.1))
  c(sum(results$match1)/nrow(results),sum(results$match)/nrow(results))
}

getFalsePositiveRate <- function(prob.column,calssification.column,cutoff){
  results=data.frame("prob"=prob.column,"classification"=calssification.column)
  results$new=ifelse(results$prob >= cutoff,1,0)
  results$match=ifelse(results$new==results$classification,0,1)
  results$fp=ifelse((results$new==1) & (results$classification==0),1,0)
  c(sum(results$match)/nrow(results),sum(results$fp)/nrow(results))
}
#Utility function to get ROC value
getROC <- function(data,model,classification.columnn){
  p <- getPredictedProb(data,model)
  pr <- prediction(p, classification.columnn)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  auc <- performance(pr, measure = "auc")
  auc@y.values[[1]]
}
#Function to get the predicted probability
getPredictedProb <- function(data,model){
  predict(model, newdata=data, type="response")
}
######Columns Renamed to remove special characters in them#######
df <- read_excel("E:/Courses/5604-Predictive/project/Complete.xlsx")
df$spam <- factor(df$spam,levels=unique(df$spam))
#colnames(df)
colnames(df)[49] <- "char_freq_semi_colon"
colnames(df)[50] <- "char_freq_l_round_bracket"
colnames(df)[51] <- "char_freq_l_square_bracket"
colnames(df)[52] <- "char_freq_exclamation"
colnames(df)[53] <- "char_freq_dollar"
colnames(df)[53] <- "char_freq_dollar"
colnames(df)[54] <- "char_freq_hash"

df <- read_excel("E:/Courses/5604-Predictive/project/Complete.xlsx")

result=data.frame("Iteration"=0,"ROC.Train"=0.00,"ROC.Validation"=0.00)

model=step(glm(train_set$spam~.,data=train_set,family = binomial("logit")),data = train_set, direction = "backward")


index=0
for(i in 1:1000){
  index=index+1;
  set.seed(index)
  split <- sample.split(df, SplitRatio = 0.75)
  #get training and validation data
  train_set <- subset(df, split == TRUE) 
  validation_set <- subset(df, split == FALSE) 
  validation.ROC=getROC(validation_set,model,validation_set$spam)
  sub.train.ROC=getROC(train_set,model,train_set$spam)
  result=rbind(result,data.frame("Iteration"=index,"ROC.Train"=sub.train.ROC,"ROC.Validation"=validation.ROC))
}
result=result[-1,]
plot(result$ROC.Validation)
validation_set$prob =  predict(model, validation_set, type="response")
train_set$prob =  predict(model, train_set, type="response")
getAsymmetricMissclassificationRate(validation_set$prob,validation_set$spam,0.5)
getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,0.5)

a= rbind(train_set,validation_set)
getAsymmetricMissclassificationRate(a$prob,a$spam,0.5)
nrow(a)

result=data.frame("Iteration"=0,"ROC.Train"=0.00,"ROC.Validation"=0.00)

index=0
for(i in 1:10){
  index=index+1;
  set.seed(index)
  split <- sample.split(df, SplitRatio = 0.75)
  #get training and validation data
  train_set <- subset(df, split == TRUE) 
  validation_set <- subset(df, split == FALSE) 
  model_1=step(glm(train_set$spam~.,data=train_set),data = train_set, direction = "backward")
  validation.ROC=getROC(validation_set,model_1,validation_set$spam)
  sub.train.ROC=getROC(train_set,model_1,train_set$spam)
  result=rbind(result,data.frame("Iteration"=index,"ROC.Train"=sub.train.ROC,"ROC.Validation"=validation.ROC))
}
result=result[-1,]
plot(result$ROC.Validation)


result=data.frame("Iteration"=0,"ROC.Train"=0.00,"ROC.Validation"=0.00)

index=0
for(i in 1:10){
  index=index+1;
  set.seed(index)
  split <- sample.split(df, SplitRatio = 0.75)
  #get training and validation data
  train_set <- subset(df, split == TRUE) 
  validation_set <- subset(df, split == FALSE) 
  model_1=step(glm(train_set$spam~.,data=train_set),data = train_set, direction = "backward")
  validation.MS=getMissclassificationRate(getPredictedProb(validation_set,model),validation_set$spam,0.5)
  sub.train.MS=getMissclassificationRate(getPredictedProb(train_set,model),train_set$spam,0.5)
  result=rbind(result,data.frame("Iteration"=index,"ROC.Train"=sub.train.MS,"ROC.Validation"=validation.MS))
}
result=result[-1,]
plot(result$ROC.Validation)
plot(result$ROC.Train)


######## Learning Curve ############

result=data.frame("Iteration"=0,"ROC.Train"=0.00,"ROC.Validation"=0.00)
splits=seq(0.1,0.9,0.05)
index=0
for(split in splits){
  index=index+1;
  set.seed(1)
  split <- sample.split(df, SplitRatio =split)
  #get training and validation data
  train_set <- subset(df, split == TRUE) 
  validation_set <- subset(df, split == FALSE) 
  model_1=step(glm(train_set$spam~.,data=train_set),data = train_set, direction = "backward")
  validation.ROC=getROC(validation_set,model_1,validation_set$spam)
  sub.train.ROC=getROC(train_set,model_1,train_set$spam)
  result=rbind(result,data.frame("Iteration"=index,"ROC.Train"=sub.train.ROC,"ROC.Validation"=validation.ROC))
}
result=result[-1,]
plot(result$ROC.Validation)
plot(result$ROC.Train)



