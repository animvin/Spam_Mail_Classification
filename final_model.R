#install.packages("randomForest")
require(readxl)
require(randomForest)
require(ROCR)
require(ggplot2)
require(caTools)

########### Utility Functions ##############
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
  results$match=ifelse(results$new==results$classification,0,ifelse(results$classification==0,2,0.5))
  c(sum(results$match1)/nrow(results),sum(results$match)/nrow(results))
}

getFalsePositiveRate <- function(prob.column,calssification.column,cutoff){
  results=data.frame("prob"=prob.column,"classification"=calssification.column)
  results$new=ifelse(results$prob >= cutoff,1,0)
  results$match=ifelse(results$new==results$classification,0,1)
  results$fp=ifelse((results$new==1) & (results$classification==0),1,0)
  c(sum(results$match)/nrow(results),sum(results$fp)/nrow(results))
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

#########Creating the initial Training Validation Split ##############
set.seed(99)
split <- sample.split(df, SplitRatio = 0.75)
train_set <- subset(df, split == TRUE) 
validation_set <- subset(df, split == FALSE) 


######### Random Forest Model Creation ###########
model = randomForest(train_set$spam ~., data = train_set,replace =TRUE, ntree=100,sampsize = nrow(train_set), importance = TRUE)
plot(model)
model

###########Variable Importance plot###############
varImpPlot(model)
summary(model)

########## Predictions ##########
t_prob = predict(model,train_set,type = "prob")
train_set$prob =t_prob[,1]

v_prob = predict(model,validation_set,type = "prob")
validation_set$prob =v_prob[,1]

########## Prediction on the whole 1999 data ############
df_prob = predict(model,df,type = "prob")
df=data.frame(df,"prediction"=ifelse(df_prob[,1]>=0.6,1,0))
# COnfusion Matrix on the whole Data
table(df$spam,df$prediction,dnn = list("SPAM","PREDICTED"))

#write.csv(df,"full_file_op_1999.csv",row.names = FALSE)

########## Prediction on the whole 2005 data ############

library(readr)
test_op <- read_csv("E:/test/sample_op.csv")
colnames(test_op) <- colnames(df)
test_op$prob = predict(model,test_op,type = "prob")[,1]
test_op$op=ifelse(test_op$prob >= 0.6,1,0)

getMissclassificationRate(test_op$prob,test_op$spam,0.6)
getTreeROC(test_op$prob,test_op$spam)
getAsymmetricMissclassificationRate(test_op$prob,test_op$spam,0.6)

table(test_op$spam,test_op$op,dnn = list("SPAM","PREDICTED"))
#write.csv(test_op,"full_file_op_2005.csv",row.names = FALSE)

######### Lift and ROC Curves ##########
pred <- prediction(validation_set$prob , validation_set$spam)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="Validation ROC curve", colorize=T)
#lift curve
perf <- performance(pred,"lift","rpp")
plot(perf, main="Validation lift curve", colorize=T)

pred <- prediction(train_set$prob , train_set$spam)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="Training ROC curve", colorize=T)
#lift curve
perf <- performance(pred,"lift","rpp")
plot(perf, main="Training Lift curve", colorize=T)

pred <- prediction(test_op$prob , train_set$spam)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="2005 ROC curve", colorize=T)
#lift curve
perf <- performance(pred,"lift","rpp")
plot(perf, main="2005 Lift curve", colorize=T)

#########Performance Testing #########
getTreeROC(validation_set$prob,validation_set$spam)
getTreeROC(train_set$prob,train_set$spam)


getMissclassificationRate(validation_set$prob,validation_set$spam,0.5)
getMissclassificationRate(train_set$prob,train_set$spam,0.5)


getAsymmetricMissclassificationRate(validation_set$prob,validation_set$spam,0.6)
getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,0.6)
a= rbind(train_set,validation_set)
getAsymmetricMissclassificationRate(a$prob,a$spam,0.6)
table(a$spam,ifelse(a$prob >=0.6,1,0))


getFalsePositiveRate(validation_set$prob,validation_set$spam,0.6)
getFalsePositiveRate(train_set$prob,train_set$spam,0.6)

getFalsePositiveRate(validation_set$prob,validation_set$spam,0.7)
getFalsePositiveRate(train_set$prob,train_set$spam,0.7)


######### Changing Cut-off Values #########

cutoffs = list(0.4,0.5,0.6,0.7,0.8)
train.mr = rep(NA,length(cutoffs))
validation.mr = rep(NA,length(cutoffs))
train.amr = rep(NA,length(cutoffs))
validation.amr = rep(NA,length(cutoffs))
i=0
for (cutoff in cutoffs){
  i=i+1
  validation.mr[i]=getAsymmetricMissclassificationRate(validation_set$prob,validation_set$spam,cutoff)[1]
  train.mr[i]=getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,cutoff)[1]
  validation.amr[i]=getAsymmetricMissclassificationRate(validation_set$prob,validation_set$spam,cutoff)[2]
  train.amr[i]=getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,cutoff)[2]
}

df1 = data.frame("x"=seq(0.4,0.8,0.1),"y1"=train.amr,"y2"=validation.amr)
g =ggplot(df1, aes(x)) +                   
  geom_line(aes(y=y1), colour="red") + 
  geom_line(aes(y=y2), colour="black") +
  ylab("Asymmetric Missclassifcation Rate") + xlab("Cut-Off Value")
g

############### Cross Validation ##########

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
  model = randomForest(train$spam ~., data = train,replace =TRUE, ntree=100,sampsize = nrow(train), importance = TRUE)
  test$prediction=predict(model,test,type = "prob")[,1]
  amr[i]=getAsymmetricMissclassificationRate(test$prediction,test$spam,0.6)[2]
}
plot(amr)
CV_AMR = n*sum(amr)/nrow(df)
CV_AMR
#0.05042382

######## Learning Curve ############
set.seed(234234)
result=data.frame("Iteration"=0,"AMR.TRAIN"=0.00,"AMR.Validation"=0.00)
split <- sample.split(df, SplitRatio = 0.9)
train <- subset(df, split == TRUE)
validation <- subset(df, split == FALSE) 
splits=seq(0,0.9,0.1)
index=0
for(split in splits){
  index=index+1;
  set.seed(565)
  split <- sample.split(train, SplitRatio =split)
  #get training and validation data
  train_set <- subset(train, split == TRUE) 
  model=randomForest(train_set$spam ~., data = train_set,replace =TRUE, ntree=100,sampsize = nrow(train_set), importance = TRUE)
  
  train_set$prob = predict(model,train_set,type = "prob")[,1]
  v_prob = predict(model,validation,type = "prob")[,1]

  validation.AMR=getAsymmetricMissclassificationRate(v_prob,validation$spam,0.6)[2]
  train.AMR=getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,0.6)[2]
  result=rbind(result,data.frame("Iteration"=index,"AMR.TRAIN"=train.AMR,"AMR.Validation"=validation.AMR))
}
result=result[-1,]

df1 = data.frame("x"=seq(0,0.9,0.1),"y1"=result$AMR.TRAIN,"y2"=result$AMR.Validation)
g =ggplot(df1, aes(x)) +                   
  geom_line(aes(y=y1), colour="red") + 
  geom_line(aes(y=y2), colour="black") +
  ylab("Asymmetric Missclassifcation Rate") + xlab("Portion")
g

########### Changing Various Parameters ###########

########### Changing NodeSize ###########
set.seed(99)
split <- sample.split(df, SplitRatio = 0.75)
train_set <- subset(df, split == TRUE) 
validation_set <- subset(df, split == FALSE) 

sizes = list(1,5,10,15,20)
val.ROC = rep(NA,length(sizes))
val.AMR = rep(NA,length(sizes))
train.ROC = rep(NA,length(sizes))
train.AMR = rep(NA,length(sizes))
i=0;
for (size in sizes){
  i=i+1
  model = randomForest(train_set$spam ~., data = train_set,replace =TRUE, ntree=100,sampsize = nrow(train_set), importance = TRUE,nodesize=size)
  t_prob = predict(model,train_set,type = "prob")[,1]
  v_prob = predict(model,validation_set,type = "prob")[,1]
  val.ROC[i] = getTreeROC(v_prob,validation_set$spam)
  train.ROC[i] = getTreeROC(t_prob,train_set$spam)
  
  val.AMR[i] = getAsymmetricMissclassificationRate(v_prob,validation_set$spam,0.5)[2]
  train.AMR[i] = getAsymmetricMissclassificationRate(t_prob,train_set$spam,0.5)[2]
}
par(mfrow=c(2,2))
plot(val.ROC,type="l",xlab = "Minimum Split Size" ,xaxt = "n")
axis(1,at=seq(1,5,1),labels =list(1,5,10,15,20) )
plot(val.AMR,type="l",xlab = "Minimum Split Size" ,xaxt = "n")
axis(1,at=seq(1,5,1),labels =list(1,5,10,15,20) )
plot(train.ROC,type="l",xlab = "Minimum Split Size" ,xaxt = "n")
axis(1,at=seq(1,5,1),labels =list(1,5,10,15,20) )
plot(train.AMR,type="l",xlab = "Minimum Split Size" ,xaxt = "n")
axis(1,at=seq(1,5,1),labels =list(1,5,10,15,20) )

########### Getting optimal tree size ###########
par(mfrow=c(1,1))
model_test=randomForest(train_set$spam ~., data = train_set,replace =TRUE,sampsize = nrow(train_set))
plot(model_test)
model_test.legend <- if (is.null(model_test$test$err.rate)) {colnames(model_test$err.rate)}else {colnames(model_test$test$err.rate)}
legend("top", cex =0.5, legend=model_test.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)

