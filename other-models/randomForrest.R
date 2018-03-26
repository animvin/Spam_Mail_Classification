#install.packages("randomForest")
require(randomForest)



df <- read_excel("E:/Courses/5604-Predictive/project/Complete.xlsx")
df$spam <- factor(df$spam,levels=unique(df$spam))
colnames(df)
colnames(df)[49] <- "char_freq_semi_colon"
colnames(df)[50] <- "char_freq_l_round_bracket"
colnames(df)[51] <- "char_freq_l_square_bracket"
colnames(df)[52] <- "char_freq_exclamation"
colnames(df)[53] <- "char_freq_dollar"
colnames(df)[53] <- "char_freq_dollar"
colnames(df)[54] <- "char_freq_hash"
#set.seed(99)
split <- sample.split(df, SplitRatio = 0.9)
train_set <- subset(df, split == TRUE) 
validation_set <- subset(df, split == FALSE) 

# model = randomForest(train_set$spam ~., data = train_set,replace =TRUE, ntree=100,sampsize = nrow(train_set))

t_data=train_set[,-which(names(train_set) %in% c("spam"))]
model <- randomForest(t_data,train_set$spam, data = train_set,replace =TRUE,ntree=100,sampsize = nrow(train_set), importance = TRUE)
plot(model)
model
varImpPlot(model)
summary(model)

df_prob = predict(model,df,type = "prob")
df=data.frame(df,"prediction"=iflse(df_prob[,1]))

t_prob = predict(model,train_set,type = "prob")
train_set$prob =t_prob[,1]

v_prob = predict(model,validation_set,type = "prob")
validation_set$prob =v_prob[,1]

getTreeROC(validation_set$prob,validation_set$spam)

getTreeROC(train_set$prob,train_set$spam)


getMissclassificationRate(validation_set$prob,validation_set$spam,0.5)
getMissclassificationRate(train_set$prob,train_set$spam,0.5)


getAsymmetricMissclassificationRate(validation_set$prob,validation_set$spam,0.6)
getAsymmetricMissclassificationRate(train_set$prob,train_set$spam,0.6)
a= rbind(train_set,validation_set)
getAsymmetricMissclassificationRate(a$prob,a$spam,0.6)
nrow(a)


getFalsePositiveRate(validation_set$prob,validation_set$spam,0.6)
getFalsePositiveRate(train_set$prob,train_set$spam,0.6)

getFalsePositiveRate(validation_set$prob,validation_set$spam,0.7)
getFalsePositiveRate(train_set$prob,train_set$spam,0.7)


k = 10
n=floor(nrow(df)/k)
roc=rep(NA,k)
amr=rep(NA,k)

for (i in 1:k){
  s1=((i-1)*n+1)
  s2=(n*i)
  sub=s1:s2
  train=df[-sub,]
  test=df[sub,]
  model = randomForest(train$spam ~., data = train,replace =TRUE, ntree=100,sampsize = nrow(train))
  test$prediction=predict(model,test,type = "prob")[,1]
  #roc[i]= getTreeROC( test$prediction,test$spam)
  amr[i]=getAsymmetricMissclassificationRate( test$prediction,test$spam,0.6)[2]
}
plot(amr)
CV_AMR = n*sum(amr)/nrow(df)
CV_AMR

#rf1 <- randomForest(y~., data=df, mtry=2, ntree=50, importance=TRUE)<br>importance(rf1,type=1)<br><br># run the party implementation<br>library(party)<br>cf1 <- cforest(y~.,data=df,control=cforest_unbiased(mtry=2,ntree=50))<br>varimp(cf1)<br>varimp(cf1,conditional=TRUE)<br>