#install.packages("neuralnet")
require(neuralnet)


df <- read_excel("E:/Courses/5604-Predictive/project/Complete.xlsx")

max = apply(df , 2 , max)
min = apply(df, 2 , min)
scaled = as.data.frame(scale(df, center = min, scale = max - min))

set.seed(99)
split <- sample.split(scaled, SplitRatio = 0.6)
train_set <- subset(scaled, split == TRUE) 
validation_set <- subset(scaled, split == FALSE) 
