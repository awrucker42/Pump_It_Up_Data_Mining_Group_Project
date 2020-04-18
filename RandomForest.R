library(randomForest)
library(ggplot2)
set.seed(100)
data <- Training_Set_Value
train <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
data[is.na(data)] = 0
data$id = 0
for(k in 1:as.integer(ncol(data)))
{
  data[,k] <- as.factor(data[,k])
  data[is.na(data)] = 0
  mean(length(levels(na.omit(data[,k]))))
  data[,k] <- as.factor(as.integer(as.integer(data[,k])/length(levels(data[,k])) * 10))
}
TrainSet <- data

model1 <- randomForest(status_group ~ ., data = TrainSet, ntree=200, mtry=6, importance = TRUE)
print(model1)

test <- Test_Set_Data
test <- as.data.frame(test)
test$id = 0
for(k in 1:as.integer(ncol(Test_Set_Data) -1 ))
{
  test[,k] <- as.factor(test[,k])
  test[is.na(test)] = 0
  test[,k] <- as.factor(as.integer(as.integer(test[,k])/length(levels(test[,k])) * 10))
}
labels <- predict(model1, data)

ggplot(data= data, mapping = aes(x = 'quantity_group',y = 'sub_village', color=ifelse(labels==data$status_group, 'Correct Prediction', 'Incorrect Prediction'))) + geom_jitter() + labs(color="Predicted Outcome") + ggtitle("Random Forest")

labels <- predict(model1, test)


i <- 1
while(i <= 14850)
{
  if(labels[i] == 3)
  {
    SubmissionFormat[i,]$status_group = 'functional'
  }
  else if(labels[i] == 6)
  {
    SubmissionFormat[i,]$status_group = 'functional needs repair'
    
  }
  else
  {
    SubmissionFormat[i,]$status_group = 'non functional'
  }
  i <- i + 1
}

write.csv(SubmissionFormat,"First_Submission.csv", row.names = FALSE)