library(ggplot2); library(caret);
setwd("C:/Users/gilyja12/Documents/Senior Thesis/Option_Thesis")
Data <- read.csv("CFM.csv", header = TRUE, sep = ",")
Data[is.na(Data)]<-0
samp1 <- Data[,1]
rownames(Data)<- samp1
Data[,1]<-NULL
intrain <- createDataPartition(y=Data$M3_LOW_VOL, p=.75, list=FALSE)
Data$M3_LOW_VOL = as.factor(Data$M3_LOW_VOL)
#Add Preprocessing Average by dividing mean by standard deveiaion
for( i in 1:10)
{
  #Data[,i] <- (Data[,i]- mean(Data[,i]))/sd(Data[,i])
  Data[,i] = sqrt(Data[,i])
}
Data[is.na(Data)]<-0
training <- Data[intrain,]
testing <- Data[-intrain,]
#training <- preProcess(training[,-11], method=c("center","scale"))
#testing <- preProcess(testing[,-11], method=c("center","scale"))
summary(training)
ctrl <- trainControl(method = "LGOCV", savePredictions= T, preProcOptions = list(thresh =.9, ICAcomp =3))
featurePlot(x=training[,c("PB","DIV_PAY", "PEG")], y=training$MKT_TO_BOOK, plot="pairs")
test_1 <- train(M3_LOW_VOL~.,model="glm", data=training, trControl=ctrl)
test_2 <- train(M3_LOW_VOL~.,model="rt", data=training, trControl=ctrl)
test_3 <- train(M3_LOW_VOL~.,model="LogitBoost", data=training, trControl=ctrl)
test_4 <- train(M3_LOW_VOL~.,model="gbm", data=training, trControl=ctrl)
#predictions_1 <-predict(test_1, newdata=testing, type="prob")
predictions_1 <-predict(test_1, newdata=testing)
predictions_2 <-predict(test_2, newdata=testing)
predictions_3 <-predict(test_3, newdata=testing)
predictions_4 <-predict(test_4, newdata=testing)
confusionMatrix(predictions_1,testing$M3_LOW_VOL)
confusionMatrix(predictions_2,testing$M3_LOW_VOL)
confusionMatrix(predictions_3,testing$M3_LOW_VOL)
confusionMatrix(predictions_4,testing$M3_LOW_VOL)
predictions_1 = as.numeric(predictions_1)
predictions_2 = as.numeric(predictions_2)
predictions_3 = as.numeric(predictions_3)
predictions_4 = as.numeric(predictions_4)
testing$M3_LOW_VOL = as.numeric(testing$M3_LOW_VOL)
library(pROC)
plot.roc(testing$M3_LOW_VOL,predictions_1)
sd(training$Free_CASH)


