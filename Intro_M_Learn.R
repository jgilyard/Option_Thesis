library(ggplot2); library(caret);
setwd("C:/Users/gilyja12/Documents/Senior Thesis/Option_Thesis")
Data <- read.csv("CFM.csv", header = TRUE, sep = ",")
Data[is.na(Data)]<-0
samp1 <- Data[,1]
rownames(Data)<- samp1
Data[,1]<-NULL
intrain <- createDataPartition(y=Data$M3_LOW_VOL, p=.75, list=FALSE)
Data$M3_LOW_VOL = as.factor(Data$M3_LOW_VOL)
training <- Data[intrain,]
testing <- Data[-intrain,]
summary(training)
trainControl(method = "LGOCV",preProcOptions = list(thresh =.9, ICAcomp =3))
featurePlot(x=training[,c("PB","DIV_PAY", "PEG")], y=training$MKT_TO_BOOK, plot="pairs")
test_1 <- train(M3_LOW_VOL~.,model="glm", data=training)
test_2 <- train(M3_LOW_VOL~.,model="rt", data=training)
test_3 <- train(M3_LOW_VOL~.,model="LogitBoost", data=training)
test_4 <- train(M3_LOW_VOL~.,model="gbm", data=training)
predictions_1 <-predict(test_1, newdata=testing)
predictions_2 <-predict(test_2, newdata=testing)
predictions_3 <-predict(test_3, newdata=testing)
predictions_4 <-predict(test_4, newdata=testing)
confusionMatrix(predictions_1,testing$M3_LOW_VOL)
confusionMatrix(predictions_2,testing$M3_LOW_VOL)
confusionMatrix(predictions_3,testing$M3_LOW_VOL)
confusionMatrix(predictions_4,testing$M3_LOW_VOL)
sd(training$Free_CASH)


