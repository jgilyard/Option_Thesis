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
test_1 <- train(M3_LOW_VOL~.,model="glm", data=training)
test_2 <- train(M3_LOW_VOL~.,model="rt", data=training)
test_3 <- train(M3_LOW_VOL~.,model="LogitBoost", data=training)
test_4 <- train(M3_LOW_VOL~.,model="gbm", data=training)