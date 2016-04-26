library(ggplot2); library(caret);library(rpart);library(reshape2)
setwd("C:/Users/gilyja12/Documents/Senior Thesis/Option_Thesis")
Data <- read.csv("CFM.csv", header = TRUE, sep = ",")
Data[is.na(Data)]<-0
samp1 <- Data[,1]
rownames(Data)<- samp1
Data[,1]<-NULL
intrain <- createDataPartition(y=Data$SI_INT, p=.65, list=FALSE)
#Add Preprocessing Average by dividing mean by standard deveiaion
NO_PROCESS_DATA = Data
for( i in 1:10)
{
  #Data[,i] <- (Data[,i]- min(Data[,i]))/(max(Data[,i]) - min(Data[,i]))
  #Sqrt Transform our data
  Data[,i] = sqrt(Data[,i])
  #Data[,i] = log10(Data[,i])
  
}
Data[is.na(Data)]<-0
training <- Data[intrain,]
testing <- Data[-intrain,]
summary(training)
featurePlot(x=training[,c("PB","DIV_PAY", "PEG", "MKT_TO_BOOK", "QUICK_RATIO")], y=training$M3_LOW_VOL, plot="pairs")
set.seed(1834)
#Show correlation between SI and other variables compare between glm and lm
holder_1 <- glm(SI_INT~ PEG + PB + DIV_PAY + PE + Free_CASH +QUICK_RATIO +ALT_SCORE + MKT_TO_BOOK + DEBT_TO_EBITBA + M3_LOW_VOL+M6_LOW_VOL + Y1_LOW_VOL, data = training)
holder_2 <- lm(SI_INT~ PEG + PB + DIV_PAY + PE + Free_CASH +QUICK_RATIO +ALT_SCORE + MKT_TO_BOOK + DEBT_TO_EBITBA + M3_LOW_VOL+M6_LOW_VOL+ Y1_LOW_VOL,data = training)
#See different ROC Plots!
#----Starting to pickup different M3 Variables using glm and rpart Algorithms
holder_3 <- glm(M3_LOW_VOL~ + SI_INT + PB + DIV_PAY + PE + Free_CASH +QUICK_RATIO +ALT_SCORE + MKT_TO_BOOK + DEBT_TO_EBITBA + PEG+M6_LOW_VOL+ Y1_LOW_VOL, family= binomial, data = training)
holder_4 <- glm(M3_LOW_VOL~ + SI_INT + PE +QUICK_RATIO +ALT_SCORE + MKT_TO_BOOK + DEBT_TO_EBITBA + PEG+M6_LOW_VOL+ Y1_LOW_VOL, family= binomial, data = training)
holder_5 <- glm(M3_LOW_VOL~ PE +QUICK_RATIO +ALT_SCORE + PEG+M6_LOW_VOL + Y1_LOW_VOL, family= binomial, data = training)
#calculate STD of Accuracy with different seeds
holder_6 <- rpart(M3_LOW_VOL~ SI_INT + PB + DIV_PAY + PE + Free_CASH +QUICK_RATIO +ALT_SCORE + MKT_TO_BOOK + DEBT_TO_EBITBA + PEG +M6_LOW_VOL +Y1_LOW_VOL, method="class", data = training)
# Add correlation plot
library('corrplot')
summary(holder_1)
summary(holder_2)
summary(holder_3)
summary(holder_4)
summary(holder_5)
summary(holder_6)

#rpart prediction matrix---
predictions_1 <- predict(holder_6, newdata=testing,type="class")
confusionMatrix(predictions_1,testing$M3_LOW_VOL)

#----allows for data to be converted from TRUE/FALSE -> 1/0
predictions_1 = as.numeric(predictions_1)
DATA_COR <-cor(Data)
corrplot(DATA_COR,method="circle")

#convert to value and metric for comparions graph to describe why we condense
plot_data_sqrt <- melt(Data[1:5])
plot_data <-melt(NO_PROCESS_DATA[1:5])

ggplot(plot_data_sqrt,aes(x=value, fill=variable)) + geom_histogram(alpha=0.25)
ggplot(plot_data,aes(x=value, fill=variable, trim=TRUE)) + geom_histogram(alpha=0.25)
#rpart ROC Curve
library(pROC)
plot.roc(testing$M3_LOW_VOL,predictions_1)
#plot.roc(training$M3_LOW_VOL,holder_6)


