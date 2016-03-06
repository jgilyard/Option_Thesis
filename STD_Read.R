library(ggplot2) 
setwd("C:/Users/gilyja12/Documents/Senior Thesis")
Data <- read.csv("Data_1_No_Header.csv", header = TRUE, sep = ",")
#creates vector for taking STD_Devs
x <- length(Data)
#Data <- na.omit(Data)
N_Matrix <- as.matrix(sapply(Data,as.numeric))
Std_Dev_10 <- vector(mode="double", length=x-1)
Std_Dev_5 <- vector(mode="double", length=x-1)
Std_Dev_2 <- vector(mode="double", length=x-1)
Std_Dev_1 <- vector(mode="double", length=x-1)
Std_Dev_6M <- vector(mode="double", length=x-1)
Std_Dev_3M <- vector(mode="double", length=x-1)
for(i in 2:x)
{
Std_Dev_10[i-1] <- sd(Data[,i])
}

for(i in 2:x)  
{
temp <- Data[,i]
temp <-temp[(x/2):x]
Std_Dev_5[i-1] <- sd(temp)
}

for(i in 2:x)
{
temp <- Data[,i]
temp <-temp[(x-(x/5)):x]
Std_Dev_2[i-1] <- sd(temp)
}

for(i in 2:x)
{
temp <- Data[,i]
temp <-temp[(x-(x/10)):x]
Std_Dev_1[i-1] <- sd(temp)
}

for(i in 2:x)
{
temp <- Data[,i]
temp <-temp[(x-(x/20)):x]
Std_Dev_6M[i-1] <- sd(temp)
}

for(i in 2:x)
{
temp <- Data[,i]
temp <-temp[(x -(x/40)):x]
Std_Dev_3M[i-1] <- sd(temp)
}

plot(density(Std_Dev_3M), xlab="Vols measured in Standard Deviations",main = "Denisty Plots for 3 Month - 10 Year Equity Volatility", lwd=2)

lines(density(Std_Dev_6M), col="blue", lwd=2)
lines(density(Std_Dev_1), col="green", lwd=2)
lines(density(Std_Dev_2), col="orange", lwd=2)
lines(density(Std_Dev_5), col="red", lwd=2)
lines(density(Std_Dev_10), col="darkblue", lwd=2)
legend(20, .8, legend=c("3M", "6M","1Y","2Y","5Y","10Y"), col=c("black","blue","green","orange","red","darkblue"), lty=1,cex=0.8)

##Vectors for the 50 Lowest Vol Equitieis
order_3M <- vector(mode="double", length=50)
min_vec_3M <- head(sort(Std_Dev_3M),50)


# Save here ----
for(i in 1:50)
{
temp_loc <- match(min_vec_3M[i],Std_Dev_3M)
order_3M[i] <- colnames(Data[temp_loc +1])
}

order_6M <- vector(mode="double", length=50)
min_vec_6M <- head(sort(Std_Dev_6M),50)

for(i in 1:50)
{
temp_loc <- match(min_vec_6M[i],Std_Dev_6M)
order_6M[i] <- colnames(Data[temp_loc +1])
}

order_1 <- vector(mode="double", length=50)
min_vec_1 <- head(sort(Std_Dev_1),50)

for(i in 1:50)
{
temp_loc <- match(min_vec_1[i],Std_Dev_1)
order_1[i] <- colnames(Data[temp_loc +1])
}

order_2 <- vector(mode="double", length=50)
min_vec_2 <- head(sort(Std_Dev_2),50)

for(i in 1:50)
{
temp_loc <- match(min_vec_2[i],Std_Dev_2)
order_2[i] <- colnames(Data[temp_loc +1])
}

order_5 <- vector(mode="double", length=50)
min_vec_5 <- head(sort(Std_Dev_5),50)

for(i in 1:50)
{
temp_loc <- match(min_vec_5[i],Std_Dev_5)
order_5[i] <- colnames(Data[temp_loc +1])
}

order_10 <- vector(mode="double", length=50)
min_vec_10 <- head(sort(Std_Dev_10),50)

for(i in 1:50)
{
temp_loc <- match(min_vec_10[i],Std_Dev_10)
order_10[i] <- colnames(Data[temp_loc +1])
}

dev.copy(png,"myfile.png",width=8,height=6,units="in",res=100)
dev.off()
