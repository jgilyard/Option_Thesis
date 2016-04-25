library(ggplot2); library(caret)
setwd("C:/Users/gilyja12/Documents/Senior Thesis/Option_Thesis")
Data <- read.csv("Data_1_No_Header.csv", header = TRUE, sep = ",")
#creates vector for taking STD_Devs
x <- length(Data)
#Data <- na.omit(Data)
N_Matrix <- as.matrix(sapply(Data,as.numeric))
# 75% sampling algorithm

#additional information
Std_Dev_10 <- vector(mode="double", length=x-1)
Std_Dev_5 <- vector(mode="double", length=x-1)
Std_Dev_2 <- vector(mode="double", length=x-1)
Std_Dev_1 <- vector(mode="double", length=x-1)
Std_Dev_6M <- vector(mode="double", length=x-1)
Std_Dev_3M <- vector(mode="double", length=x-1)
#10 Years
for(i in 2:x)
{
  Std_Dev_10[i-1] <- sd(Data[,i])
}
#5 Years
for(i in 2:x)  
{
  temp <- Data[,i]
  temp <-temp[(x/2):x]
  Std_Dev_5[i-1] <- sd(temp)
}

# 2 YEars
for(i in 2:x)
{
  temp <- Data[,i]
  temp <-temp[(x-(x/5)):x]
  Std_Dev_2[i-1] <- sd(temp)
}

#1 Year
for(i in 2:x)
{
  temp <- Data[,i]
  temp <-temp[(x-(x/10)):x]
  Std_Dev_1[i-1] <- sd(temp)
}

#6 Months
for(i in 2:x)
{
  temp <- Data[,i]
  temp <-temp[(x-(x/20)):x]
  Std_Dev_6M[i-1] <- sd(temp)
}

#3Months
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

#makes variable cutoff for each equity time period
#test at 10%...
M3_CUTOFF = .05 #originally 5
M6_CUTOFF = .10
Y1_CUTOFF = .20
Y2_CUTOFF = .40
Y5_CUTOFF = .80
Y10_CUTOFF = .96
M3_NUM = 0 
M6_NUM = 0
Y1_NUM =0
Y2_NUM = 0
Y5_NUM = 0
Y10_NUM = 0

M3_HIGH <- vector(length=0)
M6_HIGH <- vector(length=0)
Y1_HIGH <- vector(length=0)
Y2_HIGH <- vector(length=0)
Y5_HIGH <- vector(length=0)
Y10_HIGH <- vector(length=0)

#Data[Value, Company]
#Data[row,col]
#gives number of Equties for low vol by time frame
dates <- nrow(Data)

for(i in 2:x)
{
  temp <-Data[(dates -(dates/40)):dates, i]
  threshold <- Data[(dates -(dates/40)), i]
  for(j in dates/40)
  {
    if(((threshold *(1 - M3_CUTOFF)) > (temp[j])) || ((threshold * (1 + M3_CUTOFF)) < (temp[j])))
    {
      
      M3_NUM = M3_NUM +1
      M3_HIGH[M3_NUM] = colnames(Data[i])
      break
    }
  }
}

for(i in 2:x)
{
  temp <-Data[(dates -(dates/20)):dates, i]
  threshold <- Data[(dates -(dates/20)), i]
  for(j in dates/20)
  {
    if(((threshold *(1 - M6_CUTOFF)) > (temp[j])) || ((threshold * (1 + M6_CUTOFF)) < (temp[j])))
    {
      M6_NUM = M6_NUM +1
      M6_HIGH[M6_NUM] = colnames(Data[i])
      break
    }
  }
}

for(i in 2:x)
{
  temp <-Data[(dates -(dates/10)):dates, i]
  threshold <- Data[(dates -(dates/10)), i]
  for(j in x/10)
  {
    if(((threshold *(1 - Y1_CUTOFF)) > (temp[j])) || ((threshold * (1 + Y1_CUTOFF)) < (temp[j])))
    {
      Y1_NUM = Y1_NUM +1
      Y1_HIGH[Y1_NUM] = colnames(Data[i])
      break
    }
  }
}

for(i in 2:x)
{
  temp <-Data[(dates -(dates/5)):dates, i]
  threshold <- Data[(dates -(dates/5)), i]
  for(j in dates/5)
  {
    if(((threshold *(1 - Y2_CUTOFF)) > (temp[j])) || ((threshold * (1 + Y2_CUTOFF)) < (temp[j])))
    {
      Y2_NUM = Y2_NUM +1
      Y2_HIGH[Y2_NUM] = colnames(Data[i])
      break
    }
  }
}

for(i in 2:x)
{
  temp <-Data[(dates -(dates/2)):dates, i]
  threshold <- Data[(dates -(dates/2)), i]
  for(j in dates/2)
  {
    if(((threshold *(1 - Y5_CUTOFF)) > (temp[j])) || ((threshold * (1 + Y5_CUTOFF)) < (temp[j])))
    {
      Y5_NUM = Y5_NUM +1
      Y5_HIGH[Y5_NUM] = colnames(Data[i])
      break
    }
  }
}

for(i in 2:x)
{
  temp <-Data[1:dates,i]
  threshold <- Data[(2),i]
  for(j in dates)
  {
    if(((threshold *(1 - Y10_CUTOFF)) > (temp[j])) || ((threshold * (1 + Y10_CUTOFF)) < (temp[j])))
    {
      Y10_NUM = Y10_NUM +1
      Y10_HIGH[Y10_NUM] = colnames(Data[i])
      break
    }
  }
}


#make is_low_vol vs isn_low_vol based upon the cutoffs

T_F_Low_3m <- matrix(nrow = x, ncol =2)
colnames(T_F_Low_3m) <-c("Equity", "M3_LOW_VOL")
runner = 1
for( i in 2:x)
{
  for (j in 1:M3_NUM) {
    
    if(identical(colnames(Data[i]),M3_HIGH[j]))
    {
      T_F_Low_3m[runner,1] = colnames(Data[i])
      T_F_Low_3m[runner,2] = FALSE
      break
    }
    else
    {
      T_F_Low_3m[runner,1] = colnames(Data[i])
      T_F_Low_3m[runner,2] = TRUE
    }
  }
  runner = runner +1
}

T_F_Low_6m <- matrix(nrow = x, ncol =2)
colnames(T_F_Low_6m) <-c("Equity", "M6_LOW_VOL")
runner = 1
for( i in 2:x)
{
  for (j in 1:M6_NUM) {
    
    if(identical(colnames(Data[i]),M6_HIGH[j]))
    {
      T_F_Low_6m[runner,1] = colnames(Data[i])
      T_F_Low_6m[runner,2] = FALSE
      break
    }
    else
    {
      T_F_Low_6m[runner,1] = colnames(Data[i])
      T_F_Low_6m[runner,2] = TRUE
    }
  }
  runner = runner +1
}

T_F_Low_1y <- matrix(nrow = x, ncol =2)
colnames(T_F_Low_1y) <-c("Equity", "Y1_LOW_VOL")
runner = 1
for( i in 2:x)
{
  for (j in 1:Y1_NUM) {
    
    if(identical(colnames(Data[i]),Y1_HIGH[j]))
    {
      T_F_Low_1y[runner,1] = colnames(Data[i])
      T_F_Low_1y[runner,2] = FALSE
      break
    }
    else
    {
      T_F_Low_1y[runner,1] = colnames(Data[i])
      T_F_Low_1y[runner,2] = TRUE
    }
  }
  runner = runner +1
}

output_holder <-(data.frame(T_F_Low_3m,T_F_Low_6m,T_F_Low_1y))
names(output_holder) <- c("M3_LOW_VOL", "M6_LOW_VOL","Y1_LOW_VOL")
write.csv(output_holder,"list.csv", row.names = FALSE)



M3_NUM  
M6_NUM 
Y1_NUM
Y2_NUM 
Y5_NUM 
Y10_NUM 