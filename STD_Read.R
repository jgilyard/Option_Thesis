library(ggplot2); library(caret); data(spam)
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
M3_CUTOFF = .03
M6_CUTOFF = .06
Y1_CUTOFF = .12
Y2_CUTOFF = .24
Y5_CUTOFF = .48
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

for(i in 2:x)
{
  temp <-Data[(x -(x/40)):x, i]
  threshold <- Data[(x -(x/40)), i]
    for(j in x/40)
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
  temp <-Data[(x -(x/20)):x, i]
  threshold <- Data[(x -(x/20)), i]
  for(j in x/20)
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
  temp <-Data[(x -(x/10)):x, i]
  threshold <- Data[(x -(x/10)), i]
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
  temp <-Data[(x -(x/5)):x, i]
  threshold <- Data[(x -(x/5)), i]
  for(j in x/5)
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
  temp <-Data[(x -(x/2)):x, i]
  threshold <- Data[(x -(x/2)), i]
  for(j in x/2)
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
  temp <-Data[1:x,i]
  threshold <- Data[(2),i]
  for(j in x)
  {
    if(((threshold *(1 - Y10_CUTOFF)) > (temp[j])) || ((threshold * (1 + Y10_CUTOFF)) < (temp[j])))
    {
      Y10_NUM = Y10_NUM +1
      Y10_HIGH[Y10_NUM] = colnames(Data[i])
      break
    }
  }
}

M3_NUM  
M6_NUM 
Y1_NUM
Y2_NUM 
Y5_NUM 
Y10_NUM 

#list of vol vols top 2 quartiles. 
trunc <- floor(x/2)


##Vectors for the 2 Lowest Vol Equitieis
order_3M <- vector(mode="double", length=trunc)
min_vec_3M <- head(sort(Std_Dev_3M),trunc)


# Save here ----
#easier way is half of length of all daatra
for(i in 1:trunc)
{
temp_loc <- match(min_vec_3M[i],Std_Dev_3M)
order_3M[i] <- colnames(Data[temp_loc +1])
}

order_6M <- vector(mode="double", length=trunc)
min_vec_6M <- head(sort(Std_Dev_6M),trunc)

for(i in 1:trunc)
{
temp_loc <- match(min_vec_6M[i],Std_Dev_6M)
order_6M[i] <- colnames(Data[temp_loc +1])
}

order_1 <- vector(mode="double", length=trunc)
min_vec_1 <- head(sort(Std_Dev_1),trunc)

for(i in 1:trunc)
{
temp_loc <- match(min_vec_1[i],Std_Dev_1)
order_1[i] <- colnames(Data[temp_loc +1])
}

order_2 <- vector(mode="double", length=trunc)
min_vec_2 <- head(sort(Std_Dev_2),trunc)

for(i in 1:trunc)
{
temp_loc <- match(min_vec_2[i],Std_Dev_2)
order_2[i] <- colnames(Data[temp_loc +1])
}

order_5 <- vector(mode="double", length=trunc)
min_vec_5 <- head(sort(Std_Dev_5),trunc)

for(i in 1:trunc)
{
temp_loc <- match(min_vec_5[i],Std_Dev_5)
order_5[i] <- colnames(Data[temp_loc +1])
}

order_10 <- vector(mode="double", length=trunc)
min_vec_10 <- head(sort(Std_Dev_10),trunc)

for(i in 1:trunc)
{
temp_loc <- match(min_vec_10[i],Std_Dev_10)
order_10[i] <- colnames(Data[temp_loc +1])
}

dev.copy(png,"myfile.png",width=8,height=6,units="in",res=100)
dev.off()


#placing Cut off for low volatility equities.-----------------

cutoff_plot <- vector(mode="double", length=6)
cutoff_3M <-max(min_vec_3M)
summary(min_vec_3M)
cutoff_plot[1] <- signif(cutoff_3M,digits=3)

cutoff_6M <-max(min_vec_6M)
summary(min_vec_6M)
cutoff_plot[2] <- signif(cutoff_6M,digits=3)

cutoff_1 <-max(min_vec_1)
summary(min_vec_1)
cutoff_plot[3] <- signif(cutoff_1,digits=3)

cutoff_2 <-max(min_vec_2)
summary(min_vec_2)
cutoff_plot[4] <- signif(cutoff_2,digits=3)

cutoff_5 <-max(min_vec_5)
summary(min_vec_5)
cutoff_plot[5] <- signif(cutoff_5,digits=3)

cutoff_10 <-max(min_vec_10)
summary(min_vec_10)
cutoff_plot[6] <- signif(cutoff_10,digits=3)

plot_names <-c("3m","6m","1Y","2Y","5Y","10Y")
bplot <- barplot(cutoff_plot, main = "Cut off for each Level", names.arg = plot_names,beside=TRUE,horiz = TRUE)
text(cutoff_plot, bplot +.3, labels = as.character(cutoff_plot),xpd=TRUE)

#make is_low_vol vs isn_low_vol based upon the cutoffs

T_F_Low_3m <- matrix(nrow = x, ncol =2)
colnames(T_F_Low_3m) <-c("Equity", "is Low Vol")
runner = 1
for( i in 2:x)
{
  for( j in 1:trunc)
  {
    if(identical(colnames(Data[i]),order_3M[j]))
    {
      T_F_Low_3m[runner,1] = colnames(Data[i])
      T_F_Low_3m[runner,2] = TRUE
      break
    }
    else
    {
      T_F_Low_3m[runner,1] = colnames(Data[i])
      T_F_Low_3m[runner,2] = FALSE
    }
  }
  runner = runner +1
}
write.table(T_F_Low_3m, sep = ",", file="list.csv")

