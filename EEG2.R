#libraries
library(matlab)
library(moments) # calculating and plotting descriptive statistics
library(ggExtra)
library(readr)
library(ggpubr)
library(dplyr)

#import the datasets
time <- read_csv("time.csv")
x <- read_csv("X.csv")
y <- read_csv("y.csv")

#check data 
head(time)
head(x)
head(y)

#renaming columns of the datasets

colnames(x) <- paste0(rep("x", ncol(x)), 1:ncol(x))
colnames(y) <- "y"
colnames(time) <- "time"

#create new dataframe called df and combine all columns
df = cbind(time, x, y)

#check the new dataframe dframe
head(df)

#Task 1: preliminary data analysis
# summarizing the data using summary() function
summary(df)

#packages
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(GGally)

#checking for missing values in the datasets
# Use is.na function to find out
missing_values <- is.na(df)
missvalues <- sum(missing_values)
print(sprintf("The Number of the missing values: %d", missvalues))


#Task 1: preliminary data analysis
#Ime series plots for the input and audio signals

#input signal
#convert our input data into matrix 
#use the as.matrix function
xmat <- as.matrix(x)
#print the matrix representation
print(xmat)

#rename the column names
colnames(x)<-c("X1","X2")

#output signal
#convert our output data into matrix representation
ymat <- as.matrix(y)
#print the matrix representation
print(ymat)
colnames(y)<-c("Y")

#Plotting the Time series plot of input signals
xmat.ts<-ts(xmat,start = c(min(time),max(time)),frequency =1)
plot(xmat.ts,main = "Time series plot of Input Signals", xlab = "Time", ylab = "Input signal")

#plotting the time series plot of the output signal
ymat.ts<-ts(ymat,start = c(min(time),max(time)),frequency =1)
plot(ymat.ts,main = "Time series plot of output Signals", xlab = "Time", ylab = "Output signals")

#Distribution for each signal
#mode, median and mean
#Find the mode
mode_x1 <-  as.numeric(names(sort(table(df$x1),decreasing=TRUE)[1]))
mode_x2 <-  as.numeric(names(sort(table(df$x2),decreasing=TRUE)[1]))
mode_y <-  as.numeric(names(sort(table(df$y),decreasing=TRUE)[1]))

#Frequency distribution of x1 with mode, median ands mean
ggplot(aes(x = x1), data = df) + 
  geom_histogram(color = 'black', fill = "grey", binwidth = 0.5) + 
  geom_vline(aes(xintercept=median(x1),
                 color="median"), linetype="solid",
             size=1) +
  geom_vline(aes(xintercept=mean(x1),
                 color="mean"), linetype="dashed",
             size=1) +
  geom_vline(aes(xintercept=mode_x1,
                 color="mode"), linetype="dashed",
             size=1) +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "green", mode = "blue"))+
  geom_rug()+ #Show the individual observations with black lines
  labs(title = "Frequency distribution of the input x1 EEG signal",
       caption = "histogram") + 
  xlab("Input Signal x1") + 
  ylab("Frequency ")

#Frequency distribution of x2 with mode, median ands mean
ggplot(aes(x = x2), data = df) + 
  geom_histogram(color = 'black', fill = "grey", binwidth = 0.5) + 
  geom_vline(aes(xintercept=median(x2),
                 color="median"), linetype="solid",
             size=1) +
  geom_vline(aes(xintercept=mean(x2),
                 color="mean"), linetype="dashed",
             size=1) +
  geom_vline(aes(xintercept=mode_x2,
                 color="mode"), linetype="dashed",
             size=1) +
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "green", mode = "blue"))+
  geom_rug()+ #Show the individual observations with black lines
  labs(title = "Frequency distribution of the input x2 EEG signal",
       caption = "histogram") + 
  xlab("Input Signal x2") + 
  ylab("Frequency ") 

#Frequency distribution of y with mode, median ands mean
ggplot(aes(x = y), data = df) + 
  geom_histogram(color = 'black', fill = "grey" ,binwidth = 0.5) + 
  geom_vline(aes(xintercept=median(y),
                 color="median"), linetype="solid",
             size=1) +
  geom_vline(aes(xintercept=mean(y),
                 color="mean"), linetype="dashed",
             size=1) +
  geom_vline(aes(xintercept=mode_y,
                 color="mode"), linetype="dashed",
             size=1) +
  scale_color_manual(name = "Legend", values = c(median = "red", mean = "green", mode = "blue"))+
  geom_rug()+ #Show the individual observations with black lines
  labs(title = "Frequency distribution of the output EEG signal",
       caption = "histogram") + 
  xlab("Output Signal") + 
  ylab("Frequency ") 

#density plot x2
ggplot(df, aes(x = x1)) +
  geom_density(color = 3,
               fill = 4,
               alpha = 0.25, #alpha is used to set the transparency of the density plot.
               kernel = "rectangular") +  #he kernel used can also be changed with kernel argument. The possible options are "gaussian" (default), "rectangular"
  labs(title = "Spread and concentration of values for input x1 EEG signal",
       subtitle = "density plot") +
  xlab("Input Signal X1") +
  ylab("Density ")


#density plot x2
ggplot(df, aes(x = x2)) +
  geom_density(color = 3,
               fill = 4,
               alpha = 0.25, #alpha is used to set the transparency of the density plot.
               kernel = "rectangular") +  #he kernel used can also be changed with kernel argument. The possible options are "gaussian" (default), "rectangular"
  labs(title = "Spread and concentration of values for input x2 EEG signal",
       subtitle = "density plot") +
  xlab("Input Signal X2") +
  ylab("Density ")

#density plot of y
ggplot(df, aes(x = y)) +
  geom_density(color = 3,
               fill = 4,
               alpha = 0.25, #alpha is used to set the transparency of the density plot.
               kernel = "rectangular") +  #he kernel used can also be changed with kernel argument. The possible options are "gaussian" (default), "rectangular"
  geom_rug()+ #Show the individual observations with black lines
  labs(title = "Spread and concentration of values for output EEG signal",
       subtitle = "One mode with left tail extremes") +
  xlab("Output Signal") +
  ylab("Density ")

#scatter plot of x1 against y
ggplot(df, aes(x = x1, y = y,
               colour = y)) +
  geom_point(show.legend = TRUE) +
  geom_smooth(method=lm, level=0.95)+ #add linear trend line
  scale_color_gradient(low = "#EEC9E5", high = "#7C4D79") +
  labs(title = "Relationship between input X1 EEG signal & Output Signal",
       caption = " Figure showing  relationship between input signal x1 with output signal y and pearson correlation value(R)") + 
  xlab("Input Signal X1") + 
  ylab("Output Signal") + 
  stat_cor(p.accuracy = 0.05, r.accuracy = 0.01, method = "pearson" )

#Task 2.1.1
#model 1
#   Calculating ones for binding the data 
ones = matrix(1 , length(x)/2,1) 
x_model_1<-cbind(ones,(x[,"X1"])^3,(x[,"X2"])^5)

# Printing thus binded new dataframe
head(x_model_1)

#calculate estimated values of the model parameter θ^
theta_hat_1 <- solve(t(as.matrix(x_model_1)) %*% as.matrix(x_model_1)) %*% t(as.matrix(x_model_1)) %*% as.matrix(y)
# Viewing the value of theta_hat_1
print(theta_hat_1) 
#Task 2.1.2
x_model_2<-cbind(ones,(x[,"X1"])^4,(x[,"X2"])^2)
# Calculating theta_hat_2
theta_hat_2 <- solve(t(as.matrix(x_model_2)) %*% as.matrix(x_model_2)) %*% t(as.matrix(x_model_2)) %*% as.matrix(y)
# Viewing the value of theta_hat_2
print(theta_hat_2) 

#Task 2.1.3
x_model_3<-cbind(ones,(x[,"X1"])^3,(x[,"X2"]),(x[,"X1"]))
# Calculating theta_hat
theta_hat_3 <- solve(t(as.matrix(x_model_3)) %*% as.matrix(x_model_3)) %*% t(as.matrix(x_model_3)) %*% as.matrix(y)
# Viewing the value of theta_hat_3
print(theta_hat_3) 

#Task 2.1.4
x_model_4<-cbind(ones,(x[,"X1"]),(x[,"X1"])^2,(x[,"X1"])^3,(x[,"X2"])^3)
# Calculating theta_hat
theta_hat_4 <- solve(t(as.matrix(x_model_4)) %*% as.matrix(x_model_4)) %*% t(as.matrix(x_model_4)) %*% as.matrix(y)
# Viewing the value of theta_hat_4
print(theta_hat_4) 

#Task 2.1.5
# Binding for model 5
x_model_5 <- cbind(ones,(x[,"X1"])^3,(x[,"X1"])^4,(x[,"X2"]))
# Calculating theta_hat
theta_hat_5 <- solve(t(as.matrix(x_model_5)) %*% as.matrix(x_model_5)) %*% t(as.matrix(x_model_5)) %*% as.matrix(y)
# Viewing the value of theta_hat_5
print(theta_hat_5) 

#Task 2.2
#Task 2.2.1
# Before that we convert our model and theta hat into matrix
x_model_1 <- as.matrix(x_model_1)
theta_hat_1 <- as.matrix(theta_hat_1)

#Calculating Y-hat model 1
Y_hat_model_1 <- x_model_1 %*% theta_hat_1
#Calculating RSS model 1
RSS_model_1 <- sum((y - Y_hat_model_1)^2)
# printing RSS value for model 1
print(sprintf("RSS value of the model 1 is %0.4f", RSS_model_1))

#Task 2.2.2
#convert our model and theta hat into matrix
x_model_2 <- as.matrix(x_model_2)
theta_hat_2 <- as.matrix(theta_hat_2)

#Calculating Y-hat model 2
Y_hat_model_2 <- x_model_2 %*% theta_hat_2
#Calculating RSS model 2
RSS_model_2 <- sum((y - Y_hat_model_2)^2)
# printing RSS value for model 2
#print(paste("RSS value of the Model 2 is", RSS_model_2))

print(sprintf("RSS value of the model 2 is %0.4f", RSS_model_2))

#Task 2.2.3
#convert our model and theta hat into matrix
x_model_3 <- as.matrix(x_model_3)
theta_hat_3 <- as.matrix(theta_hat_3)
#Calculating Y-hat model 3
Y_hat_model_3 <- x_model_3 %*% theta_hat_3
#Calculating RSS model 3
RSS_model_3 <- sum((y - Y_hat_model_3)^2)
# printing RSS value for model 3

print(sprintf("RSS value of the model 3 is %0.4f", RSS_model_3))

#Task 2.2.4
#convert our model and theta hat into matrix
x_model_4 <- as.matrix(x_model_4)
theta_hat_4 <-  as.matrix(theta_hat_4)
#Calculating Y-hat model 4
Y_hat_model_4 <- x_model_4 %*% theta_hat_4
#Calculating RSS model 4
RSS_model_4 <- sum((y - Y_hat_model_4)^2)
# printing RSS value for model 4

print(sprintf("RSS value of the model 4 is %0.4f", RSS_model_4))

#Task 2.2.5
#convert our model and theta hat into matrix
x_model_5 <- as.matrix(x_model_5)
theta_hat_5 <- as.matrix(theta_hat_5)
#Calculating Y-hat model 5
Y_hat_model_5 <- x_model_5 %*% theta_hat_5
#Calculating RSS model 5
RSS_model_5 <- sum((y - Y_hat_model_5)^2)
# printing RSS value for model 5

print(sprintf("RSS value of the model 5 is %0.4f", RSS_model_5))


#Task 2.3
#Task 2.3.1
# Calculating length of the output signal y with nrow()  as our data are in matrix format and storing it in N

N <- nrow(y)
# Calculating the variance of model 1

Variance_model_1 = RSS_model_1/(N-1)

# Printing variance of model 1

print(sprintf("Variance of model 1 is %0.4f", Variance_model_1))

# Calculating the log-likelihood of model 1 using model residual error (RSS)

likehood_model_1 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_1))-(1/(2*Variance_model_1))*RSS_model_1

# Printing log likelihood function of model 1
print(sprintf("Log-likelihood of model 1 is %0.4f", likehood_model_1))

#Task 2.3.2
# Calculating the variance of model 2

Variance_model_2 = RSS_model_2/(N-1)

# Printing variance of model 2

print(sprintf("Variance of model 2 is %0.4f", Variance_model_2))

# Calculating the log-likelihood of model 2 using model residual error (RSS)

likehood_model_2 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_2))-(1/(2*Variance_model_2))*RSS_model_2

# Printing log likelihood function of model 2
print(sprintf("Log-likelihood of model 2 is %0.4f", likehood_model_2))

#Task 2.3.3
# Calculating the variance of model 3

Variance_model_3 = RSS_model_3/(N-1)

# Printing variance of model 3

print(sprintf("Variance of model 3 is %0.4f", Variance_model_3))
# Calculating the log-likelihood of model 3 using model residual error (RSS)

likehood_model_3 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_3))-(1/(2*Variance_model_3))*RSS_model_3

# Printing log likelihood function of model 3
print(sprintf("Log-likelihood of model 3 is %0.4f", likehood_model_3))

#Task 2.3.4
# Calculating the variance of model 4

Variance_model_4 = RSS_model_4/(N-1)

# Printing variance of model 4

print(sprintf("Variance of model 4 is %0.4f", Variance_model_4))
# Calculating the log-likelihood of model 4 using model residual error (RSS)

likehood_model_4 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_4))-(1/(2*Variance_model_4))*RSS_model_4

# Printing log likelihood function of model 4
print(sprintf("Log-likelihood of model 4 is %0.4f", likehood_model_4))


#Task 2.3.5
# Calculating the variance of model 5

Variance_model_5 = RSS_model_5/(N-1)

# Printing variance of model 5

print(sprintf("Variance of model 5 is %0.4f", Variance_model_5))
# Calculating the log-likelihood of model 5 using model residual error (RSS)

likehood_model_5 <- -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model_5))-(1/(2*Variance_model_5))*RSS_model_5

# Printing log likelihood function of model 5
print(sprintf("Log-likelihood of model 5 is %0.4f", likehood_model_5))


#Task 2.4
#Task 2.4.1
#Task 2.4.1.1
# Calculating AIC for model 1. 
# Here we are finding value of K with length() function.

AIC_1 <- 2* length(x_model_1[1,]) - 2 * likehood_model_1

print(sprintf("Length of parameter to be estimated in model 1 is %d", length(x_model_1[1,])))
# Printing AIC of model 1
print(sprintf("AIC of model 1 is %0.4f", AIC_1))

#Task 2.4.1.2
# Calculating AIC for model 2. 


AIC_2 <- 2* length(x_model_2[1,]) - 2 * likehood_model_2

print(sprintf("Length of parameter to be estimated in model 2 is %d", length(x_model_2[1,])))
# Printing AIC of model 2
print(sprintf("AIC of model 2 is %0.4f", AIC_2))

#Task 2.4.1.3
# Calculating AIC for model 3. 


AIC_3 <- 2* length(x_model_3[1,]) - 2 * likehood_model_3

print(sprintf("Length of parameter to be estimated in model 3 is %d", length(x_model_3[1,])))
# Printing AIC of model 3
print(sprintf("AIC of model 3 is %0.4f", AIC_3))

#Task 2.4.1.4
# Calculating AIC for model 4. 


AIC_4 <- 2* length(x_model_4[1,]) - 2 * likehood_model_4

print(sprintf("Length of parameter to be estimated in model 4 is %d", length(x_model_4[1,])))
# Printing AIC of model 4
print(sprintf("AIC of model 4 is %0.4f", AIC_4))

#Task 2.4.1.5
# Calculating AIC for model 5. 


AIC_5 <- 2* length(x_model_5[1,]) - 2 * likehood_model_5

print(sprintf("Length of parameter to be estimated in model 5 is %d", length(x_model_5[1,])))
# Printing AIC of model 5
print(sprintf("AIC of model 5 is %0.4f", AIC_5))

#Task 2.4.2: BIC for each model
#Task 2.4.2.1
# Calculating BIC for model 1 

BIC_1 <- length(x_model_1[1,]) * log(N) - 2 * likehood_model_1


# Printing BIC of model 1
print(sprintf("BIC of model 1 is %0.4f", BIC_1))

#Task 2.4.2.2
# Calculating BIC for model 2

BIC_2 <- length(x_model_2[1,]) * log(N) - 2 * likehood_model_2


# Printing BIC of model 2

print(sprintf("BIC of model 2 is %0.4f", BIC_2))

#Task 2.4.2.3
# Calculating BIC for model 3 

BIC_3 <- length(x_model_3[1,]) * log(N) - 2 * likehood_model_3


# Printing BIC of model 3

print(sprintf("BIC of model 3 is %0.4f", BIC_3))

#Task 2.4.2.4
# Calculating BIC for model 4

BIC_4 <- length(x_model_4[1,]) * log(N) - 2 * likehood_model_4


# Printing BIC of model 4

print(sprintf("BIC of model 4 is %0.4f", BIC_4))

#Task 2.4.2.5
# Calculating BIC for model 5

BIC_5 <- length(x_model_5[1,]) * log(N) - 2 * likehood_model_5


# Printing BIC of model 5

print(sprintf("BIC of model 5 is %0.4f", BIC_5))


#Task 2.5
#Task 2.5.1
##  Error of models 1-5  based on calculation form Task 2.2
model_1_error <- y - Y_hat_model_1
model_2_error <- y - Y_hat_model_2
model_3_error <- y - Y_hat_model_3
model_4_error <- y - Y_hat_model_4
model_5_error <- y - Y_hat_model_5

#Task 2.5.2
# Q-Q plots of prediction error for model 1

qqnorm(t(model_1_error),col = "grey", main = "Q-Q Plot for Model 1" )
qqline(model_1_error, col = "red", lwd = 1,lty = 2)

# Q-Q plots of prediction error for model 2

qqnorm(t(model_2_error), col= "grey", main = "Q-Q Plot for Model 2" )
qqline(model_2_error, col = "red", lwd = 1,lty = 2)

# Q-Q plots of prediction error for model 3

qqnorm(t(model_3_error), col= "grey", main = "Q-Q Plot for Model 3" )
qqline(model_3_error, col = "red", lwd = 1,lty = 2)

# Q-Q plots of prediction error for model 4

qqnorm(t(model_4_error),col= "grey", main = "Q-Q Plot for Model 4" )
qqline(model_4_error, col = "red", lwd = 1, lty = 2)

# Q-Q plots of prediction error for model 5

qqnorm(t(model_5_error), col= "grey",main = "Q-Q Plot for Model 5" )
qqline(model_5_error, col = "red", lwd = 1,lty = 2)


#Task 2.6
#Best regression model
# Distribution of prediction error using histogram
par(mfrow = c(3,2))
hist (model_1_error[,1], freq = FALSE, col="blue", las =1)
abline(v = median(model_1_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_1_error[,1]), col = "purple", lwd = 5)

hist (model_2_error[,1], freq = FALSE, col="green", las =1)
abline(v = median(model_2_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_2_error[,1]), col = "purple", lwd = 5)
# abline(v = getmode(model_2_error[,1]), col = "red", lwd = 5)

hist (model_3_error[,1], freq = FALSE, col="orange", las =1)
abline(v = median(model_3_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_3_error[,1]), col = "purple", lwd = 5)

hist (model_4_error[,1], freq = FALSE, col="yellow", las =1)
abline(v = median(model_4_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_4_error[,1]), col = "purple", lwd = 5)

hist (model_5_error[,1], freq = FALSE, col="pink", las =1)
abline(v = median(model_5_error[,1]), col = "grey", lwd = 5)
abline(v = mean(model_5_error[,1]), col = "purple", lwd = 5)

hist(0, main = "color code")
legend("center", legend = c("median","mean"),
       lwd = 1, col = c("grey", "purple"))

#Task 2.7
# Splitting input signals x

split_x <- initial_split(data = as.data.frame(x),prop=.7)
# Training  data for input signals x  are split and thus splitted training set is converted to matrix form ans assigned to the variable x_training_data 

x_training_set <- training(split_x)

x_training_data <- as.matrix(x_training_set)
# Testing data  are split  and thus splitted testing set is converted to matrix form ans assigned to the variable x_testing_data 

x_testing_set <- testing(split_x)

x_testing_data <- as.matrix(x_testing_set)
# Splitting the data of output signals y into train and test

split_y <- initial_split(data = as.data.frame(y),prop=.7)
# Training  data for output signals y  are split and thus splitted training set is converted to matrix form ans assigned to the variable y_training_data 

y_training_set <- training(split_y)

y_training_data <- as.matrix(y_training_set)
# Testing data for output signals y are splitted and thus splitted testing set is converted to matrix form ans assigned to the variable y_testing_data 

y_testing_set <- testing(split_y)

y_testing_data <- as.matrix(y_testing_set)

#Task 2.7.1: Estimating model parameter for selected model 3 using training dataset
#   Estimating model parameters using training set 

training_ones <- matrix(1, length(x_testing_set$X1),1) 
training_model_x <- cbind(training_ones,(x_testing_set[,"X1"])^3,(x_testing_set[,"X2"]),(x_testing_set[,"X1"]))


training_thetahat <-   solve(t(training_model_x) %*% training_model_x) %*% t(training_model_x) %*% y_testing_data

#Task 2.7.2
#Model output prediction on testing data set
y_testing_hat <- x_testing_data %*% training_thetahat


# Calculating residual sum of squares (RSS) for the testing data
RSS_testing <- sum((y_testing_set-y_testing_hat)^2) 


# Printing RSS value of testing 

print(sprintf("RSS value is testing data %0.4f", RSS_testing))

#Task 2.7.3
#Computing 95% confidence interval and visualizaing
t.test(y_testing_data, mu=700, alternative="two.sided", conf.level=0.95)

# Setting CI value based on t-test at the time of run
C_I1 <- -0.1625241

C_I2 <- 0.6853514


ggplot(data = data.frame(y_training_data), aes(x = y_training_data)) +
  geom_density(col = "black", fill = "black" , lw=1) +
  geom_vline(xintercept = C_I1, col = "cyan", linetype = "dashed") +
  geom_vline(xintercept = C_I2, col = "red", linetype = "dashed") +
  geom_rug()+
  
  ggtitle("Distribution of training Y data with 95% confidence intervals")+
  xlab("Y Training Data") +
  ylab("Density")

theme(legend.title = element_blank())

thetaHat_training  <- solve(t(x_training_data) %*% x_training_data) %*% t(x_training_data) %*% y_training_data

thetaHat_training

length(thetaHat_training)

# Plotting distribution of testing data


ggplot(data = data.frame(y_testing_data), aes(x = y_testing_data)) +
  geom_density(col = "black", fill = "black" , lw=1) +
  geom_vline(xintercept = C_I1, col = "cyan", linetype = "dashed") +
  geom_vline(xintercept = C_I2, col = "cyan", linetype = "dashed") +
  geom_rug()+
  
  ggtitle("Distribution of testing Y data with 95% confidence intervals")+
  xlab("Y testing data") +
  ylab("Density")# Plotting distribution of testing data


ggplot(data = data.frame(y_testing_data), aes(x = y_testing_data)) +
  geom_density(col = "black", fill = "black" , lw=1) +
  geom_vline(xintercept = C_I1, col = "cyan", linetype = "dashed") +
  geom_vline(xintercept = C_I2, col = "red", linetype = "dashed") +
  geom_rug()+
  
  ggtitle("Distribution of testing Y data with 95% confidence intervals")+
  xlab("Y testing data") +
  ylab("Density")

theme(legend.title = element_blank())
# Error between actual testing data and the model's predicted output on that data

error <- ((y_testing_set - y_testing_hat)) 



# Printing error value 

print(sprintf("Error between actual testing data and the model's predicted output on that data is  %0.4f", RSS_testing))

#Task 3: ABC
#Task 3.1
# Creator vector of theta_hat of selected model 2 and sorting them to find out two largest absolute value and printing it

numbers <- c(theta_hat_3)
sorted_numbers <- sort(abs(numbers), decreasing=TRUE)
largest_two_values <- sorted_numbers[1:2]
print(largest_two_values)

#Choosing parameters

theta_bias <- largest_two_values[1] 
theta_four <- largest_two_values[2]

#Constant parameter

theta_one  <-  0.010038614  
theta_three   <- -0.001912836


# Initial values

arr_1 = 0
arr_2=0
f_value=0
s_value=0

theta_hat_3

#Task 3.2
# Calculating epsilon 

epsilon <- RSS_model_3 * 2

# Number of iteration  to determines how many times the for loop will repeat and generate new values for the parameters.

# A larger number of iterations may result in a more accurate estimate, but will also increase the computational time.
num <- 100 

##Calculating Y-hat for performing rejection ABC 

counter <- 0

# Iteration from 0 -100 and calculating the range 

for (i in 1:num) {
  range1 <- runif(1,-0.483065688,0.483065688) 
  range2 <- runif(1,-0.1435789,0.1435789)
  
  # Creating new vector of  two values from range1 and range2 with the constant values theta_one,theta_three. 
  New_thetahat <- matrix(c(range1,range2,theta_one,theta_three)) 
  
  # Calculating predicted response values for the current iteration of the loop
  
  New_Y_Hat <- x_model_3 %*% New_thetahat 
  
  # Calculting new RSS valur
  new_RSS <- sum((y - New_Y_Hat)^2) 
  
  
  
  # Checking if new RSS is greater than epsilon and if the condition is true, the values of range1 and range2 are stored in arrays arr_1 and arr_2 respectively. The counter is also incremented by 1. The f_value and s_value are then defined as matrices of the arrays arr_1 and arr_2 respectively.
  
  if (new_RSS > epsilon){
    arr_1[i] <- range1 
    arr_2[i] <- range2 
    counter = counter+1
    f_value <- matrix(arr_1)
    s_value <- matrix(arr_2)
    
  } #closing else loop
} #closing for loop

#Task 3.3
# Plotting histogram of new f_values and s_values

# Frequency distribution of the f_value
ggplot(data.frame(f_value), aes(x=f_value)) + 
  geom_histogram(color = 'black', fill = "grey") + 
  geom_rug()+ #Show the individual observations with black lines
  labs(title = "Frequency distribution of the f_value"
  ) + 
  xlab("f_value") + 
  ylab("Frequency ") 

# Frequency distribution of the s_value
ggplot(data.frame(s_value), aes(x=s_value)) + 
  geom_histogram(color = 'black', fill = "grey") + 
  geom_rug()+ #Show the individual observations with black lines
  labs(title = "Frequency distribution of the s_value"
  ) + 
  xlab("s_value") + 
  ylab("Frequency ") 

# Plotting the joint and marginal posterior distribution

# Create a data frame with the values of f_value and s_value and a column for "group"
df <- data.frame(f_value, s_value, legend=rep(c("f_value","s_value"), each=length(f_value)/2))

# Plot the scatter plot using and hiding legends
p <- ggplot(df, aes(x=f_value, y=s_value, color=legend)) +
  geom_point()+
  theme(legend.position="bottom")+ # show legend in bottom
  theme(legend.title = element_blank())+ # hide legend word
  #guides(color=FALSE)+ # Uncomment to hide legend
  ggtitle("Joint and Marginal Posterior Distribution")
# 
# Show the plot
print(p)

# Plotting the joint and marginal posterior distribution

# Create a data frame with the values of f_value and s_value and a column for "group"
df <- data.frame(f_value, s_value, legend=rep(c("f_value","s_value"), each=length(f_value)/2))

# Plot the scatter plot using and hiding legends
p <- ggplot(df, aes(x=f_value, y=s_value, color=legend)) +
  geom_point()+
  theme(legend.position="bottom")+
  theme(legend.title = element_blank())+
  #guides(color=FALSE)+ # Uncomment to hide legend
  ggtitle("Joint and Marginal Posterior Distribution")
# 
# # Show the plot
# print(p)


# Marginal histograms by group
ggMarginal(p, type = "histogram",
           xparams = list(fill = 1),
           yparams = list(fill = 1)) 