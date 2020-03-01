# Clean the environment
rm(list=ls(all=T))

# Set the working directory
setwd("C:/Users/Navin Kumar/Desktop/DS/Project 2")
getwd()

# Load libraries
library(ggplot2)
library(corrgram)
library(rpart)
library(MASS)
library(usdm)
library(randomForest)
library(DataCombine)
library(fastDummies)

# Read the Source data
df = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))

###########################################Exploratory Data Analysis###########################################

# Dimension of data
dim(df)

# Structure of data
str(df)

# View first few rows
head(df)

# Fuction to plot histogram for numerical variables
univariate_numeric <- function(Var_X) {
  
  
  ggplot(df)+
    geom_histogram(aes(x=Var_X,y=..density..), fill= "grey")+
    geom_density(aes(x=Var_X,y=..density..))
  
}

# Plotting distribution of variable 'cnt'
univariate_numeric(df$cnt)

# Plotting distribution of variable 'temp'
univariate_numeric(df$temp)

# Plotting distribution of variable 'atemp'
univariate_numeric(df$atemp)

# Plotting distribution of variable 'hum'
univariate_numeric(df$hum)

# Plotting distribution of variable 'windspeed'
univariate_numeric(df$windspeed)

# Plotting distribution of variable 'casual'
univariate_numeric(df$casual)

# Plotting distribution of variable 'registered'
univariate_numeric(df$registered)


# Plot the distribution of categorical features.
bar1 = ggplot(data =  df, aes(x = season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data =  df, aes(x = weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data =  df, aes(x = holiday)) + geom_bar() + ggtitle("Count of Holiday")
bar4 = ggplot(data =  df, aes(x = yr)) + geom_bar() + ggtitle("Count of year")
bar5 = ggplot(data =  df, aes(x = mnth)) + geom_bar() + ggtitle("Count of month")
bar6 = ggplot(data =  df, aes(x = weekday)) + geom_bar() + ggtitle("Count of weekday")
bar7 = ggplot(data =  df, aes(x = workingday)) + geom_bar() + ggtitle("Count of workingday")

gridExtra::grid.arrange(bar1,bar2,bar3,bar4,bar5,bar6,bar7,ncol=2)

###########################################Missing Values Analysis###########################################

missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val

##############################################Outlier Analysis##############################################

# Detecting the outliers in numerical variables using Boxplot method
numeric_data = df[,10:16]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(df))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="botrainom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}

# Plot the Outliers
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,gn7, ncol=7)

# Missing values before outlier imputation
sum(is.na(df))

# Replace all outliers with NA and impute
for(i in cnames){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(val))
  df[,i][df[,i] %in% val] = NA
}

# Missing values after outlier deletion
sum(is.na(df))

# list of column names having null values
list_na=colnames(df)[apply(df, 2, anyNA)]

#Repace na with mean
df=data.frame(sapply(df, function(x) ifelse(is.na(x), mean(x, na.rm= TRUE), x)))

# Missing values after outlier imputation
sum(is.na(df))

##############################################Feature Selection##############################################

# Multicollinearity analysis using VIF
vifcor(df[,10:16], th = 0.9)

# plotting Correlation  between numerical variables
corrgram(df[,cnames], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

cor(df[,10:16])

# Dimension Reduction
df = subset(df, select = -c(atemp, casual, registered, instant, dteday))

#############################################Feature Engineering############################################

# Converting features into proper format
df$weathersit = as.factor(df$weathersit)
df$season = as.factor(df$season)
df$dteday = as.character(df$dteday)
df$mnth = as.factor(df$mnth)
df$weekday = as.factor(as.character(df$weekday))
df$workingday = as.factor(as.character(df$workingday))
df$yr = as.factor(df$yr)
df$holiday = as.factor(df$holiday)

# Structure of data after after feature formatting
str(df)

###########################################Creating Dummy variables###########################################

# convertion of multilevel categorical variable into binary dummy variable
cnames= c("season","mnth","weekday","weathersit","yr", "holiday", "workingday")
data_lr=df[,cnames]
cnt=data.frame(df$cnt)
names(cnt)[1]="cnt"
data_lr = fastDummies::dummy_cols(data_lr)
data_lr= subset(data_lr,select = -c(yr,season,mnth,weekday,workingday, holiday, weathersit))
d3 = cbind(data_lr,df)
d3= subset(d3,select = -c(yr,season,mnth,weekday,workingday, holiday, weathersit))
df1=d3

################################################Data Sampling################################################

rmExcept(keepers = "df1")

# Divide the data into 80% train and 20% test data
set.seed(123)
df_index = sample(1:nrow(df1), 0.8 * nrow(df1))
train =df1[df_index,]
test = df1[-df_index,]

###################################Model Development#######################################

#************************************Linear Regression************************************

# Train the Linear regression model
lm_model = lm(cnt ~., data = train)

# Summary of the model
summary(lm_model)

# Predict the test data
predictions_LR = predict(lm_model, test[,-36])

# MAPE function
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

# RMSE Fuction
RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
}

# Error Metrics
# MAPE  calculation
MAPE(test[,36], predictions_LR)

# RMSE  calculation
RMSE(test[,36], predictions_LR)

# MAPE value : 21.88%
# RMSE value : 859.869
# Accuracy   : 78.12%

#****************************************Decision Tree****************************************

# Train the  Decision Tree model
fit = rpart(cnt ~ ., data = train, method = "anova")

# Predict the test data
predictions_DT = predict(fit, test[,-36])

# Variable importance in Decision tree model
fit$variable.importance

# Plotting Decision Tree
par(cex= 0.8)
plot(fit)
text(fit)

# Error Metrics
# MAPE detection
MAPE(test[,36], predictions_DT)

# RMSE detection
RMSE(test[,36], predictions_DT)

#27, 1029

# MAPE value : 27.81%
# RMSE value : 1029.00
# Accuracy   : 72.19%

#****************************************Random Forest****************************************

# Train the Random Forest model
rf_model = randomForest(cnt~., data = train, ntree = 500)

# Predict the test data
RF_Predictions = predict(rf_model, test[,-36])

# Variable importance in Random forest model
rf_model$importance

# Error Metrics
# MAPE calculation
MAPE(test[,36], RF_Predictions)

# RMSE calculation
RMSE(test[,36], RF_Predictions)

# MAPE value : 18.88%
# RMSE value : 736.977
# Accuracy   : 81.12%