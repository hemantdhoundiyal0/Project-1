# removing all the objects in the environment
rm(list=ls())


#setting the working diectory
setwd("C:\\Users\\admin\\Downloads")

#checking, either the above working directory set or not
getwd()

#importing required libraries
library(ggplot2) #for visuvalisations
install.packages("corrgram")
library(corrgram)
install.packages("usdm")
library(usdm)
install.packages("rpart")
library(rpart)

#read the required file
df=read.csv("day.csv",header = TRUE)

#take a look of data
View(df)

#how many variables and observation in this data file
dim(df)

#check the data type of each variable and basic structure
str(df)

#now take a look of 1st few rows of the data
head(df,8)

#View some basic statistical parameters of each numeric varible like central tendencies etc.
summary(df)

########## NOW SOME PREPROCESING ##########

#removing instant column from the data
df$instant= NULL
View(df)

#outlier analysis
##finding all mumeric variables
num_index= sapply(df,is.numeric)
num_index

#boxplot for all the variables
boxplot(df)

#checking outliers for variables individually
boxplot(df$casual,
        main = "Boxplots",
        xlab = "casual",
        ylab = "count",
        col = "orange",
        border = "brown"
)
boxplot(df$holiday,
        main = "Boxplots",
        xlab = "holiday",
        ylab = "count",
        col = "orange",
        border = "brown"
)
boxplot(df$hum,
        main = "Boxplots",
        xlab = "hum",
        ylab = "count",
        col = "orange",
        border = "brown"
)
boxplot(df$windspeed,
        main = "Boxplots",
        xlab = "windspeed",
        ylab = "count",
        col = "red",
        border = "brown"
)


#summary of casual variable for quartiles
summary(df$casual)

iqr1 = 315.5
iqr3 = 1096.0
iqr = iqr3-iqr1
iqr

# now finding the upper fence for this variable
up_fnc = iqr3 + (1.5*iqr) 
up_fnc


#summary for windspeed
summary(df$windspeed)

w_iqr1 = 0.13495
w_iqr3 = 0.23321
w_iqr = w_iqr3-w_iqr1
w_iqr

# now finding the upper fence for this variable
w_up_fnc = w_iqr3 + (1.5*w_iqr) 
w_up_fnc

#taking the values of the casual which are below the upper fence
my_data =subset(df,windspeed< 0.3806 & casual<2266.75)
my_data
boxplot(my_data$windspeed)
boxplot(my_data$casual)





## checking for Missing Values ##
miss_vals=sum(is.na(df))
miss_vals



## Feature Selection & Feature Scaling ##
#checking whether data is normally distributed or not
qqnorm(df$cnt)


hist(my_data$cnt)



#feature scaling using normalisation#
# min-max normalisation #
new_var = subset(my_data,select = -c(dteday)) #removing the non-numeic variable

cnames = c("season","yr","mnth","holiday","weekday","workingday","weathersit","temp","atemp","hum","windspeed","casual","registered")

for (i in cnames) {
        print(i)
        new_var[,i]=((new_var[,i]-min(new_var[,i]))/(max(new_var[,i])-min(new_var[,i])))
        }
View(new_var)


#Finding the numeric variables in new_var
num_ind= sapply(new_var,is.numeric)
num_ind


##now selecting the important features from the dataset using correlation plot
corrgram((new_var[,num_ind]),order = F,
         upper.panel = panel.cor, text.panel = panel.txt,main="Correlation Plot")


#Removing one of the variable from two highly correlated independent variables 
#and very less correlated dependent and independent variables
n_data = subset(new_var, select = -c(mnth,temp,holiday,weekday,hum))
View(n_data)                 



#### MODELING ####
#### Linear Regression ####

train_index = sample(1:nrow(n_data),0.8*nrow(n_data))
train = n_data[train_index,]
test = n_data[-train_index,]


# checking collinearity problem
vif(n_data[,-9])
vifcor(n_data[,-9],th=0.8)

#developing the model on the train dataset
lm_model = lm(cnt~., data = train)

#summary of the model
summary(lm_model)

#predicting the values of test case
prediction_lm = predict(lm_model,test[,1:8])

#checking for the error rate (predicted VS original)
mape <- mean(abs((test[,9]-prediction_lm ))/test[,9])  
mape

