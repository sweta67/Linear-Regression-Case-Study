
#### Linear Regression ####
##### Boston Pricing Case Study#####
                                        
###########
#Objective#
##########

#To predict the Housing prices in the suburbs of Boston as a function of the different attributes.

#########################################
#Following are some steps to be followed#
#########################################

#Data Preparation : Understand summaries of the data, clean the data- missing value treatment, 
#outlier treatment, create new variables, dummy variables ,etc.

#Data Profiling: Understand the relationship between every IDV and DV (Using Correlations, visualisations, etc.)

#Build a Linear Regression model : using IDV's that have good correlation with the DV. Your goal will be to 
#create a model that best explains or predicts price.

#Evaluate the model performance and validate the model : Generate residual plots and actual Vs. predicted plots

###############################################################################################################


#Loading the data
prices<-read.csv("boston_prices.csv",header = TRUE,stringsAsFactors = FALSE)

View(prices)

#Checking data characteristics
dim(prices)
str(prices)
head(prices)
names(prices)

#Summary statistics
summary(prices)

#Missing values treatment
colSums(is.na(prices))   #Only MEDV has missing values
summary(prices$MEDV)
prices$MEDV[is.na(prices$MEDV)]<-mean(prices$MEDV,na.rm = TRUE)

#Outlier plots
par("mar")
par(mar=c(1,1,1,1))
#This allows you to plot multiple charts in a single page
par(mfrow=c(2,7))  
#Store the names of the dataset in a list format
list<-names(prices) 
list<-list[-4]
for(i in 1:length(list))
{
  #Plot the boxplots of variables and shortlist which require outlier treatment
  boxplot(prices[,list[i]],main=list[i])  
}

#Restore the par parameters to normal
dev.off()

#Outlier treatment
for(i in 1:length(list))
{
  #Replacing all outlier values with the mean value
  x<-boxplot(prices[,list[i]])
  out<-x$out
  index<-which(prices[,list[i]] %in% x$out)
  prices[index,list[i]]<-mean(prices[,list[i]])
  rm(x)
  rm(out)
}

#Exploratory analysis
library(ggplot2)

#Histogram of DV
hist(prices$MEDV)

#Understand the correlation between each IDV and the DV
#Below shows few IDV vs Dv correlation
ggplot(prices,aes(x=MEDV,y=LSTAT))+geom_point()
ggplot(prices,aes(x=MEDV,y=DIS))+geom_point()
ggplot(prices,aes(x=MEDV,y=AGE))+geom_point()


#Function to get the correlations between DV and IDV's
list1<-list[-13]
for(i in 1:length(list1))
{
  x<-cor(prices$MEDV,prices[list[i]])
  print(x)
}

#Significant variables are : B LSTAT AGE X.rooms.dwelling nitric.oxides.concentration INDUS

#Data Transformation
#Create the log transformation for all the variables
prices$log_CRIM<-log(prices$CRIM)
prices$log_ZN<-log(prices$ZN)
prices$log_NOX<-log(prices$nitric.oxides.concentration)
prices$log_RM<-log(prices$X.rooms.dwelling)
prices$log_AGE<-log(prices$AGE)
prices$log_DIS<-log(prices$DIS)
prices$log_RAD<-log(prices$RAD)
prices$log_TAX<-log(prices$TAX)
prices$log_PTRATIO<-log(prices$PTRATIO)
prices$log_B<-log(prices$B)
prices$log_LSTAT<-log(prices$LSTAT)
prices$log_MEDV<-log(prices$MEDV) #DV
prices$log_INDUS<-log(prices$INDUS)

#Function to get the list of correlations between log of DV and log of IDV's
list_log<-names(prices)[c(15:25,27)]
for( i in 1:length(list_log))
{
  xlog<-cor(prices$log_MEDV,prices[list_log[i]])
  print(xlog)
}


#Function to get the list of correlations between log of DV and IDV's
list_IDVs<-names(prices)[1:13]
list_IDVs<-list_IDVs[-4]
for(i in 1:length(list_IDVs))
{
  xlogDV<-cor(prices$log_MEDV,prices[list_IDVs[i]])
  print(xlogDV)
}

sampling<-sort(sample(nrow(prices),nrow(prices)*.7))

#Select training sample
train<-prices[sampling,]   #70% training data
test<-prices[-sampling,]   #30% test data

#Building Simple Linear Regression Model

#Model-1
Reg<-lm(log_MEDV~CRIM+ZN+INDUS+Charles.River.dummy.variable+nitric.oxides.concentration
        +X.rooms.dwelling+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data = train)

summary(Reg)

#Getting the formula
formula(Reg)

Reg1<-lm(log_MEDV~
           Charles.River.dummy.variable+
           DIS+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,data=train)

summary(Reg1)

Reg2<-lm(log_MEDV ~CRIM+INDUS+RAD+TAX+B+
             Charles.River.dummy.variable+
             DIS+ZN+PTRATIO+LSTAT+X.rooms.dwelling+nitric.oxides.concentration, data=train)

summary(Reg2)

Reg3<-lm(log_MEDV ~CRIM+RAD+
             Charles.River.dummy.variable+
             DIS+ZN+PTRATIO+LSTAT+nitric.oxides.concentration, data=train)

summary(Reg3)


Reg4<-lm(log_MEDV~INDUS  +ZN + X.rooms.dwelling + LSTAT+CRIM + Charles.River.dummy.variable,data=train)
summary(Reg4)

Reg5<-lm(log_MEDV~CRIM+ZN+Charles.River.dummy.variable+nitric.oxides.concentration+X.rooms.dwelling+DIS+B+LSTAT,data = train)
summary(Reg5)

#  According to solution provided by Faculty : The best model happens to be : Reg3

#Getting predicted values
predicted<-predict(Reg3)
plot(predicted)
length(predicted)

#Finding Residuals
residuals<-resid(Reg3)
plot(residuals)
length(residuals)

#Plotting Residuals vs Predicted values
#Checking heteroscedasticity
#There should be no trend between Residuals and Predicted values

plot(predicted,residuals,abline(0,0))

#You can notice that there seems to be an inverse pattern for some points

#So this model may not be the preferred model.

#Attaching predicted values to test data
predicted<-predict(Reg3,newdata = test)
length(predicted)
test$p<-predicted

#Calculating error in the test dataset : (Actual-predicted)/actual values
test$error<-(test$log_MEDV-test$p/test$log_MEDV)
#Average error in the dataset
mean(test$error)*100


#Plotting actual vs predicted values
plot(test$p,col="blue",type="l")
lines(test$log_MEDV,col="red",type="l")

#Checking correlation between variables
library(car)
vif(Reg3)

#can Drop the variables if they have a vif > 10 means high correlation between variables

##############################################################################################

#According to me , Reg5 is the best model.

#Getting predicted values
predicted<-predict(Reg5)
plot(predicted)
length(predicted)

#Finding Residuals
residuals<-resid(Reg5)
plot(residuals)
length(residuals)

#Plotting Residuals vs Predicted values
#Checking heteroscedasticity
#There should be no trend between Residuals and Predicted values

plot(predicted,residuals,abline(0,0))


#Attaching predicted values to test data
predicted<-predict(Reg5,newdata = test)
length(predicted)
test$p<-predicted

#Calculating error in the test dataset : (Actual-predicted)/actual values
test$error<-(test$log_MEDV-test$p/test$log_MEDV)
#Average error in the dataset
mean(test$error)*100


#Plotting actual vs predicted values
plot(test$p,col="blue",type="l")
lines(test$log_MEDV,col="red",type="l")

#Checking correlation between variables
library(car)
vif(Reg5)
save.image("CaseStudy-Linear-Regression-Practice.RData")

############################################################################################################















































