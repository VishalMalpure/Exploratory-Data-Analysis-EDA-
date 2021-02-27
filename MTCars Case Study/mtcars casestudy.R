# mtcars dataset case study
#Basics of Data science
#Exploratory Data Analysis
#Model building

data()
#Load dataset
View(mtcars)
#import and invoke required packages

library(dplyr)
library(explore)

#%>%   used for pipe

#Explore table
mtcars %>% explore_tbl()   
mtcars %>% describe()
#The data was extracted from the 1974 Motor Trend US magazine, 
#and comprises fuel consumption and 
#10 aspects of automobile design and performance for 32 automobiles (1973-74 models).


#mpg   	Miles/(US) gallon
#cyl    	Number of cylinders
#disp  	Displacement (cu.in.)
#hp	    Gross horsepower
#drat	  Rear axle ratio
#wt    	Weight (lb/1000)
#qsec  	1/4 mile time
#vs	    V/S
#am	    Transmission (0 = automatic, 1 = manual)
#gear	  Number of forward gears
#carb	  Number of carburetors



mtcars %>% 
  explore_all()


#Is there a difference between cars with 3,4 and 5 gears?
mtcars %>% 
  explore(gear)


#check relation between some of the variables and gear:
mtcars %>% 
  select(gear, mpg, hp, cyl, am) %>% 
  explore_all(target = gear)

mtcars %>% 
  select(cyl, mpg, hp, am) %>% 
  explore_all(target = cyl)

# lets find High miles per gallon?
#Let's define an interesting target: Cars that have mpg (miles per gallon) > 25

data <- mtcars %>% 
  mutate(highmpg = if_else(mpg > 25, 1, 0, 0)) %>% 
  select(-mpg)
View(data)

data %>% explore(highmpg)



data %>% 
  select(highmpg, cyl, disp, hp) %>% 
  explore_all(target = highmpg)


data %>% 
  select(highmpg, drat, wt, qsec, vs) %>% 
  explore_all(target = highmpg)


data %>% 
  select(highmpg, am, gear, carb) %>% 
  explore_all(target = highmpg)


#decision tree
data %>% 
  explain_tree(target = highmpg)


data %>% explore(wt, target = highmpg)
data %>% explore(wt, target = highmpg, split = FALSE)
mtcars %>% explore(wt, mpg)


#relation between horsepower and other variables like number of cylinder?
#Let's build a decision tree with horsepower as target:

mtcars %>% 
  explain_tree(target = hp, minsplit=15)



mtcars %>% 
  select(hp, cyl, mpg) %>% 
  explore_all(target = hp)

summary(mtcars)



###############REGRESSION TECHNIQUES#################
###### 1. Simple Linear Regression
#we use this technique when we want to consider only one variable for making a prediction 
#here w have one dependant (continuos) and one independant variables (continuos or discrete)
#import mtcars dataset
summary(mtcars) 
attach(mtcars)

c<-cor(mtcars[,-1])#first col is char type so remove it while calc correlation
c

corrplot(c, method = "circle")#so we identify that there is a strong -ve relation between mpg and wt so will build a model using this two

reg_mtcars<-lm(mpg~hp, data=mtcars)#generating linear regression model Both variable should be continus
reg_mtcars #we are getting beta0 i.e intercept and beta1 i.e error term.
summary(reg_mtcars) 

######### how to read summary of regression model######
# The residuals are the difference between the actual values of the variable you're predicting and predicted values from your regression (y - y). For most regressions you want your residuals to look like a normal distribution when plotted. If our residuals are normally distributed, this indicates the mean of the difference between our predictions and the actual values is close to 0.

# The t-values test the hypothesis that the coefficient is different from 0. You can get the t-values by dividing the coefficient by its standard error. The t-values also show the importance of a variable in the model.

# #in this p value will generate that identifies significances level of ur variables.if p<0.05 then it is most significant i.e *** 

# R-squared is close to 1 then our model is good 

# Adjusted R-squared shows the same as R2 
# Residual standard error: the standard deviation of the residuals

#DF is degree of freedom which is equal to total no of rows - beta0 and beta 1 i.e 109(total no of rows)-2=107

# F-statistic & p-value: the p-value of the model. 
######

# Give save the chart file set name.
png(file = "linearregression.png")
getwd()#to check where chart file is stored

# Plot the chart.
plot(mtcars$mpg,mtcars$wt,col = "blue",main = "mpg & wt Regression",
     abline(lm(mtcars$wt~mtcars$mpg)),cex = 1.3,pch = 16,xlab = "Weight of car",ylab = "miles per gallon")

# Save the file.
dev.off()
attach(mtcars)

new_data<-data.frame(wt=2.1)#create a data frame of new data for making a prediction on it
predict(reg_mtcars, new_data, interval = "predict")#predict the "mpg" for new entry of "wt" of car within prediction interval by using reg model
predict(reg_mtcars, new_data, interval = "conf")#predict the "mpg" for new entry of "wt" of car within confidance interval by using reg model

reg1<-lm(mpg~log(wt))#Logarithmic Transformation to increase accuracy of model

summary(reg1)#acuracy increased
a<-exp(coef(reg1))#as we hv made a log transformation on variables the coefficients also in log form
#and we can not build an expression using log values so convert it to exponensial form
predict(reg1, new_data, interval = "predict")#so make a prediction using new model
attach(mtcars)
model.car<-lm(mpg~+hp+drat+wt)
summary(model.car)

reg2<-lm(log(mpg)~wt)#Logarithmic Transformation to increase accuracy of model
summary(reg2)#accuracy of model decreases so we choose the reg1 moddel which has a greater accuracy than other two



###### 2. Multiple Linear Regression
#we use this technique when we want to consider 2 or more variables for making a prection 
#here w have one dependant (continuos) and multiple independant variables (continuos or discrete)
#import cars dataset
attach(Cars)
summary(Cars)

#calculate the correlation coefficient for each variable
c1<-cor(Cars)#first col is char type so remove it while calc correlation
corrplot(c1, method = "circle")# correlation plot of all variables
#so we identify HP, SP are strongly correlated to mpg



model.car<-lm(MPG~VOL+HP+SP+WT)#multiple linear regression model
summary(model.car)#we identify that Vol and WT are not significant to model so remove it and again buid a model

model.car1<-lm(MPG~HP+SP)
summary(model.car1)#it decreases the accurracy of our model

model.carV<-lm(MPG~VOL)#build a model by using only VOL
summary(model.carV)# now the variables r significant bt, it decreases the accurracy of our model

model.carW<-lm(MPG~WT)#build a model by using only WT
summary(model.carW)# now the variables r significant bt, it decreases the accurracy of our model

model.carVW<-lm(MPG~WT+VOL)#build a model by using VOL and WT
summary(model.carVW)#the combination of VOL and WT is not significant to the model

#if u have some error values in ur dataset den all the other values can b influenced by it
#to check the error values from ur dataset we use influence index plot
#install package mvinfluence
influenceIndexPlot(model.car)#function provides index plots of Cook's distances, leverages, Studentized residuals, and outlier significance levels for a regression object
#overall graph shows 71 st and 77 th observation from dataset is influential
#so go to dataset and check 71 and 77 row we identifies that it is not correctly calculated

model.car1<-lm(MPG~VOL+SP+HP+WT, data=Cars[-77,])#remove 77th obsv and build a model
summary(model.car1)#we notice that the accuracy getting increased

vif(model.car)#to check the co-linearity problem if vif>10 hv more co-linearity problem
#The VIF(Variance Infation Factor) estimates how much the variance of a regression coefficient is inflated due to multicollinearity in the model.
# it's presence can adversely affect your regression results. 
avPlots(model.car)#we identify that there is no linear relation between mpg with vol and wt
#bt have a -ve linear reln btw mps and hp, +ve linear reln in btw mpg and sp

finalmodel.car<-lm(MPG~VOL+SP+HP, data = Cars[-77,])#create a new model using vol, sp, hp
summary(finalmodel.car)#accuracy increased

plot(finalmodel.car)#it shows 4 different plot from that check QQ-plot to check normal distribution of data points


