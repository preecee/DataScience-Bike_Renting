##--------------------------------------------------------------------------------------------------------------------------##
#### Project Assignment by Preeti Chauhan ##
##This R code is for project : Bike Renting
##The objective of this Case is Predication of bike rental count on daily based on the environmental and seasonal settings
##--------------------------------------------------------------------------------------------------------------------------##

#/*I am going to divide whole project in to 8 parts:
# --->  1.) Define and categorize problem statement
# --->  2.) Gather the data
# --->  3.) Prepare data for consumption
# --->  4.) Perform Exploratory Data Analysis
# --->  5.) Models Building
# --->  6.) Evaluate and compare Model performances and choose the best model
# --->  7.) Hypertune the selected model
# --->  8.) Produce sample output with tuned model*/

#-----------------------------------------------------------------------------------------------------------------------------------##

## ----------- Part 1: Define and categorize the problem statement --------------
#### The problem statement is to "Predict the daily bike rental count based on the environmental and seasonal settings"
##### This is clearly a 'Supervised machine learning regression problem' to predict a number based on the input features
## ----------- Part 1 ends here ----------------- 
##-------------------------------------------------------------------------------------------------------------------------------------------------------------------#

##------------- Import all the required libraries--------------

#---- for model building
library(caret)
library(randomForest)
library(gbm)

#---- for visualization---
library(ggplot2)
library(corrgram) 
#------ for model evaluation -----

#---- All the required libraried imported-----------------
##-------------------------------------------------------------------------------------------------------------------------------------------------------------------#

## ------------------- Part 2: Gather the data -----------------
# set the working directory
set.seed(1)
setwd('D:/DataScience_Edwisor/edWisor/Projects/Bike_Rental/')
### Here data is provided as .csv file with the problem.
### Let's import the data 
bike <- read.csv("Dataset/day.csv")
head(bike)
##---------- Part 2 ends here --------------------------
##-------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# ------------Part 3 : Prepare the data for consumption(Data Cleaning) ---------------
#### 3a.) Check the shape/properties of the data
#### 3b.) Completing -- Perform missing value analysis and impute missing values if necessary
#### 3c.) Correcting -- Check for any invalid data inputs , for outliers or for any out of place data
#### 3d.) Creating -- Feature extraction . Extract any new features from existing features if required
#### 3e.) Converting -- Converting data to proper formats
#-------------------------------------------------------------------------------------
#### --------3a.) Check the shape/properties of the data
## Check the shape of the data
dim(bike)
# what we can infer:
## ->the dataset has 731 observations and 16 features

## Check the properties of the data
str(bike)
summary(bike)
# what we can infer:
# ->There are no null values in the dataset
# -> The datatypes are int,num and factor

# --------- 3b.) Completing -- Perform missing value analysis and impute missing values if necessary
#Checking nulls
sapply(bike, function(x) sum(is.na(x)))
# what we can infer:
# ->There are no null values in the dataset.If it had, then eithere the rows/columns had to be dropped or the null values be imputed based on the % of null values

#### ------3c.) Correcting -- Check for any invalid data inputs , for outliers or for any out of place data
# From above observations data doesnot seem to have any invalid datatypes to be handled
# However feature 'instant' doesnot seem relevant to our analysis. Lets drop it
bike['instant'] <- NULL
# Let's check for the outliers in EDA step

#### -------3d.) Creating -- Feature extraction . Extract any new features from existing features if required
str(bike['dteday'])
## We can see that here we have 'dteday', which gives us the exact date. This features has 2 years of data(2011, 2012), all through 12 months(1 to 12) of a year
## Now, year and month information is given in 'yr' and 'mnth' column respectively.
## However, date(day of month) information is not saperately given.
## Lets extract 'date' from 'dteday' column
bike$dteday <- as.Date(tempbike$dteday, format= "20%y-%m-%d")
bike$date<-as.integer(gsub("(.*)[-]", "", bike$dteday))
## Now, 'dteday' column is not required, since we already have year, month, date info in other columns. So lets drop it.
bike$dteday <- NULL
head(bike)
str(bike)

#### 3e.) ------- Converting -- Converting data to proper formats
#We can clearly see that "season", "yr","mnth","holiday","weekday","workingday","weathersit","date" are categories,rather than continous variable.
#Let them convert to categories
categoryFeatureList = c("season", "yr","mnth","holiday","weekday","workingday","weathersit","date")
continousFeatureList = c('temp','atemp','hum','windspeed','casual','registered')
bike[,categoryFeatureList] <- data.frame(apply(bike[categoryFeatureList], 2, as.factor))
str(bike)
# ------------Part 3 : Prepare the data for consumption(Data Cleaning) ENDS here---------------
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

# ------------Part 4 : Exploratory Data Analysis(EDA) STARTS here -----------

#----- 4 a.) Outlier Analysis -----------
## -- Lets do the outlier analysis ----
## -- Visualize continous variables(cnt,temp,atemp,hum,windspeed) and 
##  count with respect to categorical variables("season", "yr","mnth","holiday","weekday","workingday","weathersit","date")with boxplots ---
# Boxplot of MPG by Car Cylinders 

ggplot(aes_string(y = "cnt"), data = bike) + geom_boxplot()
ggplot(aes_string(y = "atemp"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "temp"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "hum"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "windspeed"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)

ggplot(aes_string(y = "cnt", x = "season"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "cnt", x = "yr"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "cnt", x = "mnth"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "cnt", x = "holiday", fill= "yr"), data = bike) + geom_boxplot(outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "cnt", x = "weekday"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "cnt", x = "workingday"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "cnt", x = "weathersit"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)
ggplot(aes_string(y = "cnt", x = "date"), data = bike) + geom_boxplot(fill='blue', outlier.color = 'red', outlier.size = 3, outlier.shape = 18)

#checking the outliers values
boxplot.stats(bike$hum)$out
boxplot.stats(bike$windspeed)$out

# what we can infer from above boxplots:
# -> Overall, not many outliers. Data seems balanced.
# -> Count values doenot have any outliers
# -> There are some outliers for hum and windspeed. However that seems ok, as it tells dataset has datapoints for extreme weather as well
# Lets keep these outliers for now, till we complete full EDA
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#---- 4b.) Correlation Analysis
#--- Explore continous features
#--- Explore categorical features

###---calculating correlation of each continous variable with target variable 'cnt'
PearsonCoef = cor(x = bike[continousFeatureList],y=bike$cnt,method = 'pearson')
cor_df <- data.frame(PearsonCoef)
cor_df$Variables <- rownames(cor_df)

##-----plotting bargraph for correlation analysis --------------
corr_barplot <-ggplot(data=cor_df, aes(x=Variables, y=PearsonCoef)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=PearsonCoef), vjust=-0.35, color="black", size=3)+
  theme_minimal()
corr_barplot
## from the above correlation plot, it is evident that 'atemp' and 'temp' has good correlation with target variable.
##  'casual' and 'tregistered' are not the input variables for our analysis(they are the leaky variable), will drop them.
## 'hum' and 'windspeed' does not have a great coefficient, we'll keep them for noe though.

###------heatmap for correlation matrix---------##
##to check multicollinearity ---##
corrgram(bike, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Bike Rental Correlation Data")

#Clearly, from above heatmap, we can se that the dataset has multicolinearity. 'temp' and 'atemp' are highly correlated.
#Will need to drop one of them.

#Should we do ANOVA test for correlation btw categorical variable and target continous variables. Let's see'
#First explore the relationships through more visualizations

#---- Visualizing PAIRPLOTS---
pairs(bike[,continousFeatureList])
# evident from this 'temp' and 'atemp' are highly correlated. One needs to be dropped.

#---------------- Check distribution of target variables ---------------

d <- density(bike$cnt)
plot(d, main="Kernel Density of Total Count of Rented Bikes")
polygon(d, col="red", border="blue") 

#The target variable is more or less normally distributed around 4500 count.fields. This is good for input to models
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
# --------------Explore categorical features-----------
head(bike[,categoryFeatureList])

#All the values in categorical variables are already 'numbers'. No need to do 'string' -> 'number' conversions as storing categorical data in numbers is more efficient.

#------- Lets see the distribution of each categorical variable with pie-chart distribution

library(dplyr)
#------------------------------------------
bike_cat <-bike[,categoryFeatureList]

# -------------create pie plot for 'yr'-------------
  df_pie <- bike_cat %>%
  group_by(yr) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(yr)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = yr)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()

# -------------create pie plot for 'season--------
df_pie <- bike_cat %>%
  group_by(season) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(season)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = season)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()
# -------------create pie plot for 'weathersit'--------------
df_pie <- bike_cat %>%
  group_by(weathersit) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(weathersit)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = weathersit)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()
# -------------create pie plot for 'mnth'--------------
df_pie <- bike_cat %>%
  group_by(mnth) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(mnth)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = mnth)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()
# -------------create pie plot for 'date------------
df_pie <- bike_cat %>%
  group_by(date) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(date)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = date)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()
# -------------create pie plot for 'holiday'-----
df_pie <- bike_cat %>%
  group_by(holiday) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(holiday)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = holiday)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()

# -------------create pie plot for 'weekday'-----
df_pie <- bike_cat %>%
  group_by(weekday) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(weekday)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = weekday)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()

# -------------create pie plot for 'workingday'-----
df_pie <- bike_cat %>%
  group_by(workingday) %>%
  summarise(counts = n())
df_pie <- df_pie %>%
  arrange(desc(workingday)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
#  pie plot
ggplot(df_pie, aes(x = "", y = prop, fill = workingday)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  theme_minimal()

#What we can infer from above pieplots-------
#-> Most of the categorical variables are uniformally distributed, except 'holiday','weathersit','workingday'
#-> This makes sense for 'weathersit', as extreme weather is rare and hence %percentage of extreme weather in whole dataset is low
#-> This makes sense for 'holiday', as number of holidays are less in comparison to working days
#-> This makes sense for 'workingday' for the same reason as above
#-> So, categorical data seems o be pretty much uniformly distributed

#------- Lets see how individual categorical features affect count of the rented bike----------
#graph individual categorical features by count

#-- Lets see how 'yr' affects the count of rented bikes----

cat <- aggregate(x=bike$cnt, by=list(Category=bike$yr), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Year') + ylab('Count of Rented Bikes')+
  theme_minimal()
cat_barplot1

# Does 'yr' affects count of rented bikes
#--> YES. the count have an upward trend wrt year
#------------------------------------------------------------------
#-- Lets see how 'season' affects the count of rented bikes----

cat <- aggregate(x=bike$cnt, by=list(Category=bike$season), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('seasons') + ylab('Count of Rented Bikes')
theme_minimal()
cat_barplot1

# Does 'season' affects count of rented bikes
#--> YES, it seems ppl rent more bikes during season 3 and 2, 
#i.e. highest in fall and summer and less in winter and springs. This makes sense as weather is good to ride during summer and fall.

#------------------------------------------------------------------
#-- Lets see how 'mnth' affects the count of rented bikes----

cat <- aggregate(x=bike$cnt, by=list(Category=bike$mnth), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Months') + ylab('Count of Rented Bikes')
theme_minimal()
cat_barplot1

#Does 'month' affects count of rented bikes
#-->YES.ppl are likely to rent bikes more btwn the months May- October and lowest in month of Jan,Feb and Dec(in that order). 
#This again makes sense, as this trend is in sync with favourable weather conditions

#------------------------------------------------------------------
#-- Lets see how 'holiday' affects the count of rented bikes----

cat <- aggregate(x=bike$cnt, by=list(Category=bike$holiday), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Holiday') + ylab('Count of Rented Bikes')
theme_minimal()
cat_barplot1
#Does 'holiday' affects count of rented bikes
#--> YES. ppl rent more bikes on non-holiday than holiday. It makes sense as bikers who commute to work/school will be less on holiday.

#------------------------------------------------------------------
#-- Lets see how 'weekday' affects the count of rented bikes----
cat <- aggregate(x=bike$cnt, by=list(Category=bike$weekday), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Weekday') + ylab('Count of Rented Bikes')
theme_minimal()
cat_barplot1

#Does 'weekday' affects count of rented bikes
#--> To some extent Yes. ppl seems to rent lesser bikes on Sat/ Sun. ie. over the weekend. Again makes sense as school and offices are closed on weekend.
#Monday also has lesser count of rented bikes. It may be possible the ppl visit to other places/cities over weekend and travel back in car on Monday, istead of renting bikes.

#------------------------------------------------------------------
#-- Lets see how 'weather' affects the count of rented bikes----
cat <- aggregate(x=bike$cnt, by=list(Category=bike$weather), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Weather') + ylab('Count of Rented Bikes')
theme_minimal()
cat_barplot1

#Does 'weather' affects count of rented bikes
#--> Most definately YES. noone rented bike on extreme weather(season=4). ppl rent maximum bikes during a clear day (weathersit=1)

#------------------------------------------------------------------
#-- Lets see how 'weather' affects the count of rented bikes----
cat <- aggregate(x=bike$cnt, by=list(Category=bike$weather), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Weather') + ylab('Count of Rented Bikes')
theme_minimal()
cat_barplot1

#Does 'weather' affects count of rented bikes
#--> Most definately YES. noone rented bike on extreme weather(season=4). ppl rent maximum bikes during a clear day (weathersit=1)

#------------------------------------------------------------------
#-- Lets see how 'date' affects the count of rented bikes----
cat <- aggregate(x=bike$cnt, by=list(Category=bike$date), FUN=sum)
cat_barplot1 <-ggplot(data=cat, aes(x=Category,y=x)) +
  geom_bar(stat="identity", fill="blue")+
  xlab('Date') + ylab('Count of Rented Bikes')
theme_minimal()
cat_barplot1

#Does 'date' affects count of rented bikes
#--> Well there is no set trends. It seems to be random. Lets keep this for now

#------------------------------------------------------------------

#------ Exploratory Data Analysis ENDS Here--------------------------------------------------------------------------------------------------------------------------
# Final observations:
#1.) 'casual' and 'registered' are leak variables. They need to be dropped from the dataset
#2.) 'atemp' and 'temp' are very strongly correlated . Drop 'atemp' from the dataset(since it has higher p-value than 'temp')
#3.) 'date' does not seem to have any affect on count of bikes, it can be dropped from the dataset
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

#---- Drop the features mentioned above(as part of feature engineering)

bike_aftr_ftr_eng <- bike[ , !(names(bike) %in% c('casual','registered','temp'))]
head(bike_aftr_ftr_eng)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------Part 5 : Model Builing starts here ----------------------
#Train the models with both datasets(before and after feature engineering)
# For models,I'll only use the dataset with feature engineering implemented
#------------------------------------------------------------------

# 1.) I am selecting 3 models to test and evaluate
#   -> Linear Regression Model
#   -> Random Forrest (ensemble method using bagging technique)
#   -> Gradient Boosting (ensemble method using boosting technique)
#2.) Cross validation    
#3.) All these 3 models will be compared and evaluated(with and without feature engineering)
#4.) We'll choose the best out of 3

#------------------------------------------------------------------
#----- 5a.) -- Selecting train and test datasets for cross validations
#split in to test and train(after featr engineering)
train_index=createDataPartition(bike_aftr_ftr_eng$cnt, p=0.8, list = FALSE)
train=bike_aftr_ftr_eng[train_index,]
test=bike_aftr_ftr_eng[-train_index,]
head(train)
head(test)

X_train = train[ , !(names(train) %in% c('cnt'))]
Y_train = train['cnt']
X_test = test[ , !(names(test) %in% c('cnt'))]
Y_test = test['cnt']

dim(X_train)
dim(X_test)
#--- *AFT <=> After Feature Engineering------
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#--- define a function which takes model, predicted and test values and returns evalution matrix: R-squared value and MeanAbsoluteError
#------ Define Function ----------------------------------------
model_eval_matrix <- function (model,actual, predicted)
{
r_squared = 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
mae = MAE(actual, predicted)
return(c(model,r_squared,mae))
}
#------ Function definition ends here --------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------- 5c.) Define and fit models ---------------

#-------------------------  Linear regession model ----------------------------------
lrm_regressor = lm(cnt~. , data = train) # Define and train the model

summary(lrm_regressor)
#summary(lrm_regressor)$r.squared
#Residual standard error: 780.4 on 529 degrees of freedom
#Multiple R-squared:  0.8555,	Adjusted R-squared:   0.84 
#F-statistic: 54.95 on 57 and 529 DF,  p-value: < 2.2e-16

Y_predict_lrm = predict(lrm_regressor, X_test) #predict with the model

actuals_preds_lrm = data.frame(cbind(actuals=test$cnt, predicteds=Y_predict_lrm))
actuals_preds_lrm

#calling function and storing performance values
performance_vector_lrm = model_eval_matrix('LRM',test$cnt,Y_predict_lrm)

#------- Random Forest Model (Ensemble method using Bagging technique) --------------

forest_reg = randomForest(cnt~. , data = train, ntree = 800, importance = TRUE) # 800 tree is best bestameter as tested
#RF2List

Y_predict_forest = predict(forest_reg, X_test)

actuals_preds_forest <- data.frame(cbind(actuals=test$cnt, predicteds=Y_predict_forest))
actuals_preds_forest

MAE(test$cnt,Y_predict_forest)

#calling function and storing performance values
performance_vector_rf = model_eval_matrix('RF',test$cnt,Y_predict_forest)

## ----------- Building Gradient Boosting Model (Ensemble method using Boosting technique) ---------------
#gbm_reg = gbm(cnt~. ,data = train , distribution = "gaussian", n.trees = 2000) # without parameter hypertuning
gbm_reg = gbm(cnt~. ,data = train , distribution = "gaussian", n.trees = 2000, shrinkage = 0.01, interaction.depth = 4) # with parameter hypertuning

Y_predict_gbm = predict(gbm_reg, X_test, n.trees = 2000)

actuals_preds_gbm <- data.frame(cbind(actuals=test$cnt, predicteds=Y_predict_gbm))
actuals_preds_gbm

#calling function and storing performance values
performance_vector_gbm = model_eval_matrix('GBM',test$cnt,Y_predict_gbm)

MAE(test$cnt,Y_predict_gbm)
#------After parameter hypertuning----------
#561.1772 -- using parameter 100 trees
#499.1291 -- using parameter 1000 trees
#418.8214 -- using parameter 2000 trees and interaction.depth = 4

summary(gbm_reg)
#> summary(gbm_reg)
#var    rel.inf
#atemp           atemp 37.6942000
#yr                 yr 30.8209159
#season         season  9.2478848
#hum               hum  5.6385557
#weathersit weathersit  5.5856582
#mnth             mnth  5.1422646
#windspeed   windspeed  2.7114747
#date             date  2.5198982
#weekday       weekday  0.3664802
#holiday       holiday  0.2726677
#workingday workingday  0.0000000

#-------------------------------------------Part 5 ENDS here -------------------------------------------------------------------------------------------------------

#----------------------------------------Part 6 : Model comparisions STARTS here---------------------------

#-----Stroring all model performances in dataframe to compare----
df_mod_performance = data.frame(rbind(performance_vector_lrm,performance_vector_rf,performance_vector_gbm))
colnames(df_mod_performance) = c('Model','R-Squared','MAE')
df_mod_performance
#It gives the following results

#Model         R-Squared              MAE
#LRM 0.846566706683683 576.630584074184
#RF 0.844487249966827 578.234136312984
#GBM 0.907104622398796 417.697573647421

# Clearly, from above table, Gradient Boosting is the best model out of 3
# So, Our final model will be Gradient Boosting Model 

#-------------- Hypertuning the final model -----
# We used hit and trial method to find the best parameters for our model
#------After parameter hypertuning----------
#MAE = 561.1772 -- using parameter 100 trees
#MAE = 499.1291 -- using parameter 1000 trees
#MAE = 418.8214 -- using parameter 2000 trees and interaction.depth = 4

#----------------------------------------Part 6 : Model comparisions ENDS here ---------------------

#-------------------------------------------Part 7 : Produce sample output with tuned model STARTS here----------------------

#What can be inferred from above observations:
#-->It is evidently clear that gradient boost gives the best performance out of all the models
#-->Hence we'll consider Gradient Boosting as our final model
#
final_bike_prediction_df=X_test
final_bike_prediction_df['ActualCount'] = Y_test
final_bike_prediction_df['PredictedCount'] = Y_predict_gbm
#--- Sample output(with actual counts and predicted counts) ---
final_bike_prediction_df











































