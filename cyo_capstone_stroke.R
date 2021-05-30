# Install required R libraries to initial project.
if (!require(caret)) install.packages('caret')
if (!require(class)) install.packages('class')
if (!require(corrplot)) install.packages('corrplot')
if (!require(data.table)) install.packages('data.table')
if (!require(dplyr)) install.packages('dplyr')
if (!require(e1071)) install.packages('e1071')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(httr)) install.packages('httr')
if (!require(knitr)) install.packages('knitr')
if (!require(lubridate)) install.packages('lubridate')
if (!require(randomForest)) install.packages('randomForest')
if (!require(readxl)) install.packages('readxl')
if (!require(rpart)) install.packages('rpart')
if (!require(ROSE)) install.packages('ROSE')
if (!require(tidyverse)) install.packages('tidyverse')

# Load libraries
library(caret)
library(class)
library(corrplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(httr)
library(knitr)
library(lubridate)
library(randomForest)
library(readxl)
library(rpart)
library(ROSE)
library(tidyverse)

# Report up to 4 significant digits
options(digits = 4)

# Load the Required Data - Stroke PRediction Dataset
# Data comes from Kaggle (https://www.kaggle.com/fedesoriano/stroke-prediction-dataset) courtesy of fedesoriano.
# We load the data from GitHub where the csv file is stored. We accomplish this as follows:

strokedata <- read.csv("https://raw.githubusercontent.com/RossenBradley/stroke-prediction/main/strokedata.csv")

# We run the class() function to determine the class of our dataset.
class(strokedata)

# We run the glimpse() function in order to gain valuable insights into the breakdown of the dataset.
glimpse(strokedata)

# We run the summary() function in order to gain even more valuable insights into the breakdown of the dataset and the respctive columns.
summary(strokedata)
# From this function, we can see that we will have some issues in the modeling stages with some of our character class variables such as bmi, smoking_status and ever_married. In the modeling stage, we will transform these variables to make it possible to conduct our analyses.

# Check the number of Null values in the dataset. With no Null values in the data, we don't need to clean the data to account for this potential issue.
sum(is.na(strokedata))

# Checking our dataset participants by gender
table(strokedata$gender)

# From the information above, we can see that one person in the dataset chose "Other" as the gender. We need to clean this dataset and remove this row of data as this is obviously an outlier and removing this row only trims our dataset by 0.02%.
strokedata<- strokedata[strokedata$gender !="Other", ]
table(strokedata$gender)

# After filtering out our outlier gender variable, we can better understand the breakdown of our dataset by gender, ever married, residence_type etc.
prop.table(table(strokedata$gender))
prop.table(table(strokedata$ever_married))
prop.table(table(strokedata$Residence_type))

# We will continue to explore our dataset as follows:
stroke <- strokedata %>% filter(stroke==1)
nostroke <- strokedata %>% filter(stroke==0)
tableinput <- ifelse(strokedata$stroke==0,"No_Stroke","Stroke")
table(strokedata$gender, tableinput)
prop.table(table(strokedata$gender, tableinput))
# The table shows that we have 249 patients that were diagnosed with stroke which represents 4.9% of all people included in the dataset.

# One nice piece of our data exploration through R is we can come to conclusions using raw totals and proportions such as;
table(strokedata$ever_married, tableinput)
prop.table(table(strokedata$ever_married, tableinput))
# The first line of code tells us the raw totals of how many patients make up each distinction. For example, we have 220 married people who developed a stroke.
# Conversely, using the second line of code, we can see that approx 4.3% of all members of our dataset were married and suffered a stroke.

# Next, we are going to explore the correlation of the variables in the dataset. This is done as follows:
corrplot(round(cor(data.frame(data.matrix(strokedata))),2), method = "square", col= colorRampPalette(c("white","lightpink", "red","brown"))(10), type = "lower", order = "hclust", tl.col = "black", tl.srt = 5)
# Based on this correlation matrix, we can develop some questions of the dataset, which we can answer visually through additional data exploration.
# One key question revolves around the relationship between age and 'Stroke Status - (Stroke or No Stroke)
boxplot(age~stroke, data=strokedata, main="BoxPlots for Stroke by Age", xlab="Suffered A Stroke? (Binary Variable)", ylab="Age",col=(c("purple","yellow")))
# From the boxplot results above, we confirm the results from our correlation matrix showing a positive correlation between age and stroke likelihood. This means that the older the patient, the higher risk they inherently have to suffer a stroke.
# This can be shown in a different way, depending on reader preference, using a histogram as follows:
hist(strokedata$age, main="Histogram of Stroke Frequency by Age", xlab="Patient Age", ylab="Frequency",col=(c("purple","yellow")))
# We can run these boxplots on a number of different variable combinations such as age versus average glucose level which is done as follows:
boxplot(avg_glucose_level~stroke, data=strokedata, main="BoxPlots for Stroke by Avg Glucose Level", xlab="Suffered A Stroke? (Binary Variable)", ylab="Avg Glucose Level",col=(c("purple","yellow")))

# Using the following 3 functions we determine that:
mean(strokedata$age)
mean(stroke$age)
mean(nostroke$age)
# - The mean age of our dataset is 43.23
# - The mean age of a stroke victim in our dataset is 67.73
# - The mean age of a non-stroke victim in our dataset is 49.98

# We can also analyze our dataset age distribution by using a density plot as follows:
strokedata %>% ggplot(aes(x = age, fill = gender)) + geom_density(alpha = 0.4) + geom_vline(aes(xintercept=mean(age)), color = "purple", linetype = "dashed", size = 1.5) + labs(title = "Stroke Data Age Density Plot: All Persons", x = "Person Age", y = "Dataset Density") +scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Or we can run the same plot for only persons in our dataset who have suffered a stroke as follows:
stroke %>% ggplot(aes(x = age, fill = gender)) + geom_density(alpha = 0.4) + geom_vline(aes(xintercept=mean(age)), color = "purple", linetype = "dashed", size = 1.5) + labs(title = "Age Density Plot: Suffered Stroke", x = "Person Age", y = "Dataset Density") +scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# We can also understand the relationship between stroke and some other input variables in the dataset.
# Stroke vs Gender
plot(ggplot(strokedata, aes(x = stroke, fill = gender)) + geom_bar(position = "fill")+ stat_count(geom = "text", aes(label = stat(count)), position = position_fill(vjust = 0.75), color = "black") + scale_fill_manual(values = c("#F0E442", "#CC79A7")))
# Stroke vs Ever Married
plot(ggplot(strokedata, aes(x = stroke, fill = ever_married)) + geom_bar(position = "fill")+ stat_count(geom = "text", aes(label = stat(count)), position = position_fill(vjust = 0.75), color = "black") + scale_fill_manual(values = c("#F0E442", "#CC79A7")))

# Again, there are a large number of connections between these variables that can be explored (stroke likelihood vs martial status, stroke likeilihood versus presence of hypertension etc) but the above diagrams show how we make these connections to gather useful insights into the makeup of the dataset.
# Our correlation matrix does a good job to highlight these connections between variables.

# Model Development Section
# First thing we want to do is to remove the ID column since it doesn't factor into our analyses
stroke <- stroke[2:12]
nostroke <- nostroke[2:12]
strokedata <- strokedata[2:12]

# Before we get going on this step, we need to engage in some data manipulation. Right now, a number of our variables are in character form or are binary variables (0s and 1s) which we want to convert over to "Yes" or "No".
strokedata$hypertension<- factor(strokedata$hypertension, levels = c(0,1), labels = c("No", "Yes"))
strokedata$heart_disease<- factor(strokedata$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
strokedata$stroke<- factor(strokedata$stroke, levels = c(0,1), labels = c("No", "Yes"))
strokedata$gender<-as.factor(strokedata$gender)
strokedata$ever_married<-as.factor(strokedata$ever_married)
strokedata$work_type<-as.factor(strokedata$work_type)
strokedata$Residence_type<-as.factor(strokedata$Residence_type)
strokedata$bmi<-as.numeric(strokedata$bmi)
strokedata$smoking_status<-as.factor(strokedata$smoking_status)

# When we run the summary() function again, we see this populates a more elaborate and detailed description of our dataset.
summary(strokedata)

# Before initiating this splitting process, we need to check for any NA values as follows:
colSums(is.na(strokedata))
# This table shows us that we have 201 A values in the BMI column that we need to account for before proceeding. This is confirmed by:
summary(strokedata$bmi)
# In addition to the 201 rows containing NA data for BMI, we have another issue identified here pertaining to outlier data in this column of data. We can show this visually as follows:
plot(ggplot(strokedata, aes(x=bmi)) + geom_histogram(color="purple", fill="yellow", binwidth = 8) + geom_vline(aes(xintercept=28.9), color="purple", linetype="dashed", size=1.5))+ labs(title="Stroke Data: BMI Frequency Plot",x="BMI Value", y = "Frequency Count")
# According to data published on the WHO (World Health Organization) website, BMI values range from 10 to 50. 15 would be 'Extreme Thinness' and 45 would be 'Extreme Obesity'. From this dataset, we can see that out maximum BMI is 97.6 which is an astronomical figure.
# We need to account for these extreme values in the BMI column when we are accounting for the NA issues with BMI column.
# First, we run the na.omit() function to get rid of the NAs in the BMI column. This takes away the 201 NA values we have detected.
full_strokedata <- strokedata
strokedata <- na.omit(strokedata)
# Running the following line of code, we find that there are 35 BMI inputs that are greater than 54.99999. We allowed some freedom over the 50 maximum denoted by the WHO to account for those grossly obese people whose BMI are slightly above the expected maximum of 45. But this helps us account for the extremely unexpected variables like our pre-existing maximum of 97.6.
sum(strokedata$bmi > 54.99999)
# We then transition these variables into NAs and then omit them from the dataset.
strokedata$bmi[strokedata$bmi > 54.99999] <- NA
strokedata <- na.omit(strokedata)
# We then run the following line of code, seen previously, to make sure these NAs are accounted for and removed.
colSums(is.na(strokedata))
# Using the glimpse() function, we see that our mutated dataset (removing NAs in the BMI and extreme outliers in the BMI column) consists of 4,873 rows which is down from the original 5,109.
# After completing this data cleaning, we are still left with 95.4% of our original dataset, but this dataset should now allow us to make more meaningful conclusions through the following modeling.
nrow(strokedata)/nrow(full_strokedata)
# Finally, we can re run our BMI plot to compare with the one we sat previously.
plot(ggplot(strokedata, aes(x=bmi)) + geom_histogram(color="purple", fill="yellow", binwidth = 4) + geom_vline(aes(xintercept=28), color="purple", linetype="dashed", size=1.5))+ labs(title="Stroke Data: BMI Frequency Plot Adj",x="BMI Value", y = "Frequency Count")

# Splitting the Dataset into Training and Test Sets
# The train-test split procedure is used to estimate the performance of machine learning algorithms when they are used to make predictions on data not used to train the model.
# We split our data into train and test splits to prevent your model from overfitting and to accurately evaluate your model.
# 70/30 and 80/20 splits are common in the data science world and allow us more freedom than if we chose a 90/10 split. 90/10 splits are more commonly reserved for very large datasets where the 10% still captures a high number of rows/information. 
set.seed(123)
split<-createDataPartition(strokedata$stroke, p = 0.7, list = FALSE)
strokedata_train<-strokedata[split,]
strokedata_test<-strokedata[-split,]

# We also want to check out the proportion (Yes/No to Stroke) for our Train Set compared with our overall model.
prop.table(table(strokedata_train$stroke))
prop.table(table(strokedata_test$stroke))
prop.table(table(strokedata$stroke))
# Our proportions are very similar which minimizes the risk to us that our model are over-fit.

#1. Logistic Regression Analysis 
# At this point of the exercise, we are able to run our initial model: Logistic Regression
lin_model <- glm(formula = stroke ~ gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status, family = "binomial", data = strokedata_train)
summary(lin_model)
# From the results achieved in the summary() function, we see that we have a number of statistically insignificant variables in the dataset. We can use the stepwise regression method to run a regression that better hunts out the most statistically significant variables.
lin_stepwise <- step(object = glm(stroke ~ ., family = "binomial", data = strokedata_train), scope = list(lower = glm(stroke ~ 1, family = "binomial", data = strokedata_train), upper = glm(stroke ~ ., family = "binomial", data = strokedata_train)), direction = "both", trace = F)
summary(lin_stepwise)
# This adjusted logistic regression now contains the most statistically significant variables in our dataset. We need to set up our test data so that we can run this model against that test data to assess its predictability and then report the findings in a confusion matrix.
strokedata$test$prediction <- predict(lin_stepwise, type = "response", newdata = strokedata_test)
strokedata_pred <- predict(lin_stepwise, type = "response", newdata = strokedata_test)
result_pred <- ifelse(strokedata_pred >= 0.5, "Yes", "No")
strokedata_test$prediction <- result_pred
glm_conf_mat <- print(confusionMatrix(as.factor(result_pred), reference = strokedata_test$stroke, positive = "Yes"))

#2. RMSE Modeling
# Root Mean Squared Error, which measures the model prediction error. It corresponds to the average difference between the observed known values of the outcome and the predicted value by the model. RMSE is computed as RMSE = mean((observed - predicted)^2) %>% sqrt() . The lower the RMSE, the better the model. Our first step is to convert the dataset to a new dataframe and to switch required variables over to dummy variables.
strokedata_dummy <- dummyVars("~stroke + gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status", data = strokedata)
strokedata_dummy <- data.frame(predict(strokedata_dummy, newdata = strokedata))
str(strokedata_dummy)
# Now, our step is to remove double counted data categories (such as ever_married No and ever_married Yes counting the same variable which is ever_married)
strokedata_dummy$stroke.No <- NULL
strokedata_dummy$gender.Female <- NULL
strokedata_dummy$hypertension.No <- NULL
strokedata_dummy$heart_disease.No <- NULL
strokedata_dummy$ever_married.No <- NULL
strokedata_dummy$Residence_type.Rural <- NULL
#After creating the dummy dataset, we can create new test and train datasets for the purposes of us breaking down the model.
index<- createDataPartition(strokedata_dummy$stroke, p = 0.7, list = FALSE)
train_d<-strokedata_dummy[index,]
test_d<-strokedata_dummy[-index,]
# Calculate the mean of the test data and calculate the RMSE
mu <- print(mean(train_d$stroke))
model_a <- print(RMSE(test_d$stroke, mu))
# Our RMSE is very low, which on the surface appears to be an excellent thing, this model is suffering from an imbalance between the proportions of our target variable who have suffered a stroke and those who have not. In order to properly run a prediction model, we need to account for this issue. A good way of thinking about this is that 4.3% of our persons have suffered a stroke and if we simply created a model which predicted 'No Stroke' for each person in the 4873 strokedata set, this model would be 95.73% accurate in overall terms but we would be 0% effective at predicting any of our 208 stroke persons.

#3. ROSE Library Modeling Attempt - Accounting for Sampling Biases in Target Variable
# As such, we will take a more involved attempt to establish a modeling technique which maintains a quality accuracy but accounts for the distribution difference.
# The library we will use for this is the ROSE library. This stands for Randomly Oversampling Examples.
over_stroke <- ovun.sample(stroke~., data = strokedata_train, method = "over", N = 8500)$data
rf_over <- randomForest(stroke~., data = over_stroke)
confusionMatrix(predict(rf_over, strokedata_test), strokedata_test$stroke, positive = "Yes")
# The results from this confusion matrix are much much more promising as we are now better able to predict stroke victims after better accounting for our sampling biases.
strokedata_rose <- ROSE(stroke~., data = strokedata_train, N = 9000, seed = 1000)$data
table(strokedata_rose$stroke)
rfrose <- randomForest(stroke~., data = strokedata_rose)
rose_conf_mat <- print(confusionMatrix(predict(rfrose, strokedata_test), strokedata_test$stroke, positive = "Yes"))
# This is our best model to date for prediction purposes. While the accuracy is much lower than we have seen previously (which isn't a 'real' accuracy measure because of the issues predicting stroke cases), we do a much better job at predicting both cases rather than just the no stroke victims. 

# Conclusions and Comments Section
# What started out as an interesting dataset on Kaggle soon turned into quite a battle. Above, we explored a number of different modeling techniques (both researched and shown in the course) but in the end the ROSE model was the best one as it helped us overcome some of our issues with predicting persons who suffered a stroke.
# without accounting for the balancing issues in the target variable, the different machine learning models (specifically shown with the first RandomForest model to the second RandomForest Model) were not picking up on the stroke victims in a way that would help us detect and predict their presence.
# In the future, more work can be done to balance out the data and account for the differences in sample sizes for our target variable (Yes/No to Stroke).
# I found this project and dataset challenging because of this imbalance, and any further techniques to account for this would be best served to help us better predict the stroke victims in the test set.
# The ROSE library/extension was a gamechanger for this project as it was the only real way to account for the sampling biases in the target variable.