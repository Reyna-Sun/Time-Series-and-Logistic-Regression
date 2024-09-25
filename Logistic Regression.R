#REYNA VARGAS ANTONIO
#ID: X23127635

#==================================
# LIBRARIES
#=================================
install.packages("Hmisc")
install.packages("pscl")
install.packages("ggcorrplot")
install.packages("aod")
library(FactoMineR)
library(ggfortify)
library(devtools)
library(factoextra)
library(dplyr) 
library(tidyverse)
library(caret)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(psych)
library(RColorBrewer)
library(ggcorrplot)
library(scales)
library(Amelia)
library(aod)
library(pscl)
library(plotly)
library(ROCR)
theme_set(theme_bw())

# Switch your working directory to where ever you have downloaded the file.
setwd('C:/Users/reyna/Documents/MSc in Data Analytics/Statistics for data analytics/0.1 Final Project')

# Read in our csv and put it in a data.frame.
cardiac_df <- read.csv('cardiac.csv',  header=T, na.strings=c(""), stringsAsFactors = T)

#=================================
# EXPLORATORY DATA ANALYSIS
#=================================
#view first six rows of dataset
head(cardiac_df)
#summarize dataset
summary(cardiac_df)

#set "caseno" column as row names
cardiac_df <- cardiac_df %>% column_to_rownames(., var = 'caseno')
head(cardiac_df)

#---------------------------------
# Count the number of Missing Values
#---------------------------------
colSums(is.na(cardiac_df))
sum(is.na(cardiac_df))
missmap(cardiac_df, main = "Missing values vs Observed")

#---------------------------------
# Checking unique values in each variable
#---------------------------------
sort(unique(cardiac_df$age))
sort(unique(cardiac_df$weight))
sort(unique(cardiac_df$gender))
sort(unique(cardiac_df$fitness_score))
sort(unique(cardiac_df$cardiac_condition))

#---------------------------------
# VISUALIZATIONS OF NUMERIC VARIABLES 
#---------------------------------
#create histogram of values for "age"
age <-ggplot(data=cardiac_df, aes(x=age)) + geom_histogram(fill="steelblue", color="black") +
          ggtitle("Histogram of Age Values")
ggplotly(age)
# create boxplot of values for "weight"
weight <- ggplot(data=cardiac_df, aes(x=weight)) + geom_histogram(fill="steelblue", color="black") +
              ggtitle("Histogram of Weight Values")
ggplotly(weight)
# create boxplot of values for "fitness_score"
score<-ggplot(data=cardiac_df, aes(x=fitness_score)) + geom_histogram(fill="steelblue", color="black") +
          ggtitle("Histogram of Fitness Score Values")
ggplotly(score)

#---------------------------------
# VISUALIZATIONS OF CATEGORICAL VARIABLES 
#---------------------------------
#create Bar chart of values for "gender"
gender<-ggplot(data=cardiac_df, aes(y=gender)) + geom_bar(fill="steelblue", color="black") +
           ggtitle("Bar chart of Gender Values")
ggplotly(gender)
#create Bar chart of values for "cardiac_condition"
condition<-ggplot(data=cardiac_df, aes(y=cardiac_condition)) + geom_bar(fill="steelblue", color="black") +
              ggtitle("Bar chart of Cardiac Condition Values")
ggplotly(condition)

#---------------------------------
# VISUALIZATIONS BETWEEN VARIABLES
#---------------------------------
#create scatterplot of Age vs Weight by Cardiac Condition 
scat1<-ggplot(cardiac_df, aes(x = age, y = weight)) + geom_point(aes(color = factor(cardiac_condition))) + ggtitle("Distribution of Age vs Weight")
ggplotly(scat1)
#create scatterplot of Age vs fitness Score by Cardiac Condition 
scat2<-ggplot(cardiac_df, aes(x = age, y = fitness_score)) + geom_point(aes(color = factor(cardiac_condition))) + ggtitle("Distribution of Age vs fitness Score")
ggplotly(scat2)
#create scatterplot of Weight vs Fitness Score by Cardiac Condition 
scat3<-ggplot(cardiac_df, aes(x = weight, y = fitness_score)) + geom_point(aes(color = factor(cardiac_condition))) + ggtitle("Distribution of weight vs fitness Score")
ggplotly(scat3)

#---------------------------------
# Handling Outliers
#---------------------------------
#create boxplot of cardiac_condition vs age
ggplot(data=cardiac_df, aes(y=cardiac_condition, x=age)) + geom_boxplot(fill='steelblue', color='black', notch = TRUE)+
  geom_jitter(position=position_jitter(0.2)) + ggtitle("Boxplot of Cardiac Condition vs Age")
boxplot.stats(cardiac_df$age)$out

#find Q1, Q3, and interquartile range for values in points column
Q1_age <- quantile(cardiac_df$age, .25)
Q3_age <- quantile(cardiac_df$age, .75)
IQR_age <- IQR(cardiac_df$age)
#subset data where points value is outside 1.5*IQR of Q1 and Q3
outliers_age <- subset(cardiac_df, cardiac_df$age<(Q1_age - 1.5*IQR_age) | cardiac_df$age>(Q3_age + 1.5*IQR_age))
#view outliers
outliers_age

#create boxplot of cardiac_condition vs weight
ggplot(data=cardiac_df, aes(y=cardiac_condition, x=weight)) + geom_boxplot(fill='steelblue', color='black', notch = TRUE)+
  geom_jitter(position=position_jitter(0.2)) + ggtitle("Boxplot of Cardiac Condition vs Weight")
boxplot.stats(cardiac_df$weight)$out

#create boxplot of cardiac_condition vs weight
ggplot(data=cardiac_df, aes(y=cardiac_condition, x=fitness_score)) + geom_boxplot(fill='steelblue', color='black', notch = TRUE)+
  geom_jitter(position=position_jitter(0.2)) + ggtitle("Boxplot of Cardiac Condition vs Fitness Score")
boxplot.stats(cardiac_df$fitness_score)$out

#Remove Outliers
df<- subset(cardiac_df, age<63)
dim(df)
    
#---------------------------------
# CORRELATION MATRIX
#--------------------------------- 
# correlation between only numerical variables
corr<-cor(df[,unlist(lapply(df, is.numeric))])
corr
corrplot(corr, method = "color", type = "upper", addCoef.col = "black")

#---------------------------------
# ONE-HOT ENCODING
#---------------------------------
print(is.factor(df$gender))
print(is.factor(df$cardiac_condition))

dummy<- dummyVars("~.", data = df)
df1 <- data.frame(predict(dummy, newdata=df))      
head(df1)

#==================================
# LOGISTIC REGRESSION
#=================================

#---------------------------------
# SPLITTING DATA INTO TRAINING AND TEST SETS
#---------------------------------
#create a random seed equa to ID student number = 23127635 
set.seed(23127635)
#Use 70% of dataset as training set and remaining 30% as testing set
splitting <- sample(c(TRUE, FALSE), nrow(df1), replace=TRUE, prob=c(0.7,0.3))
train  <- df1[splitting, ]
test   <- df1[!splitting, ]
head(train)

#---------------------------------
# MODEL
#---------------------------------
model <- glm( cardiac_condition.Absent ~ age + weight + gender.Female + fitness_score , family=binomial(),data= train)
summary(model)

anova(model, test = "Chisq")

caret::varImp(model)

car::vif(model)

coef(model)

#odds ratio and 95% CI
exp(cbind(OR = coef(model), confint(model)))

pR2(model)

#---------------------------------
# MODEL 2
#---------------------------------
model2 <- glm( cardiac_condition.Absent ~ age + weight + fitness_score , family=binomial(),data= train)
summary(model2)

anova(model2, test = "Chisq")

caret::varImp(model2)

car::vif(model2)

coef(model2)

#odds ratio and 95% CI
exp(cbind(OR = coef(model2), confint(model2)))

pR2(model2)

#---------------------------------
# MODEL 3
#---------------------------------
model3 <- glm( cardiac_condition.Absent ~ age + weight, family=binomial(),data= train)
summary(model3)

anova(model3, test = "Chisq")

caret::varImp(model3)

car::vif(model3)

coef(model3)

#odds ratio and 95% CI
exp(cbind(OR = coef(model3), confint(model3)))

pR2(model3)


#---------------------------------
# PREDICTIONS
#---------------------------------
predic <- predict(model3, test, type="response")
predic <- ifelse(predic> 0.5, 1, 0)
predic
#---------------------------------
# EVALUATION
#---------------------------------
ClassError <- mean(predic != test$cardiac_condition.Absent )
print(paste('Accuracy', 1-ClassError))

# ROC-AUC Curve
ROCPred <- prediction(predic, test$cardiac_condition.Absent)
ROCPer <- performance(ROCPred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc
# Plotting curve
roc = performance(ROCPred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1) 
legend(.6, .4, auc, title = "AUC", cex = 1)

confusionMatrix(table(predic, test$cardiac_condition.Absent))

