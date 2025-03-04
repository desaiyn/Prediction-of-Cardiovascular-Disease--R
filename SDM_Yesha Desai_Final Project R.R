rm(list=ls())
setwd("C:/USF BAIS/sem2/SDM")


library(rio)
library(moments)
library(dplyr)
library(ggplot2)
library(caret)
library(stargazer)
library(pROC)

cardio=import("cardio_train.csv")
colnames(cardio)=tolower(make.names(colnames(cardio)))
attach(cardio)
names(cardio)
str(cardio)

# Data cleaning
# Drop the 'id' column
cardio <- cardio %>% select(-id)

# Convert Age from days to years
cardio$age <- cardio$age / 365
# Convert age from years to whole numbers
cardio$age <- floor(cardio$age)


# Check for and remove duplicate rows
cardio <- cardio[!duplicated(cardio), 
                 ]
# Check for missing values
if (sum(is.na(cardio)) > 0) {
  print("The dataset contains missing values.")
} else {
  print("The dataset is complete, with no missing values.")
}


cardio

# Data visualizations

# Count occurrences of each age
age_counts <- table(cardio$age)

# Convert the age_counts table to a data frame for plotting
age_counts_df <- as.data.frame(age_counts)
names(age_counts_df) <- c("Age", "Count")

# Plot 
barplot(age_counts_df$Count, 
        names.arg = age_counts_df$Age, 
        xlab = "Age", 
        ylab = "Count", 
        col= 'skyblue',
        main = "Count of Ages")

# Count occurrences of each gender
gender_counts <- table(cardio$gender)

# Convert the age_counts table to a data frame for plotting
gender_counts_df <- as.data.frame(gender_counts)
names(gender_counts_df) <- c("gender", "count")

# Plot
barplot(gender_counts_df$count, 
        names.arg = gender_counts_df$gender, 
        xlab = "Gender", 
        ylab = "Count", 
        col= c('red','blue'),
        main = "Count of gender")

# Count occurrences of cholesterol
cholesterol_counts <- table(cardio$cholesterol)

# Convert the age_counts table to a data frame for plotting
cholesterol_counts_df <- as.data.frame(cholesterol_counts)
names(cholesterol_counts_df) <- c("cholesterol", "count")

# Plot
barplot(cholesterol_counts_df$count, 
        names.arg = gender_counts_df$cholesterol, 
        xlab = "cholesterol level", 
        ylab = "Count", 
        col= c('red','yellow','lightgreen'),
        main = "Count of cholesterol level")

# Count occurrences of glucose
glucose_counts <- table(cardio$gluc)

# Convert the age_counts table to a data frame for plotting
glucose_counts_df <- as.data.frame(glucose_counts)
names(glucose_counts_df) <- c("glucose", "count")

# Plot
barplot(glucose_counts_df$count, 
        names.arg = glucose_counts_df$gluc, 
        xlab = "Glucose level", 
        ylab = "Count", 
        col= c('red','yellow','lightgreen'),
        main = "Count of Glucose level")

# Count occurrences of smoke
smoke_counts <- table(cardio$smoke)

# Convert the age_counts table to a data frame for plotting
smoke_counts_df <- as.data.frame(smoke_counts)
names(smoke_counts_df) <- c("smoke", "count")

# Plot
barplot(smoke_counts_df$count, 
        names.arg = smoke_counts_df$smoke, 
        xlab = "Smoke", 
        ylab = "Count", 
        col= c('lightgreen', 'red'),
        main = "Patients who smoke and who doesnot")

# Count occurrences of alcohol
alcohol_counts <- table(cardio$alco)

# Convert the age_counts table to a data frame for plotting
alcohol_counts_df <- as.data.frame(alcohol_counts)
names(alcohol_counts_df) <- c("alcohol", "count")

# Plot
barplot(alcohol_counts_df$count, 
        names.arg = alcohol_counts_df$alco, 
        xlab = "Alcohol", 
        ylab = "Count", 
        col= c('lightgreen', 'red'),
        main = "Alcohol intake")

# Count occurrences of physical activity
pa_counts <- table(cardio$active)

# Convert the age_counts table to a data frame for plotting
pa_counts_df <- as.data.frame(pa_counts)
names(pa_counts_df) <- c("pa", "count")

# Plot
barplot(pa_counts_df$count, 
        names.arg = pa_counts_df$active, 
        xlab = "Physical Activity", 
        ylab = "Count", 
        col= c('red', 'lightgreen'),
        main = "Engagement in Physcial Activity")

# Count occurrences of cardio
cardio_counts <- table(cardio$cardio)

# Convert the age_counts table to a data frame for plotting
cardio_counts_df <- as.data.frame(cardio_counts)
names(cardio_counts_df) <- c("cardio", "count")

# Plot
barplot(cardio_counts_df$count, 
        names.arg = cardio_counts_df$cardio, 
        xlab = "Cardio diesase", 
        ylab = "Count", 
        col= c( 'blue', 'red'),
        main = "Presense or absense of cardio disease")


# Set the figure size
options(repr.plot.width = 11, repr.plot.height = 8)

# Create the count plot
ggplot(cardio, aes(x = age, fill = factor(cardio))) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Age", y = "Count", fill = "Cardio") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  ggtitle("Exposure to CVD based on Age")
  

# Model 1- Logistic regression with train and test data
set.seed(1234)

# Splitting data into training and test sets
train_cardio <- createDataPartition(cardio$cardio, p = 0.75, list = FALSE)
traind_cardio <- cardio[train_cardio, ]
testd_cardio <- cardio[-train_cardio, ]

# logistic regression model
logitmodel <- glm(cardio ~ age+ gender+ height+ weight+ ap_hi+ ap_lo+
                    cholesterol+ gluc+ smoke+ alco+ active, data = traind_cardio, family = "binomial")

summary(logitmodel)

# Evaluation on the test dataset
cardio_pred <- predict(logitmodel, newdata = testd_cardio, type = "response")
cardio_pred <- ifelse(cardio_pred>0.5,1,0)
cardio_pred



# Model 2- Probit Model
set.seed(1234)

# Splitting data into training and test sets
train_cardio <- createDataPartition(cardio$cardio, p = 0.75, list = FALSE)
traind_cardio <- cardio[train_cardio, ]
testd_cardio <- cardio[-train_cardio, ]

# Probit regression model
probitmodel <- glm(cardio ~ age + gender + height + weight + ap_hi + ap_lo +
                      cholesterol + gluc + smoke + alco + active, 
                    data = traind_cardio, family = binomial(link = "probit"))

summary(probitmodel)

# Evaluation on the test dataset
cardio_pred_probit <- predict(probitmodel, newdata = testd_cardio, type = "response")
cardio_pred_probit <- ifelse(cardio_pred_probit > 0.5, 1, 0)
cardio_pred_probit



# Model 3- Complementary log log reg Model
set.seed(1234)

# Splitting data into training and test sets
train_cardio <- createDataPartition(cardio$cardio, p = 0.75, list = FALSE)
traind_cardio <- cardio[train_cardio, ]
testd_cardio <- cardio[-train_cardio, ]

# Complementary log-log regression model
cloglog_model <- glm(cardio ~ age + gender + height + weight + ap_hi + ap_lo +
                       cholesterol + gluc + smoke + alco + active, 
                     data = traind_cardio, family = quasibinomial(link = "cloglog"))

summary(cloglog_model)

# Evaluation on the test dataset
cardio_pred_cloglog <- predict(cloglog_model, newdata = testd_cardio, type = "response")
cardio_pred_cloglog <- ifelse(cardio_pred_cloglog > 0.5, 1, 0)
cardio_pred_cloglog

# Output of all three models
stargazer(logitmodel, probitmodel, cloglog_model, type = "text")


# quality check
# Residual analysis for Logistic Regression
logit_res <- residuals(logitmodel, type = "deviance")
hist(logit_res, main = "Residuals Distribution - Logistic Regression", xlab = "Residuals")

# Linearity check for Logistic Regression
plot(logitmodel$fitted.values ~ traind_cardio$age, main = "Linearity Check - Logistic Regression")

# Homoscedasticity check for Logistic Regression
plot(logit_res ~ logitmodel$fitted.values, main = "Homoscedasticity Check - Logistic Regression")

# Multicollinearity check for Logistic Regression
library(car)
vif(logitmodel)


# Residual analysis for Probit Model
probit_res <- residuals(probitmodel, type = "deviance")
hist(probit_res, main = "Residuals Distribution - Probit Model", xlab = "Residuals")

# Linearity check for Probit Model
plot(probitmodel$fitted.values ~ traind_cardio$age, main = "Linearity Check - Probit Model")

# Homoscedasticity check for Probit Model
plot(probit_res ~ probitmodel$fitted.values, main = "Homoscedasticity Check - Probit Model")


# Multicollinearity check for Probit Model
vif(probitmodel)


# Residual analysis for Complementary Log-Log Model
cloglog_res <- residuals(cloglog_model, type = "deviance")
hist(cloglog_res, main = "Residuals Distribution - Cloglog Model", xlab = "Residuals")

# Linearity check for Complementary Log-Log Model
plot(cloglog_model$fitted.values ~ traind_cardio$age, main = "Linearity Check - Cloglog Model")

# Homoscedasticity check for Complementary Log-Log Model
plot(cloglog_res ~ cloglog_model$fitted.values, main = "Homoscedasticity Check - Cloglog Model")

# Multicollinearity check for Complementary Log-Log Model
vif(cloglog_model)


# Confusion metrics
#logit model
table1 <- table(testd_cardio$cardio, cardio_pred)
table1

# Calculating Recall, Precision, Accuracy, F1score, AUC
recall <- table1[2,2]/(table1[2,2]+table1[2,1])
recall

precision <- table1[2,2]/(table1[2,2]+table1[1,2])
precision

accuracy <- sum(diag(table1)) / sum(table1)
accuracy

f1_score <- (2 * precision * recall) / (precision + recall)
f1_score

roc_obj <- roc(response = testd_cardio$cardio, predictor = cardio_pred)
auc <- auc(roc_obj)
auc


# Probit model
# Calculating Recall, Precision, Accuracy, F1score, AUC
table2 <- table(testd_cardio$cardio, cardio_pred_probit)
table2

recall <- table2[2,2]/(table2[2,2]+table2[2,1])
recall

precision <- table2[2,2]/(table2[2,2]+table2[1,2])
precision

accuracy <- sum(diag(table2)) / sum(table2)
accuracy

f1_score <- (2 * precision * recall) / (precision + recall)
f1_score

roc_obj <- roc(response = testd_cardio$cardio, predictor = cardio_pred_probit)
auc <- auc(roc_obj)
auc


# Complementary log- log model
# Calculating Recall, Precision, Accuracy, F1score, AUC
table3 <- table(testd_cardio$cardio, cardio_pred_cloglog)
table3

recall <- table3[2,2]/(table3[2,2]+table3[2,1])
recall

precision <- table3[2,2]/(table3[2,2]+table3[1,2])
precision

accuracy <- sum(diag(table3)) / sum(table3)
accuracy

f1_score <- (2 * precision * recall) / (precision + recall)
f1_score

roc_obj <- roc(response = testd_cardio$cardio, predictor = cardio_pred_cloglog)
auc <- auc(roc_obj)
auc











