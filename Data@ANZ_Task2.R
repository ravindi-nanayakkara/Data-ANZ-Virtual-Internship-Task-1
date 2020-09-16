#Data@ANZ Virtual Internship Program

#Task 2 - Predictive Analytics

library(dplyr)
library(ggplot2)
library(forecast)
library(ggplot2)
library(GGally)
library(ggmap)
library(lubridate)


# Importing the dataset
ANZ_df <- read.csv('ANZ_data.csv', header = TRUE, na.strings=c("","NA"))

#Saving the date as a date variable
ANZ_df$date <- as.Date(ANZ_df$date,format = "%d/%m/%Y")

#Encoding categorical data
ANZ_df$movement <- factor(ANZ_df$movement,
                         levels = c('debit', 'credit'),
                         labels = c(1, 2))


#Finding the unique set of 100 hypothetical customers
ANZ_df_id <- unique(ANZ_df$customer_id)

ANZ_df_cus1 <- ANZ_df %>% group_by(customer_id, age, long_lat, gender) %>% filter(txn_description == "PAY/SALARY") %>% summarise(count = n(), total_amount = sum(amount), mean_amount = mean(amount), annual_salary = (total_amount*4))
 
#Visualizing the annual salary of customers
boxplot(ANZ_df_cus1$annual_salary, horizontal = TRUE, col = "#3399FF", xlab= "Annual Salary", main="Distribution of Annual Salary of Customers")

summary(ANZ_df_cus1$annual_salary)
#Average annual salary of a customer = 67063


#Finding the Annual Purchasing Cost of ANZ Customers
ANZ_df_cus2 <- ANZ_df %>% group_by(customer_id, age, long_lat, gender) %>%  filter((txn_description %in% c("INTER BANK", "PHONE BANK","PAYMENT", "POS", "SALES-POS"))) %>% summarise(count = n(), total_amount = sum(amount), mean_amount = mean(amount), annual_purchases = (total_amount*4))

#Visualizing the annual purchasing cost of customers
boxplot(ANZ_df_cus2$annual_purchases, horizontal = TRUE, col = "#3399FF", xlab= "Annual Purchasing Cost", main="Distribution of Annual Purchasing Cost")

summary(ANZ_df_cus2$annual_purchases)
#Average annual purchasing cost of a customer = 23468


##################################################################################################################################################################################################################################

#Conducting Correlation Analysis

#Scatter Plot of Annual Salary Vs. Age
ggplot(ANZ_df_cus1, aes(age, annual_salary)) + geom_point(colour = "#FF0033", size = 3) + labs(title="Scatter Plot of Annual Salary vs. Age", x="Age", y="Annual Salary")

#Computing the Correlation Coefficient between Annual Salary and Age
cor(ANZ_df_cus1$age, ANZ_df_cus1$annual_salary)


salarypurchase_sum <- data.frame(ANZ_df_cus1$age, ANZ_df_cus2$annual_purchases, ANZ_df_cus1$annual_salary)
attach(salarypurchase_sum)

#Scatter Plot of Annual Salary Vs. Annual Purchasing Cost
ggplot(salarypurchase_sum, aes(ANZ_df_cus2.annual_purchases, ANZ_df_cus1.annual_salary)) + geom_point(colour = "#FF0033", size = 3) + labs(title="Scatter Plot of Annual Salary vs. Annual Purchasing Cost", x="Annual Purchasing Cost", y="Annual Salary")

#Computing the Correlation Coefficient between Annual Salary and Age
cor(salarypurchase_sum$ANZ_df_cus2.annual_purchases, salarypurchase_sum$ANZ_df_cus1.annual_salary)

#Scatter Plot of Annual Purchasing Cost Vs. Age
ggplot(salarypurchase_sum, aes(ANZ_df_cus1.age, ANZ_df_cus2.annual_purchases)) + geom_point(colour = "#FF0033", size = 3) + labs(title="Scatter Plot of Annual Purchasing Cost vs. Age", x="Age", y="Annual Purchasing Cost")

#Computing the Correlation Coefficient between Annual Purchasing Cost and Age
cor(ANZ_df_cus1.age, ANZ_df_cus2.annual_purchases)

ggcorr(salarypurchase_sum,  method = c("pairwise", "pearson"), label = TRUE, label_alpha = TRUE)

#############################################################################################################################################################################################################

#Simple Linear Regression Models

library(MLmetrics)

#Fitting Simple Linear Regression Models to Predict the Annual Salary of ANZ Customers

#Predicting Annual Salary using Age as the explanatory variable
annualsal_model1<-lm(ANZ_df_cus1.annual_salary ~ ANZ_df_cus1.age, salarypurchase_sum)
summary(annualsal_model1)

#Computing the Root Mean Square Error of Model 1
RMSE(annualsal_model1$fitted.values, ANZ_df_cus1.annual_salary)

#Predicting Annual Salary using Annual Purchasing Cost as the explanatory variable
annualsal_model2<-lm(ANZ_df_cus1.annual_salary ~ ANZ_df_cus2.annual_purchases, salarypurchase_sum)
summary(annualsal_model2)

#Computing the Root Mean Square Error of Model 2
RMSE(annualsal_model2$fitted.values, ANZ_df_cus1.annual_salary)


#Multiple Linear Regression Model


#Fitting a Multiple Linear Regression Model to Predict the Annual Salary of ANZ Customers
#Predicting Annual Salary using Age and Annual Purchasing Cost as the explanatory variables
annualsal_model3<-lm(ANZ_df_cus1.annual_salary ~ ANZ_df_cus1.age + ANZ_df_cus2.annual_purchases, salarypurchase_sum)
summary(annualsal_model3)

#Computing the Root Mean Square Error of Model 2
RMSE(annualsal_model3$fitted.values, ANZ_df_cus1.annual_salary)

#################################################################################################################################

#Decision Tree Based Models
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

#Building the Decision Tree 1 using one independent variable = Age

salaryage <- data.frame(ANZ_df_cus1$age, ANZ_df_cus1$annual_salary)
colnames(salaryage) <- c("Age", "Salary")

dtree1 = rpart(formula = Salary ~ .,
               data = salaryage)

# Visualizing the Decision Tree Regression results (higher resolution)

x_grid = seq(min(salaryage$Age), max(salaryage$Age), 0.01)
ggplot() +
  geom_point(aes(x = salaryage$Age, y = salaryage$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(dtree1, newdata = data.frame(Age = x_grid))),
            colour = 'blue') +
  ggtitle('Annual Salary (Decision Tree Regression)') + xlab('Age') +
  ylab('Annual Salary')

plot(dtree1)
text(dtree1)

prp(dtree1, faclen = 0, cex = 0.8, extra = 1)

fancyRpartPlot(dtree1)

y_pred1 = predict(dtree1, data.frame(salaryage))

RMSE(y_pred1, salaryage$Salary)

#Building the Decision Tree 2 using one independent variable = Annual Purchasing Cost

salarypurch <- data.frame(ANZ_df_cus2$annual_purchases, ANZ_df_cus1$annual_salary)
colnames(salarypurch) <- c("Purchasing_Cost", "Salary")

dtree2 = rpart(formula = Salary ~ .,
               data = salarypurch)

# Visualizing the Decision Tree Regression results (higher resolution)

x_grid = seq(min(salarypurch$Purchasing_Cost), max(salarypurch$Purchasing_Cost), 0.1)
ggplot() +
  geom_point(aes(x = salarypurch$Purchasing_Cost, y = salarypurch$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(dtree2, newdata = data.frame(Purchasing_Cost = x_grid))),
            colour = 'blue') +
  ggtitle('Annual Salary (Decision Tree Regression)') + xlab('Annual Purchasing Cost') +
  ylab('Annual Salary')

plot(dtree2)
text(dtree2)

prp(dtree2, faclen = 0, cex = 0.8, extra = 1)

fancyRpartPlot(dtree2)

y_pred2 = predict(dtree2, data.frame(salarypurch))

RMSE(y_pred2, salarypurch$Salary)


#Building the Decision Tree 3 using two independent variables
colnames(salarypurchase_sum) <- c("Age", "Purchasing_Cost", "Salary")

dtree3 = rpart(formula = Salary ~ .,
                  data = salarypurchase_sum)

plot(dtree3)
text(dtree3)

prp(dtree3, faclen = 0, cex = 0.8, extra = 1)

fancyRpartPlot(dtree3)

y_pred3 = predict(dtree3, data.frame(salarypurchase_sum))

RMSE(y_pred3, salarypurchase_sum$Salary)
