# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read the data from Excel file
df <- read_excel('GitHub/Desktop_Mac_Daniel/Basic-Models/other_files/images_analyzed.xlsx')
print(head(df))

# Scatterplots with linear regression fit using ggplot2
ggplot(df, aes(x=Time, y=Images_Analyzed, color=Age)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE) +
  labs(title="Scatterplot with Linear Regression Fit and 95% Confidence Interval")

ggplot(df, aes(x=Coffee, y=Images_Analyzed, color=Age)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ poly(x, 2), se=TRUE) +
  labs(title="Polynomial Regression Fit (Order 2)")

# Linear Regression model using the lm() function
model <- lm(Images_Analyzed ~ Time + Coffee + Age, data=df)

summary(model)
# Print the coefficients and intercept
print(coef(model))
print(summary(model)$coefficients)

# Predict the number of images someone would analyze given the predictors
new_data <- data.frame(Time = 13, Coffee = 2, Age = 23)
predicted_value <- predict(model, newdata = new_data)
print(predicted_value)
