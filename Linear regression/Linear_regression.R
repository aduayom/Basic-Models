# Import necessary libraries
library(readr)
library(ggplot2)
library(dplyr)

# Read the CSV file
df <- read_csv('GitHub/Desktop_Mac_Daniel/Basic-Models/other_files/cells.csv')
print(df)

# Scatter plot using ggplot2
ggplot(df, aes(x = time, y = cells)) +
  geom_point(color = 'red') +
  labs(x = 'time', y = 'cells')

# Define x and y for the linear regression model
x_df <- df %>% select(-cells)
y_df <- df$cells

print(str(x_df))  # Print the structure of x_df to ensure it's a data frame

# Create a linear regression model
model <- lm(cells ~ time, data = df)

# Print the summary of the model to get R^2 value and coefficients
summary(model)

# Predict cells for a given time value
predicted_cells <- predict(model, data.frame(time = 2.3))
print(paste("Predicted # cells for time 2.3: ", predicted_cells))

# Get the intercept and coefficient values
b <- coef(model)[1]
m <- coef(model)[2]

# Manually verify the calculation
manual_calculation <- m * 2.3 + b
print(paste("From manual calculation, cells = ", manual_calculation))

# Predict cells for a list of times from another CSV file
cells_predict_df <- read_csv("GitHub/Desktop_Mac_Daniel/Basic-Models/other_files/cells_predict.csv")
print(head(cells_predict_df))

# Use the model to predict cells
predicted_cells <- predict(model, cells_predict_df)
print(predicted_cells)

# Add the predicted cells as a new column to the dataframe
cells_predict_df <- cells_predict_df %>%
  mutate(cells = predicted_cells)
print(cells_predict_df)

# Save the dataframe with predicted values to a new CSV file
write_csv(cells_predict_df, "GitHub/Desktop_Mac_Daniel/Basic-Models/other_files/cells_predict.csv")

# Using ggplot2 for plotting with a regression line
ggplot(df, aes(x = time, y = cells)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = 'time', y = 'cells')

# Using base R to calculate slope and intercept using the linear model
slope <- coef(model)[2]
intercept <- coef(model)[1]
print(paste("Slope: ", slope, " Intercept: ", intercept))

