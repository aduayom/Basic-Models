

import pandas as pd

df = pd.read_excel('other_files/images_analyzed.xlsx')
print(df.head())

#A few plots in Seaborn to understand the data

import seaborn as sns


sns.lmplot(x='Time', y='Images_Analyzed', data=df, hue='Age')  #Scatterplot with linear regression fit and 95% confidence interval
sns.lmplot(x='Coffee', y='Images_Analyzed', data=df, hue='Age', order=2)
#Looks like too much coffee is not good... negative effects

#sns.lmplot(x='Age', y='Images_Analyzed', data=df, hue='Age')

import numpy as np
from sklearn import linear_model

#Create Linear Regression object
reg = linear_model.LinearRegression()

#Now let us call fit method to train the model using independent variables.
#And the value that needs to be predicted (Images_Analyzed)

reg.fit(df[['Time', 'Coffee', 'Age']], df.Images_Analyzed) #Indep variables, dep. variable to be predicted

#Model is ready. Let us check the coefficients, stored as reg.coef_.
#These are a, b, and c from our equation. 
#Intercept is stored as reg.intercept_
print(reg.coef_, reg.intercept_)

#All set to predict the number of images someone would analyze at a given time
print(reg.predict([[13, 2, 23]]))
