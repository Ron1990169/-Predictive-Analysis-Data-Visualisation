library(dplyr)
library(tidyverse)

dat<- read.csv("Life Expectancy Data.csv")

# Checking for NA values

sum(is.na(dat))


#Removing them-

dat1<- na.omit(dat)
#Now no NA values in the dataset.

#Lets condisder Life expectancy to be ut target varialbe


#1 Removing the variables not to be used

dat2<- dat1[c(-1,-2,-3)]


# Splitting the data sset into training and test set

set.seed(123)
training.samples <- dat2$Life.expectancy %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat2[training.samples, ]
test.data <- dat2[-training.samples, ]

#Fitting the model

model<- lm(Life.expectancy~., data = train.data)

summary(model)


#From the model summary we can get that the R square value is 83%, whcihn is quite good. Also, p value is less than significant 0.05.
# The output shows the variables impacting the model. The variables having p value less than 0.05 are highly significant ones.
# Therefore removing the insignificant ones.

model_updated<- lm(Life.expectancy~Adult.Mortality+infant.deaths+percentage.expenditure+ under.five.deaths+
                     Total.expenditure+ Diphtheria+ HIV.AIDS+ Income.composition.of.resources+Schooling, data = train.data)

summary(model_updated)
# Now all variables are highly significant.

# now predicting the with test set-

# Predict test data based on model
predict_reg <- model_updated %>% predict(test.data)
predict_reg 


linear_rmse<- data.frame(
  RMSE = caret::RMSE(predict_reg, test.data$Life.expectancy),
  Rsquare = caret::R2(predict_reg, test.data$Life.expectancy)
)

# We can see that the RMSE value is low i.e 3.689 and R square is high i.e 81.8%, meaning model is good.
