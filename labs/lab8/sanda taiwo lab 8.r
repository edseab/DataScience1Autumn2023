
###################################
###################################
########                   ########
########   Data Science 1  ########
########       Lab 8       ######## 
########  11th Dec. 2023   ########
########                   ########
###################################
###################################

## Welcome to lab #8. 

# Lets start by learning about p-hacking


# 1. THE ICE BATH STUDY
# In 2022 a paper was published in a medical journal that claimed to show 
# that taking ice-cold baths could have health benefits. They asssigned 50%
# of their subjects to the treatment group and received the ice baths
# and 50% to a control group that received no ice cold baths 
# (they could have received warm baths instead, but this study didn't do this)
# The paper abstract boasted that the ice baths had statistically significant (at the 5% level)
# effects on 3 health outcomes (perceived energy levels, mental alertness, probability of catching a cold within 1 week of treatment)
# Looking into the methods section of the paper, we can see that they actually 
# tested 60 different possible health outcomes (physical strength, skin health, blood pressure...)

# Could they have gotten these results purely by chance?

# Let's simulate the cold-water therapy experiment
# 1.1 Start by saving a sample size of 100 to an object N

# 1.2 Next, create a vector of 1s and 0s, so that half of your sample receives the treatement (coded 1), and the other half doesn't (coded 0)


# Next we will run 60 different t-tests using a for loop, and save the p-value for each of these tests
# 1.3 start by initializing an empty vector for your p-values using an empty c() function. 

# 1.4 Next, write a for loop for a total of 60 loops
# For each loop, save a new y-variable that you have sampled completely at random
# your y-variable should be of length N, but otherwise you can sample it however you like (rnorm,runif,rbinom... any probability distribution you like with any parameter values you like)

# Next (still within the for loop), run a t-test comparing the y-values corresponding to treatment == 1 with the y-values corresponding to treatment ==0

# extract the p-value for that t-test and save it to the vector of p-values


# 1.5 After your for-loop is run, calculate the sum of the saved p-values that are less than 0.05

# 1.6 How many significant results are there at the 0.05 level?


# 1.7 Run the whole for loop again several times. How many significant results do you find each time?

# 1.8 Write a for loop that runs the previous for-loop multiple times, and save the number of significant results you get each time. Run this for loop at least 100 times


# 1.9 Plot the histogram of the number of significant results you get for each multiple comparisons experiment. What is the average number?


# 1.10 Run this whole code again, but this time increase the sample size N. Does your histogram look any different? why?

# 1.11 Run the code again, but this time instead of counting how many results are less than 0.05, divide this threshold by the total number of comparisons (60).



# 1.12 What does your new histogram look like?

# 1.13 This (dividing the p-value required for significance by the number of comparisons made) is called Bonferroni correction for multiple comparisons.
# Why is it important?


# 2. Partitioned regression

# 2.1 Run a multiple regression using the mtcars database with fuel efficiency (mpg)
# as the dependent (outcome) variable and weight (wt) and number of cylinders (cyl)
# as independent (predictor) variables
# Look at the output of this regression and note the value of the parameter estimates for the effects of wt and cyl.

# 2.2 save the residuals of the regression with formula (mpg ~ wt) into an object called mpg_wt_residuals

# 2.3 Interpret this object. What does it represent?

# 2.4 Save the residuals of the regression with formula (cyl ~ wt) into an object called cyl_wt_residuals

# 2.5 Interpret this object. What does it represent?

# 2.6 Run a regression with the formula (mpg_wt_residuals ~ cyl_wt_residuals)

# 2.7 Based on your answers to 2.3 and 2.5, how do you interpret the slope estimate of the previous regression?

# 2.8 Compare this estimate to the estimate of wt in the original full model. 
# What do you conclude?

# 2.9 Use the same method to get the estimate of cyl from the original model using only simple linear regressions.



#############################
##                         ##
##        HOMEWORK         ##
##                         ##
#############################

library(dplyr)

# 3.1 Load the dataset 'toycars' from the package 'DAAG'
install.packages('DAAG')
library("DAAG")
data(toycars)
read.csv("toycars.csv") -> toycars
# This dataset contains information about experiments launching toy cars at different angles and measuring the distances they travel before falling to the ground.
# Learn more about this dataset by looking up ?toycars

# There are 3 different types of toy cars, numbered 1, 2, and 3

# We want to model the relationship between the type of car and the distance they travel. 
# 3.2 Which variable should be the outcome (Dependent) variable? Why?

#The Dependent variable is the distance covered by the car, because it can be measured as the outcome depending on the type of car (which is the independent variable).


# 3.3 Run a regression model with the formula distance ~ car. How would you interpret the regression coefficient? What is the problem with this model? How would you rectify this problem?
View(toycars)
model <- lm(distance ~ car, data = toycars)
summary(model)
plot(y = toycars$distance, x = toycars$car, main = "Linear model of distance ~ car ", xlab= "Car type", ylab= "distace of toy car (meters)")
abline(model)

#the intercept is the distance of the toycar when there is no car type
#the interpretation of the slope is that the change in the distance of the car when we move from one group to another is -0.04111 unit decrease but this model is not very helpful currently because there is no reference point

#The problem with the model is the X-variable which is a categorical variable has no reference point, because R is seeing it as a numerical variable, hence the intercept will not have a reasonalble interpretation

# 3.4 Recode the 'car' variable so that car 1 is coded as 'green', car 2 is 'yellow', and car 3 is 'red'
library(dplyr)
toycars$car<- case_when(toycars$car == 1 ~ "Green",
                        toycars$car == 2 ~ "Yellow",
                        toycars$car == 3 ~ "Red")
View(toycars)

# 3.5 Rerun the regression model using this recoded variable and interpret the coefficients. What does this model say about whether how choosing different cars affects the distances they travel?

plot(y = toycars$distance, x = toycars$car, main = "Linear model of distance ~ car ", xlab= "Car type", ylab= "distace of toy car (meters)")      #base r seems to not work with ctaegoric

library(ggplot2)

ggplot(data=toycars, mapping = aes(x=car,y=distance)) +
geom_point() + 
xlab(label = "Car type") +
ylab(label= "distace of toy car (meters)")
model <- lm(distance ~ car, data = toycars)
abline(model)
summary(model)


#The intercept in this new model refers to the predicted distance of the green car whih is 0.59111

#The slope estimate of carRed refers to the change in the distance when you move from a green car to a red car which is -0.0822 unit decrease in distance

#The slope estimate of carYellow refers to the change in the distance when you move from a green car to a Yellow car which is 0.11111 unit increase in distance

# 3.6 Rerun the model again, this time including 'angle' as an independent variable. Interpret all 4 coefficients.

model_angle <- lm(distance ~ car + angle, data = toycars)

summary(model_angle)


#The intercept in this new model refers to the predicted distance of the green car which is 0.092524

#The slope estimate of carRed refers to the change in the distance when you move from a green car to a red car which is -0.0822m decrease in distance

#The slope estimate of carYellow refers to the change in the distance when you move from a green car to a Yellow car which is 0.11111 unit increase in distance

#the angle co-efficients signifies that the unit change in distance of the car after controlling for the car type is 0.188541 unit increase in distance

# 3.7 What is the predicted distance traveled by a red car launched at 3 degrees?

#new_toycars <- data.frame(angle=3)
?predict
#predict(model_angle, newdata=new_toycars, interval= 'confidence', level =0.90)

#as.data.frame(predict(model_angle, newdata=new_toycars, interval= 'confidence', level =0.90)) -> predictions


# 3.8 What is the predicted distance traveled by a white car launched at 2.5 degrees?

# 3.9 Plot the results of this model using ggplot with 90% confidence intervals and appropriate colors






