
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

N <- 100


# 1.2 Next, create a vector of 1s and 0s, so that half of your sample receives the treatement (coded 1), and the other half doesn't (coded 0)


# Next we will run 60 different t-tests using a for loop, and save the p-value for each of these tests
# 1.3 start by initializing an empty vector for your p-values using an empty c() function. 

p <- c()

# 1.4 Next, write a for loop for a total of 60 loops
# For each loop, save a new y-variable that you have sampled completely at random
# your y-variable should be of length N, but otherwise you can sample it however you like (rnorm,runif,rbinom... any probability distribution you like with any parameter values you like)

for (i in 1:60){
  
  y <- runif(N, 60, 150)
  
  test <- t.test(y[treatment==1], y[treatment==0])
  
  test$p.value -> p[i]
}

sum(p<0.05)

# Next (still within the for loop), run a t-test comparing the y-values corresponding to treatment == 1 with the y-values corresponding to treatment ==0

# extract the p-value for that t-test and save it to the vector of p-values


# 1.5 After your for-loop is run, calculate the sum of the saved p-values that are less than 0.05

# 1.6 How many significant results are there at the 0.05 level?


# 1.7 Run the whole for loop again several times. How many significant results do you find each time?

# 1.8 Write a for loop that runs the previous for-loop multiple times, and save the number of significant results you get each time. Run this for loop at least 100 times


# 1.9 Plot the histogram of the number of significant results you get for each multiple comparisons experiment. What is the average number?

hist(n_significant)

# 1.10 Run this whole code again, but this time increase the sample size N. Does your histogram look any different? why?

N <- 500
n_significant <- c()

for (j in 1:150){
  
  for (i in 1:60){
    y <- runif(N, 60, 150)
    
    test <- t.test(y[treatment==1], y[treatment==0])
    
    test$p.value -> p[i]
  }
  n_significant[j] <- sum(p<0.05)
}

# 1.11 Run the code again, but this time instead of counting how many results are less than 0.05, divide this threshold by the total number of comparisons (60).

N <- 100
n_significant <- c()

for (j in 1:150){
  
  for (i in 1:60){
    y <- runif(N, 60, 150)
    
    test <- t.test(y[treatment==1], y[treatment==0])
    
    test$p.value -> p[i]
  }
  n_significant[j] <- sum(p<0.05/60)
}

# 1.12 What does your new histogram look like?

# 1.13 This (dividing the p-value required for significance by the number of comparisons made) is called Bonferroni correction for multiple comparisons.
# Why is it important?


# 2. Partitioned regression

data(mtcars)
View(mtcars)

# 2.1 Run a multiple regression using the mtcars database with fuel efficiency (mpg)
# as the dependent (outcome) variable and weight (wt) and number of cylinders (cyl)
# as independent (predictor) variables
# Look at the output of this regression and note the value of the parameter estimates for the effects of wt and cyl.

mult_mod <- lm(mpg~wt + cyl,data=mtcars)

summary(mult_mod)

# 2.2 save the residuals of the regression with formula (mpg ~ wt) into an object called mpg_wt_residuals


mpg_wt_residuals <- residuals(lm(mpg ~ wt, data=mtcars))
mpg_wt_residuals

plot(mtcars$wt,mpg_wt_residuals)


# Comparing with the normal plot below
plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt, mtcars$mpg, pch=20)
plot(mpg_wt_residuals, mtcars$mpg, pch=20)

# 2.3 Interpret this object. What does it represent?

# it is the difference between the prediction of mpg from the weight, and the actual observed fuel efficiency

# it can also be interpreted like this
# it is what remains of the fuel efficiency, after the effect weight has been accounted for 


# 2.4 Save the residuals of the regression with formula (cyl ~ wt) into an object called cyl_wt_residuals

cyl_wt_residuals <- residuals(lm(cyl ~ wt, data=mtcars))
cyl_wt_residuals

# 2.5 Interpret this object. What does it represent?

# the difference between the predicited number of cylinders as a function of weight and the actual number of cylinder

# it is what remains of the number of cylinders after the effect of weight has beeen accounted for

# 2.6 Run a regression with the formula (mpg_wt_residuals ~ cyl_wt_residuals)

mod_rescyl_resmpg <- lm(mpg_wt_residuals~cyl_wt_residuals)
mod_rescyl_resmpg

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
library(DAAG)
toycars <- read.csv("toycars.csv")
View(toycars)

# This dataset contains information about experiments launching toy cars at different angles and measuring the distances they travel before falling to the ground.
# Learn more about this dataset by looking up ?toycars

# There are 3 different types of toy cars, numbered 1, 2, and 3

# We want to model the relationship between the type of car and the distance they travel. 
# 3.2 Which variable should be the outcome (independent) variable? Why?

# The variable that should be the independent is the car because i can easily manipulate whatever variable i want be it, speed or type of car 

# The outcome which is the dependent variable is the distance, and this is because it can be easily measured.


# 3.3 Run a regression model with the formula distance ~ car. How would you interpret the regression coefficient? What is the problem with this model? How would you rectify this problem?

reg_model <- lm(distance~car, data=toycars)
summary(reg_model)
plot(toycars$car,toycars$distance)

# 3.4 Recode the 'car' variable so that car 1 is coded as 'green', car 2 is 'yellow', and car 3 is 'red'

toycars <- toycars %>%
  mutate(car = recode(car,"1"="green","2"="yellow","3"="red"))
View(toycars)

# 3.5 Rerun the regression model using this recoded variable and interpret the coefficients. What does this model say about whether how choosing different cars affects the distances they travel?

reg_model <-lm(distance~car,data=toycars)
summary(reg_model)

# The intercept value of 0.59111 meters is the distance travelled by the green car when other cars are at rest.
#this means that the green car travels 0.5911 meters more than other cars

# 3.6 Rerun the model again, this time including 'angle' as an independent variable. Interpret all 4 coefficients.

new_reg_model <-lm(distance~car + angle, data=toycars)
summary(new_reg_model)

# 3.7 What is the predicted distance traveled by a red car launched at 3 degrees?

new_df <- data.frame(car="red",angle= 3)
predict(new_reg_model, new_df, interval ='confidence')

#The predicted distance travelled by a red car launched at 3 degrees is 0.576 meters


# 3.8 What is the predicted distance traveled by a white car launched at 2.5 degrees?

new_df2 <- data.frame(car="white",angle= 2.5)
predict(new_reg_model, new_df2, interval ='confidence')

# We cannot use this dataset to predict the distance travelled by a white car bnecause non of the cars in the dataset is white

# 3.9 Plot the results of this model using ggplot with 90% confidence intervals and appropriate colors

ggplot(toycars, aes(angle, distance, color = car)) + geom_point() + labs(title = "A plot of distance vs angle vs car type", y = " Distance travleled (meters)", x = "Angle (degrees)") + geom_smooth(method = lm , level = 0.90)





