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


N <- 500

# 1.2 Next, create a vector of 1s and 0s, so that half of your sample receives the treatement (coded 1), and the other half doesn't (coded 0)

treatment <- c(rep(1, N / 2), rep(0, N / 2))


# Next we will run 60 different t-tests using a for loop, and save the p-value for each of these tests
# 1.3 start by initializing an empty vector for your p-values using an empty c() function.

p <- c()


# 1.4 Next, write a for loop for a total of 60 loops
# For each loop, save a new y-variable that you have sampled completely at random
# your y-variable should be of length N, but otherwise you can sample it however you like (rnorm,runif,rbinom... any probability distribution you like with any parameter values you like)


for (i in 1:60) {
    y <- runif(N, 60, 150)


    test <- t.test(y[treatment == 1], y[treatment == 0])


    p[i] <- test$p.value
}
# Next (still within the for loop), run a t-test comparing the y-values corresponding to treatment == 1 with the y-values corresponding to treatment ==0
p
# extract the p-value for that t-test and save it to the vector of p-values
sum(p < 0.05)
# 2

sum(p < 0.05)
# 1.5 After your for-loop is run, calculate the sum of the saved p-values that are less than 0.05

# 1.6 How many significant results are there at the 0.05 level?


# 1.7 Run the whole for loop again several times. How many significant results do you find each time?

n_significant <- c()


for (j in 1:150) {
    for (i in 1:60) {
        y <- runif(N, 60, 150)


        test <- t.test(y[treatment == 1], y[treatment == 0])


        p[i] <- test$p.value
    }

    n_significant[j] <- sum(p < 0.05 / 60)
}

n_significant

hist(n_significant)

mean(n_significant)

# 2.90667 when N which is the number of participants is 150

# 2.986667 when N is increased 500

# Not a sample size problem

# Bonferoni Correction for multiple comparisons - helps to account for the amount of times we ran the experiment


# When doing multiple tests, divide alpha level by number of number of tests ran on the experiment

# 1.8 Write a for loop that runs the previous for-loop multiple times, and save the number of significant results you get each time. Run this for loop at least 100 times


# 1.9 Plot the histogram of the number of significant results you get for each multiple comparisons experiment. What is the average number?


# 1.10 Run this whole code again, but this time increase the sample size N. Does your histogram look any different? why?

# 1.11 Run the code again, but this time instead of counting how many results are less than 0.05, divide this threshold by the total number of comparisons (60).



# 1.12 What does your new histogram look like?

# 1.13 This (dividing the p-value required for significance by the number of comparisons made) is called Bonferroni correction for multiple comparisons.
# Why is it important?

# It's important because otherwise we can almost guaranterr significant results by doing lots of tests


###############################################################################################################
# PARTITIONED REGRESSION    # PARTITIONED REGRESSION    # PARTITIONED REGRESSION    # PARTITIONED REGRESSION



# 2. Partitioned regression

data(mtcars)
multiple_model <- lm(mpg ~ wt + cyl, data = mtcars)



summary(multiple_model)
# 2.1 Run a multiple regression using the mtcars database with fuel efficiency (mpg)
# as the dependent (outcome) variable and weight (wt) and number of cylinders (cyl)
# as independent (predictor) variables
# Look at the output of this regression and note the value of the parameter estimates for the effects of wt and cyl.

# 2.2 save the residuals of the regression with formula (mpg ~ wt) into an object called mpg_wt_residuals


mpg_wt_residuals <- residuals(lm(mpg ~ wt, data = mtcars))
mpg_wt_residuals

plot(mtcars$wt, mtcars$mpg, pch = 16)

plot(mtcars$wt, mpg_wt_residuals)

# 2.3 Interpret this object. What does it represent?

# It is the difference between the prediction of mpg from the wt, and the actual observed fuel efficiency

# It is what remains of the fuel efficiency, after the effect of wt has been accounted for

# 2.4 Save the residuals of the regression with formula (cyl ~ wt) into an object called cyl_wt_residuals
cyl_wt_residuals <- residuals(lm(cyl ~ wt, , data = mtcars))

cyl_wt_residuals
# 2.5 Interpret this object. What does it represent?


# It is what is remained after we have taken account of the effect of the wt

# 2.6 Run a regression with the formula (mpg_wt_residuals ~ cyl_wt_residuals)


mod_rescyl_resmpg <- lm(mpg_wt_residuals ~ cyl_wt_residuals)
summary(mod_rescyl_resmpg)



# 2.7 Based on your answers to 2.3 and 2.5, how do you interpret the slope estimate of the previous regression?

# 2.8 Compare this estimate to the estimate of wt in the original full model.
# What do you conclude?


# They are the same



# 2.9 Use the same method to get the estimate of cyl from the original model using only simple linear regressions.



#############################
##                         ##
##        HOMEWORK         ##
##                         ##
#############################

library(dplyr)

# 3.1 Load the dataset 'toycars' from the package 'DAAG'
install.packages("DAAG")
library(DAAG)
data(toycars)

?toycars()
View(toycars)
# This dataset contains information about experiments launching toy cars at different angles and measuring the distances they travel before falling to the ground.
# Learn more about this dataset by looking up ?toycars

# There are 3 different types of toy cars, numbered 1, 2, and 3

# We want to model the relationship between the type of car and the distance they travel.
# 3.2 Which variable should be the outcome (independent) variable? Why?

# The distance should be the Outcome Variable, because based on the data, we may want to see the distance the different cars 1,2 and 3 can travel given a certain tilt (angle).

# 3.3 Run a regression model with the formula distance ~ car. How would you interpret the regression coefficient? What is the problem with this model? How would you rectify this problem?

linear_regression_model <- lm(distance ~ car, data = toycars)
linear_regression_model


summary(linear_regression_model)

# The Intercept is the estimated Distance travelled when the car type is zero. However, we cannot interpret this result because the predictor variable is categorical hence, we cannot tell what the Intercept is.


# 3.4 Recode the 'car' variable so that car 1 is coded as 'green', car 2 is 'yellow', and car 3 is 'red'

View(toycars)
str(toycars)

toycars <- toycars %>%
    mutate(
        car = case_when(
            car == 1 ~ "Green",
            car == 2 ~ "Yellow",
            car == 3 ~ "Red"
        )
    )

View(toycars)


# 3.5 Rerun the regression model using this recoded variable and interpret the coefficients. What does this model say about whether how choosing different cars affects the distances they travel?

linear_regression_model2 <- lm(distance ~ car, data = toycars)
linear_regression_model2

summary(linear_regression_model2)


# Intercept - This is the estimated distance travelled by the reference car type - Green


# carRed - This is the estimated difference in distance travelled between the reference car type - Green and the car type -  Red. Here (-0.08222) means that the red car is estimated to travel 0.08222 metres less than the green car


# carYellow - This is the estimated difference in distance travelled between the reference car type - Green and the car type - Yellow. Here (0.11111) is suggests that the yellow car is estimated to travel 0.11111 metres more than the green car.

# That Yellow cars are estimated to cover the most distance.

# 3.6 Rerun the model again, this time including 'angle' as an independent variable. Interpret all 4 coefficients.


linear_regression_model3 <- lm(distance ~ car + angle, data = toycars)
linear_regression_model3



summary(linear_regression_model3)

# Intercept - This is estimated distance travelled by the reference car - Green, when the angle titled is zero

# carRed - This is the estimated difference in distance travelled between the Red Car and the green Car keeping the angle tilted is held constant or without the effects of the angle.


# carYellow - This is the estimated difference in distance travelled between the Yellow Car and the Green Car, keeping the angle constanct


# angle - This is the estimated change in distance with one unit increase in angle tilted when the other car categories - Yellow and Red are kept constant


# 3.7 What is the predicted distance traveled by a red car launched at 3 degrees?

View(toycars)


# new_data1 <- data.frame(
#     carRed = 1,
#     carYellow = 0,
#     angle = 3
# )



# prediction <- predict(linear_regression_model3, newdata = new_data1)


predicted_dist1 <- 0.092524 + (-0.082222 * 1) + (0.111111 * 0) + (0.188541 * 3)
predicted_dist1

# 3.8 What is the predicted distance traveled by a white car launched at 2.5 degrees?

# We cannot predict a white car since white is not given in the dataframe

# 3.9 Plot the results of this model using ggplot with 90% confidence intervals and appropriate colors


library(ggplot2)

ggplot(toycars, aes(x = angle, y = distance, color = car, shape = car)) +
    geom_point() +
    geom_smooth(
        method = "lm", level = 0.90, aes(group = car), linetype = "dashed", size = 1
    ) +
    labs(
        title = "Scatter Plot of Distance by Car Type and Angle",
        x = "Angle (degrees)",
        y = "Distance (metres)",
        color = "Car Type",
        shape = "Car Type"
    ) +
    scale_shape_manual(values = c(16, 17, 18), name = "Car Type") +
    scale_color_manual(values = c("Green" = "green", "Yellow" = "yellow", "Red" = "red"), name = "Car Type") +
    theme_minimal()
