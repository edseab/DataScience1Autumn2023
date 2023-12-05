###################################
###################################
########                   ########
########   Data Science 1  ########
########       Lab 7       ######## 
########   5th Dec. 2023   ########
########                   ########
###################################
###################################


# Lets load up the dataset 'ToothGrowth', which shows the result
# of an experiment looking at the effect of vitamin C on tooth growth
# in guinea pigs


data('ToothGrowth')
?ToothGrowth
d <- ToothGrowth


# 1.1
# Plot the tooth growth against the dose of vitamin C given

# 1.2 Run a linear regression exploring the association between vitamin C
# and tooth growth in Guinea pigs.

# 1.3 Which variable did you use as your outcome variable? Why?

# 1.4 Interpret the estimates for the intercept and the slope of the regression line

# 1.5 Extract the 96 % confidence intervals for the slope and the intercepts and interpret them

# 1.6 Interpret the p-values of the slope and intercept estimates. 

# 1.7 What is the connection between the confidence intervals and the p-values? 

# 1.8 Using the function predict() calculate the estimate and confidence interval for the predicted rate of tooth growth when the prescribed dose is 1mg/day

# 1.9 Now get the confidence intervals for all possible values of the dosage between 0 and 2 mg/day, spaced out 0.01 mg/day from each other

# 1.10 plot the predicted values, upper, and lower confidence intervals as lines on the data

# 1.11 Compare this to using geom_smooth(method=lm) using ggplot2



#####################
####    Loops    ####
#####################

# Loops are algorithms that repeat a procedure over and over until they are instructed to stop.
# There are two main kinds:
# FOR loops iterate over a predetermined set (vector, list, etc)
fruits <- c('apple','banana', 'pineapple','mango','orange')
for(i in fruits){
  print(paste('My favourite fruit is',i,sep=': '))
}

useless_function <- function(n){
  for (i in 1:n){
    print(paste0(i,'. This number is: ', c('even','odd')[i%%2 +1]))
  }
}
useless_function(7)

### 2.1 
data(iris)

# Write a for loop that iterates over the column names of the iris dataset and print each together with the number of characters in the column name in parenthesis. Example output: Sepal.Length (12). To get the number of characters use the function nchar().


# Next, WHILE loops continue to loop until the boolean statment in the defining parentheses, e.g.

### 2.2 How many numbers do you need in the sequence 1*2*3*4*5*... before the product exceeds 10 million?
# Use a while loop to get the answer


