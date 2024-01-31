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
plot(ToothGrowth$dose, ToothGrowth$len)

# 1.2 Run a linear regression exploring the association between vitamin C
# and tooth growth in Guinea pigs.
pig_dataset <- lm(len ~ dose, data= d)
#pig_dataset <- lm(data = d, len ~ dose)
summary(pig_dataset)
# 1.3 Which variable did you use as your outcome variable? Why?

#the outcome variable is length of the toothgrowth because we are measuring the impact of dosage
#of vitamin c on the length of toothgrowth, our outcome variable. 


# 1.4 Interpret the estimates for the intercept and the slope of the regression line

#the intercept of the regression line is the length of tooth at 7.4mm, if zero vitamin c is admnistered
#the slope of the regression line is the increae in a unit of y for for every increase in x.
#the slope is estimated as 9.8m, for every additional unit of vitamin c given to the pig, they grow
#an additional 9.8m growth is achieved

# 1.5 Extract the 96 % confidence intervals for the slope and the intercepts and interpret them

confint(pig_dataset, level=0.96)

# 1.6 Interpret the p-values of the slope and intercept estimates. 

#we are 96% confidence that the intercept will be within the value of 4.7mm and 10.1 mm.
#and that we are 96% confidence that the true effect of giving 1mg dose of vitamin c is between 7.7 to 11.7 additional growth in the tooth

#the proability of seeing a slope as steep as the observed (as extreme as 9.78), if the true slope is 10^(-14)
#we therefore reject the null hypothesis, and the research provides significant evidence that vitamin c affects
#tooth growth as the hypothesis stated

# 1.7 What is the connection between the confidence intervals and the p-values? 

#The confidence interval is the estimated range of a parameter for the observed values,
#while the p value check the significance and strength of the estimate.

#the relationship between the confidence interval and p value is that the 
 #IF an X% confidence interval doesn't contain 0, then the p-value for that estimate will be significant at the (1-X)% level.
  #E.G if a 97% CI doesnt contain 0, the p-value will be smaller than 0.03 

# 1.8 Using the function predict() calculate the estimate and confidence interval for the predicted rate of tooth growth when the prescribed dose is 1.2mg/day

new_data <- data.frame(dose = 1.2)
new_data
predict(pig_dataset, new_data, interval="confidence", level= 0.95)

#calculating the actual predicted value ourselves we can do that using the value of the
#intercept + the slope multiply by the predicted dose rate

#a + bx = 
7.4225 + 9.7636*1.2

# 1.9 Now get the confidence intervals for all possible values of the dosage between 0 and 2 mg/day, spaced out 0.01 mg/day from each other

a_new_data <- data.frame(dose = seq(0,2, 0.01))
a_new_data
my_predictions <- as.data.frame(predict(pig_dataset, a_new_data, interval="confidence", level= 0.95))
my_predictions
# 1.10 plot the predicted values, upper, and lower confidence intervals as lines on the data

plot(my_predictions$fit, my_predictions$upr)
library(ggplot2)
ggplot

plot(d$dose,d$len, pch=20)
abline(pig_dataset)

lines(a_new_data$dose, my_predictions$upr, lty='dotdash')
lines(a_new_data$dose, my_predictions$lwr, lty='dotdash')

# 1.11 Compare this to using geom_smooth(method=lm) using ggplot2
ggplot(d, aes(dose, len))+
geom_point()+
geom_smooth(method='lm')


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
for (i in colnames(iris)){
  print(paste0(i, ' (', nchar(i),')'))
}


# Next, WHILE loops continue to loop until the boolean statment in the defining parentheses, e.g.
set.seed(123)
x <- 0
while(x<100){
  print(x)
  x <- x+sample(1:20,1)
}

### 2.2 How many numbers do you need in the sequence 1*2*3*4*5*... before the product exceeds 10 million?
# Use a while loop to get the answer
i <- 1
x <- 1
while (x<10000000){
  print(i)
  print(x)
  i <- i+1
  x <- x*i
}

i <- 1
while (factorial(i)<10000000){
  print(i)
  print(factorial(i))
  i <- i+1
}


