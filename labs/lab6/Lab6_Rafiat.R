###################################
###################################
########                   ########
########   Data Science 3  ########
########       Lab 6       ######## 
########  21st Nov. 2023    ########
########                   ########
###################################
###################################


# Welcome to lab number 6. Today we'll be learning a few more 
# R/general programming concepts before moving onto lecture examples

########################################
####    Boolean logic, continued    ####
########################################

# the main logic operators in R are as follows:
# the NOT operator, !, turns TRUE into FALSE and vice versa
!TRUE
!FALSE
# the AND operator, &, returns TRUE only if elements on both sides of the operator are TRUE
1==1 & 2==1
# the inclusive OR operator, |, returns TRUE if EITHER of the elements are TRUE
1==1 | 2==1

# there is no operator for exclusive OR, but there is a function, xor()
xor(1==1 , 2==2)

# 1.1 
# mtcars is a database of cars with several variables such as horsepower, weight, number of cylinders etc.
data(mtcars)
head(mtcars)
# Using indexing (square brackets) and the & operator, write a line of code
# that selects only the rows of mtcars with at least 6 cylinders (mtcars$cyl >= 6) and horsepower of at least 110 (mtcars$hp >= 110). Remember to include all the columns.

d2 <- mtcars[mtcars$cyl >= 6 & mtcars$hp >= 110, ]
d2$newcolumn <- 0

# create a new categorical variables called 'powerful' that takes the value 'low' when the horsepower is in the bottom quartile 'medium' when the horsepower is in the middle 2 quartile, and high when the horsepower is in the top quartile.
mtcars$powerful <- NA
mtcars$powerful[mtcars$hp < quantile(mtcars$hp,0.25)] <- 'low'
mtcars$powerful[mtcars$hp >= quantile(mtcars$hp,0.25)
                & mtcars$hp <= quantile(mtcars$hp,0.75)] <- 'medium'
mtcars$powerful[mtcars$hp > quantile(mtcars$hp,0.75)] <- 'high'
head(mtcars$powerful)

# another way to do this is to use cut
mtcars$powerful <- cut(mtcars$hp, breaks = c(0, 96.5, 180, 1000), labels = c('low', 'medium', 'high'))
mtcars$powerful <- cut(mtcars$hp, breaks = quantile(mtcars$hp, c(0, 0.25, 0.75,1)), labels = c('low', 'medium', 'high'))
head(mtcars)
plot(table(mtcars$powerful))
barplot(table(mtcars$powerful)) # the table is added so it can turn the meium, high & low to a varible

# you can also make use of tidyverse

mtcars %>% mutate(powerful = case_when (hp < 90 ~ 'low',
                                        hp >= 90 & hp <= 180 ~ 'medium',
                                        hp>180 & hp <= 270~ 'high',
                                        hp>270 ~ 'very high')) -> mtcars

tcars$powerful <- factor(mtcars$powerful, levels=c('low','medium', 'high', 'very high'))

barplot(table(mtcars$powerful))
hist(mtcars$hp, breaks=c(0, 90,180,270,360) )

### 1.2
# Now select only those rows with either high efficiency (miles per gallon (mpg) of at least 25) or low weight (wt <= 2.5)

#############################
####    If statements    ####
#############################

# The if() function will execute everything after it, either on the same line or in {} brackets,
# only if there is a TRUE boolean statement within the parentheses
x <- 5
if(x > 3) print ('This statement is true')
if(x-4==1){
  new_object <- c('this','statement','is','also','true')
  print(new_object)
}

### 2.1
# Write a function called probe, that takes two arguments, n and w.
# The function should return a character vector of length n, consisting of 'Water' and 'Land', sampled with probability w. (so probability of sampling 'Water' is w)
# If the p argument is not numeric, or if it is not between 0 and 1, the function should return the following message:
# "Please input a probability between 0 and 1"

probe <- function(n,w){
  if(w<0 | w>1 | !is.numeric(w)){
    return("Please input a probability between 0 and 1")
  } 
    listwl <- sample(c("Water", "Land"), n, prob = c(w, 1-w), replace=T)
    return(listwl)
  }
 probe(10,2)
 probe(10,1)
 probe(10, 0)
 
# After the if statement we can put an else statement:
if(x-4>1){
  new_object <- c('this','statement','is','also','true')
  print(new_object)
}else{
  print('This is untrue')
}

# A simplified version of if()else is the following:
ifelse(x/2==7,print('Definitely true'),print('categorically false'))

##################################
####    Paste and strsplit    ####
##################################

# paste() and strsplit() can be used to join and separate
# character objects (strings), respectively. For example:

paste('Hello','world!', sep='_')
strsplit('Hello to you too. /My name is Ed.', split='/')

# You'll notice that strsplit returns a list. This allows us to vectorise the function:
strsplit(rownames(mtcars),split=' ')


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

### 4.1 
data(iris)

# Write a for loop that iterates over the column names of the iris dataset and print each together with the number of characters in the column name in parenthesis. Example output: Sepal.Length (12). To get the number of characters use the function nchar().

# Next, WHILE loops continue to loop until the boolean statment in the defining parentheses, e.g.
x <- 0
while(x<100){
  print(x)
  x <- x+sample(1:20,1)
}

### 4.2 How many numbers do you need in the sequence 1*2*3*4*5*... before the product exceeds 10 million?
# Use a while loop to get the answer

###################################
####    Linear models intro    ####
###################################

# We can run an OLS linear model using lm()
# Inside the lm and other model functions we use formulas
# Formulas have the dependent variable on the left and the independent (predictor) variables on the right with a ~ in between
# Lets run a bivariate regression of car weight (in 1000 pounds/500 kg) on miles per gallon (1mpg = 1km/L)
model <- lm(mtcars$mpg ~ mtcars$wt)
summary(model)
### 5.1
# What does the Estimate for the (Intercept) number represent?

plot(mtcars$wt, mtcars$mpg, pch=20, xlim = c(-1, 6))
abline(model) 
# the intercept (37.2851)  is the predicted fuel efficiency of a car that weighs 0 lbs
### 5.2
# What does the Estimate for the mtcars$wt number represent?

# this estimate represent the predicted change in fuel efficiency associated with a 1 unit change in weight (1000lbs change in weight) (ANSWERED IN CLASS)


### 5.3 
# Is the relationship between these two variables positive or negative? Why do you think that might be?

# it is negative (slope is negative i.e -5.3445) because bigger cars use more energy it takes to move.

### 5.4 What is the predicted average efficiency in miles per gallon of a 4000 pound (2000kg) car?
# use y=a+bx to answer where a= intercept, b=slope and x is 4
37.2851 + (-5.3445)*4 # where 37.2851 is the intercept and -5.3445 is the slope 
# = 15.9071mpg

# Let's transform the independent variable:

mtcars$wt_centred <- mtcars$wt - mean(mtcars$wt)

### 5.5
# compare the mean and variance of the new variable with the untransformed variable. What do you notice?

mean(mtcars$wt) # 3.21725
var(mtcars$wt)  # 0.957379
mean(mtcars$wt_centred) # equal to 0
var(mtcars$wt_centred) #0.957379 it is still the same because we have not done anything with the variance

### 5.6
model2 <- lm(mtcars$mpg ~ mtcars$wt_centred) # run a linear model that predict mpg from wt centered 
summary(model2)


# the slope stays the same but the intercept changes(which is the predicted change in fuel effeiciency when the centered weight is 0 i.e when the weight is equal to the average weight(mean))
# The new value of the intercept represent the predicted fuel efficiency of a car of average weight

# Run a new regression with new independent variable
# What do you notice about the estimates?
# What is the interpretation of the (Intercept) estimate in this regression?

### 5.7
# Run the following code:
y <- mtcars$mpg
x <- cbind(1,mtcars$wt)

# A couple of functions for you to know:
# t() returns the transpose of any matrix
# solve() returns the inverse of any (invertible) matrix
# %*% is matrix multiplication
# with that in mind try to code the following expression in R:
# (x'x)^(-1) * (x'y)
# where ' means the transpose
# Run the code you have written. What do you find?



