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
head(d)
summary(d)

# 1.1
# Plot the tooth growth against the dose of vitamin C given

library(ggplot2)
ggplot(data=d, mapping = aes(x=dose, y=len)) + 
  geom_point() +
  xlab(label="Vitamin C dosagge (mg/day)") +
  ylab(label= "Length of growth cells (mm)") +
  labs(tittle='Effect of Vitamin C on Guinea pig tooth growth')



plot(d$dose,d$len, pch=20,
     xlab="Vitamin C dosagge (mg/day)")
ylab="Length of growth cells (mm)"
main="Effect of Vitamin C on Guinea pig tooth growth"

# 1.2 Run a linear regression exploring the association between vitamin C
# and tooth growth in Guinea pigs.

tooth_growth_vc <- lm(len~dose, data=d)
tooth_growth_vc

# 1.3 Which variable did you use as your outcome variable? Why?

# We used  tooth growth as the outcome because the hypothesis is that tooth growth is causially affected by the dosage of Vitamin C 
#... because tooth growth is the outcome of the experimental design
#... because we are manipulating the dosage and measuring the change in tooth growth as a result


# 1.4 Interpret the estimates for the intercept and the slope of the regression line

summary(tooth_growth_vc)

# the estimate for the intercept is 7.4mm. this mean that the predicted tooth length
#of a guinean pig that was NOT(i.e without been administered) any VitaminC is 7.4mm
#The estimates of the slope is 9.8mm. This means that for each additional mg/day(unit) of Vitamin C administered, the guinea pigs are predicted to grow an additional 9.8mm


# 1.5 Extract the 96 % confidence intervals for the slope and the intercepts and interpret them

confint(tooth_growth_vc, level = 0.96) # intercept( 4.775103, 10.06990), dose(slope) (7.762328, 11.76482)

# we have 96% CONFIDENCE that the true value of the intercept is between 4.8 and 10.1, and that the true value of the effect of 1mg/day of vitamin C is between 7.8 and 11.8mm
# in 96% of samples we would expect the true value of the intercept and the slope to fall within our confidence intervals.

# 1.6 Interpret the p-values of the slope and intercept estimates. 

summary(tooth_growth_vc)

# the probability of seeing a slope (1.23e-14) as steep as the one we have observed (a number as extreme as 9.76) if the true slope was 0 is 10^(-14)

# we can say therefore that we reject the null hypothesis at a 0.001 level and that this study provides support for the hypothesis that Vitamin C affects tooth growth in Guinea pigs

# 1.7 What is the connection between the confidence intervals and the p-values? 

# IF an X% confidence interval does not contaion 0, then the p-value for that estimate will be significant at the (1-X)% level(i.e the p-value is smaller than that X% level)

# E.G if a 97% CI does not contain 0, then the p-vlaue will be smaller than 0.03
# If a 95% condnfidence interval does contain 0, the p-value will be greater than 0.05

# 1.8 Using the function predict() calculate the estimate and confidence interval for the predicted rate of tooth growth when the prescribed dose is 1mg/day

new_data <- data.frame(dose=1.2)
predict(tooth_growth_vc, newdata=new_data, interval='confidence', level = 0.95)

# a + bx = 7.4225 + 9.7636*1.2 (we can calsulate it ourselves using this)

# 1.9 Now get the confidence intervals for all possible values of the dosage between 0 and 2 mg/day, spaced out 0.01 mg/day from each other


new_data <- data.frame(dose=seq(0,2,0.01))
predict(tooth_growth_vc, newdata=new_data, interval='confidence', level = 0.95)

predictions <- as.data.frame(
  predict(tooth_growth_vc, newdata=new_data, interval='confidence', level=0.95))

# 1.10 plot the predicted values, upper, and lower confidence intervals as lines on the data

plot(d$dose, d$len, pch=20)
lines(new_data$dose,predictions$fit)
lines(new_data$dose,predictions$upr, lty='dotdash')
lines(new_data$dose,predictions$lwr, lty='dotdash')


# 1.11 Compare this to using geom_smooth(method=lm) using ggplot2

ggplot(d, aes(dose,len)) +
  geom_point()+
  geom_smooth(method = 'lm')


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

paste("hello", "world", sep ='')
paste0("hello", "world")

### 2.1 
data(iris)

# Write a for loop that iterates over the column names of the iris dataset and print each together with the number of characters in the column name in parenthesis. Example output: Sepal.Length (12). To get the number of characters use the function nchar().

for(i in colnames(iris)){
  print(paste0(i, '(', nchar(i), ')'))
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

i = 1
x = 1
while(x<10000000){
  print(i)
  print(x)
  i <- i+1
  x <- x*i
}
# the answer is 10 times

# also using the factorial option
i <- 1
while (factorial(i)<10000000){
  print(i)
  print(factorial(i))
  i <- i+1
}