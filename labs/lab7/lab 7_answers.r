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

library(ggplot2)
ggplot(data=d,mapping=aes(x=dose,y=len)) +
geom_point() +
xlab(label="Vitamin C dosage (mg/day)") +
ylab(label = "Length of growth cells (mm)") +
labs(title='Effect of Vitamin C on Guinea Pig tooth growth')

png(filename = 'example_plot.png', height = 400, width = 600)
plot(d$dose,d$len, pch=20,
     xlab="Vitamin C dosage (mg/day)",
     ylab = "Length of growth cells (mm)",
     main= 'Effect of Vitamin C on Guinea Pig tooth growth')
dev.off()

# 1.2 Run a linear regression exploring the association between vitamin C
# and tooth growth in Guinea pigs.

tooth_growth_vc <- lm(len~dose, data=d)

# 1.3 Which variable did you use as your outcome variable? Why?

# We used tooth growth as the outcome because the hypothesis is that tooth growth is causally affected by the dosage of vitamin C
# ... because tooth growth is the outcome of the experimental design
# ... because we are manipulating the dosage and measuring the change in tooth growth as a result


# 1.4 Interpret the estimates for the intercept and the slope of the regression line

summary(tooth_growth_vc)

# The estimate for the intercept is 7.4 mm. This means that the predicted tooth length
# Of a Guinea pig that was NOT given any VC is 7.4mm
# The estimate of the slope is 9.8mm. This means that for each additional mg/day of VC administered, the Guinea pigs are predicted to grow an additional 9.8mm

# 1.5 Extract the 96 % confidence intervals for the slope and the intercepts and interpret them

confint(tooth_growth_vc, level=0.96)

# We have 96% CONFIDENCE that the true value of the intercept is between 4.8 and 10.1 mm,and that the true value of the effect of 1mg/day of VC is between 7.8 and 11.8mm

# In 96% of samples we would expect the true value of the intercept and slope to fall within our confidence intervals.

# 1.6 Interpret the p-values of the slope estimate. 

# The probability of seeing a slope as steep as the one we've observed (a number as extreme as 9.76) IF the true slope was 0 is 10^(-14).

# We can say therefore that we reject the null hypothesis at a 0.001 level and that this study provides support for the hypothesis that VC affects tooth growth in Guinea pigs

# 1.7 What is the connection between the confidence intervals and the p-values? 

# IF an X% confidence interval doesn't contain 0, then the p-value for that estimate will be significant at the (1-X)% level.

# E.G if a 97% CI doesnt contain 0, the p-value will be smaller than 0.03
# If a 95% confidence interval does contain 0, the p-value will be greater than 0.05




# 1.8 Using the function predict() calculate the estimate and confidence interval for the predicted rate of tooth growth when the prescribed dose is 1.2 mg/day

new_data <- data.frame(dose=1.2)
predict(tooth_growth_vc, newdata=new_data, interval='confidence', level = 0.95)

a + bx = 
7.4225 +9.7636*1.2

# 1.9 Now get the confidence intervals for all possible values of the dosage between 0 and 2 mg/day, spaced out 0.01 mg/day from each other

new_data <- data.frame(dose=seq(0,2,0.01))

predictions <- as.data.frame(predict(tooth_growth_vc, newdata=new_data, interval='confidence', level = 0.95))



# 1.10 plot the predicted values, upper, and lower confidence intervals as lines on the data

plot(d$dose,d$len, pch=20)
# lines(new_data$dose, predictions$fit)
abline(tooth_growth_vc)

lines(new_data$dose, predictions$upr, lty='dotdash')
lines(new_data$dose, predictions$lwr, lty='dotdash')


# 1.11 Compare this to using geom_smooth(method=lm) using ggplot2

ggplot(d, aes(dose,len))+
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


# Next, WHILE loops continue to loop until the boolean statment in the defining parentheses, e.g.

### 2.2 How many numbers do you need in the sequence 1*2*3*4*5*... before the product exceeds 10 million?
# Use a while loop to get the answer


