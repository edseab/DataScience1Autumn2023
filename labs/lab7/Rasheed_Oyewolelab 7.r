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
data('ToothGrowth')
?ToothGrowth
d <- ToothGrowth
d <- ToothGrowth

view(ToothGrowth)
# 1.1
# Plot the tooth growth against the dose of vitamin C given

library(ggplot2)

ggplot(d, aes(dose, len)) + geom_point() + labs(x="Dose of vitamin C (mg/d)", y = "Length of tooth (mm)", title ="A plot of Dose of Vitamin C against the length of the tooth")

plot(d$dose,d$len, ylab="Length of tooth (mm)",pch=20, xlab="Dose of Vitamin C (mg/day)", main="A plot of Vitamin C dose against length of cell growth")

# 1.2 Run a linear regression exploring the association between vitamin C
# and tooth growth in Guinea pigs.
model<-lm(d$len ~ d$dose)


model <- lm(len~dose, data =d)
summary(model)
plot(d$dose,d$len,pch=20)>abline(model)
# 1.3 Which variable did you use as your outcome variable? Why?

# We want to see if the dose of vitamin C affects the tooth growth not the other way round.

#We used tooth growth as the outcome because the hypothesis is that toth growth is causally affected by the dosage of vitamin C.

# 1.4 Interpret the estimates for the intercept and the slope of the regression line
summary(model)
#The estimate for the intercept is 7.4mm. This means that the prodicted tooth length of a Guinea pig that was NOT given any VC is 7.4mm

#The estimate of the slope is 9.8mm. This means that for each additional mg/day of VC administered, the Guinea pigs are predicted to grow an additional 9.8mm.


# 1.5 Extract the 96 % confidence intervals for the slope and the intercepts and interpret them

confint(model, level=0.96)
?confint.default

confint(model, level= 0.96)

#We have 96% CONFIDENCE that the true value of the intercept is between 4.8 and 10.1mm,  and that the true value of the effect of 1mg/day of VC is between 7.8 and 11.8mm


#In 96% of samples we would expect the true value of the intercept and slope to fall within our confidence intervals.

# 1.6 Interpret the p-values of the slope and intercept estimates. 

# The probability of seeing a slope and steep as the observed one (a number as extreme as 9.76) if the true slope was zero is 10^(-14) 

# 1.7 What is the connection between the confidence intervals and the p-values? 

#Tf an X% of CI doesn't contain 0, then the p-value for that estimate will be significant at the (1-X)% level 

#if a 97% CONFIDENCE INTERVAL does not contain 0, then the p- value for that estimate is significant at a level of 0.03

#if a 95% cinfidence interval does contain zero, then the p-value is not significant as it is  greater than 0.05

# 1.8 Using the function predict() calculate the estimate and confidence interval for the predicted rate of tooth growth when the prescribed dose is 1.2mg/day

new_data <-data.frame(dose=1.2)

predict(model,new_data=new_data,interval='confidence',level=0.95)


# 1.9 Now get the confidence intervals for all possible values of the dosage between 0 and 2 mg/day, spaced out 0.01 mg/day from each other


new_dataa<- data.frame(dose = seq(0,2,0.01 ))

predictions <- as.data.frame(predict(model, new_dataa, interval ='confidence')
) 
# 1.10 plot the predicted values, upper, and lower confidence intervals as lines on the data

plot(d$dose, d$len, pch =20)

lines(new_dataa$dose, predictions$fit)
lines(new_dataa$dose, predictions$upr, lty = "dotdash")
lines(new_dataa$dose, predictions$lwr, lty = "dotdash")
predictions$fit

# 1.11 Compare this to using geom_smooth(method=lm) using ggplot2

ggplot(d, aes(dose,len)) + geom_point() + geom_smooth(method = 'lm', levels= 0.95)


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


