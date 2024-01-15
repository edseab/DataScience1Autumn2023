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

d <- ToothGrowth
?ToothGrowth

# 1.1
# Plot the tooth growth against the dose of vitamin C given

png(filename = 'example_plot.png', height = 400, width = 600) #to save as png
plot(d$dose, d$len, pch=20, 
    xlab = '"vitamins C dossage(mg/day)',
    ylab= "Length of growth cells (mn)", 
    main="Effect of Vitamin C on Guinea Pig Tooth Growth")
dev.off()

ggplot(d, aes(x=dose, y=len)) +
geom_point() + 
labs(x="vitamins C dossage(mg/day)", 
y="Length of growth cells (mn)", 
title= "Effect of Vitamin C on Guinea Pig Tooth Growth", subtitle="plot") + 
geom_smooth(method="lm", level=0.90)

library('ggplot2')
library(tidyverse)
ggplot(data=d, mapping = aes(x=dose,y=len)) +
geom_point() + 
xlab(label = "vitamins C dossage(mg/day)") +
ylab(label= "Length of growth cells (mn)") + 
labs(title=("Effect of Vitamin C on Guinea Pig Tooth Growth"))

install.packages('httpgd')

ggplot(data=d , mapping=aes(x=dose , y=len))+
geom_point()+
xlab("Vitamin C dosage (mgg/day)") + 
ylab(label="Lenght of growtch cells (mm)")+
labs(title = "Effect of Vitamin C on Guinea Pig Tooth growth")


ggplot(data=d , mapping=aes(x=dose , y=len))+
geom_point()+
xlab("Vitamin C dosage (mgg/day)") + 
ylab("Lenght of growtch cells (mm)")+
labs("Effect of Vitamin C on Guinea Pig Tooth growth")

# 1.2 Run a linear regression exploring the association between vitamin C
# and tooth growth in Guinea pigs.

model_TG <- lm(d$len ~ d$dose)
model_TG <- lm(len~dose, data=d)

# 1.3 Which variable did you use as your outcome variable? Why?

#because we are manipulating the dosage and measuring the change in tooth growth as the outcome and result

# 1.4 Interpret the estimates for the intercept and the slope of the regression line
summary(model_TG)

#the estimate of the intercept is 7.4. this means the predicted tooth length of a guinea pig that was not given any vitamin is 7.4mm

#predicted length of the tooth when there was no vitamin administered

#for every additional unit of vitamin mm per day ministered, the guinea pigs are predicted to grow an addtional 9.8mm in tooth growth i.e 9.8mm more 

# 1.5 Extract the 96 % confidence intervals for the slope and the intercepts and interpret them

confint.default(model_TG, level = 0.96)

#we have 96% confidence that the true intercept is between these two numbers (4.8, 10.1) and that the true value of the effect of 1mm/day of vitamin is between 7.8 and 11.8
#OR
#it means that in 98% of the samples we expect that the true valur of the intercpet and slope to fall within our confidence intervals

# 1.6 Interpret the p-values of the slope and intercept estimates. 
#this is the probability of seeing a slope as steep as the one we observed (9.76) on the two tails if the true slope is 0 (or if the Ho is 0) is 0.00000000000000123 (very insignificant), so we fail to reject the H0.  or it is unlikely that this observation is due to random chance (COMFIRM)


# 1.7 What is the connection between the confidence intervals and the p-values?

#if and X % CI doesnt contain zero, then the pvalue for that estimate will be significant at the 1-x percent level e.g 99% CI without zero, like if it doeasnt contain 0 (btween 4-10) it means that Pvalue will be 0.01
E.G if a 97% CT doesnt contain 

# 1.8 Using the function predict() calculate the estimate and confidence interval for the predicted rate of tooth growth when the prescribed dose is 1.2mg/day

#create a new dataframe
new_data <- data.frame(dose=1.2)
?predict
predict(model_TG, newdata=new_data, interval= 'confidence', level =0.95)

a + bx=
7.4225 + 9.7636*1.2
# 1.9 Now get the confidence intervals for all possible values of the dosage between 0 and 2 mg/day, spaced out 0.01 mg/day from each other
new_data <- data.frame(dose =seq(0, 2, 0.01)) #create a new data of points from 0 to 2, with 0.01 interval


predict(model_TG, newdata=new_data, interval= 'confidence', level =0.95)

as.data.frame(predict(model_TG, newdata=new_data, interval= 'confidence', level =0.95)) -> predictions #you need to convert the data predicted from matrices format into a data

# 1.10 plot the predicted values, upper, and lower confidence intervals as lines on the data

plot(d$dose, d$len, pch= 20)
lines(new_data$dose, predictions$fit)
#if you are only interested in drawing the line without the CI
abline(model_TG)
lines(new_data$dose, predictions$upr, lty ='dotdash')
lines( new_data$dose, predictions$lwr, lty ='dotdash') #check class videos

# 1.11 Compare this to using geom_smooth(method=lm) using ggplot2

ggplot(d, aes(dose,len)) + 
geom_point() +
geom_smooth(method ='lm')




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

for(i in colnames(iris)) {
  print(paste0(i, ' (', nchar(i), ')'))
}


# Next, WHILE loops continue to loop until the boolean statment in the defining parentheses, e.g.

set.seed(123)
x <- 0
while (x<100) {
  print(x)
  x <- x+ sample(1:20,1)
}
### 2.2 How many numbers do you need in the sequence 1*2*3*4*5*... before the product exceeds 10 million?
# Use a while loop to get the answer

i <- 1
x <- 1
while(x<10000000) {
  print(i)
  print(x)
  x
}





d<- data("state")
head(d)