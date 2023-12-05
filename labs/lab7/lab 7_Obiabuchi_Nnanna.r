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
d



View(d)
# 1.1
# Plot the tooth growth against the dose of vitamin C given
plot(d$dose, d$len, psch= 20)

library(ggplot2)

ggplot(data=d) + geom_point(mapping=aes(x= dose,y=len))
#OR
ggplot(data= d, aes(x= dose, y= len)) + geom_point()+ ylab(label= "Length of growth cell(mm)") +xlab(label = " Vitamin C dosage (mg/day)")+labs(title= 'Effect of vitamin C on Guinea Pig Tooth growth')

png(file)
plot(d$dose, d$len, psch= 20)

# 1.2 Run a linear regression exploring the association between vitamin C
# and tooth growth in Guinea pigs.
tooth_growth_vc <-lm(d$len~d$dose)
tooth_growth_vc

tooth_growth_vc1 <-lm(len~dose, data=d)
tooth_growth_vc1
# 1.3 Which variable did you use as your outcome variable? Why?
# we used tooth growth as our outcome because the hypothesis is that tooth growth is causually affect by the dosage of the vitamin c
# 1.4 Interpret the estimates for the intercept and the slope of the regression line

summary(tooth_growth_vc1)

#The estimate for the intercept is 7.4mm. This means that predicted tooth length
# of a Guinea pig that was NOT given any 7.4mm
#The estimate of the slope is 9.8.mm. YTjis means that for each additional mg/day  of th VC administered The guinea pigs are  predicted to grow an additional 9.8mm.


# 1.5 Extract the 96 % confidence intervals for the slope and the intercepts and interpret them
confint(tooth_growth_vc1, level=0.96)
#we 96% CONFIDENCE  that the true VALUES intersect falls between  4.7mm and 10.1m
#and THAT THE TRUE VALUE OF THE EFFECT OF 1mg/day of VC is betwwen 7.8 and 11.8m

# 1.6 Interpret the p-values of the slope and intercept estimates. 
#the probability of seeing a slope as steep as the one we have observed ( a number as extreme as 9.76). 
#If the true slope was 0 is 10^(-14).
#We can say therefore that  theat we reject the null hypothesis at a 0.0001 level and 
#that his  study provides the support for the hypthesis that affects tooth growth in guinea pig

# 1.7 What is the connection between the confidence intervals and the p-values? 
#If an X% confidence interval does not contain 0, then the pvalue for that estimate will be significant at the (1-x)% level


E.G if a 97 % of CI does not  contain ), the p-value will be smaller than 0.03

# 1.8 Using the function predict() calculate the estimate and confidence interval for the predicted rate of tooth growth when the prescribed dose is 1mg/day
new_data <- data.frame(dose= 1.2)
new_data
predict(tooth_growth_vc1, newdata= new_data, interval ='confidence', level =0.95)

at bx=
7.4225 +9.7636*1.2

new_data <- data.frame(dose=seq(0,2,0.01))

new_data

predictions<-as.data.frame(predict(tooth_growth_vc1, newdata= new_data, interval ='confidence', level =0.95))
predictions
plot(d$dose, d$len, psch= 20)
lines(new_data$dose, predictions$fit)
lines(new_data$dose, predictions$upr, lty= 'dotf=dash')
lines(new_data$dose, predictions$lwr,lty= 'dotdash' )




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


