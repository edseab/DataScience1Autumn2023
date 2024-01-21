###################################
###################################
########                   ########
########   Data Science 2  ########
########       Lab 3       ######## 
########  24th Oct. 2023   ########
########                   ########
###################################
###################################


# Some excercise to practice what we learned this week.

# 1. In the country of Examplia, it is known that people's heights are precisely normally distributed,
#    with a mean of 170cm and a standard deviation of 15cm.
#    Amir is 194cm tall. In what height percentile of the population is he? Round to the nearest integer number.
#    (Remember: 1st percentile = shortest 1 percent of the population, 99th percentile: tallest 1 percent of the population)

pnorm(194,170,15, lower.tail=FALSE)



# 2. A group of Examplians start a club called the Tall and Short Club (TSC), where you have to be in the tallest 2.5% of the population 
#    OR the bottom 2.5% of the population in height to be let in. 
#    How tall or how short do you have to be to be accepted into the TSC? Would Amir be accepted?

qnorm(0.25,170,15)  #the shortest one XD
qnorm(0.75 , 170 , 15)   # The tallest one :)

# 3. In response to the Tall and Short club, another group of Examplians start the Average People's Club (APC), where you have to be
#    within 0.1 standard deviations from the mean in height in order to get in.
#    What is the range of heights acceptable to join the APC? What percentage of the total population is eligible to join?

lower<-qnorm(0.5 - 0.1, 170 , 15)
higher<-qnorm(0.5 + 0.1, 170 , 15)
cat("the range should be from : ", lower , " to" , higher )


# 4. If we selected 10 Examplians at random from the population, what is the probability that none of them are eligible to join either
#    the TSC or the APC? 

dbinom(0,10,0.13)
dbinom(2,10,0.08)
# 5. What is the probability that exactly 2 are eligible to join the APC and the rest are not?
dbinom(2,10,0.08) #
# 6. What is the probability that at least 3 of them are eligible to join the TSC?

sum(dbinom(3:10,10,0.05)) 
1 - pbinom(2,10,0.05)



Part 2: Data manipulation and visualization
Load up the state.x77 database with the command data(state). To learn more about the dataset type ?state and look for state.x77.
a)	Save this database into a new object called d. It should be a data frame (you can check the object type using the ‘class()’ function) and contain contain 50 rows – one for each U.S. state – and 8 columns. 

d <-  as.data.frame(state.x77)

b)	Some of the column names are badly formatted: they contain a space in the column name. Please rename these columns or save them into new columns with appropriate names.

 	library(dplyr)
	d <- rename(d, HS_Grad = ‘HS Grad’, Life_Exp = ‘Life Exp’)

c)	Create a new column recoding the ‘Population’ variable into a categorical variable, where states with a population size of less than 2 million are coded ‘small’, between 2 and 4 million are coded ‘medium’, and more than 4 million are coded ‘large’. Careful – the unit of the ‘Population’ variable is 1000s of people (so a value of 1000 means 1 million people).

d$Pop_Size <- case_when(d$Population<=2000 ~ 'small',
                             d$Population>2000 & d$Population<=4000 ~ 'medium',
                             d$Population>4000 ~'large')

d)	Transform this character column into a factor variable, where the levels are ordered from small to large.

d$Pop_Size <- factor(d$Pop_Size, levels = c(small’,’medium’,’large’)


e)	Use an appropriate plot to visualize the murder rates in small, medium and large states. What do you notice?

plot(d$Pop_Size,d$Murder, main = 'Murder rates in US States by population size',
                          xlab = 'Population size',
                          ylab = 'Murder rate (per 100k population)')


The median murder rate appears to increase with the population size.

f)	Now plot the relationship between the state murder rate and the state life expectancy? What do you notice?

plot(d$Murder,d$Life_Exp, main = 'Life expectancy in US States as a function of the murder rate',
     xlab = 'Murder rate (per 100k population)',
     ylab = 'Life expectancy at birth (years)',
     pch = 20)

There appears to be a negative correlation between the murder rate and the life expectancy in US states.

g)	Provide 2 different possible explanations for the association between the murder rate and the life expectancy.

One possibility is that murders decrease life expectancy. Another possibility is that having a low life expectancy makes people more likely to commit crimes like murder (reverse causality). Another possibility is that a third factor affects both murder rates and life expectancy (confounding). For example poverty rates might decrease life expectancy because people have lower access to healthcare, and also increase crimes such as murder.








#Part 3. Model interpretation
#Run a bivariate linear regression with the state life expectancy as the outcome (dependent) variable and the state murder rate as the predictor (independent) variable.
	m1 <- lm(Life_Exp~Murder,data=d)
summary(m1)
#a)	Interpret the intercept and slope estimates in this regression model.
#The intercept estimate can be interpreted to mean that the predicted life expectancy of a state with 0 yearly murders is 72.97 years.
The slope estimate can be interpreted to mean that for every additional yearly murder per 100k population, the life expectancy of the state is predicted to be 0.28 years lower.

b)	Interpret as a probability the p-value associated with the ‘murder rate’ estimate.

This p-value can be interpreted to mean that there is a 2.26*10-11 probability that we would see a slope estimate this large in magnitude or more IF there was no true association between the murder rate and the life expectancy.

c)	In a null hypothesis significance test, what is the alpha level?
The alpha level is the accepted false positive rate, the rate at which we accept that we may reject the null hypothesis even though there is no true effect.

d)	If we had selected 0.01 as our alpha level, what would we conclude about the association between the murder rate and the life expectancy?

We would reject the null hypothesis at the 0.01 level and conclude that there is an association between the murder rate and the life expectancy.

e)	Run a new model with the same outcome and predictor variables, but this time controlling for the percentage of high school graduates and population size (using the categorical variable you coded in part 2).

m2 <- lm(Life_Exp~Murder + HS_Grad + Pop_Size,data=d)
summary(m2)

f)	What is the new predicted ‘murder rate’ estimate? What is its interpretation? Why is it different from the estimate in the bivariate model?


The new estimate of the effect of the murder rate is -0.26. It can be interpreted as the predicted change in life expectancy (in years) associated with every additional yearly murder per 100k people, AFTER the effects of population size and education have been accounted for. It is different from the bivariate model because the bivariate model does not account for the effects of population size and education.


g)	What is the interpretation of the estimate for the ‘medium’ population size?

The ‘medium’ population size estimate can be interpreted to mean that relative to small states, medium states have on average 0.56 years higher life expectancy, after adjusting for the effects of murder and education.

h)	What is the predicted life expectancy according to our model of a hypothetical state with a murder rate of 3 per 100 000, 60% High School graduates, and medium population size? Show your calculation and don’t use the ‘predict’ function.

69.58 + (-0.26)*3 + 0.052*60 + 0.56*1 + 0.66*0 = 72.48
The predicted life expectancy in this hypothetical state would be 72.48 years
