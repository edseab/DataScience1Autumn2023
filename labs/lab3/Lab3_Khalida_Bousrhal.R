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

pnorm(194, 170, 15)

# 2. A group of Examplians start a club called the Tall and Short Club (TSC), where you have to be in the tallest 2.5% of the population 
#    OR the bottom 2.5% of the population in height to be let in. 
#    How tall or how short do you have to be to be accepted into the TSC? Would Amir be accepted?

qnorm(0.025, 170, 15)
qnorm(0.025, 170, 15, lower.tail = F)
#Amir is less then 199.39m so he is not accepted

# 3. In response to the Tall and Short club, another group of Examplians start the Average People's Club (APC), where you have to be
#    within 0.1 standard deviations from the mean in height in order to get in.
#    What is the range of heights acceptable to join the APC? What percentage of the total population is eligible to join?

#0.1 of the stander deviation means 0.1 of the 15cm : 15*0.1 = 1.5
#So the range of heights is [170-1.5;170+1.5] = [168.5;171.5]
pnorm(171.5, 170, 15)-pnorm(168.5, 170, 15)

# 4. If we selected 10 Examplians at random from the population, what is the probability that none of them are eligible to join either
#    the TSC or the APC? 

InTeam =  (pnorm(171.5, 170, 15)-pnorm(168.5, 170, 15)) +(0.025*2)
dbinom(0, size = 10, prob = InTeam ) 
# 5. What is the probability that exactly 2 are eligible to join the APC and the rest are not?
dbinom(2, size = 10, prob =  (pnorm(171.5, 170, 15)-pnorm(168.5, 170, 15))) 
rbinom(10, size = 5, prob = 0.5)
# 6. What is the probability that at least 3 of them are eligible to join the TSC?
pbinom(2, size = 10, prob = (0.025*2), lower.tail = F ) 