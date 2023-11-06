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
pnorm(194,170,15)

#Amir is at the 95th percentile:tallest 5 percent of the population
# 2. A group of Examplians start a club called the Tall and Short Club (TSC), where you have to be in the tallest 2.5% of the population 
#    OR the bottom 2.5% of the population in height to be let in. 
#    How tall or how short do you have to be to be accepted into the TSC? Would Amir be accepted?
qnorm(0.025,170,15)
qnorm(0.025,170,15, lower.tail=FALSE)

#141cm short  and since Amir is 194cm tall he can't be a member of TSC

# 3. In response to the Tall and Short club, another group of Examplians start the Average People's Club (APC), where you have to be
#    within 0.1 standard deviations from the mean in height in order to get in.
#    What is the range of heights acceptable to join the APC? What percentage of the total population is eligible to join?

#To calculate the range of height, we need to specify our boundary (upper and lower)
mean.height <- 170
sd.height <- 15
upper.limit <- mean.height + 0.1 * sd.height
upper.limit
lower.limit <- mean.height - 0.1 * sd.height
lower.limit
#To join the APC your height must be between 168.5cm and 171.5cm
#percentage population is calculated below
pnorm(171.5,170,15)-pnorm(168.5,170,15)
#This is about 7.96 approximately 8 percent of the total population were allowed to join APC
# 4. If we selected 10 Examplians at random from the population, what is the probability that none of them are eligible to join either
#    the TSC or the APC? 
#Total population = 0.08+0.025+0.025 = 0.13
(1-0.13)^10
dbinom(10,10,0.87) #or
dbinom(0,10,0.13)
# 5. What is the probability that exactly 2 are eligible to join the APC and the rest are not?
dbinom(2,10,0.08)

dbinom(2,10,0.08)
# 6. What is the probability that at least 3 of them are eligible to join the TSC?

1-pbinom(2,10,0.05) #or
pbinom(10,10,0.05)-pbinom(2,10,0.05) #or
