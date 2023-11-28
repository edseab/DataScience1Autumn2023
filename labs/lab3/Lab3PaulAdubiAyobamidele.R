###################################
###################################
########                   ########
########   Data Science 2  ########
########       Lab 3       ########
########  24th Oct. 2023   ########
########                   ########
###################################
###################################

?qbinom()
# Some excercise to practice what we learned this week.

# 1. In the country of Examplia, it is known that people's heights are precisely normally distributed,
#    with a mean of 170cm and a standard deviation of 15cm.
#    Amir is 194cm tall. In what height percentile of the population is he? Round to the nearest integer number.
#    (Remember: 1st percentile = shortest 1 percent of the population, 99th percentile: tallest 1 percent of the population)

?qnorm()

mean <- 170
sd <- 15

# dbinom(7, 10, 0.6)
prod_density <- pnorm(194, mean, sd)
round_prod_density <- round(prod_density * 100)
round_prod_density


# 2. A group of Examplians start a club called the Tall and Short Club (TSC), where you have to be in the tallest 2.5% of the population
#    OR the bottom 2.5% of the population in height to be let in.
#    How tall or how short do you have to be to be accepted into the TSC? Would Amir be accepted?

tall_cutoff <- qnorm(0.025, mean, sd, lower.tail = FALSE)
tall_cutoff
short_cutoff <- qnorm(0.025, mean, sd)
short_cutoff

amir_height <- 194
Amir_be_accepted <- amir_height >= tall_cutoff
Amir_be_accepted


# Amir_be_accepted returns True, therefore Amir will be accepted into the club



# 3. In response to the Tall and Short club, another group of Examplians start the Average People's Club (APC), where you have to be
#    within 0.1 standard deviations from the mean in height in order to get in.

#    What is the range of heights acceptable to join the APC? What percentage of the total population is eligible to join?
upper_bound <- mean + 0.1 * sd
lower_bound <- mean - 0.1 * sd


range <- c(lower_bound, upper_bound)
range

percentage_total_population <- pnorm(upper_bound, mean, sd) - pnorm(lower_bound, mean, sd)
percentage_total_population


# 4. If we selected 10 Examplians at random from the population, what is the probability that none of them are eligible to join either
#    the TSC or the APC?

eligible_for_TSC <- 0.025 * 2

eligible_for_TSC
eligible_for_APC <- percentage_total_population

eligible_for_APC


non_eligible_for_APC_and_TSC <- (1 - eligible_for_TSC * 1 - eligible_for_APC)


non_eligible_for_APC_and_TSC
for_ten_examplians <- non_eligible_for_APC_and_TSC^10
for_ten_examplians


dbinom(10, 10, 0.87)
dbinom(0, 10, 0.13)


# 5. What is the probability that exactly 2 are eligible to join the APC and the rest are not?

probability_three_apc <- dbinom(2, 10, 0.08)


# 6. What is the probability that at least 3 of them are eligible to join the TSC?

probability_three_tsc <- 1 - pbinom(3, 10, 0.05)



# OR we can use
# 1 - sum(dbinom(x=0:2, 10, 0.05))
# sum(dbinom(x=3:10, 10, 0.05))


probability_three_tsc
