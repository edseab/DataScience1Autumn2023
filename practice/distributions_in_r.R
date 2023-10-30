# Binomial Distribution
# When a dice is rolled, what is the probability of rolling a 4,
# if we roll the dice independently eight times, what is the
# probability of observing a success five times in total?

dbinom(5, 8, (1 / 6))

# to obtain the full PMF table for the above, we can supplu the vector
# 0:8 to x

total_PMF <- dbinom(x = 0:8, 8, (1 / 6))

sum(total_PMF)

# This gives the total of 1, which is the probability of success.

?hist()
barplot(total_PMF,
    names.arg = 0:8, space = 2, xlab = "x",
    ylab = "Probability"
)

# Find the probability of finding at least three 4s in eight trials
?pbinom()
1 - pbinom(2, 8, 1 / 6)

rbinom(2, 8, (1 / 6))

# A forested nature reserve has 13 bird-viewing platforms scattered
# throughout a large block of land. The naturalists claim that at
# any point in time, there is a 75 percent chance of seeing birds at
# each platform. Suppose you walk through the reserve and visit
# every platform. If you assume that all relevant conditions are sat-
# isfied, let X be a binomial random variable representing the total
# number of platforms at which you see birds.

# a. Visualize the probability mass function of the binomial distribu-
# tion of interest.
# b. What is the probability you see birds at all sites?
# c. What is the probability you see birds at more than 9 platforms?
# d. What is the probability of seeing birds at between 8 and 11
# platforms (inclusive)? Confirm your answer by using only the
# d-function and then again using only the p-function.
# e. Say that, before your visit, you decide that if you see birds at
# fewer than 9 sites, you’ll make a scene and demand your entry
# fee back. What’s the probability of your embarrassing yourself in
# this way?
# f. Simulate realizations of X that represent 10 different visits to the
# reserve; store your resulting vector as an object.
# g. Compute the mean and standard deviation of the distribution of
# interest.


# a
range <- 0:13
prob_dist <- dbinom(range, 13, 0.75)

barplot(prob_dist,
    xlab = "Number of Platforms with Birds",
    ylab = "Probability", main = "Binomial Distribution PMF",
    col = "skyblue"
)

# b
dbinom(13, 13, 0.75)

# c
pbinom(9, 13, 0.75, lower.tail = FALSE)

# d
eight <- dbinom(8, 13, 0.75)
nine <- dbinom(9, 13, 0.75)
ten <- dbinom(10, 13, 0.75)
eleven <- dbinom(11, 13, 0.75)

total_dist <- sum(eight, nine, ten, eleven)
total_dist

pbinom(11, 13, 0.75) - pbinom(7, 13, 0.75)

# e

pbinom(8, 13, 0.75)

# f

simulations_ten <- rbinom(10, 13, 0.75)
simulations_ten

# g

# mean
n <- 13
p <- 0.75
mean_of_dist <- n * p
mean_of_dist

# SD
sd <- sqrt(n * p * (1 - p))
sd


# PDF UNIFORM DISTRIBUTION
?dunif()

# You visit a national park and are informed that the
# height of a certain
# species of tree found in the forest is uniformly distributed
# between 3
# and 70 feet.
# a. What is the probability you encounter a tree shorter than
# 5 1
# 2 feet?
# b. For this probability density function, what is the height that
# marks the cutoff point of the tallest 15 percent of trees?
# c. Evaluate the mean and standard deviation of the tree height
# distribution.
# d. Using (c), confirm that the chance that you encounter a tree
# with a height that is within half a standard deviation (that is,
# below or above) of the mean height is roughly 28.9 percent.
# e. At what height is the density function itself? Show it in a plot.
# f. Simulate 10 observed tree heights. Based on these data, use
# quantile (refer to Section 13.2.3) to estimate the answer you
# arrived at in (b). Repeat your simulation, this time generating
# 1,000 variates, and estimate (b) again. Do this a handful of
# times, taking a mental note of your two estimates each time.
# Overall, what do you notice of your two estimates (one based on
# 10 variates at a time and the other based on 1,000) with respect
# to the “true” value in (b)?

# a
punif(5.5, 3, 70)


# b
qunif(0.85, 3, 70)

# c

# mean
a <- 3
b <- 70

mean <- (a + b) / 2
mean

# SD

sd <- (b - a) / sqrt(12)
sd

# d
probability_within_half_std_dev <- punif(mean + (0.5 * sd), 3, 70) - punif(mean + (0.5 * sd), 3, 70)
probability_within_half_std_dev

# e
plot <- dunif(x = 3:70, 3, 70)
hist(plot)

# f


# NORMAL DISTRIBUTION (GAUSSIAN)
?dnorm()

# A tutor knows that the length of time taken to complete a certain
# statistics question by first-year undergraduate students, X, is
# normally distributed with a mean of 17 minutes and a standard
# deviation of 4.5 minutes.
# i. What is the probability a randomly selected undergraduate
# takes more than 20 minutes to complete the question?
# ii. What’s the chance that a student takes between 5 and 10
# minutes to finish the question?
# iii. Find the time that marks off the slowest 10 percent of
# students.
# Plot the normal distribution of interest between ±4σ and
# shade in the probability area of (iii), the slowest 10 percent
# of students.
# v. Generate a realization of times based on a class of 10 stu-
# dents completing the question.
# b. A meticulous gardener is interested in the length of blades
# of grass on his lawn. He believes that blade length X follows a
# normal distribution centered on 10 mm with a variance of 2 mm.
# i. Find the probability that a blade of grass is between 9.5 and
# 11 mm long.
# ii. What are the standardized values of 9.5 and 11 in the context
# of this distribution? Using the standardized values, confirm
# that you can obtain the same probability you found in (i)
# with the standard normal density.
# iii. Below which value are the shortest 2.5 percent of blade
# lengths found?
# iv. Standardize your answer from (iii)


# a
m <- 17
s <- 4.5

1 - pnorm(20, m, s)

# b

pnorm(10, m, s) - pnorm(5, m, s)

# c
slowest_10_percent_time <- qnorm(0.1, m, s)

# d
?seq()
x <- seq(m - s * 4, m + s * 4, length.out = 1000)

dist <- dnorm(x, m, s)

plot(x, dist,
    type = "l", xlab = "Time (minutes)",
    ylab = "Density",
    main = "Normal Distribution of Completion Time"
)

polygon(x[c(x <= slowest_10_percent_time, slowest_10_percent_time)],
    c(dist[x <= slowest_10_percent_time], 0),
    col = "skyblue"
)
