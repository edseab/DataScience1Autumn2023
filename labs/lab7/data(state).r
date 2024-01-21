d <-  as.data.frame(state.x77)
library(dplyr)
d <- rename(d, HS_Grad = "HS Grad", Life_Exp = "Life Exp")
d$Pop_Size <- case_when(d$Population<=2000 ~ 'small',
                             d$Population>2000 & d$Population<=4000 ~ 'medium',
                             d$Population>4000 ~'large')
d$Pop_Size <- factor(d$Pop_Size, levels = c("small","medium","large"))
plot(d$Pop_Size,d$Murder, main = "Murder rates in US States by population size",
                          xlab = "Population size",
                          ylab = "Murder rate (per 100k population)")
plot(d$Murder,d$Life_Exp, main = "Life expectancy in US States as a function of the murder rate",
     xlab = 'Murder rate (per 100k population)',
     ylab = 'Life expectancy at birth (years)',
     pch = 20)
m1 <- lm(Life_Exp~Murder,data=d)
summary(m1)

m2 <- lm(Life_Exp~Murder + HS_Grad + Pop_Size,data=d)
summary(m2)

#h)	What is the predicted life expectancy according to our model of a hypothetical state with a murder 
#rate of 3 per 100 000, 60% High School graduates, and medium population size? Show your calculation 
#and don’t use the ‘predict’ function.

3*(-0.26)+0.052*60+0.56*1+0.66*0 + 69.58


mean_Amal <- 65
sd_Amal <- 10
score_Amal <- 82

# Maximum score student's information
mean_Max <- 68
sd_Max <- 12
score_Max <- 87

# Calculate cumulative probabilities
P_Amal <- pnorm(score_Amal, mean_Amal, sd_Amal)
P_Max <- pnorm(score_Max, mean_Max, sd_Max)

P_Amal
pbinom(0,10,0.1)  #0.3486784
sum(dbinom(3:10 , 10 , 0.1))  #0.07019083
