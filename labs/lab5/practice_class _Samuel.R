data(mtcars)
plot(mtcars$wt,mtcars$mpg,
 ylim=c(0,50),
 xlim=c(-0.5,7),
 pch =20,
 main="Car Efficiency as function 0f Weight",
 ylab ="Fuel Efficiency(Miles per gallon)",
 xlab="Weight")


 model <- lm(mtcars$mpg~mtcars$wt)
 summary(model)

 nrow(mtcars)