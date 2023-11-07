###################################
###################################
########                   ########
########   Data Science 3  ########
########       Lab 4       ######## 
########  31st Oct. 2023   ########
########                   ########
###################################
###################################

## Welcome to lab number 4. Today we will start with an introduction to base R plotting and ggplot2

########################
####    Factors     ####
########################

# Quick note before we start on plotting. One final type of variable we haven't looked at yet is factors
# Factors are used for categorical data. Instead of just allowing any value, like a character variable, factors only allow one of a fixed number of 'levels', with one level serving as a 'reference category'.
# To change the reference category, you can use 
# relevel(x, ref=...)

# For now, just bear in mind that sometimes in certain statistical packages, character variables will need to be recoded as factors using factor(),
# and other times variables that shouldn't be factors get loaded as such and need to be changed back with as.character().

example <- c('a','b','c')
ex_factor <- as.factor(example)
c(ex_factor,'d')
c(ex_factor,as.factor('d'))
c(ex_factor,as.factor('a'))

########################
####    Datasets    ####
########################

# R contains a number of inbuilt datasets
# To see a list of them, run
data()

# To load one into your environment, run for example
data(mtcars)

########################
####    Plotting    ####
########################

# Base R has a lot of good functions for plotting, and can be very powerful.

# run the following:
example("plot")
# to get a series of examples of things you can do with the plot() function

# Plot() is the most basic function, and with no arguments it creates a scatterplot.
# it can either take an x variable and a y variable as its 2 first arguments, or it can take a formula
plot(mtcars$wt,mtcars$mpg)
plot(mtcars$mpg~mtcars$wt)

# You can change the shape of the points in a scatterplot with the 'pch' argument
plot(mtcars$wt,mtcars$mpg, pch=20)
# There are 25 preset values for the scatterplot points. To see them all run:
plot(1:25,rep(1,25),pch=1:25)

# You can also put in any symbol you like in quote marks:
plot(mtcars$wt,mtcars$mpg, pch="$")
plot(mtcars$wt,mtcars$mpg,pch="✌")

# To change x axis and y axis labels, and add a title you can use
plot(mtcars$wt,mtcars$mpg, pch=20, xlab='Weight (1000 lbs)', ylab='Fuel efficiency (mpg)', main='Association between car weight and fuel efficiency')

# And to change the range of the plot, use
plot(mtcars$wt,mtcars$mpg, pch=20, ylim = c(0,40), xlim = c(0,7))

# Instead of a scatterplot, you can change the type of the plot using 'type'
plot(dnorm(seq(-4,4,0.2)),pch=20)
plot(dnorm(seq(-4,4,0.2)),type='l')
plot(dnorm(seq(-4,4,0.2)),type='h')
plot(dnorm(seq(-4,4,0.2)),type='b')
plot(dnorm(seq(-4,4,0.2)),type='s')
plot(dnorm(seq(-4,4,0.2)),type='o')

# And the color using color
plot(dnorm(seq(-4,4,0.2)),type='o', col='red')

# For colors you can use preloaded R colors, which you can look up here:
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf

# You can also use hex codes, such as:
plot(dnorm(seq(-4,4,0.2)),type='o', col='#6D1ACF')

# Pick your own color and find its hex code here:
# https://htmlcolorcodes.com/color-picker/

# For turning frequencies into histograms or density plots,
# use hist() and density() 

# For barplots and boxplots, we use barplot() and boxplot()


# Axes can be removed from the original plot using xaxt='n' and yaxt = 'n'
# New axes can be added using the axis() function
# text(), segments(), arrows(), polygon(), legend(), add these respective elements to your plot. 
# Experiment with them or run ?text, ?segments, etc. to find out more.

# Fonts can be changed using the arguments cex (for size), family (for font type), and font (for bold or italics)

# Lines can be added to plots using abline()
plot(mtcars$wt,mtcars$mpg, pch=20, ylim = c(0,40), xlim = c(0,7),
     abline(v=0,h=0,lm(mpg~wt,data=mtcars)))

# Other plots can be made using barplot(), 


#######################
####    Devices    ####
#######################

# When you create a plot, it gets stored to a graph device. This gets opened automatically when you plot, and in Rstudio is displayed on one of the panes.
# You can also open a graphics device manually using quartz() on Mac and Linux and windows() on Windows

# The par() function sets the graphical parameters for the device.
# You can use it to set the margins of the graph (in inches) using mar
par(mar=c(5,5,5,5))
plot(mtcars$wt,mtcars$mpg)

# These parameters will remain attached to the graphical device until you turn them off, with:
dev.off()

# par() can be used to generate multiple plots in the same device with the argument mfrow
# mfrow takes a vector of 2 numbers, first the number of rows then the number of columns of the plot grid

par(mfrow=c(2,2),mar=c(2,2,1,1))
plot(mtcars$wt,mtcars$mpg, pch='%', col='dodgerblue')
plot(mtcars$wt,mtcars$mpg, pch='e', col='#13fee0')
plot(mtcars$wt,mtcars$mpg, pch='^', col='firebrick')
plot(mtcars$wt,mtcars$mpg, pch='4', col=adjustcolor('black',alpha.f=0.4))
dev.off()

#############################
####    Saving graphs    ####
#############################
 
# You can save graphs directly from your device (in RStudio, you would click on the 'export' button)
# However, this can be annoying if you have to go through the process every time you make a small adjustment to your graph
# Also, there is no guarantee of conistency if you save your graphs manually
# Generally, it is better to save your plots using code
# To do this, you must open a graphical device directly in a file
# There are multiple functions for this, see a list here: https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/Devices.html

# Example: to save a graph as a .png
# First open your png device, provide the dimensions, and name the file:
png(file="my.example.graph.png",
    width=600, height=450)

# then put all of the code for your graph
plot(mtcars$wt,mtcars$mpg)

# When you are done, close the device with
dev.off()




#########################
####    EXERCISES    ####
#########################






