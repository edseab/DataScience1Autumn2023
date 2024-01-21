###################################
###################################
########                   ########
########   Data Science 1  ########
########       Lab 5       ######## 
########  07th Nov. 2023   ########
########                   ########
###################################
###################################

## Welcome to lab #5. Today will be an introduction to the tidyverse, specifically dplyr and ggplot2




#####################
####    Pipes    ####
#####################

# The pipe operator looks like this: |>
# It signifies: take the element on the left and use it as the first argument in the function on the right

# Example

set.seed(123)
hist(rnorm(200), breaks=seq(-4,4,0.5))
# is equivalent to

set.seed(123)
rnorm(200) |> hist(breaks=seq(-4,4,0.5))

# The purpose of the pipe is mostly aesthetic, in particular to avoid large numbers of parentheses when something needs to be transformed using multiple functions:



### 1.1 Rewrite the following expression using pipes:
set.seed(123)
round(sqrt(log(runif(10,1,10))),2)

set.seed(123)
10 |> runif(1,10) |> 
      log() |> 
      sqrt() |> 
      round(2) -> object_name
              
# Pipes were initially created in a package called magrittr, part of the 'tidyverse' group of packages

########################
####    IF/ELSE     ####
########################

x <- sample(0:10, 1)
if (x>5){
     print('HIGH')
     y <- x*2
     print(c(x,y))
}else{
    print("LOW")
}

ifelse(sample(0:10,1)>5, print('HIGH'), print('LOW'))

########################
####    Packages    ####
########################

# Packages are essentially environments full of functions and/or objects which aren't loaded into R by default, but must be first installed (generally from a repository called CRAN), and then loaded

# Here we will install tidyverse, which is not one but a series of packages written by the same group of people, which share a number of syntactical features

install.packages('tidyverse')

# After you have installed tidyverse into your packages, you must also load it into your environment

library(tidyverse)

# You will only have to install a package once. However, every time you start fresh in a new R environment, you will have to load the packages you need again.

#########################
####    TIDYVERSE    ####
#########################

# The tidyverse encompasses a very large number of packages, and is fairly powerful (in that you can do a lot using tidyverse packages)
# However, you DO NOT need to use tidyverse exclusively! I mostly don't use it, for example, preferring to stick to base R except for a few functionalities.

# Downsides of tidyverse include: unique syntax which is very different from other programming languages such as python
# 'Black box' functions that have specific purposes that must be remembered, as opposed to more general-purpose functions in base R
# Often actually more verbose than base R (ie. requires more lines of code to do the same thing)
# Using tidyverse is a matter of preference!

# The pipe operator was originally introduced in the tidyverse package magrittr. 
# The tidyverse pipe operator looks like this: %>%

set.seed(123)
runif(10,1,10) %>% 
  log() %>%
  sqrt() %>%
  round(2)

# In this class we will look at 2 important tidyverse packages.
# The first is dplyr, which is used to manipulate and transform data.
# Then we will start learning ggplot2, which is used for plotting and data visualisation

#####################
####    dplyr    ####
#####################

# The philosophy behind dplyr is that every data transformation can be done using a function, and multiple transformations can be done by piping an object through multiple functions
# For example, in dplyr we no longer use indexing.
# Instead we use the functions 'select', to choose specific columns in a data frame, and 'filter', to choose specific rows.

data(mtcars)

# so instead of:
mtcars[mtcars$cyl == 6, 1:5]
# we would write:
mtcars %>%
  filter(cyl==6) %>%
  select(1:5)

### 2.1
# using select() and filter(), create a new database of cars that are over 4000 lbs in weight, retaining only the wt and mpg columns. Save this database to an object called 'df'.

mtcars %>%
  filter(wt>4) %>%
  select('wt','mpg') -> df

# After you have selected the rows and columns you are interested in, you can 
# change the order of the rows using arrange

df %>% arrange(wt)

df %>% arrange(desc(wt))


# To change variables, we can use mutate()
df <- df %>% mutate(wt_kg=wt*453.592,
                    km_per_l = mpg*1.60934/3.78541)

# And we can use ifelse() within mutate()
mtcars <- mtcars %>%
            mutate(wt_class = ifelse(wt>=4, 'Oversized','Standard'))

# We can even do a sultiple ifelse statment using case_when()
mtcars <- mtcars %>%
            mutate(
              efficiency = case_when(
                mpg<=15 ~ 'low',
                mpg>15 & mpg<=20 ~ 'medium',
                mpg>20 ~ 'high'   
                ))

# Next, summarise (or summarize) is a useful function which collapses a dataframe into a single row and can calculate summary statistics, eg:

mtcars %>% 
  summarise(
    mean_wt = mean(wt),
    sd_wt = sd(wt),
    n = n()
  ) -> summary_table

# We can also use summarise to collapse a data frame not into one single row, but into as many rows as we have groups of interest. 
# To do this, first we need to use group_by()

mtcars %>% group_by(efficiency)

# You'll notice that this automatically changes the data frame into a new kind of object, called a tibble.
# Tibbles are basically tidyverse dataframes, that display information slightly differently, and are a bit more particular about certain things like not wanting empty cells.
# Tibbles can also be grouped, which allows for further operations down the line
# For example:

mtcars %>% 
  group_by(efficiency) %>%
  summarise(
    avg_wt=mean(wt),
    n=n()
  ) %>%
  arrange(
    c('high','medium','low')
  ) %>%
  ungroup()-> summary_table_efficiency  



# You can group by multiple variables

mtcars %>% 
  group_by(efficiency,cyl) %>%
  summarise(
    avg_wt=mean(wt),
    n=n()
  ) %>% View()

# After grouping a tibble, remember to ungroup it later using ungroup(), or you may have issues down the line.

# 3.1
data(iris)
# using the dplyr functions do the following:
# create a new column called Petal.Area which is the product of the petal width and petal length columns.
# For each of the different species of iris, present the mean and standard deviation for the sepal length, sepal width, and petal area, as well as the number of samples (n)
# Order this database in decreasing order of average petal area.

iris %>% mutate(Petal.Area = Petal.Length*Petal.Width) %>%
group_by(Species) %>% 
summarise(mn_sepal_length = mean(Sepal.Length),
          sd_sepal_length = sd(Sepal.Length),
          mn_sepal_width = mean(Sepal.Width),
          sd_sepal_width = sd(Sepal.Width),mn_petal_area = mean(Petal.Area),
          sd_petal_area = sd(Petal.Area),
          n = n()) %>%
          arrange(desc(mn_petal_area)) -> final_iris_db

#######################
####    ggplot2    ####
#######################

# ggplot2 is the tidyverse graphics package, and one of the most popular data visualization tools that currently exists

# ggplot2 syntax is based on the so-called 'Grammar of Graphics', where different elements of a graph are added sequentially in layers

# The gold standard resource for learning ggplot2 is Hadley Wickham's 'ggplot2: Elegant graphics for data analysis'
# which is available for FREE here: https://ggplot2-book.org/

# I can provide no better introduction to ggplot2 than the 'First Steps' section of this book. 

# You can also find a cheat sheet here: https://rstudio.github.io/cheatsheets/html/data-visualization.html

data(mpg)


# MIDTERM revisions:

# summary statistics

# Probabilities and distributions

# Null hypothesis significance tests - specifically t-tests

# P-values and confidence intervals

# Data standardization (scaling, centering, z-scoring)

# Data manipulation and visualisation


