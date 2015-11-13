# R Programming, Quiz 3 Answers

library(datasets)

# Question 1
# Take a look at the 'iris' dataset that comes with R. The data can be 
# loaded with the code:
#   
#   library(datasets)
#   data(iris)
# 
# A description of the dataset can be found by running
# 
# ?iris
# 
# There will be an object called 'iris' in your workspace. In this dataset, 
# what is the mean of 'Sepal.Length' for the species virginica?

data(iris)
?iris
virginica <- subset(iris, Species == 'virginica')
answer1 <- mean(virginica$Sepal.Length)

# Question 2
# Continuing with the 'iris' dataset from the previous Question, what R 
# code returns a vector of the means of the variables 'Sepal.Length', 
# 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?

answer2 <- 'apply(iris[, 1:4], 2, mean)'

# Load the 'mtcars' dataset in R with the following code
# 
# library(datasets)
# data(mtcars)
# 
# There will be an object names 'mtcars' in your workspace. You can find 
# some information about the dataset by running
# 
# ?mtcars
#
# How can one calculate the average miles per gallon (mpg) by number of 
# cylinders in the car (cyl)?

data(mtcars)
?mtcars
answer3 <- 'tapply(mtcars$mpg, mtcars$cyl, mean)'

# Question 4
# Continuing with the 'mtcars' dataset from the previous Question, what is 
# the absolute difference between the average horsepower of 4-cylinder cars 
# and the average horsepower of 8-cylinder cars?

hpByCyls <- tapply(mtcars$hp, mtcars$cyl, mean)
answer4 <- abs(hpByCyls[[1]] - hpByCyls[[3]])

# Question 5
# If you run
# 
# debug(ls)
# 
# what happens when you next call the 'ls' function?

answer5 <- "Execution of 'ls' will suspend at the beginning of the 
            function and you will be in the browser."



