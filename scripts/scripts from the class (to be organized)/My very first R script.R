
# -----
# MY VERY FIRST INTRODUCTION TO R 
# Author: Mustafa Anil Kocak
# Date: March 4 2024
# Description: First lecture notes from the CP Bootcamp
# ----

# Introduction ----

3  + 4 
3 ^42


3 + 
  4

3 + 5 

3 + 5 ;  12 - 8


3

6 / 2^2 -1
(6 / 2)^2 - 1

# This is my first vector: 
1:6 


# HEre is a longer vector: 
23:59


# R Objects ----
# My first R object
a <- 3 

a + 1 

die <- 1:6


# Naming objects:
namesAreCaseSensitive <- 3
namesAreCaseSENSITIVE <- 5
namesAreCaseSENSITIVE = 5

# They cannot start with a number or a special character:
1object <- 100
@noSpecialCharacters <- 200

underscores_or.dots.are.okay <- 10

namesAreCaseSensitive  / 2 


ls()

NA <- 3 
NULL <- 5
TRUE <- 10

# elementwise opearations 
die 
die  - 1 
doubled_die <- die * 2 
doubled_die

die * die
die * doubled_die

die %*% die
die %o% die

die
rep(1/6, 6)
probability = rep(1/6, 6)
sum(die * probability)
die %*% probability

c(1,2,1,2,1,2)
die * 1:2
die * c(1,2,1,2,1,2)

die %o% c(1,2)

die * 1:4

# Functions ----

round(3.5)

rnorm(1)
factorial(5)
exp(2)
log(100)
log2(16)
log10(120)

rnorm(n = 10, mean = 0, sd = 3)
mean(1:10)
mean

round(mean(1:10))
round(exp(2))

a <- exp(2)
rounded_a <- round(a)


sample(x = die,size = 2, replace = TRUE)
sample(die, 2, TRUE)
sample(size = 2, x = die, replace = TRUE)

sample(size = 3)
sample(x = die, size = 7)

?sample()

sample(x = 6,size = 2, replace = TRUE)

sample


??sample

roll2 <- function(){
  dice <- sample(x = 1:6, size = 2, replace = TRUE)
  return(sum(dice))
}

roll2()
roll2()
roll2()


die

roll3 <- function(die = 1:6){
  dice <- sample(x = die, size = 2, replace = TRUE)
  return(sum(dice))
}

roll3()


die 

roll4 <- function(die) {
  dice <- sample(x = die, size = 2, replace = TRUE)
  sum(dice)
}

# Packages ----

?install.packages()


install.packages("useful")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ComplexHeatmap")


library(ComplexHeatmap)
library(tidyverse)

ComplexHeatmap::Heatmap()

Heatmap()


qplot()

x <- c(-1, 0.8, 0.5, 3, -2, 10, 1.1, 5)
x <- sort(x)
y <- x^3
y
plot(x,y, type = "b")

?base::sort
?base::plot

qplot(x,y)

qplot(y)

# BREAK -----

# write a function that rolls a pair of dice and reports their sum

roll_fair <- function(){
  dice <- sample(x = 1:6, size = 2, replace = TRUE)
  return(sum(dice))
}




# write a function that rolls a pair of loaded dice that are twice more likely
# to come 5 or 6 than any other numbers and returns their sum

roll_loaded <- function(){
  #dice <- sample(x = c(1:6, 5,6), size = 2, replace = TRUE)
  dice <- sample(x = 1:6, size = 2, replace = TRUE, prob = c(1,1,1,1,2,2))
  return(sum(dice))
}


rep(1, 10)

roll_fair()
roll_loaded()

fair_sums <- replicate(1000, roll_fair())
loaded_sums <- replicate(1000, roll_loaded())

fair_sums

plot(loaded_sums)
hist(loaded_sums, 12)
hist(fair_sums, 12)


fig1 <- ggplot2::qplot(fair_sums)
fig2 <- ggplot2::qplot(loaded_sums)

fig1

install.packages("cowplot")

cowplot::plot_grid(fig1, fig2,
                   nrow = 1)


roll_many(n ) # --> roll n pairs of dice and return the sums

roll_many <- function(n){
  
  replicate(n, roll_fair())
}



roll_many <- function(n){
  die1 <- sample(x = 1:6, size = n, replace = TRUE)
  die2 <- sample(x = 1:6, size = n, replace = TRUE)
  return(die1 + die2)
}

qplot(roll_many(1e4))


library(magrittr)
 
x <- 3
y <- exp(x)
z <- sqrt(y)
t <- log10(z)
s <- abs(t)

s <- abs(log10(sqrt(exp(3))))

s <- x %>% 
  exp() %>%
  sqrt() %>%
  log10() %>% 
  abs()

substraction <- function(x, y){
  return(x - y)
}

x <- 3

x %>% 
  substraction(1)

x %>% 
  substraction(1, .)

x %>% f()


# R OBJECTS -----

# atomic vectors 
die = 1:6
is.vector(die) 
length(die) # will use very common

five <- 5
is.vector(five)
length(five)

near(sqrt(2)^2, 2)


typeof(die)
typeof(five)
sqrt(2)^2 - 2 
sqrt(4)^2 - 4


logicals <- c(TRUE, FALSE, T , F , F , F)
logicals
typeof(logicals)

text <- c("Hello", "World")  
length(text)
typeof(text)


five <- 5L
typeof(five)

# Coercion ---
logicals
int <- c(1L, 5L)

c(logicals, int)

logicals %>% 
  c(int)

as.logical(int)

logicals %>% 
  c(int) %>%
  c(die) %>% 
  c(text) %>% 
  typeof()


logicals %>% 
  c(int) %>%
  c(die) %>% 
  c(text) 


as.numeric("5e4")
as.logical()
as.character()










