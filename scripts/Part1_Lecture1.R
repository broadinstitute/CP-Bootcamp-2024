
# this is our first comment on our first script.

# ----
# pay attention to the folding icon appears when you put four dashes in a comment
# also check out https://support.posit.co/hc/en-us/articles/205753617-Code-Diagnostics

# we like to start with a header: 


# ----
# CP BOOTCAMP 2024 : INTRODUCTION TO R PROGRAMMING - LECTURE 1
# Author: Mustafa A. Kocak
# Date: March 4, 2024
# Description: A very first encounter with R and R studio ide.
# ----

# ----
# Introduction
# ----

# we can perform arithmetic operations
3 + 4
3^42


# commands can expand to multiple lines (though they better not)
2 * 
  3 


# you can enter multiple commands in a single line (though you better not)
3 + 4 ;   5 * 4


# R follows standard mathematical prioritizaition, but use parantheses to be on the safe side
6 / 2^2 - 1
(6 / 2)^2- 1
6 / (2^2- 1)


# here is our first vector
1:6

# here is a longer one - check out the output
150:233


# here is an object with a value assigned to it
a <- 1 

# objects stands for their values
a + 3

# let's create a die (check out the "Environment" panel)
die <- 1:6

# variables can take almost (!) any names and they are case-sensitive - see the red marks on the left! 
namesarecasesensitive <- 3
Namesarecasesensitive <- 4
namesarecasesensitive
Namesarecasesensitive

1theyCannotStartWithANumber <- 45
@noSpecialCharacters <- 22
under_scores_or.dots.are.okay <- 35

# don't use special words as variable names - see the highlighted colors
NA <- 3
NULL <- 3
TRUE <- 5

# you can list objects on your environment 
ls()

# elementwise operations
die 
die - 1
die * 2
die * die

# inner product
die %*% die
# outer product
die %o% die

# recycling 
1:2
die * 1:2
die + 1:2
die + 1:4 # warning!

# ----
# Functions
# ----

# R comes with very many essential functions
round(3.1415)
factorial(6)
exp(2)


# any R object can be an argument to a function
mean(1:6)
mean(die)


# we can use them cascade
round(mean(die))
round(exp(2)) 
# equivalent to x = exp(1); round(x) 
# we will learn an alternative notation in couple of minutes :) 


# functions can have multiple arguments
sample(x = 1:5, size = 3)
sample(x = 1:5, size = 3, replace = TRUE) # sampling with replacement


# if names are not provided, the order or arguments becomes crucial
sample(1:5, 3)
sample(3, 2:5) # not the intended outcome!  


# you can check the list of arguments for a function:
args(sample)

# you can use "tab" to get a quick reminder, try pressing tab after "," below.
# TAKE ADVANTAGE OF R's AUTO-COMPLETE AS MUCH AS YOU CAN!
round(exp(1), digits = 3)

# better though, you can ask for a help page:
?sample()


# let's roll our die 
sample(x = die, size = 1)
sample(x = die, size = 1)
sample(x = die, size = 1)

# what about rolling a pair of dice
sample(x = die, size = 2, replace = TRUE)

# note functions are also R objects, if we call them without parantheses we see what is inside them
sample
# next we will learn how to write our own functions! 

# function constructor
my_function <- function(){
  
}

# let's write a function that rolls a pair of dice and returns the sum of them:
roll <- function(){
  die = 1:6
  dice = sample(x = die, size = 2, replace = 2)
  sum(dice)
}

roll()
roll()
roll



# functions should have access to the object they will be using:
roll2 <- function(){
  dice = sample(x = bones, size = 2, replace = 2)
  sum(dice)
}

roll2()


# we can provide them as arguments
roll2 <- function(bones){
  dice = sample(x = bones, size = 2, replace = 2)
  sum(dice)
}

roll2()
roll2(1:6)
roll2(1:20)




# a neat trick to write a function is using extract function of RStudio
# select the code block below and click Code > Extract function from the menu.

die = 1:6
dice = sample(die, 2, TRUE)
sum(dice)


dice = sample(x = bones, size = 2, replace = 2)
sum(dice)

# always double-check automatically generated code!! 



# we can assign default values to the arguments
roll2 <- function(bones = 1:6){
  dice = sample(x = bones, size = 2, replace = 2)
  sum(dice)
}



# ----
# Packages 
# ----

# packages are bundled R code written by others and published 
# here are some ways of install them:

# default are installed from CRAN: https://cran.r-project.org/web/packages/useful/index.html
# check for reference manual and vignettes
install.packages(useful)

# many useful packages are living on bioconductor
# for example: https://www.bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

# more tailored / special use / in-development packages are living on github repos:
# https://github.com/broadinstitute/taigr   
# please note to be able to use taiga you need to create a token following the 
# instructions at https://github.com/broadinstitute/taigapy
devtools::install_github("https://github.com/broadinstitute/taigr")


# check the packages panel to see what packages you have installed on your system.

# let's try to use functions in the packages we have
# qplot is a function living in ggplot2

qplot() # not working yet! 

ggplot2::qplot() # worked because we told R where to look, though not very useful yet :)

library(ggplot2) # load whole library
qplot() # now no need to mention the library name, but it is good practice to include it anyway! 
qplot



# let's try to do something more meaningful

# c (short for concatenate) is one of the most common functions you will use
x <- c(-1, -0.8, 0.5, 0.3 , -0.2, -.6, -.1 ,1)
x

y <- x^3
y

qplot(x,y)

# more points:
x <- runif(n = 1000, min = -1, max = 1)
y <- x^3 
qplot(x,y)


# if we don't give a y value qplot generates histograms
qplot(x = y, binwidth = .1)


# now we know how to simulate dice's and how to create histograms
# let's roll a dice many times and look at its histogram

# let's take a closer look at the help page for "replicate"
?replicate

fair.dice <- replicate(n = 1e5, expr = roll())
qplot(fair.dice, binwidth = .5)


# note we can cancel a command with the short-cut ctrl + c (cancel)  or 
# clicking the "stop" button on the top of the console.
replicate(n = 1e7, expr = roll())

# let's write a function to roll a dice with un-equal probabilities

roll.loaded <- function() {
  die = 1:6
  dice = sample(x = die, size = 2, replace = TRUE, prob = c(1/7, 1/7, 1/7, 1/7, 1/7, 2/7))
  sum(dice)
}

roll.loaded()

loaded.dice <- replicate(n = 1e5, expr = roll.loaded())
qplot(loaded.dice, binwidth = 0.5)

fig1 <- qplot(fair.dice, binwidth = .5)
fig2 <- qplot(loaded.dice, binwidth = 0.5)

fig1
fig2

# install.packages("cowplot")
cowplot::plot_grid(fig1, fig2, ncol =2, nrow = 1)

# let's finish with a small digression
# for a better and more complete explanation : https://r4ds.had.co.nz/pipes.html
library(magrittr)

x = 2:7
x1 = x + 2
x2 = exp(x1)
x3 = x2 * 2
x4 = log(x3)
x5 = round(x4, 1)
x5 

x5 <- round(log(exp(x + 2) * 2), 1)
x5

x5 <- x %>% 
  add(2) %>%
  exp() %>% 
  multiply_by(2) %>%
  log() %>%
  round(1)
x5


# ----
# BREAK
# ----
# R Objects
# ----

# atomic vectors 
die <- c(1,2,3,4,5,6)
length(die)
is.vector(die)


five <- 5 
is.vector(five)
length(five)

# there are 5 basic types: doubles, integers, characters, logicals, complex, raw
# you will mostly need only doubles, characters, and logicals

typeof(die)
typeof(five)

five <- 5L
typeof(five)
sqrt(2)^2 - 2

logicals <- c(TRUE, FALSE, T, F, F, T)
logicals
typeof(logicals)


text <- c("Hello", "World")
text    # note the quotes!  
typeof(text)


# all the elements of a vector should be the same and one of the basic types, 
# if not R "coerce" them into a single type

logicals
int <- c(1L, 5L)
typeof(int)

logicals %>% typeof()

logicals %>%
  c(int) %>%
  typeof()

logicals %>%
  c(int)%>%
  c(die) %>% 
  c(text) %>% 
  typeof()


# we can enforce type coercion manually as well
as.numeric(TRUE)
as.character(TRUE)
as.logical(c(12, 1, -1 , 0))
as.logical("Deneme")

# complex and raw types
comp <- c(1 + 1i, 1 + 2i, -1 + 3i)
comp
typeof(comp)

raw(3)
typeof(raw(3))


# royal flush
hand <- c("ace", "king", "queen", "jack", "ten")
typeof(hand)


# matrices are two dimensional vectors 
# check ?matrix for details
m <- matrix(die, nrow = 2)
m

m <- matrix(die, nrow = 2, byrow = TRUE)
m

# arrays are generalized matrices to more than two dimensions
ar <- array(c(11:14, 21:24, 31:34), dim = c(2, 2, 3))
ar


# we can attach extra information to R objects 
attributes(die)
attributes(m)

# a common attribute for vectors is names
names(die) <- c("one", "two", "three", "four", "five", "six")
die
attributes(die)

# forcing dimensions to vector turns it into a matrix
dim(die) <- c(2,3)
die
attributes(die)
typeof(die)

# for matrices rownames and colnames are natural attributes
rownames(die) <- c("row1", "row2")
colnames(die) <- c("col1", "col2", "col3")
die
attributes(die)

# you can remove a attribute by assigning NULL to it
colnames(die) <- NULL
die


# while changing the dimensions of an object/container, we don't change its type
# but we change its class
die = 1:6
class(die)
dim(die) <- c(2,3)
class(die)
dim(die) <- c(1,2,3)
class(die)

class("hello")
class(15)



# we use lists to group arbitrary objects 
list1 <- list(100:130, "R", list(TRUE, FALSE))
list1

names(list1) <- c("numeric", "text", "logical")
list1

list1 <- list("numeric" = 100:130, "text" = "R", "logical" = list(TRUE, FALSE))
list1


# data frames are 2 dimensional version of lists
# a data frame is a list of vectors of the same length.
# check ?data.frame for details
df <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3))
df

# if any of the vectors is a constant, it will be recycled
data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1))

typeof(df)
class(df)
str(df)


# let's create a deck of cards
# please check ?rep and ?seq
faces = c("king", "queen", "jack", "ten", "nine",
         "eight", "seven", "six", "five", "four",
         "three", "two", "ace")
values = seq(13,1,-1)
suits = c("spades", "clubs", "diamonds", "hearts")

deck <- data.frame( suit = rep(suits, each = 13),
                    face = rep(faces, times = 4),
                   value = rep(values, times = 4))



# to save/load a data.frame
write.csv(x = deck, file = "data/deck.csv")
deck.read <- read.csv(file = "data/deck.csv")
head(deck)

write.csv(x = deck, file = "data/deck.csv", row.names = FALSE)
deck.read <- read.csv(file = "data/deck.csv")
head(deck.read)


# save and load R objects while keeping them intact
bundle <- list(deck = deck, roll = roll)
replicate(n = 10, bundle$roll())

saveRDS(object = bundle, file = "data/bundle.RDS")
bundle.read <- readRDS(file = "data/bundle.RDS")

bundle.read

# Mustafa's suggestion to read csv's 
deck.read <- data.table::fread("data/deck.csv")
deck.read


# reading a matrix with fread is trickier, but let's revisit it later :) 
random.matrix <- matrix(rnorm(25*15), 25,15, 
                        dimnames = list(paste0("r", 1:25),
                                        paste0("c", 1:15)))

random.matrix %>% 
  useful::corner()

write.csv(x = random.matrix, 
          file = "data/random_matrix.csv")

random.matrix.read <- data.table::fread("data/random_matrix.csv")

random.matrix.read %>%
  useful::corner()

random.matrix.read <- data.table::fread("data/random_matrix.csv") %>% 
  tibble::column_to_rownames("V1") %>%
  as.matrix() 

random.matrix.read %>% 
  useful::corner() 


# finally we can also import and export using the IDE

# ----
# Slicing datasets
# ----

# selecting values / slicing dataset
# potential values for indexes: 
# positive integers, negative integers, zero, blank spaces,
# logical values, names

dim(deck)
head(deck)
tail(deck)
nrow(deck)


deck[1,1]
deck[1, 1:3]
deck[1:5, 2:3]

# you can repeat values
deck[c(1,1), 1:3]

# indexing starts with 1
vec <- c(6, 1, 3, 6, 10, 5)
vec[1:3]

# drop = FALSE preserves the structure for single columns
deck[1:10,1]
deck[1:10,1, drop = F]

# negative numbers exclude the corresponding rows/columns
deck[-(4:52), 1:3]

# you cannot mix positive and negative numbers
deck[c(-1, 1), 1]

# blank values mean "every"
deck[1:4, ]
deck[,1:2]

# zero is a not a useful index
deck[0, 0]
deck[,0]
deck[0, ]

# logicals should be the same size of the dimension
deck[1:3, c(TRUE, TRUE, FALSE)]

deck[deck$suit == "spades", ]


# you can access columns (or rows when you have rownames) 
# by name as well
deck[1, c("face", "suit", "value")]
deck[ , "value", drop = FALSE]


# with dolar sign you can access to the individual elements of lists by name
lst <- list(numbers = c(1, 2), logical = TRUE, strings = c("a", "b", "c"))
lst

lst$numbers
lst$logical
lst$strings

# remember data frames are lists of vectors
deck$value
mean(deck$value)
median(deck$value)

deck$suit
unique(deck$suit)
table(deck$suit)

# this is tricky! a slice of a list is a list
lst[1]  # this is same as lst["numbers"]
lst[1] %>% class()
lst[1] %>% mean()

# but we can "strip off" each layer of a list with double brackets
lst[[1]] # this is same as lst[["numbers"]] or lst$numbers
lst[[1]] %>% class()
lst[[1]] %>% mean()

lst[[c(1,2)]]

# ----
# Modifying values
# ----

# a fresh copy of the deck
deck2 <- deck

# we can modify individual values in-place
vec <- c(0, 0, 0, 0, 0, 0)
vec
vec[1]
vec[1] <- 1000
vec
vec[c(1, 3, 5)] <- c(1, 1, 1)
vec
vec[4:6] <- vec[4:6] + 1
vec

# a new value (making the vector longer)
vec[7] <- 0
vec

# a new column to a dataframe
deck2$new <- 1:52
head(deck2)

# we can remove a column by assigning NULL
deck2$new <- NULL
head(deck2)

# let's change the value of aces to 14
# we will learn a much more elegant way of doing this in a few days
deck2[c(13, 26, 39, 52), ] # aces
deck2[c(13, 26, 39, 52), 3] # values of aces 
deck2$value[c(13, 26, 39, 52)] # same as above
deck2$value[c(13, 26, 39, 52)] <- c(14, 14, 14, 14) # assigning higher values
deck2$value[c(13, 26, 39, 52)] <- 14 # same as above - remember recycling
head(deck2, 13)


# let's get a shuffled deck
deck3 <- deck[sample(52),]
head(deck3)

# we can create logical values using logical comparisons
1 > 2
1 > c(0, 1, 2)
c(1, 2, 3) == c(3, 2, 1)

1 %in% c(3, 4, 5)
c(1, 2) %in% c(3, 4, 5)
c(1, 2, 3) %in% c(3, 4, 5)
c(1, 2, 3, 4) %in% c(3, 4, 5)

# how many aces we have in our deck
sum(deck3$face == "ace")

# where are they
deck3$face == "ace"

# what are their values
deck3$value[deck3$face == "ace"]

# bump them into 14
deck3$value[deck3$face == "ace"] <- 14
head(deck3)
deck3[deck3$face == "ace",]


# in the game of hearts, all cards have value 0, except hearts (value of 1) and 
# queen of spades (values of 13)
deck4 <- deck
deck4$value <- 0
head(deck4, 13)

deck4$value[deck4$suit == "hearts"] <- 1

# we can combine conditionals through boolean operators
queenOfSpades <- deck4$face == "queen" & deck4$suit == "spades"
deck4$value[queenOfSpades] <- 13

deck4


# exercises with boolean operators 
a <- c(1, 2, 3)
b <- c(1, 2, 3)
c <- c(1, 2, 4)

a == b
b == c
a == b & b == c


# write code to represent the following questions
# is w positive?
# is x greater than 10 and less than 20?
# is object y the word February?
# is every value in z a day of the week?
w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")


# let's prepare a deck for blackjack, where each number card has a value equal to 
# its face value. Each face card (king, queen, or jack) has a value of 10. Finally,
# each ace has a value of 11 or 1, depending on the final results of the game.
deck5 <- deck
facecard <- deck5$face %in% c("king", "queen", "jack")
deck5$value[facecard] <- 10


# NA (not available) is a protected word in R and used pretty commonly
# it is also contigious!
1 + NA
NA == 1
c(NA, 1:50)
mean(c(NA, 1:50))


# sometimes we use NA's to signal ambiguity
deck5$value[deck5$face == "ace"] <- NA
deck5


# we typically detect NA's with is.na(),  ignore/remove them using na.rm = TRUE 
is.na(c(NA, 1:50))
mean(c(NA, 1:50), na.rm = TRUE)


# let's finish with writing functions to shuffle and deal cards.
deal <- function(deck, n_card = 4) {
  shuffled_deck <- deck[sample(nrow(deck)),]
  list(hand = shuffled_deck[1:n_card, ], 
       rest = shuffled_deck[(n_card+1):nrow(deck), ])
}

result <- deal(deck)
hand1 <- result$hand
result <- deal(result$rest)
hand2 <- result$hand
result <- deal(result$rest)
hand3 <- result$hand
result <- deal(result$rest)
hand4 <- result$hand
rest <- result$rest

hand1
hand2
hand3
hand4
rest

