
# ----
# CP Bootcamp 2024 - Day 2 
# Mustafa Kocak
# 05/03/2024
# We are continuing with Base R
# ----

# Load the necessary libraries ---

library(useful)
library(tidyverse)

# Rest of the class -----



# a royal flush
hand <- c("ace", "king", "queen", "jack", "ten")
typeof(hand)
length(hand)

# matrices are two dimensional vectors

?matrix()
die = 1:6
matrix(data = die)
m <- matrix(data = die, nrow = 2)
matrix(die, ncol =2)
matrix(die, ncol = 3, byrow = TRUE)

matrix(c(die,1), nrow = 2)

ar <- array(c(11:14, 21:24, 31:34), 
      dim = c(2,2,3))

ar

attributes(die)
attributes(m)

names(die) <- c("one", "two", "three", "four", "five", "six")
attributes(die)
die
typeof(die)

dim(die) <- c(2,3)
die

attributes(die)
rownames(die) <- c("r1", "r2")
colnames(die) <- c("c1", "c2", "c3")
die

attributes(die)

colnames(die) <- NULL
die


class(die)
die <- 1:6
class(die)



# we use lists to group arbitrary objects

list1 <- list(1:10, 3, "five", mean, matrix(1:12, 3))
list1

names(list1) <- c("vector", "numeric", "character", 
                  "func", "matrix")
list1

list2 <- list("vector" = 1:10, 
              "numeric" = 3,
              "character" = "five", 
              "func" = mean, 
              "matrix" = matrix(1:12, 3))
list2

# a data frame is a list of vectors of the same length.
df <- data.frame(face = c("ace", "two", "six"),
           suit = c("clubs", "clubs", "clubs"),
           value = c(1,2,3))

df

# if any o the entries is a constant, it will be recycled
data.frame(face = c("ace", "two", "six"),
                 suit = c("clubs", "clubs", "clubs"),
                 value = 1)

typeof(df)
class(df)
str(df)

?base::rep


rep(c("a", "b","c"), each = 2)

data.frame(let = rep(c("a", "b", "c"), 6),
           num = rep(c(1,2,3,4,5,6), 3))

faces <- c("king", "queen", "jack", "ten", "nine",
           "eigth", "seven", "six", "five", "four",
           "three", "two", "ace")
suits <- c("spades", "clubs", "diamonds", "hearts")
values <- seq(13,1, -1)

deck <- data.frame(suit = rep(suits, each = 13) ,
                   face = rep(faces, times = 4),
                   value = rep(values, times = 4))

deck

# how to save and load a data.frame
write.csv(x = deck, file = "Data/deck.csv")
deck.read <- read.csv("Data/deck.csv")
deck.read

write.csv(x = deck, file = "Data/deck.csv", row.names = FALSE)
deck.read <- read.csv("Data/deck.csv")
deck.read

head(deck, n = 10)
tail(deck)
summary(deck)
glimpse(deck)
str(deck)

list1
saveRDS(list1, file = "Data/list.RDS")
list1.read <- readRDS("Data/list.RDS")
list1.read

deck.read <- data.table::fread("Data/deck.csv")
deck.read

random_matrix <- matrix(rnorm(3000), 50)
dim(random_matrix)
head(random_matrix)


corner(random_matrix)

write.csv(random_matrix,
          "Data/random_matrix.csv",
          row.names = FALSE)


random_matrix.read <- read.csv("Data/random_matrix.csv") %>%
  as.matrix()

random_matrix.read %>% 
  corner()

random_matrix.read <- data.table::fread("Data/random_matrix.csv") %>% 
  as.matrix() 

# Dicing and selecting datasets -----

dim(deck)
head(deck)
nrow(deck)

deck[1,1]
deck[1,1:3]
deck[1:5, 2:3]

deck[,0]

deck[1:5,-3]
deck[1:4, c(-1,3)] # this doesn't work

deck[1:4,c("suit", "value")]

names(die) <- c("one", "two","three", "four", "five", "six")
die
die["three"]


deck[1:4, c(TRUE, FALSE, TRUE)]

deck[1:5,2, drop = FALSE]

deck$suit
deck$face
deck[deck$suit == "spades" , ]


list1$vector
list1$character

list2 <- list1[1]
list2 %>% class()
length(list2)

vector2 <- list1[[1]]
vector2



list1[1:2]
list1[-1]


deck$value %>% 
  median()

deck$suit %>% 
  unique()

suit.table <- deck$suit %>% 
  table()

suit.table["clubs"]

sum(deck$suit == "clubs")
mean(deck$suit == "clubs")

# Modifying values -----

# a fresh copy of the deck
deck2 <- deck

vec <- rep(0, 6)
vec
vec[1]
vec[1] <- 1000
vec
vec[c(1,3,5)] <- c(1,2,1)
vec
vec[4:6] <- vec[4:6] + 1

vec[1:3] <- vec[1:3] + vec[4:6]

vec[7] <- 0
vec
vec[7] <- NA
vec 
vec <- vec[-7]
vec

deck2 %>% 
  head()

deck2$new <- 1:52

deck2 %>% 
  head()

deck2$new <- NULL

deck2 %>% 
  head()

# let's bump the values of aces to 14
# where are the aces ? 
deck2[c(13,26,39,52), 3] <- c(14,14,14,14)
deck2
# doing the same thing
deck2[c(13,26,39,52), 3] <- 14
deck2$value[c(13,26,39,52)] <- 14
deck2[c(13,26,39,52), ]$value <- 14
deck2[c(13,26,39,52), "value"] <- 14

sample(6)



# shuffling the deck
deck3 <- deck[sample(52), ]
deck3

deck3[deck3$face == "ace", 3] <- 14


# some simple examples with booleans
1 > 2
2 > 1
1 > c(0, 1, 2)
c(1,2) > c(0,1,2)
c(1,2,3) == c(3,2,1)
all(c(1,2,3) == c(3,2,1))

1 %in% c(3,4,5)
c(1,2) %in% c(3,4,5) # no recycling! 
c(1,2,3) %in% c(3,4,5)
c(1,2,3,4) %in% c(3,4,5)

any(1 == c(3,4,5)) # equal to 1 %in% c(3,4,5)


deck4 <- deck[sample(52), ]

# where are the hearts
hearts <- deck4$suit == "hearts"
not_hearts <- deck4$suit != "hearts"
deck4[hearts, 3] <- 1
deck4[!hearts, 3] <- 0

deck4$value <- 0
deck4[hearts, 3] <- 1


hearts <- deck4$suit == "hearts"

hearts

deck4$value <- as.numeric(hearts)


queens <- deck4$face == "queen"
spades <- deck4$suit == "spades"
deck4[queens & spades,3] <- 13
deck4[queens & spades,3] <- 13


# NA's are contigious
1 + NA
NA == 1
c(NA, 1:50)
mean(c(NA, 1:50))

mean(c(NA, 1:50), na.rm = TRUE)

x <- c(NA, 1:50)

x[!is.na(x)]
x <- c(NA, 1:50, 1/0)
x

is.finite(x)


deck

deal <- function(deck, n = 5){
  n.deck <- nrow(deck)
  shuffled_deck <- deck[sample(n.deck), ]
  hand <- head(shuffled_deck, n)
  rest <- tail(shuffled_deck, n.deck-n)
  return(list(hand = hand, rest = rest))
}

current.deck <- deck

temp <- deal(current.deck)
hand1 <- temp$hand
current.deck <- temp$rest

temp <- deal(current.deck)
hand2 <- temp$hand
current.deck <- temp$rest

temp <- deal(current.deck)
hand3 <- temp$hand
current.deck <- temp$rest

temp <- deal(current.deck)
hand4 <- temp$hand
current.deck <- temp$rest

hand1

hand2

hand3

hand4




deal <- function(deck, n = 5){
  n.deck <- nrow(deck)
  if(n.deck >= n){
    shuffled_deck <- deck[sample(n.deck), ]
    hand <- head(shuffled_deck, n)
    rest <- tail(shuffled_deck, n.deck-n)
    return(list(hand = hand, rest = rest))
  } else{
    return(NULL)
  }
}




# Conditionals ----


num <- -2 

absolute_value <- function(num) {
  if(num < 0){
    num <- num * -1
  }
  num
}

absolute_value(4)
absolute_value(-4)

absolute_value <- function(num) {
  if(num < 0){
    print("Input is negative, don't worry I will fix it.")
    num <- num * -1
  }
  num
}

absolute_value(4)
absolute_value(-4)

# Quick quiz
x <- 1 
if( 3 == 3){
  x <- 2
}
x


x <- 1
if(TRUE){
  x <- 2
}
x



x <- 1
if(x == 1){
  x <- 2
  if(x == 1){
    x <- 3
  }
}
x 

# alternative path
a <- 3.14
dec <- a - trunc(a)
dec

if(dec >= 0.5){
  a <- trunc(a) + 1
} else{
  a <- trunc(a)
}



# multiple paths
a <- 20
b <- 20

if(a > b){
  print("A wins!")
}else if(a < b){
  print("B wins!")
}else{
  print("Tie.")
}

# write a function that takes one integer as input and 
# prints "Fizz" if it is a multiple of 3, prints "Buzz" if it is 
# a multiple of 5, and "FizzBuzz" if it is a multiple of 15. 
# If the input it none of those, print the input itself.
#
# Example input ---> output
# n = 45 ---> "FizzBuzz"
# n = 46 ---> "46"
# n = 48 ---> "Fizz"
# n = 50 ---> "Buzz"

# Remainder
47 %% 3

# Quotient
47 %/% 3


f <- function(n){
  if(n %% 15 == 0){
    print("FizzBuzz")
  }else if(n %% 3 == 0){
    print("Fizz")
  }else if(n %% 5 == 0){
    print("Buzz")
  }else{
    print(n)
  }
}


f(45)
f(46)
f(47)
f(48)
f(49)
f(50)

# Sometimes it is easier to use look-up table:

print_die <- function(x){
  if(x == 1){
    print("One")
  }else if(x ==2){
    print("Two")
  } ...
  else{
    print("Input is not between 1 and 6!")
  }
}


print_die <- function(x){
  die = c("one", "two", "three", "four", "five", "six")
  if(x %in% 1:6){
    print(die[x])
  }else{
    print("Input is not between 1 and 6!")
  }
}



# Pooled cell lines example:

cell_line_data <- data.frame(cell_line = paste0("c", 1:100), # cell line index
                             growth_rate = runif(100, 0.5, 5), # doubling per day
                             lag_period = rexp(100, 1), # in days
                             initial_cell_count = sample(c(25,50,100), 100, replace = TRUE, prob = c(0.25, 0.5, 0.25)))



cell_line_data$lag_period %>%
  sort() %>% 
  plot(type = "s")


assay_end_point <- 5
pcr_bottleneck <- 2e4
seq_depth <- 1e6
threshold <- 40 * seq_depth / pcr_bottleneck




simulate_experiment <- function(cell_line_data, 
                                assay_end_point = 5,
                                pcr_bottleneck = 2e4,
                                seq_depth = 1e6,
                                threshold = NULL){
  
  if(is.null(threshold)){
    threshold <- 40 * seq_depth / pcr_bottleneck
  }
  
  # calculate cell counts at the assay end-point
  lysate_cell_counts <- simulate_growth(cell_line_data, assay_end_point)
  
  # simulate pcr input (bottleneck)
  pcr_counts <- simulate_pcr(lysate_cell_counts, pcr_bottleneck)
  
  # simulate sequencing 
  sequencing_counts <- simulate_sequencing(pcr_counts, seq_depth)
  
  # count the detected lines
  # detected_lines <- count_detected(sequencing_counts, threshold)
  # detected_lines <- sum(sequencing_counts >= threshold)
  detected_lines <- cell_line_data$cell_line[sequencing_counts >= threshold]
  
  
  return(detected_lines)
}


simulate_growth <- function(cell_line_data, assay_end_point){
  
  # how long each cell doubled
  exponential_period <- pmax(assay_end_point - cell_line_data$lag_period,0)
  # look for ?pmax()
  
  # how many doublings
  number_of_doublings <-   exponential_period * cell_line_data$growth_rate
  
  # final cell counts
  lysate_cell_counts <- cell_line_data$initial_cell_count *  2^number_of_doublings
  
  return(lysate_cell_counts)
}


simulate_pcr <- function(lysate_cell_counts, pcr_bottleneck){
  
  n <- length(lysate_cell_counts)
  
  # there is some room for improvement here
  sampled_cells <- sample(n, pcr_bottleneck, replace = TRUE, prob = lysate_cell_counts)
  
  pcr_counts <- rep(0, n)
  
  for(x in 1:n){
    pcr_counts[x] <- sum(sampled_cells == x)
  }
  
  ## Alternative - without a for loop
  # sampled_table <- table(sampled_cells)
  # pcr_counts <- rep(0, 100)
  # names(pcr_counts) <- 1:n
  # pcr_counts[names(sampled_table)] <- sampled_table
  
  return(pcr_counts)
}


simulate_sequencing <- function(pcr_counts, seq_depth){
  n <- length(pcr_counts)
  sampled_counts <- sample(n, seq_depth, replace = TRUE, prob = pcr_counts)
  sequencing_counts <- rep(0, n)
  
  for(x in 1:n){
    # as if x = 2
    sequencing_counts[x] <- sum(sampled_counts == x)
  }
  
  return(sequencing_counts)
}


# A quick test
lysate_cell_counts <- simulate_growth(cell_line_data, 5)
plot(simulate_growth(cell_line_data, 3), 
     simulate_growth(cell_line_data, 5))

pcr_counts <- simulate_pcr(lysate_cell_counts, pcr_bottleneck)
hist(lysate_cell_counts)
hist(pcr_counts)

plot(log2(1 + lysate_cell_counts), 
     log2(1 + pcr_counts))

sequencing_counts <- simulate_sequencing(pcr_counts, seq_depth)

data.frame(initial = log2(1 + cell_line_data$initial_cell_count),
           lysate = log2(1 + lysate_cell_counts),
           pcr_input = log2(1 + pcr_counts), 
           seq_output = log2(1 + sequencing_counts))  %>% 
  psych::pairs.panels()



simulate_experiment(cell_line_data)



# Some simple examples of Loops! 


# For loops (iterative)
for(dummy in vector){
  do this for each value of dummy in the vector
}


for(value in c("my", 'first', "for", "loop")){
  print("one run")
}



for(value in c("my", 'first', "for", "loop")){
  print(value)
}
value


for(word in c("my", 'first', "for", "loop")){
  print(word)
}

chars <- rep("", 4)
words <- c("My", "second", "for", "loop")

for(i in 1:4){
  chars[i] <- words[i]
}


chars <- c()
for(i in 1:4){
  chars <- c(chars, words[i])
}




# while loops:

while(condition){
  run this as long as the condition is TRUE
}

roll <- function(){
  sample(6, 2, replace = TRUE)
}

roll()

n = 4
while(n > 0){
  print(factorial(n))
  n = n - 1
}


# let's write a loop that calls roll() till it gets a c(6,6) and 
# counts how many times it is called.

count <- 1
dice <- roll()
while(any(dice != c(6,6))){
  count <- count + 1 
  dice <- roll()
}
count

# Let's simulate this experiment 1000 times and plot the histogram of the counts

roll_till_6_6 <- function() {
  count <- 1
  dice <- roll()
  while(any(dice != c(6,6))){
    count <- count + 1 
    dice <- roll()
  }
  count
}

results <- replicate(1000, roll_till_6_6())


results <- rep(0, 1000)
for(ix in 1:1000){
  results[ix] <- roll_till_6_6()
}


results <- rep(0, 1000)
for(ix in 1:1000){
  count <- 1
  dice <- roll()
  while(any(dice != c(6,6))){
    count <- count + 1 
    dice <- roll()
  }
  results[ix] <- count
}

hist(results,100)

# one more example 


fizzbuzz <- function(n){
  if(n %% 15 == 0){
    return("FizzBuzz")
  }else if(n %% 3 == 0){
    return("Fizz")
  }else if(n %% 5 == 0){
    return("Buzz")
  }else{
    return(as.character(n))
  }
}

fizzbuzz(50)



# Task: Write a vector version of the fizzbuzz
# Test
c(45,46,48,50) ---> c("FizzBuzz", "46", "Fizz", "Buzz")

fizzbuzz_vector <- function(vec){
  n = length(vec)
  results = rep("", n)
  for(ix in 1:n){
    results[ix] <- fizzbuzz(vec[ix])
  }
  return(results)
}

fizzbuzz_vector2 <- function(vec){
  n = length(vec)
  results = rep("", n)
  for(ix in 1:n){
    if(vec[ix] %% 15 == 0){
      results[ix] = ("FizzBuzz")
    }else if(vec[ix] %% 3 == 0){
      results[ix] = ("Fizz")
    }else if(vec[ix] %% 5 == 0){
      results[ix] = ("Buzz")
    }else{
      results[ix] = (as.character(n))
    }
  }
  return(results)
}

fizzbuzz_vector3 <- function(vec){
  results <- as.character(vec)
  results[vec %% 3 == 0] <- "Fizz"
  results[vec %% 5 == 0] <- "Buzz"
  results[vec %% 15 == 0] <- "FizzBuzz"
  
  return(results)
}


system.time(fizzbuzz_vector(1:1e6))
system.time(fizzbuzz_vector3(1:1e6))















































































