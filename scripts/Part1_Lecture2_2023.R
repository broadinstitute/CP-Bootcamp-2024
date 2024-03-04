
# ----
# CP BOOTCAMP 2023 : INTRODUCTION TO R PROGRAMMING - LECTURE 2
# Author: Mustafa A. Kocak
# Date: Feb 7, 2023 
# Description: Wrapping up nuts and bolts of R
# ----



# ----
# Growth dynamics example
# ----

# let's ASSUME the growth rates are independent from the seeding and pooling 
cell_line_data <- data.frame(cell_line = paste0("c", 1:100), 
           growth_rate = runif(100, .5, 5),
           pool = rep(paste0("p", 1:5), each = 20))


pool_data <- data.frame(pool = paste0("p", 1:5),
           initial_cc = c(25,25,25,50,100),
           lysate_proportion = c(0.2, 0.2 ,0.2 , 0.2, 0.2) )

assay_end_point <- 5
pcr_bottleneck <- 1e6



cell_line_data$total_growth <- 2^(cell_line_data$growth_rate * assay_end_point)

for(pool %in% pool_data$pool){
  
}



cell_line_data$pool


pool_data$initial_cc


grow_cells <- function(cell_line_data, assay_end_point){
  
  
  return(final_population)
}



lysate_bottleneck <- function(cell_line_data, pool_data,
                              final_population, pcr_bottleneck){
  
  return(pcr_input)
}



# Slot machine example (Conditional statements)
# ----

# we will create a slot machine
play <- function() {
  
  # step 1: generate symbols
  symbols <- get_symbols()
  
  # step 2: display the symbols
  print(symbols)
  
  # step 3: score the symbols
  score(symbols)
}




# first part is relatively easy
get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

symbols <- get_symbols()

print(symbols)

# tiny bit fancier, please check ?paste0()
print_symbols <- function(symbols) {
  print(paste0(symbols, collapse = " - "))
}

# score is the trickier part, we need to be able to CHOOSE which path to follow:
# some warm-up with conditional statements

num <- -2

if (num < 0) {
  num <- num * -1
}

num

num <- 4

if (num < 0) {
  num <- num * -1
}

num



num <- -1

if (num < 0) {
  print("num is negative.")
  print("Don't worry, I'll fix it.")
  num <- num * -1
  print("Now num is positive.")
}

num

# quiz: guess the outcome(s)
x <- 1
if (3 == 3) {
  x <- 2
}
x

x <- 1
if (TRUE) {
  x <- 2
}
x


x <- 1
if (x == 1) {
  x <- 2
  if (x == 1) {
    x <- 3
  }
}
x

# we can add a B plan with "else" statement

a <- 3.14
dec <- a - trunc(a)
dec

if (dec >= 0.5) {
  a <- trunc(a) + 1
} else {
  a <- trunc(a)
}

a


# we can chain if and else statements for multiple choices
a <- 10
b <- 2

if (a > b) {
  print("A wins!")
} else if (a < b) {
  print("B wins!")
} else {
  print("Tie.")
}


# now are are ready to write the score function:
score <- function(symbols) {

# calculate a prize

prize
}



# solution
score <- function(symbols) {
  prize <- 0
  if(length(unique(symbols)) == 1){
    # three of a kind
    symbol <- symbols[1]
    if (symbol == "DD") {
      prize <- 100
    } else if (symbol == "7") {
      prize <- 80
    } else if (symbol == "BBB") {
      prize <- 40
    } else if (symbol == "BB") {
      prize <- 25
    } else if (symbol == "B") {
      prize <- 10
    } else if (symbol == "C") {
      prize <- 10
    } else {
      prize <- 0
    }
  } else if(all(symbols %in% c("B", "BB", "BBB"))){
    # all bars but not three of a kind
    prize <- 5
  } else{
    cherries <- sum(symbols == "C")
    if (cherries == 2) {
      prize <- 5
    } else if (cherries == 1) {
      prize <- 2
    } else {
      prize <- 0
    }
  }
  
  # counts the diamonds
  diamonds <- sum(symbols == "DD")
  
  # calculate the prize
  prize <- prize * 2^diamonds
  
  prize
  
}

play <- function() {
  symbols <- get_symbols()
  print_symbols(symbols)
  score(symbols)
}

simulated_prizes = replicate(1000, play())
summary(simulated_prizes)
ggplot2::qplot(simulated_prizes, binwidth = 1)


# we can simplify our code by using look-up tables
# please use tricks to make your code more efficient and readable, not shorter!
score <- function(symbols) {
  
  prize <- 0
  if(length(unique(symbols)) == 1){
    # three of a kind
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, 
                 "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
    prize <- payouts[symbols[1]]
  } else if(all(symbols %in% c("B", "BB", "BBB"))){
    # all bars but not three of a kind
    prize <- 5
  } else{
    cherries <- sum(symbols == "C")
    prize <- c(0, 2, 5)[cherries + 1]
  }
  
  # counts the diamonds
  diamonds <- sum(symbols == "DD")
  
  # correct the prize
  prize <- prize * 2^diamonds
  
  prize
  
}

# ----
# Expected values (Loops)
# ----

# remember the die we started 
die <- c(1, 2, 3, 4, 5, 6)

prob_loaded <- c(rep(1/8, 5), 3/8)


sum(die * 1/6)
sum(die * prob_loaded)


# let's enlist the outcomes for a pair of dice - check ?expand.grid()
rolls <- expand.grid(die, die)
rolls

# let's add the sum as a vector
rolls$value <- rolls$Var1 + rolls$Var2

rolls %>% head

# let's add the probabilities for each die outcome
rolls$prob1 <- prob_loaded[rolls$Var1]
rolls$prob2 <- prob_loaded[rolls$Var2]

rolls$prob <- rolls$prob1 * rolls$prob2

rolls %>% head

# expected value of the outcome 
sum(rolls$value * rolls$prob)



# next we will repeat the same exercise for the slot machine using the score function
# 1. list out every possible outcome of playing the machine. 
# 2. calculate the probability of getting each combination when you play the machine.
# 3. determine the prize that we would win for each combination

# list the combinations
wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = F)
combos %>% head
combos %>% tail()

# calculate the probability for each combination
prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, 
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]

combos$prob <- combos$prob1 * combos$prob2 * combos$prob3

combos %>% head()
sum(combos$prob) # sanity check!


# calculate the scores
# first let's try to compute the prize for the first row
symbols <- c(combos[1,1], combos[1,2], combos[1,3])
symbols
score(symbols)
# we need to apply score function for EACH row of the combos data frame

# we can do this with a for loop

for (value in c("My", "first", "for", "loop")) {
  print("one run")
}

for (value in c("My", "second", "for", "loop")) {
  print(value)
}

value

# note value is a dummy variable
for (word in c("My", "second", "for", "loop")) {
  print(word)
}
for (string in c("My", "second", "for", "loop")) {
  print(string)
}
for (i in c("My", "second", "for", "loop")) {
  print(i)
}

# we can fill a vector/list through a loop
chars <- vector(length = 4)
words <- c("My", "fourth", "for", "loop")
# note how the index set is defined
for (i in 1:4) {
  chars[i] <- words[i]
}


# let's use this to calculate the expected return on a dollar 
combos$prize <- NA
head(combos, 3)

for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}

combos %>% head()
sum(combos$prize * combos$prob)

## CHALLENGE: modify score function to count diamonds (DD) as wild symbol and re-calculate the expected return

score <- function(symbols) {
  
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  # identify case
  # since diamonds are wild, only nondiamonds 
  # matter for three of a kind and all bars
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
  # assign prize
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    # diamonds count as cherries
    # so long as there is one real cherry
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  
  # double for each diamond
  prize * 2^diamonds
}

# while loops iterates as long as the condition is true
# here is an example to see how long we can play till we go broke

plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}

plays_till_broke(100)


# we can write the same function with a repeat loop as well, it does exactly same job but it is more cumbersome
plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  repeat {
    cash <- cash - 1 + play()
    n <- n + 1
    if (cash <= 0) {
      break
    }
  }
  n
}

plays_till_broke(100)

# ----
# Speed
# ----

# absolute value function using a for loop
abs_loop <- function(vec){
  for (i in 1:length(vec)) {
    if (vec[i] < 0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}

# vectorized version of the same function
abs_sets <- function(vec){
  negs <- vec < 0
  vec[negs] <- vec[negs] * -1
  vec
}

# a quick benchmark 
long <- rep(c(-1, 1), 50000000)


system.time(abs_loop(long))

system.time(abs_sets(long))

system.time(abs(long)) # note most R functions are already vectorized


# here is another simple example
change_symbols <- function(vec){
  for (i in 1:length(vec)){
    if (vec[i] == "DD") {
      vec[i] <- "joker"
    } else if (vec[i] == "C") {
      vec[i] <- "ace"
    } else if (vec[i] == "7") {
      vec[i] <- "king"
    }else if (vec[i] == "B") {
      vec[i] <- "queen"
    } else if (vec[i] == "BB") {
      vec[i] <- "jack"
    } else if (vec[i] == "BBB") {
      vec[i] <- "ten"
    } else {
      vec[i] <- "nine"
    } 
  }
  vec
}

vec <- c("DD", "C", "7", "B", "BB", "BBB", "0")

change_symbols(vec)

many <- rep(vec, 1000000)

system.time(change_symbols(many))

# let's try to vectorize this function
change_vec <- function (vec) {
  vec[vec == "DD"] <- "joker"
  vec[vec == "C"] <- "ace"
  vec[vec == "7"] <- "king"
  vec[vec == "B"] <- "queen"
  vec[vec == "BB"] <- "jack"
  vec[vec == "BBB"] <- "ten"
  vec[vec == "0"] <- "nine"
  
  vec
}

system.time(change_vec(many))

# an alternative one
change_vec2 <- function(vec){
  tb <- c("DD" = "joker", "C" = "ace", "7" = "king", "B" = "queen", 
          "BB" = "jack", "BBB" = "ten", "0" = "nine")
  unname(tb[vec])
}

system.time(change_vec(many))

# if you can, move code out of the for loop, and make sure you don't copy your data at each iteration
system.time({
  output <- rep(NA, 10000000) 
  for (i in 1:10000000) {
    output[i] <- i + 1
  }
})

system.time({
  output <- NA 
  for (i in 1:10000000) {
    output[i] <- i + 1
  }
})


# to practice, let's play with our slot machine again 
# first let's calculate the expected return with simulation
winnings <- vector(length = 1000000)
for (i in 1:1000000) {
  winnings[i] <- play()
}

mean(winnings)

system.time(for (i in 1:1000000) {
  winnings[i] <- play()
})


# we can get symbols for many runs at once
get_many_symbols <- function(n) {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  vec <- sample(wheel, size = 3 * n, replace = TRUE,
                prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
  matrix(vec, ncol = 3)
}

get_many_symbols(5)

# we can score each row with a for loop or an using "apply" function
# see ?apply() and related functions
apply(get_many_symbols(10), 1, score)
mean(apply(get_many_symbols(1000), 1, score))



# we can play many functions at a time (note we stop printing the outcomes) and 
# optimize further by vectorizing score function as well
play_many <- function(n) {
  symb_mat <- get_many_symbols(n = n)
  data.frame(w1 = symb_mat[,1], w2 = symb_mat[,2],
             w3 = symb_mat[,3], prize = score_many(symb_mat))
}

# score_many is a good challenge to take on, try to write your own version before moving on:
# symbols should be a matrix with a column for each slot machine window
score_many <- function(symbols) {
  
  # Step 1: Assign base prize based on cherries and diamonds ---------
  ## Count the number of cherries and diamonds in each combination
  cherries <- rowSums(symbols == "C")
  diamonds <- rowSums(symbols == "DD") 
  
  ## Wild diamonds count as cherries
  prize <- c(0, 2, 5)[cherries + diamonds + 1]
  
  ## ...but not if there are zero real cherries 
  ### (cherries is coerced to FALSE where cherries == 0)
  prize[!cherries] <- 0
  
  # Step 2: Change prize for combinations that contain three of a kind 
  same <- symbols[, 1] == symbols[, 2] & 
    symbols[, 2] == symbols[, 3]
  payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40, 
               "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
  prize[same] <- payoffs[symbols[same, 1]]
  
  # Step 3: Change prize for combinations that contain all bars ------
  bars <- symbols == "B" | symbols ==  "BB" | symbols == "BBB"
  all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
  prize[all_bars] <- 5
  
  # Step 4: Handle wilds ---------------------------------------------
  
  ## combos with two diamonds
  two_wilds <- diamonds == 2
  
  ### Identify the nonwild symbol
  one <- two_wilds & symbols[, 1] != symbols[, 2] & 
    symbols[, 2] == symbols[, 3]
  two <- two_wilds & symbols[, 1] != symbols[, 2] & 
    symbols[, 1] == symbols[, 3]
  three <- two_wilds & symbols[, 1] == symbols[, 2] & 
    symbols[, 2] != symbols[, 3]
  
  ### Treat as three of a kind
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  ## combos with one wild
  one_wild <- diamonds == 1
  
  ### Treat as all bars (if appropriate)
  wild_bars <- one_wild & (rowSums(bars) == 2)
  prize[wild_bars] <- 5
  
  ### Treat as three of a kind (if appropriate)
  one <- one_wild & symbols[, 1] == symbols[, 2]
  two <- one_wild & symbols[, 2] == symbols[, 3]
  three <- one_wild & symbols[, 3] == symbols[, 1]
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  # Step 5: Double prize for every diamond in combo ------------------
  unname(prize * 2^diamonds)
  
}

system.time(play_many(10000000))