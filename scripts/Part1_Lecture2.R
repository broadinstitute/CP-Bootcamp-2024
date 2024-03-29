
# ----
# CP BOOTCAMP 2024 : INTRODUCTION TO R PROGRAMMING - LECTURE 2
# Author: Mustafa A. Kocak
# Date: March 5, 2024 
# Description: Wrapping up nuts and bolts of R
# ----



# ----
# Growth dynamics example
# ----


# Let's create some cell line data: 


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

# First attempt (planning)
simulate_experiment <- function(){
  
  # calculate cell counts at the end-point
  lysate_cell_counts <- simulate_growth()
  
  # simulate the pcr bottleneck
  pcr_counts <- simulate_pcr()
  
  # simulate sequencing
  sequencing_counts <- simulate_sequencing()
  
  # visualize final population
  hist(sequencing_counts, 100)
  
  # count the detected cell lines
  detected_lines <- count_detected()
}




# Second attempt (let's put the arguments in place)
simulate_experiment <- function(cell_line_data,  assay_end_point, pcr_bottleneck, seq_depth,  threshold){
  
  # calculate cell counts at the end-point
  lysate_cell_counts <- simulate_growth(cell_line_data, assay_end_point)
  
  # simulate the pcr bottleneck
  pcr_counts <- simulate_pcr(lysate_cell_counts, pcr_bottleneck)
  
  # simulate sequencing
  sequencing_counts <- simulate_sequencing(pcr_counts, seq_depth)
  
  # visualize final population
  plot(log10(sort(sequencing_counts)), type = "h")
  
  # count the detected cell lines
  detected_lines <- count_detected(sequencing_counts, threshold)
  
  return(detected_lines)
}


threshold

lysate_cell_counts %>% sum

# Let's fill in the other functions: 

simulate_growth <- function(cell_line_data, assay_end_point){
  
  growth_period <- assay_end_point - cell_line_data$lag_period
  
  final_cell_count <- cell_line_data$initial_cell_count * 2^(cell_line_data$growth_rate * growth_period)
  
  # plot(sort(final_cell_count), type = "h")
  return(final_cell_count)
}

simulate_pcr <- function(lysate_cell_counts, pcr_bottleneck){
  # note there is some room for improvement for this one ! 
  n <- length(lysate_cell_counts)
  sampled_cells <- sample(1:n, size = pcr_bottleneck, replace = TRUE, prob = lysate_cell_counts)
  
  sampled_table <- table(sampled_cells)
  pcr_counts = rep(0, n); names(pcr_counts) = 1:n
  pcr_counts[names(sampled_table)] <- sampled_table
  
  plot(log2(lysate_cell_counts), log2(pcr_counts))
  
  return(pcr_counts)
}

simulate_sequencing <- function(pcr_counts, seq_depth){
  
  n <- length(pcr_counts)
  sampled_cells <- sample(1:n, size = seq_depth, replace = TRUE, prob = pcr_counts)
  
  sampled_table <- table(sampled_cells)
  seq_counts = rep(0, n); names(seq_counts) = 1:n
  seq_counts[names(sampled_table)] <- sampled_table
  
  plot(log2(pcr_counts), log2(seq_counts))
  plot(sort(log10(1 + seq_counts)), type = "h")
  
  return(seq_counts)
}

count_detected <- function(sequencing_counts, threshold){
  which(sequencing_counts >= threshold) 
}


# Let's make a final iteration on the original plan to have more comprehensive return: 

# Third attempt (polishing up)
simulate_experiment_2 <- function(cell_line_data, 
                                assay_end_point = 5, pcr_bottleneck = 2e4, seq_depth = 1e6, 
                                threshold = 2e3){
  
  # calculate cell counts at the end-point
  cell_line_data$lysate_cell_counts <- simulate_growth(cell_line_data, assay_end_point)
  
  # simulate the pcr bottleneck
  cell_line_data$pcr_counts <- simulate_pcr(cell_line_data$lysate_cell_counts, pcr_bottleneck)
  
  # simulate sequencing
  cell_line_data$sequencing_counts <- simulate_sequencing(cell_line_data$pcr_counts, seq_depth)

  # identify the well detected lines
  cell_line_data$is_detected <- cell_line_data$sequencing_counts >= threshold

  return(list(cell_line_data = cell_line_data,
              number_of_detected_lines = sum(cell_line_data$is_detected)))
}


result <- simulate_experiment(cell_line_data)

result$number_of_detected_lines
result$cell_line_data %>% View



# -----
# Conditionals
# -----


# We use conditionals when we need to CHOOSE which path to follow:
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

# classical interview question: 
fizzbuzz <- function(x){
  
}

# solution
fizzbuzz <- function(x){
  if(x %% 15 == 0){
    print("FizzBuzz")
  }else if(x %% 5 == 0){
    print("Fizz")
  }else if(x %% 3 == 0){
    print("Buzz")
  }else{
    print(x)
  }
}
    


# Sometimes using a look-up table makes the code less clunky
# Please use tricks to make your code more efficient and readable, not shorter!

print_die <- function(x){
  if(x == 1){
    print("One")
  }else if(x == 2){
    
  } ....
}

print_die2 <- function(x){
  face = 1:6; names(face) = c("one", "two", "three", "four", "five", "six")
  
  if(x %in% 1:6){
    print(face[ix])
  }else{
    print("Input is not between 1 and 6!")
  }
  
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

# practice: should we write a print function for the roll of a pair of dies ?





# When we need to run a function for EACH row of a dataset, we can use a loop.
# Here is a for loop:

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


# Sometimes we need to run a loop not to iterate over a fixed list, but till a condition is satisfied

# Let's roll a pair of dice till we get a pair of sixes


roll_till_six_six <- function() {
  dice = sample(6,2, replace = TRUE)
  count = 1
  while(sum(dice) != 12){
    dice = sample(6,2, replace = TRUE)
    count = count + 1
  }
  count
}

simulated_rolls <- replicate(100, roll_till_six_six())

hist(simulated_rolls, 100)
plot(simulated_rolls, type = "h")


# We can write the same function with a repeat loop as well, it does exactly same job but it is more cumbersome

roll_till_six_six <- function() {
  count = 0
  repeat{
    dice = sample(6,2, replace = TRUE)
    count = count + 1
    if(sum(dice) != 12){
      break
    }
  }
  count
}




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



# practice: write fizzbuzz function for a vector input using a for loop, 
# then repeat again using logical subsetting


fizzbuzz2 <- function(x){
  n = length(x)
  for(ix in 1:n){
    fizzbuzz(x[ix])
  }
}

fizzbuzz2(45:50)


fizzbuzz3 <- function(x){
  n = length(x)
  str <- rep("", n)
  for(ix in 1:n){
    if(x[ix] %% 15 == 0){
      str[ix] <- "FizzBuzz"
    }else if(x[ix] %% 3 == 0){
      str[ix] <- "Buzz"
    }else if(x[ix] %% 5 == 0){
      str[ix] <- "Fizz"
    }else{
      str[ix] <- as.character(x[ix])
    }
  }
  str
}

fizzbuzz3(45:50)

fizzbuzz4 <- function(x){
  y = as.character(x)
  y[x %% 5 == 0] <- "Fizz"
  y[x %% 3 == 0] <- "Buzz"
  y[x %% 15 == 0] <- "FizzBuzz"
  return(y) 
}

fizzbuzz4(45:50)

system.time(fizzbuzz3(1:1e6))
system.time(fizzbuzz4(1:1e6))



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



# TO BE EDITED! ------

# Let's wrap up by writing a function to compute correlations and p-values





