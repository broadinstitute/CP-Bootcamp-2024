
# ----
# CP BOOTCAMP 2024 : INTRODUCTION TO TIDYVERSE - LECTURE 2
# Author: Mustafa A. Kocak
# Date: March 6, 2024
# Description: Introduction to tidy-verse - Part 2 - Chapter 3 ...
# ----


# DATA TRANSFORMATIONS ----

library(nycflights13)

head(flights) # note the variable types at the top
glimpse(flights)
summary(flights)


# Filtering rows ----

# flights that departed on Jan 1st
flights %>% 
  dplyr::filter(month == 1, day == 1)

# assign to a variable instead of printing
jan1 <- dplyr::filter(flights, month == 1, day == 1)

# both assign and print
(dec25 <- dplyr::filter(flights, month == 12, day == 25))


# note = is not a comparison! 
filter(flights, month = 1)

# be careful with the doubles
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1

# check ?near()
near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)

# Flights in November or December
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))

# DeMorgan's law may simplifies queries
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)


# NA's are contiguous
NA > 5
10 == NA
NA + 10
NA / 2
NA == NA # !!!

# Let x be Mary's age. We don't know how old she is.
x <- NA
# Let y be John's age. We don't know how old he is.
y <- NA
# Are John and Mary the same age?
x == y
# We don't know!

# If you want to determine if a value is missing, use is.na():
is.na(x)
# Check also ?is.finite()

# Filter considers NA's as FALSE!
df <- tibble(x = c(1, NA, 3))
filter(df, !(x > 1))
filter(df, is.na(x) | x > 1)

# Exercises
# 1. Find all flights that
#    Had an arrival delay of two or more hours
#    Flew to Houston (IAH or HOU)
#    Were operated by United, American, or Delta
#    Departed in summer (July, August, and September)
#    Arrived more than two hours late, but didn’t leave late
#    Were delayed by at least an hour, but made up over 30 minutes in flight
#    Departed between midnight and 6am (inclusive)
# 2. Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenges?
# 3. How many flights have a missing dep_time? What other variables are missing? What might these rows represent?
# 4. Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)


# Arranging rows ----

# Similar to filter, but instead just change the order of the rows
arrange(flights, year, month, day)

# Sorting in the descending order
arrange(flights, desc(dep_delay))

# Missing values are always at the end
df <- tibble(x = c(5, 2, NA))
df
arrange(df, x)
arrange(df, desc(x))

# Exercises
# 1. How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
# 2. Sort flights to find the most delayed flights. Find the flights that left earliest.
# 3. Sort flights to find the fastest (highest speed) flights.
# 4, Which flights travelled the farthest? Which travelled the shortest?


# Selecting columns -----

# Select columns by name
select(flights, year, month, day)

distinct(flights, month)

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

# A useful variation to rename columns
rename(flights, tail_num = tailnum)

# everything() is also useful to pull few columns to the left
select(flights, time_hour, air_time, everything())

# See also ?distinct()
select(flights, carrier, tailnum)
distinct(flights, carrier, tailnum)

# Please check the documentation for ?paste0() ?past() ?word() ?toupper() and ?tolower()

# Exercises
# 1. Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.
# 2. What happens if you include the name of a variable multiple times in a select() call?
# 3. What does the any_of() function do? Why might it be helpful in conjunction with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
# 4, Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?
select(flights, contains("TIME"))

# Adding new variables ----

# we’ll start by creating a narrower dataset so we can see the new variables
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)

flights_sml %>% head

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

# you can refer to columns that you’ve just created
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)


# you only want to keep the new variables, use transmute():
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

# modular arithmetic functions: integer division and remainder
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

# offset functions
(x <- 1:10)
lag(x)
lead(x)
diff(x)

# cumulative aggregates
x
cumsum(x)
cummean(x)

# ranking
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
# also: 
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)
ntile(x, n = 2)

# Exercises
# 1. Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.
# 2. Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?
# 3.Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?
# 4. Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().
# 5. What does 1:3 + 1:10 return? Why?
# 6. What trigonometric functions does R provide?

#  Grouped Summaries ----

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

daily <- dplyr::group_by(flights, year, month, day) 
summarise(daily, delay = mean(dep_delay, na.rm = TRUE))

# summarise() is not terribly useful unless we pair it with group_by()
# let's summarize again by grouping by date
flights %>% 
  dplyr::group_by(year, month, day) %>% 
  dplyr::summarise(delay = mean(dep_delay, na.rm = TRUE))



# note this would be messier without the pipe! 
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dest != "HNL")

delays

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
ggplot(data = delays, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# Beware of the NA's!
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = T))

# alternatively we can remove the missing rows
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))


not_cancelled <- not_cancelled %>% 
  group_by(year, month, day) 

not_cancelled %>% 
  group_by(tailnum) 

not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(delay = mean(arr_delay)) %>% 
  ggplot(aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)


# The shape of this plot is very characteristic: whenever you plot a mean
# (or other summary) vs. group size, you’ll see that the variation decreases
# as the sample size increases.
not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(delay = mean(arr_delay, na.rm = TRUE),
            n = n()) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(delay = mean(arr_delay, na.rm = TRUE),
            n = n()) %>% 
  dplyr::filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


# A new example: Lahman dataset (batting averages)
# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)

head(batting)
summary(batting)

# ab : opportunities to hit
# ba: hitting average
batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

# Does the trend make sense?
batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)


# There are more lucky players at the top, rather than skilled ones
batters %>% 
  arrange(desc(ba))
# http://varianceexplained.org/r/empirical_bayes_baseball/ and http://www.evanmiller.org/how-not-to-sort-by-average-rating.html

# some useful summaries
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

# Why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )


not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

# count is a useful helper! 
not_cancelled %>% 
  dplyr::ungroup() %>% 
  count(dest)

not_cancelled %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(dest) %>%
  dplyr::summarise(n = n()) 


# total number of miles for each plane
not_cancelled %>%
  dplyr::ungroup() %>% 
  count(tailnum, wt = distance)

distances <- not_cancelled %>% 
  dplyr::group_by(tailnum) %>%
  dplyr::summarise(n = sum(distance, na.rm =TRUE)) 




# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))


# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_prop = mean(arr_delay > 60))


# when you group with multiple variables, summarise peels of one layer of grouping
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))


# don't forget to ungroup your data when you are done:
daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights


# Exercises
# 1. Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights. Consider the following scenarios:
#    A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.
#    A flight is always 10 minutes late.
#    A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
#    99% of the time a flight is on time. 1% of the time it’s 2 hours late.
# Which is more important: arrival delay or departure delay?
# 2. Come up with another approach that will give you the same output as 
# not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count()).
# 3. Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. Why? Which is the most important column?
# 4. Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of cancelled flights related to the average delay?
# 5. Which carrier has the worst delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? 
# (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))
# 6. What does the sort argument to count() do. When might you use it?


#  Grouped Mutates (and Filters) ----

# Top 9 most delayed flights every day
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

flights_sml %>%
  dplyr::group_by(year,month,day) %>% 
  top_n(n = 9, wt = arr_delay)


# Filtering only the large groups
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

# Standardise to compute per group metrics:
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

# Exercises
# 1. Refer back to the lists of useful mutate and filtering functions. Describe 
# how each operation changes when you combine it with grouping.
# 2. Which plane (tailnum) has the worst on-time record?
# 3. What time of day should you fly if you want to avoid delays as much as possible?
# 4. For each destination, compute the total minutes of delay. For each flight, 
# compute the proportion of the total delay for its destination.
# 5. Delays are typically temporally correlated: even once the problem that caused 
# the initial delay has been resolved, later flights are delayed to allow earlier 
# flights to leave. Using lag(), explore how the delay of a flight is related to 
# the delay of the immediately preceding flight.
# 6. Look at each destination. Can you find flights that are suspiciously fast? 
# (i.e. flights that represent a potential data entry error). Compute the air time 
# of a flight relative to the shortest flight to that destination. Which flights 
# were most delayed in the air?
# 7. Find all destinations that are flown by at least two carriers. Use that 
# information to rank the carriers.
# 8.For each plane, count the number of flights before the first delay of greater than 1 hour.




# Extra ----
library(ggthemes)


dat = tibble(x = c("heard", "seen", "heard + seen", "said", "applied", "discovered"), 
             order = 1:6, mag = c(1,2,4,3,5,8))

dat %>% 
  ggplot() +
  geom_col(aes(x = reorder(x, -order), y = mag,
               fill = order > 4), width = .25, show.legend = F) + 
  coord_flip() + 
  theme_tufte(base_size = 20, base_family = "GillSans", ticks = F) +
  labs(y = "", x ="") +
  scale_fill_manual(values = c("gray50", "red"))

# install.packages("ggrepel)
library(ggrepel)

fig1 <- mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = class)) + 
  geom_point() +
  geom_text_repel(aes(label = trans), 
                  max.overlaps = 10, show.legend = FALSE)

fig1  

plotly::ggplotly(fig1)




#install.packages("ggalluvial")
library(ggalluvial)

titanic_wide <- data.frame(Titanic)
head(titanic_wide)

ggplot(data = titanic_wide,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,
           y = Freq)) +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  labs(x = "Demographic",
       title = "passengers on the maiden voyage of the Titanic",
       subtitle = "stratified by demographics and survival" ) + 
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.2, .05)) 


mpg %>% 
  ggplot(aes(x = ))
head

# TIDY DATA ----

library(tidyverse)


# Tidy Data ----
# Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>% 
  count(year, wt = cases)


# Visualise changes over time
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))


# Exercises
# 1. Using prose, describe how the variables and observations are organised in each of the sample tables.
# 2. Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
#    a. Extract the number of TB cases per country per year.
#    b. Extract the matching population per country per year.
#    c. Divide cases by population, and multiply by 10000.
#    d. Store back in the appropriate place.
#   Which representation is easiest to work with? Which is hardest? Why?
# 3. Recreate the plot showing change in cases over time using table2 instead of table1. What do you need to do first?

# Pivoting ----

# pivoting longer
table4a
table4a %>% 
  pivot_longer(c(`1999`, `2000`), # here you can use all select helpers
               names_to = "year", 
               values_to = "cases")

table4b %>% 
  pivot_longer(c(`1999`, `2000`),
               names_to = "year", 
               values_to = "population")

# We will learn left_join in the next section
tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")
left_join(tidy4a, tidy4b)

# pivoting wider
table2 %>%
  pivot_wider(names_from = type, values_from = count)



# Exercises
# 1. Why are pivot_longer() and pivot_wider() not perfectly symmetrical?
# Carefully consider the following example:
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")
# (Hint: look at the variable types and think about column names.)
# pivot_longer() has a names_ptypes argument, e.g.  names_ptypes = list(year = double()). What does it do?
# 2. Why does this code fail?
table4a %>% 
  pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")
# 3. What would happen if you widen this table? Why? How could you add a new column to uniquely identify each value?
people <- tribble(
  ~name,             ~names,  ~values,
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
# 4. Tidy the simple tibble below. Do you need to make it wider or longer? What are the variables?
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

# Separating and Uniting ----

# problem with this dataset is rate contains 2 columns worth of information
table3

# by default separate splits by the first non-alphanumeric character
# note the new values are still characters
table3 %>% 
  separate(rate, into = c("cases", "population"))

# we can specify the separators - with strings or regex's
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

# columns are converted into integers
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

# you can also give a position to "sep" to split by location
table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)


# the inverse of separate is unite
table5 %>% head

table5 %>%
  unite(new, century, year)

# we again have a separator argument (default is "_")
table5 %>%
  unite(new, century, year, sep = "")

# Exercises
# 1. What do the extra and fill arguments do in separate()? 
# Experiment with the various options for the following two toy datasets.
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"))
# 2. Both unite() and separate() have a remove argument. What does it do? 
# Why would you set it to FALSE?
# 3. Compare and contrast separate() and extract(). Why are there three 
# variations of separation (by position, by separator, and with groups), 
# but only one unite?

# Missing Values ----

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

# making the missing values explicit
stocks %>% 
  pivot_wider(names_from = year, values_from = return)


# making the missing values implicit (dropping them)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(
    cols = c(`2015`, `2016`), 
    names_to = "year", 
    values_to = "return", 
    values_drop_na = TRUE
  )

# complete fills the missing combinations - making explicit NA's
stocks

stocks %>% 
  complete(year, qtr)


# Exercises
# 1. Compare and contrast the fill arguments to pivot_wider() and complete().
# 2. What does the direction argument to fill() do?

# RELATIONAL DATA ----

library(tidyverse)
library(nycflights13)

# nycflights13 actually has 4 tibbles besides flights
airlines
airports
planes
weather

# Keys ----

# checking if keys actually uniquely identifies each observation is important
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)


# some datasets don't have any primary keys! 
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, tailnum, flight) %>% 
  filter(n > 1)


# we can always create a new one 
flights %>% 
  dplyr::mutate(identifier = 1:n())  %>%
  dplyr::select(identifier, year, month, day,
                tailnum, everything())  %>% 
  head

# Exercises
# 1. Add a surrogate key to flights.
# 2. Identify the keys in the following datasets
#     Lahman::Batting,
#     babynames::babynames
#     nasaweather::atmos
#     fueleconomy::vehicles
#     ggplot2::diamonds
# (You might need to install some packages and read some documentation.)
# 3. Draw a diagram illustrating the connections between the Batting, People, and 
# Salaries tables in the Lahman package. Draw another diagram that shows the 
# relationship between People, Managers, AwardsManagers.
# How would you characterise the relationship between the Batting, Pitching, and Fielding tables?

# Mutating Joins ----

# a narrower dataset to print easier
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

airlines

# let's add the carrier names
flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")


# old way  - don't do this
airline_names <- airlines$name
names(airline_names) <- airlines$carrier

flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airline_names[flights2$carrier])


# toy datasets
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

# inner join adds columns based on the intersection of the shared keys
# i.e. unmatched rows are not included
x %>% 
  inner_join(y, by = "key")


# duplicate keys 
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)


left_join(x, y, by = "key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")

# defining the key columns

# natural join: matching using all the common columns
flights2 %>% 
  left_join(weather)


# by giving a subset of the overlapping column names
flights2 %>% 
  left_join(planes, by = "tailnum")

flights2 %>% 
  left_join(planes)

# by giving  column names from each dataset matched with "="
flights2 %>% 
  left_join(airports, c("dest" = "faa"))

flights2 %>% 
  left_join(airports, c("origin" = "faa"))

# Exercises
# 1. Compute the average delay by destination, then join on the airports data 
# frame so you can show the spatial distribution of delays. Here’s an easy way 
# to draw a map of the United States:
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
# (Don’t worry if you don’t understand what semi_join() does — you’ll learn about it next.)
# You might want to use the size or colour of the points to display the average
# delay for each airport.

# 2. Add the location of the origin and destination (i.e. the lat and lon) to flights.
# 3. Is there a relationship between the age of a plane and its delays?
# 4. What weather conditions make it more likely to see a delay?
# 5. What happened on June 13 2013? Display the spatial pattern of delays, and then 
# use Google to cross-reference with the weather.

# Filtering Joins ----

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest

# easy to keep only the top destionations
flights %>% 
  filter(dest %in% top_dest$dest)

# but this is easier to extend to multiple columns
flights %>% 
  semi_join(top_dest)


# there are many flights with their tail numbers are not listed in planes
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)

# Exercises
# 1. What does it mean for a flight to have a missing tailnum? What do the 
# tail numbers that don’t have a matching record in planes have in common? 
# (Hint: one variable explains ~90% of the problems.)
# 2. Filter flights to only show flights with planes that have flown at least 100 flights.
# 3. Combine fueleconomy::vehicles and fueleconomy::common to find only the records 
# for the most common models.
# 4. Find the 48 hours (over the course of the whole year) that have the worst delays. 
# Cross-reference it with the weather data. Can you see any patterns?
# 5. What does anti_join(flights, airports, by = c("dest" = "faa")) tell you? 
# What does anti_join(airports, flights, by = c("faa" = "dest")) tell you?
# 6. You might expect that there’s an implicit relationship between plane and
# airline, because each plane is flown by a single airline. Confirm or reject this
# hypothesis using the tools you’ve learned above.

# Set Operators ----

df1 <- tribble(~x, ~y,
               1,  1,
               2,  1)
df2 <- tribble(~x, ~y,
               1,  1,
               1,  2)

df1
df2

#The four possibilities are:
intersect(df1, df2)

# Note that we get 3 rows, not 4
union(df1, df2)

setdiff(df1, df2)
setdiff(df2, df1)

