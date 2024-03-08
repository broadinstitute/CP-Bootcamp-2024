
# -----
# CP Bootcamp 2024: Introduction to Tidyverse Part 2
# Mustafa A Kocak
# 7 March 2024
# Learning dplyr and tidyr
# ------

# Libraries ----
library(tidyverse)
library(nycflights13)
library(taigr)
options(taigaclient.path=path.expand("/Users/mkocak/anaconda3/envs/taigapy/bin/taigaclient"))



flights
head(flights)
glimpse(flights)
summary(flights)


# Filtering Rows -----

jan1 <- flights %>% 
  dplyr::filter(month == 1, day == 1)

# both assign and print
(march24 <- flights %>%
    dplyr::filter(month == 3, day == 24))


filter(flights, month == 1)

# This is a common mistake! 
filter(flights, month = 1)

sqrt(2)^2 == 2
1/49 * 49 == 1

near(1/49 * 49, 1)

nov_dec <- flights |> 
  dplyr::filter(month == 11 | month == 12)

nov_dec <- flights |> 
  dplyr::filter(month > 10)

nov_dec <- flights |> 
  dplyr::filter(month >= 11)

nov_dec <- flights |> 
  dplyr::filter(month %in% c(11,12))

# What does this mean?
flights %>% 
  dplyr::filter(!(arr_delay > 120 | dep_delay > 120)) 


flights %>% 
  dplyr::filter(arr_delay <= 120 , dep_delay <= 120)

flights %>% 
  dplyr::filter(arr_delay <= 120) %>%
  dplyr::filter(dep_delay <= 120)


# NA's are contigous
NA > 5 
10 == NA
NA == NA 

is.na(NA)

df <- tibble(x = c(1,NA,3))

df %>% 
  dplyr::filter(x > 1)

df %>% 
  dplyr::filter(!(x > 1))


df %>% 
  dplyr::filter((x > 1) | is.na(x))


flights %>% 
  dplyr::filter(arr_delay >= 2)


# Houston : IAH, HOU
flights %>% 
  dplyr::filter(dest %in% c("IAH", "HOU"))

flights %>% 
  dplyr::filter(dest == "IAH"  | dest == "HOU")


flights %>% 
  dplyr::filter(is.na(dep_time)) 


# Arrange to sort the table

flights %>% 
  dplyr::arrange(year, month, day)

flights %>% 
  dplyr::arrange(dep_delay)


# These two are equivalent for numerical variables
flights %>% 
  dplyr::arrange(-dep_delay)

flights %>% 
  dplyr::arrange(desc(dep_delay))


flights %>% 
  dplyr::arrange(desc(dep_delay), day)


# Default behaviour of sorting is putting NA's to the end of the list
df
dplyr::arrange(df, x)
dplyr::arrange(df, desc(x))




OmicsSomaticMutations <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsSomaticMutations') %>%
  as_tibble()


# Selecting columns

flights
OmicsSomaticMutations

# Selecting columns by name
flights %>% 
  dplyr::select(year, month, day)

flights %>%
  dplyr::distinct(year, month, day)

names(flights)

flights %>% 
  dplyr::select(year:day)

# all but things between year and day
flights %>% 
  dplyr::select(-(year:day))

flights %>% 
  dplyr::select(!(year:day))

flights %>%
  dplyr::distinct(year, month, day, .keep_all = TRUE)

flights %>%
  dplyr::select(Month = month, day, year) %>% 
  head

flights %>%
  dplyr::rename(Month = month) %>% 
  head

flights %>% 
  dplyr::relocate(dep_time, sched_dep_time,
                  .before = month)

flights %>% 
  dplyr::relocate(dep_time, sched_dep_time,
                  .after = year)


flights %>% 
  dplyr::select(flight, time_hour, air_time, everything())


flights %>%
  dplyr::relocate(flight, time_hour, air_time)

flights %>% 
  dplyr::select(flight, time_hour, air_time, ends_with("delay"))


?dplyr::select


# Elementary string processing
a <- "Mustafa Anil Kocak"
b <- "MUSTAFA ANIL KOCAK"
c <- "mustafa anil kocak"
d <- "mustafa::anil::kocak"

tolower(c(a,b,c))
toupper(c(a,b,c))
make.names(c(a,b,c))

word(d, 1,-2, sep = fixed("::"))
substr(d, 1,5)

flights %>% 
  dplyr::count(month, day, year)

flights %>% 
  dplyr::select(month, month, day)



trimmed_mutations <- OmicsSomaticMutations %>% 
  dplyr::select(ProteinChange,
                HugoSymbol,
                ModelID,
                HessDriver,
                LikelyLoF)


trimmed_mutations 

# How many distinct driver (Hess) mutations 
# with specified protein changes ?

trimmed_mutations %>% 
  dplyr::filter(HessDriver, !is.na(ProteinChange)) %>%
  dplyr::distinct(ProteinChange, HugoSymbol)

# For your favorite 3-5 genes, create a 
# barplot to show their counts across depmap panel
# highlighting Hess drivers.

trimmed_mutations %>%
  dplyr::filter(HugoSymbol %in% c("RB1", "EGFR", "BRAF", "PTEN", "VHL")) %>%
  ggplot() +
  geom_bar(aes(x = HugoSymbol, fill = HessDriver))



# Mutate: Creating/adding new columns -----


flights_small <- flights %>% 
  dplyr::select(year:day,
                ends_with("delay"),
                distance,
                air_time)

flights_small  

flights_small %>% 
  dplyr::mutate(gain = dep_delay - arr_delay, 
                speed = distance / air_time)


flights_small %>% 
  dplyr::mutate(gain = dep_delay - arr_delay, 
                speed = distance / air_time,
                .before = dep_delay)

# Some misc. functions 

x <- 1:10
x
lag(x)
lead(x)
diff(x)

sum(x)
cumsum(x)
cummean(x)

y <- c(1,2,2,NA,3,4)
rank(y)
min_rank(y)



flights_small %>% 
  dplyr::mutate(average_dep_delay = mean(dep_delay, na.rm = T),
                log2_dep_delay = log2(dep_delay))
head


# Grouping Summaries -----


flights %>% 
  dplyr::summarise(ave_dep_delay = mean(dep_delay, na.rm = TRUE),
                   ave_arr_delay = mean(arr_delay, na.rm = TRUE),
                   med_dep_delay = median(dep_delay, na.rm = TRUE),
                   med_arr_delay = median(arr_delay, na.rm = TRUE))


flights %>% 
  dplyr::sample_n(1000) %>% 
  ggplot(aes(x = dep_delay,
             y = arr_delay)) +
  geom_point()


flights %>% 
  dplyr::group_by(month, day, year)


day_summaries <- flights %>% 
  dplyr::group_by(month, day, year) %>% 
  dplyr::summarise(ave_dep_delay = mean(dep_delay, na.rm = TRUE),
                   ave_arr_delay = mean(arr_delay, na.rm = TRUE),
                   med_dep_delay = median(dep_delay, na.rm = TRUE),
                   med_arr_delay = median(arr_delay, na.rm = TRUE)) %>%
  dplyr::ungroup()

day_summaries <- day_summaries %>%
  dplyr::group_by(month)


delays <- flights %>% 
  dplyr::group_by(dest) %>% 
  dplyr::summarise(count = n(), 
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)) %>% 
  dplyr::filter(count > 20, dest != "HNL")

delays %>%
  ggplot(aes(x = dist, y = delay)) +
  geom_point() +
  geom_smooth(se = FALSE)

not_cancelled <- flights %>% 
  dplyr::filter(is.finite(dep_delay), is.finite(arr_delay))


not_cancelled %>% 
  dplyr::group_by(tailnum) %>% 
  dplyr::summarise(delay = mean(arr_delay)) %>% 
  ggplot(aes(x = delay)) +
  geom_freqpoly(binwidth = 10)




not_cancelled %>% 
  dplyr::group_by(tailnum) %>% 
  dplyr::summarise(n = n(),
                   delay = mean(arr_delay)) %>%
  ggplot() +
  geom_point(aes(x = n, y = delay), alpha = .1)


not_cancelled %>% 
  dplyr::group_by(tailnum) %>% 
  dplyr::summarise(n = n(),
                   delay = mean(arr_delay)) %>%
  dplyr::filter(n > 25) %>% 
  ggplot() +
  geom_point(aes(x = n, y = delay), alpha = .1)


not_cancelled %>%
  dplyr::group_by(month) %>% 
  dplyr::slice_head(n = 3)


not_cancelled %>%
  dplyr::group_by(month) %>% 
  dplyr::slice_tail(n = 3)

not_cancelled %>%
  dplyr::group_by(month) %>% 
  dplyr::arrange(dep_delay) %>% 
  dplyr::slice_head(n = 3)

not_cancelled %>%
  dplyr::group_by(month) %>% 
  dplyr::top_n(n = 3, -dep_delay)

not_cancelled %>%
  dplyr::group_by(month) %>% 
  dplyr::sample_n(10)

# A useful short-cut
not_cancelled %>%
  dplyr::group_by(year, month, day) %>% 
  dplyr::summarise(arr_delay1 = mean(arr_delay),
                   arr_delay2 = mean(arr_delay[arr_delay > 0]))


not_cancelled %>%
  dplyr::mutate(arr_delay2 = ifelse(arr_delay > 0, arr_delay, NA)) %>% 
  dplyr::group_by(year, month, day) %>% 
  dplyr::summarise(arr_delay1 = mean(arr_delay),
                   arr_delay2 = mean(arr_delay2, na.rm = T))

# Why some distances are more variable?
not_cancelled %>%
  dplyr::group_by(dest) %>% 
  dplyr::summarise(distance_sd = sd(distance), distance_mean = mean(distance)) %>% 
  dplyr::arrange(desc(distance_sd)) 

# When do the first and last flights leave each day?

not_cancelled %>%  
  dplyr::group_by(year, month, day) %>% 
  dplyr::summarise(earliest = min(dep_time),
                   latest = max(dep_time)) %>%
  dplyr::arrange(desc(earliest)) %>%
  View
print(20)



flights_small %>% 
  dplyr::group_by(day, month, year) %>% 
  dplyr::arrange(desc(arr_delay)) %>%
  dplyr::slice_head(n = 9)

flights_small %>% 
  dplyr::group_by(day, month, year) %>% 
  dplyr::top_n(9, arr_delay)



popular_destinations <- flights %>%
  dplyr::group_by(dest) %>% 
  dplyr::filter(n() > 200)

popular_destinations %>%
  dplyr::relocate(dest)

popular_destinations %>% 
  View()

popular_destinations <- flights %>%
  dplyr::group_by(dest) %>% 
  dplyr::filter(n() > 200) %>%
  dplyr::ungroup()


popular_destinations %>% 
  dplyr::filter(arr_delay > 0) %>% 
  dplyr::group_by(dest) %>% 
  dplyr::mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  dplyr::select(year:day, dest, arr_delay, prop_delay) %>%
  dplyr::ungroup()



popular_destinations %>% 
  dplyr::summarise(ave_del = mean(dep_delay, na.rm =T),
                   .by = c(dest, month))


# Tidy Data ----

# Same data different representations
table1
table2
table3
table4a
table4b

# Compute rate per 10,000
table1 %>% 
  dplyr::mutate(rate = cases / population * 10000)

# Count cases per year
table1 %>% 
  dplyr::count(year, wt = cases)

table1 %>% 
  ggplot(aes(x = year, y = cases)) +
  geom_line(aes(group = country), color = "gray25") +
  geom_point(aes(color = country)) +
  theme_bw()




# Pivoting ----

# Pivot longer 
table4a

tidy4a <- table4a %>% 
  tidyr::pivot_longer(cols = c(`1999`, `2000`),
                      names_to = "year",
                      values_to = "cases")
tidy4b <- table4b %>% 
  tidyr::pivot_longer(cols = 2:3,
                      names_to = "year",
                      values_to = "population")

# We will learn this tomorrow morning, but we reproduced table1
dplyr::left_join(tidy4a, tidy4b)



# Pivot wider 
table2 %>%
  tidyr::pivot_wider(names_from = type,
                     values_from = count)

# Uniting and separating

table3

table3 %>% 
  tidyr::separate(rate, into = c("cases", "population")) %>%
  dplyr::mutate(cases = as.numeric(cases),
                population = as.numeric(population))


table3 %>% 
  tidyr::separate(rate, into = c("cases", "population"),
                  convert = TRUE) 


table3 %>% 
  tidyr::separate(year, into = c("century", "year"),
                  sep = 2, 
                  convert = TRUE) 


# inverse of the separate is unite 
table5 %>% 
  tidyr::unite(year, century, year, sep = "")











