# ----
# CP BOOTCAMP 2024 : INTRODUCTION TO TIDYVERSE - LECTURE 2
# Author: Mustafa A. Kocak
# Date: March 6, 2024
# Description: Introduction to tidy-verse - Chapter 9 - Visualization 
# ----

#install.packages("tidyverse")
library(tidyverse)
install.packages(c("nycflights13", "gapminder", "Lahman"))

# ----
# Tibbles vs Data Frames
# ----

# for further treatment see 
vignette("tibble")

# we can create tibbles from data frames
iris
as_tibble(iris)

# or we can create new ones with tibble()
# note the recycling and using previously defined columns
tibble( x = 1:5, y = 1,  z = x ^ 2 + y)

data.frame( x = 1:5, y = 1,  z = x ^ 2 + y)

# non-syntactic column names. Refer them by surrounding them with backticks, `:
tb <- tibble(`:)` = "smile", ` ` = "space", `2000` = "number")
tb

# there is also tribble() function - but I don't use it much!
tribble( ~x, ~y, ~z,
         "a", 2, 3.6,
         "b", 1, 8.5)

# tibbles has more screen friendly printing behaviour
tibble( a = lubridate::now() + runif(1e3) * 86400,
        b = lubridate::today() + runif(1e3) * 30,
        c = 1:1e3,
        d = runif(1e3),
        e = sample(letters, 1e3, replace = TRUE))


nycflights13::flights

library(nycflights13)
flights


nycflights13::flights %>% 
  print(n = 10, width = Inf)

nycflights13::flights %>% 
  View()

# Standard subsetting works
df <- tibble(x = runif(5),y = rnorm(5))
df$x
df[["x"]]
df[[1]]

# To use these in a pipe, you’ll need to use the special placeholder .:
df %>% .$x
df %>% .[["x"]]

df$x




# You can turn back to a data frame using as.data.frame
class(as.data.frame(tb))

# The main reason that some older functions don’t work with tibble is the [ function. 
# With base R data frames, [ sometimes returns a data frame, and sometimes returns a 
# vector. With tibbles, [ always returns another tibble.

tb[1,]
tb[,1]
tb[1]
as.data.frame(tb)[1,]
as.data.frame(tb)[,1]
as.data.frame(tb)[1]

# Exercises
# 1. How can you tell if an object is a tibble? (Hint: try printing mtcars, which is a regular data frame).
# 2. Compare and contrast the following operations on a data.frame and equivalent tibble. What is different? Why might the default data frame behaviours cause you frustration?
df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]
# 3. If you have the name of a variable stored in an object, e.g. var <- "mpg", how can you extract the reference variable from a tibble?
# 4. What does tibble::enframe() do? When might you use it?

# -----
# DATA VISUALIZATION
# -----
# ----
# First Steps
# ----
# Do cars with big engines use more fuel than cars with small engines? 
# What does the relationship between engine size and fuel efficiency look like? 
mpg
mpg %>% View() 
?mpg
mpg %>% summary()
mpg %>% Hmisc::describe() # do you need to install Hmisc package?

ggplot(data = mpg)

# our first plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# Does this confirm or refute your hypothesis about fuel efficiency and engine size?

# Exercises
# 1. How many rows are in mpg? How many columns?
# 2. What does the drv variable describe? Read the help for ?mpg to find out.
# 3. Make a scatterplot of hwy vs cyl.
# 4. What happens if you make a scatterplot of class vs drv? Why is the plot not useful?
  
mpg %>% 
  ggplot() +
  geom_point(aes(x = hwy, y = cyl))

mpg %>% 
  ggplot() +
  geom_point(aes(class, drv)) # try geom_jitter

mpg %>% 
  ggplot() +
  geom_jitter(aes(class, drv)) # ? geom_jitter()

# ----
# Aesthetic Mappings
# ----

# adding color as an aesthetic to our graph
# note the default scale and legend
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, 
                           color = class)) +
  theme_bw() +
  labs(x = "Displacement (liters)", 
       y = "Highway Efficiency (galons / mile)",
       color = "", title = "Except 2 seaters,
       larger engine less efficient")
  

# we get a warning if we want to represent class as size aesthetic
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,
                           size = class))

# transparency / alpha
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# shape 
# hey what happened to suv's??
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# we can set aesthetic values manually
# but you need to choose an appropriate value for each aesthetic
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# A quick warning 
ggplot(data = mpg) 
+ geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# Exercises
# 1. What’s gone wrong with this code? Why are the points not blue?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
# 2. Which variables in mpg are categorical? Which variables are continuous? How can you see this information when you run mpg?
# 3. Map a continuous variable to color, size, and shape. How do these aesthetics behave differently for categorical vs. continuous variables?
# 4. What happens if you map the same variable to multiple aesthetics?
# 5. What does the stroke aesthetic do? What shapes does it work with? (Hint: use ?geom_point)
# 6. What happens if you map an aesthetic to something other than a variable name, like aes(colour = displ < 5)? Note, you’ll also need to specify x and y.

# ----
# Facets
# ----
# Facets are an alternative to aesthetics to encode new information 

# note all the variables we give to facet_wrap are discrete 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(drv~ class, nrow = 2, scales = "free_x")

# facet_grid creates a full grid
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ class + drv)

library(ggforce)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,
                           color = (displ > 5) & (hwy > 20))) +
  facet_zoom(xy = (displ > 5) & (hwy > 20),  split = TRUE) +
  theme_bw()

# Exercises
# 1. What happens if you facet on a continuous variable?
# 2. What do the empty cells in plot with facet_grid(drv ~ cyl) mean? How do they relate to this plot?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))
# 3. What plots does the following code make? What does . do?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
# 4. Take the first faceted plot in this section:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
#  What are the advantages to using faceting instead of the colour aesthetic? What are the disadvantages? How might the balance change if you had a larger dataset?
# 5. Read ?facet_wrap. What does nrow do? What does ncol do? What other options control the layout of the individual panels? Why doesn’t facet_grid() have nrow and ncol arguments?
# 6. When using facet_grid() you should usually put the variable with more unique levels in the columns. Why?

# Some notes: Please note facet_wrap has a "scales" argument, which is fairly useful! 
# Install ggforce package. Check out facet_grid_paginate(), facet_wrap_paginate(), and facet_zoom functions

# ----
# Geometric Objects
# ----

# points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# smooth line
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# group is a common aesthetic to create distinct geoms for subsets of the data
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

# geom_smooth works well with "linetype" aesthetic
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# to highlight the difference more
ggplot(mpg) +
  geom_smooth(aes(x = displ, y = hwy, color = drv, lty = drv))

# we can turn part of the legend off
ggplot(mpg) +
  geom_smooth(aes(x = displ, y = hwy, color = drv, lty = drv),
              show.legend = FALSE)

# we can add multiple geoms on to the same canvas
mpg %>%
  ggplot() +
  geom_point(aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(aes(x = displ, y = hwy, color = drv, lty = drv), show.legend = F)

# we can provide the mapping to the ggplot function directly, then each
# following geom inherits them
mpg %>%
  ggplot(aes(x = displ, y = hwy, color = drv, lty = drv)) +
  geom_point() +
  geom_smooth(show.legend = F)

# in each geom you can over-ride/describe the relevant aesthetics
mpg %>%
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(show.legend = F, se = F)

# you can also over-ride the data argument for each geom
mpg %>%
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(data  = mpg[mpg$class == "subcompact", ], 
              show.legend = F, se = F)

# we will learn how to use filter function soon!
mpg %>%
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(data  = filter(mpg, class == "subcompact"), 
              show.legend = F, se = F)

# Exercises
# 1. What geom would you use to draw a line chart? A boxplot? A histogram? An area chart?
# 2. See the slide 15:
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(group = drv), se = F)

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(se = F)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(aes(lty = drv), se = F)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(fill =drv), shape = 21 , 
             color = "white", stroke = 1, size = 2)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color ="white",  size =3 )+
  geom_point(aes(color =drv))

# extra:
library(ggthemes)
mpg %>%
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class), size = 2) +
  geom_smooth(data = dplyr::filter(mpg, class != "2seater"),
               show.legend = F, se = F, color = "black") +
  theme_base(base_family = "GillSans", base_size = 12) +
  scale_color_ptol() +
  labs(x = "Engine Displacement (litres)",
       y = "Fuel efficiency in Highway (mpg)",
       shape = "Drive train", color = "Class", 
       title = "Efficiency decreases as engine gets larger")

# ----
# Statistical Transformations
# ----

# Check out diamonds dataset
diamonds

# we have more high quality diamonds than low quality ones
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
# where the values on the y-axis came from? 
?geom_bar  # ---> it uses stat_count()

# You can generally use geoms and stats interchangeably.
ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

# You can override the stats
demo <- data.frame( cut = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
                    freq = c(1610, 4906, 12082, 13791, 21551))

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

diamonds %>%
  dplyr::count(cut) %>% 
  ggplot() +
  geom_bar(aes(x = cut, y = n), stat = "identity")

#  You may want to use other transformed variables - confusing! 
diamonds %>%
  ggplot() +
  geom_bar(aes(x = cut, y = after_stat(prop), group = 1))

# this works too
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))


# You might want to draw greater attention to the statistical transformation in your code. 
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median)


# Exercises
# 1. What is the default geom associated with stat_summary()? 
# How could you rewrite the previous plot to use that geom function 
# instead of the stat function?
# 2. What does geom_col() do? How is it different to geom_bar()?
# 3. Most geoms and stats come in pairs that are almost always used 
# in concert. Read through the documentation and make a list of all 
# the pairs. What do they have in common?
# 4.  What variables does stat_smooth() compute? What parameters 
# control its behaviour?
# 5. In our proportion bar chart, we need to set group = 1. Why? 
# In other words what is the problem with these two graphs?

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop)))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = after_stat(prop)))

# ----
# Position Adjustments
# ----

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

# barcharts for each clarity are stacked on top of each other
# this is the default behavior for "position" argument.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# other three options are (besides "stack"): "identity", "dodge", "fill

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "stack")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "identity", alpha =.2)


# another useful position argument:
p1 = ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

p2 = ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

cowplot::plot_grid(p1,p2, ncol = 1)

# Exercises
# 1. What is the problem with this plot? How could you improve it?
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()
# 2. What parameters to geom_jitter() control the amount of jittering?
# 3. Compare and contrast geom_jitter() with geom_count().
# 4. What’s the default position adjustment for geom_boxplot()? Create a visualisation of the mpg dataset that demonstrates it.

# ----
# Coordinate Systems
# ----

# coord_flip is useful for box-plots 
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

# a common trick we use with box-plots
ggplot(data = mpg, mapping = aes(x = reorder(class, hwy), y = hwy)) + 
  geom_boxplot() +
  coord_flip() + labs(x = "class")


# coord_polar becomes handy with bar-plots
bar <- ggplot(data = diamonds) + 
  geom_bar( mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar
bar + coord_flip()
bar + coord_polar()

o1 = cowplot::plot_grid(p1, p2)
o2 = cowplot::plot_grid(p3, p4, p5, ncol = 3)


cowplot::plot_grid(o1, o2, ncol =1)

# type coord_ and try to complete with tab.
coord_
# I typically use coord_cartesian and coord_trans

# Exercises
# 1. Turn a stacked bar chart into a pie chart using coord_polar().
# 2. What does labs() do? Read the documentation.
# 3. What does the plot below tell you about the relationship between city and highway mpg? 
#    Why is coord_fixed() important? What does geom_abline() do?
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()

# ------ Extra

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




# Practice: https://simplystatistics.org/posts/2019-08-28-you-can-replicate-almost-any-plot-with-ggplot2/
