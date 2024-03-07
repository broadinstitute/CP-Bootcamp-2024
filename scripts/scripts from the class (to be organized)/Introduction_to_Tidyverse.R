

# -----
# Introduction to Tidyverse
# Mustafa A. Kocak
# Match 6, 2024
# -----

# Load necessary libraries ----
library(tidyverse)
library(useful)
library(nycflights13)
library(ggthemes)
library(scales)
library(ggrepel)
library(ggridges)
library(ggforce)


# Tibbles vs Data Frames----

vignette("tibble")

iris %>% head
iris_t <- as_tibble(iris) 


tibble(x = 1:5,
       y = 1,
       z = x^2 + 1)

iris1 <- iris[,1:2]
iris2 <- iris[,3:4]

tibble(iris1, iris2)


# you cannot refer to the previous column while creating a data.frame
data.frame(x = 1:5,
       y = 1,
       z = x^2 + 1)

tibble(`:(` = "sad", ` ` = "space", `200` = "number")


iris[, "Sepal.Length", drop = FALSE] %>% head 
iris_t[, "Sepal.Length"]



tribble(~x, ~y, ~z,
        "a", 1, 2,
        "b", 3, 4)



nycflights13::flights
flights

flights %>% 
  print(n = 10, width = Inf)

flights %>% 
  View


df <- tibble(x = runif(5),
             y = rnorm(5))

df$x
df[["x"]]
df[[1]]

df %>%
  .$x

df %>%
  .[["x"]]


as.data.frame(df)

df[1, ]
df[,1]
df[1]


as.data.frame(df)[1, ]
as.data.frame(df)[,1]


# Data Visualization -----


?mpg
mpg %>% View
mpg %>% summary()
mpg %>% str
mpg %>% glimpse()
mpg %>% Hmisc::describe()



ggplot(data = mpg) +
  geom_point(mapping = aes(x = hwy, y = cyl))
  


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

dim(mpg)
mpg
nrow(mpg)
ncol(mpg)



ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))

ggplot(data = mpg) +
  geom_jitter(mapping = aes(x = class, y = drv),
              width = 0.1, height = 0.1)


?geom_jitter


ggplot(mpg) +
  geom_point(aes(x = displ,
                 y = hwy,
                 color = class)) +
  theme_bw(base_size = 14, base_family = "GillSans") +
  labs(x = "Engine Displacement (liters)",
       y = "Highway Efficiency (galons/mile)",
       color = "", 
       title = "Except 2 seaters, larger engine less efficient",
       subtitle = "I can add a subtitle too!")


ggplot(mpg) +
  geom_point(mapping = aes(x = displ, y = hwy,
                           size = class))



ggplot(mpg) +
  geom_point(mapping = aes(x = displ, y = hwy,
                           alpha = class))


ggplot(mpg) +
  geom_point(mapping = aes(x = displ, y = hwy,
                           shape = class)) +
  scale_shape_manual(values = c(1:7))




ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, alpha = .75) +
  geom_text_repel(data = mpg[mpg$class == "2seater", ], 
    aes(label = class)) + 
  theme_bw(base_size = 14, base_family = "GillSans") +
  scale_color_pander() + 
  labs(x = "Engine Displacement (liters)",
       y = "Highway Efficiency (galons/mile)",
       color = "", 
       title = "Except 2 seaters, larger engine less efficient")


ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, alpha = .75) +
  geom_text_repel(aes(label = ifelse(class %in% c("2seater", "minivan"), class, NA)),
                  color = "black") + 
  theme_bw(base_size = 14, base_family = "GillSans") +
  scale_color_pander() + 
  labs(x = "Engine Displacement (liters)",
       y = "Highway Efficiency (galons/mile)",
       color = "", 
       title = "Except 2 seaters, larger engine less efficient")



ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy),
                 colour = "firebrick3")
mpg

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy,
                 color = cty)) +
  scale_color_viridis_d()



# Facets -----

ggplot(mpg) +
  geom_point(aes(x = displ,
                 y = hwy)) +
  facet_wrap(. ~ class ,
             nrow = 1)


ggplot(mpg) +
  geom_point(aes(x = displ,
                 y = hwy)) +
  facet_grid(cyl ~ class + drv, scales = "free")


ggplot(mpg) +
  geom_point(aes(x = displ,
                 y = hwy)) +
  facet_wrap_paginate(cyl ~ class + drv, scales = "free",
                      nrow = 2, ncol = 3,
                      page = 3)



ggplot(mpg) +
  geom_point(aes(x = displ,
                 y = hwy))+
  facet_zoom(xy = (displ > 5) & (hwy > 20),
             split = TRUE) +
  theme_bw()


# Geometric Objects ----

# points
ggplot(mpg) +
  geom_point(aes(x = displ,
                 y = hwy))

# smooth line
ggplot(mpg) +
  geom_smooth(aes(x = displ,
                 y = hwy))


# smooth line
ggplot(mpg) +
  geom_smooth(aes(x = displ,
                  y = hwy,
                  linetype = drv,
                  color = drv),
              show.legend = FALSE)

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(lty = drv), show.legend = FALSE)


mpg %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class), size = 2) +
  geom_smooth(data = dplyr::filter(mpg, class != "2seater"),  # mpg[mpg$class != "2seater",], 
              se = FALSE, color = "black") + 
  scale_color_ptol() +
  theme_base(base_size = 12, base_family = "GillSans") +
  labs(x = "Engine Displacement (litres)",
       y = "Fuel Efficiency in Highway (mpg)",
       color = "Class", shape = "Drive train",
       title= "Efficiency decreases as engine gets larger",
       subtitle = "2 Seaters are excluded from the fitted curve")


# practice 

ggplot(mpg,aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)


ggplot(mpg,aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(group = drv), se = FALSE)


ggplot(mpg,aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(group = drv), se = FALSE)

ggplot(mpg,aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(se = FALSE)


ggplot(mpg,aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(aes(lty = drv), se = FALSE)



ggplot(mpg,aes(x = displ, y = hwy)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = drv)) 




mpg_tiny <- mpg %>% 
  dplyr::filter(cyl %in% c(4,6),
                drv %in% c("4", "f"))


p1 =mpg_tiny %>% 
  ggplot(aes(x = drv, y = hwy,
             color = drv, fill =drv)) +
  geom_violin(color = "black", 
              alpha = .1, show.legend = FALSE) + 
  geom_sina(show.legend = FALSE) + 
  geom_boxplot(alpha = 0, color = "black", width = .1, show.legend = FALSE)  +
  theme_bw() +
  facet_wrap(cyl ~., nrow = 1)



p2 = mpg_tiny %>% 
  ggplot(aes(x = as.factor(cyl), y = hwy,
             color = cyl, fill =cyl)) +
  geom_violin(color = "black", 
              alpha = .1, show.legend = FALSE) + 
  geom_sina(show.legend = FALSE) + 
  geom_boxplot(alpha = 0, color = "black", width = .1, show.legend = FALSE)  +
  theme_bw() +
  facet_wrap(drv ~., nrow = 1)

cowplot::plot_grid(p1,p2, ncol = 1)
  
mpg_tiny %>% 
  ggplot() +
  geom_density(aes(x = hwy,
                   color = class))

mpg_tiny %>% 
  ggplot() +
  geom_density(aes(x = hwy,
                   fill = class), alpha = .5)


mpg_tiny %>% 
  ggplot() +
  geom_histogram(aes(x = hwy,
                   fill = class), alpha = .5, 
                binwidth = 10)


mpg_tiny %>% 
  ggplot() +
  geom_histogram(aes(x = hwy,
                     fill = class), 
                 position = "identity", 
                 alpha = .5)


library(ggridges)
mpg_tiny %>% 
  ggplot() +
  geom_density_ridges(aes(x = hwy,
                   fill = class,
                   y = class), alpha = .5)



library(ggbeeswarm)
ggplot(mpg,aes(class, hwy,
               color = drv)) + 
  geom_quasirandom()

ggplot(mpg,aes(class, hwy,
               color = drv)) + 
  geom_beeswarm()



mpg %>% 
  ggplot() +
  geom_bar(aes(x = class))

mpg %>% 
  ggplot() +
  stat_count(aes(x = class))

mpg %>% 
  dplyr::count(class) %>% 
  ggplot() +
  geom_bar(aes(x = class, y = n), stat = "identity")
  

mpg %>% 
  ggplot() +
  geom_bar(aes(x = class, y = after_stat(count)))




mpg %>% 
  ggplot() +
  geom_bar(aes(x = class, y = after_stat(prop), group = 1))


mpg %>% 
  ggplot() +
  stat_summary(aes(x = class, y = hwy),
               fun = median,
               fun.min = min, fun.max = max) 



mpg %>% 
  ggplot() +
  geom_bar(aes(x = class, y = after_stat(prop)))


mpg %>% 
  ggplot() +
  geom_bar(aes(x = class, y = after_stat(prop), group = 1))


mpg %>% 
  ggplot() +
  geom_bar(aes(x = class, fill = drv, y = after_stat(prop)))

mpg %>% 
  ggplot() +
  geom_bar(aes(x = class, fill = drv, y = after_stat(prop), group = drv))


diamonds

diamonds %>%
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = "stack")


diamonds %>%
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = "dodge")

diamonds %>%
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = "fill")


diamonds %>%
  ggplot() +
  geom_bar(aes(x = cut, fill = clarity),
           position = "identity",
           alpha = 0.5)


diamonds %>%
  ggplot() +
  geom_bar(aes(x = cut, color = clarity),
           position = "identity",
           alpha = 0)


diamonds %>% 
  ggplot() +
  geom_histogram(aes(x = carat, fill = color))


diamonds %>% 
  ggplot() +
  geom_histogram(aes(x = carat, fill = color),
                 position = "dodge") # ?

diamonds %>% 
  ggplot() +
  geom_histogram(aes(x = carat, fill = color),
                 position = "fill") 


diamonds %>% 
  ggplot() +
  geom_histogram(aes(x = carat, fill = color),
                 position = "identity", alpha = .5)  +
  scale_fill_ptol() + theme_bw()


mpg %>% 
  ggplot() +
  geom_point(aes(x = displ, y = hwy),
             position = "jitter")

mpg %>% 
  ggplot() +
  geom_boxplot(aes(x = class, y = hwy))


mpg %>% 
  ggplot() +
  geom_boxplot(aes(x = as.character(cyl),
                   y = hwy))

mpg %>% 
  ggplot() +
  geom_boxplot(aes(x = as.character(cyl),
                   y = hwy,
                   color = class),
               position = "identity") +
  labs(x = "cyl")



mpg %>% 
  ggplot() +
  geom_boxplot(aes(x = as.factor(cyl),
                   y = hwy))


mpg %>% 
  ggplot() +
  geom_boxplot(aes(y = reorder(trans, displ, median),
                   x = displ))




mpg %>% 
  ggplot() +
  geom_boxplot(aes(x = reorder(trans, displ, median),
                   y = displ))


mpg %>% 
  ggplot() +
  geom_boxplot(aes(x = reorder(trans, displ, median),
                   y = displ)) +
  coord_flip()


?fct_infreq 
reorder()
  


  
c(2,4,6) %>% 
  as.factor()

# Coordinate Systems ----

mpg %>% 
  ggplot(aes(x = class , y= hwy)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))



mpg %>% 
  ggplot(aes(x = class , y= hwy)) +
  geom_boxplot() +
  coord_flip()



mpg %>% 
  ggplot(aes(x = reorder(class,hwy) , y= hwy)) +
  geom_boxplot() +
  coord_flip()

bar <- ggplot(diamonds) +
  geom_bar(aes(x = cut, fill = cut),
           show.legend = FALSE, width = 1) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)


bar2 <- bar + coord_flip()

bar3 <- bar + coord_polar()

p1 <- cowplot::plot_grid(bar, bar2, ncol = 1)
cowplot::plot_grid(p1, bar3, nrow = 1)



mpg %>% 
  ggplot() +
  geom_point(aes(x = displ, y= hwy)) +
  coord_trans(x = "log", y = "sqrt")


mpg %>% 
  ggplot(aes(x = cty, y= hwy)) +
  geom_point() +
  geom_smooth() + 
  coord_cartesian(xlim = c(0, 50),
                  ylim = c(0, 50))

mpg %>% 
  ggplot(aes(x = cty, y= hwy)) +
  geom_point() +
  geom_smooth() +
  xlim(10,20) + ylim(10,30)

mpg %>% 
  ggplot(aes(x = cty, y= hwy)) +
  geom_point() +
  geom_smooth() +
  geom_abline() + 
  geom_vline(aes(xintercept = 15)) + 
  geom_hline(aes(yintercept = 20)) + 
  geom_abline(slope = 2, intercept = 1, lty = 2) + 
  coord_cartesian(xlim = c(10,40), ylim = c(10,40))

















