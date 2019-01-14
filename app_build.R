library(dplyr)
library(boot)
library(plotrix)
library(magrittr)
library(ggplot2)
library(ggforce)
library(purrr)

sample_data <- data_frame(values =  rnorm(1000))

sample_stats <- c(mean(data$y), std.error(data$y))

p1 <- ggplot(sample_data, aes(values)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, colour = "firebrick1", size = 1) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p1 <- p1 +  geom_point(aes(mean(sample_data$values), 0), colour = "dodgerblue", size = 2) +
  geom_errorbarh(aes(xmin = sample_stats[1] - sample_stats[2] * 2, 
                          xmax = sample_stats[1] + sample_stats[2] * 2,
                          y = 0),
                      height = 0.2,
                      colour = "dodgerblue",
                      size = 1) 

p2 <- p1 + geom_point(aes(mean(sample_data$values), 0), colour = "dodgerblue", size = 8) +
  geom_errorbarh(aes(xmin = sample_stats[1] - sample_stats[2] * 2, 
                                xmax = sample_stats[1] + sample_stats[2] * 2,
                                y = 0),
                            height = 0.2,
                            colour = "dodgerblue",
                            size = 2) +
  coord_cartesian(xlim = c(sample_stats[1] - sample_stats[2] * 3, sample_stats[1] + sample_stats[2] * 3),
                           ylim = c(-0.2, 0.2)) 

cowplot::plot_grid(p1, p2, ncol = 1)


mean(1:10)
1:10 %>% mean()

sample_stats[1] - sample_stats[2] * 2 
sample_stats[1] + sample_stats[2] * 2 

data$y %>% mean()

boot(data$y, statistic = function(x, index) mean(x[index]), R = 1000) %>% 
  norm.ci(conf = 0.95) 
  pluck("bca") %>% 
  .[4:5]
  
bcanon(data$y, 1000, mean, alpha = c(0.025, 0.975)) %>% 
  pluck("confpoints") %>% 
  as.double() %>% 
  .[3:4]

data$y %>% mean() - boot(data$y, statistic = function(x, index) mean(x[index]), R = 1000) %>% 
  boot.ci(type = "bca", conf = 0.95) %>% 
  pluck("bca") %>% 
  .[4]

boot(data$y, statistic = function(x, index) mean(x[index]), R = 1000) %>% 
  boot.ci(type = "bca", conf = 0.95) %>% 
  pluck("bca") %>% 
  .[5] - data$y %>% mean()

data$y %>% mean() - bcanon(data$y, 1000, mean, alpha = c(0.025, 0.975)) %>% 
  pluck("confpoints") %>% 
  as.double() %>% 
  .[3]

bcanon(data$y, 1000, mean, alpha = c(0.025, 0.975)) %>% 
  pluck("confpoints") %>% 
  as.double() %>% 
  .[4] - data$y %>% mean()

a <- boot(data$y, statistic = function(x, index) mean(x[index]), R = 1000) %>% 
  boot.ci(type = "bca")

a$bca

library(fGarch)

data$y %>% std.error()

ifelse(1 == 1, c(mean(data$y) - 2 * std.error(data$y), mean(data$y) + 2 * std.error(data$y)), "no")

c(mean(data$y) - 2 * std.error(data$y), mean(data$y) + 2 * std.error(data$y))

rsnorm(100, mean = 0, sd = 1, xi = -100) %>% hist()

library(magrittr)

c("95" = 1, "5" = 2) %>% typeof()

library(magrittr)

library(tidyverse)

map(c(10, 1000), ~ rsnorm(100, mean = 0, sd = 1, xi = .)) %>% 
  bind_cols() %>% 
  gather(variable, value) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ variable)


