# Statistical Inference via Data Science
# https://moderndive.com/v2/
# Chapter 1 - Getting started with data in R
# https://moderndive.com/v2/01-getting-started.html

# 1.4 Explore your first datasets ----

library(nycflights23)
library(dplyr)
library(knitr)

flights

View(flights)

glimpse(flights)

airlines
kable(airlines)
airlines$name
