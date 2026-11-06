# Statistical Inference via Data Science
# https://moderndive.com/v2/
# Chapter 3 - Data wrangling
# https://moderndive.com/v2/03-wrangling.html


# Packages ----

library(dplyr)
library(ggplot2)
library(nycflights23)

# filter rows ----

phoenix_flights <- flights |> 
  filter(dest == "PHX")
View(phoenix_flights)

btv_sea_flights_fall <- flights |> 
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & month >= 10)
View(btv_sea_flights_fall)

btv_sea_flights_fall <- flights |> 
  filter(origin == "JFK", (dest == "BTV" | dest == "SEA"), month >= 10)
View(btv_sea_flights_fall)

not_BTV_SEA <- flights |> 
  filter(!(dest == "BTV" | dest == "SEA"))
View(not_BTV_SEA)

many_airports <- flights |> 
  filter(dest == "SEA" | dest == "SFO" | dest == "PHX" | 
           dest == "BTV" | dest == "BDL")

many_airports <- flights |> 
  filter(dest %in% c("SEA", "SFO", "PHX", "BTV", "BDL"))
View(many_airports)

# summarize variables ----

