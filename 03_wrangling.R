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

weather |> 
  summarise(mean = mean(wind_speed), std_dev = sd(wind_speed))

weather |> 
  summarise(mean = mean(wind_speed, na.rm = TRUE), 
            std_dev = sd(wind_speed, na.rm = TRUE))

# group by rows ----

weather |> 
  group_by(month) |> 
  summarise(mean = mean(wind_speed, na.rm = TRUE), 
            std_dev = sd(wind_speed, na.rm = TRUE))

diamonds

diamonds |> 
  group_by(cut)

diamonds |> 
  group_by(cut) |> 
  summarise(avg_price = mean(price))

diamonds |> 
  group_by(cut) |> 
  ungroup()

flights |> 
  group_by(origin, month) |> 
  summarise(count = n())

flights |> 
  group_by(origin) |> 
  group_by(month) |> 
  summarise(count = n())

# mutate existing variables ----