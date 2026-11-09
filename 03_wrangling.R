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

weather |> 
  mutate(temp_in_C = (temp - 32) / 1.8)

weather |> 
  mutate(temp_in_C = (temp - 32) / 1.8) |> 
  group_by(month) |> 
  summarise(
    mean_temp_in_F = mean(temp, na.rm = TRUE),
    mean_temp_in_C = mean(temp_in_C, na.rm = TRUE)
  )

flights |> 
  mutate(gain = dep_delay - arr_delay)

flights |> 
  mutate(gain = dep_delay - arr_delay) |> 
  summarize(
    min = min(gain, na.rm = TRUE),
    q1 = quantile(gain, 0.25, na.rm = TRUE),
    median = quantile(gain, 0.5, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE),
    max = max(gain, na.rm = TRUE),
    mean = mean(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain))
  )

flights |> 
  mutate(gain = dep_delay - arr_delay) |> 
  ggplot(mapping = aes(x = gain)) +
  geom_histogram(color = "white", bins = 20)

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )

# arrange and sort rows ----

flights |> 
  group_by(dest) |> 
  summarize(num_flights = n())

flights |> 
  group_by(dest) |> 
  summarize(num_flights = n()) |> 
  arrange(num_flights)

flights |> 
  group_by(dest) |> 
  summarize(num_flights = n()) |> 
  arrange(desc(num_flights))

# join data frames ----

flights |> 
  inner_join(airlines, by = "carrier")

flights |> 
  inner_join(airports, by = c("dest" = "faa"))

flights |> 
  group_by(dest) |> 
  summarise(num_flights = n()) |> 
  arrange(desc(num_flights)) |> 
  inner_join(airports, by = c("dest" = "faa")) |> 
  rename(airport_name = name)

flights |> 
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))

# Other verbs ----

flights |> 
  select(carrier, flight)

flights |> 
  select(-year)

flights |> 
  select(month:day, arr_time:sched_arr_time)

flights |> 
  select(starts_with("a"))

flights |> 
  select(ends_with("delay"))

flights |> 
  select(contains("time"))

flights |> 
  select(year, month, day, hour, minute, time_hour, everything())

flights |> 
  relocate(hour, minute, time_hour, .after = day)

flights |> 
  select(dep_time, arr_time) |> 
  rename(departure_time = dep_time, arrival_time = arr_time)

named_dests <- flights |>
  group_by(dest) |>
  summarize(num_flights = n()) |>
  arrange(desc(num_flights)) |>
  inner_join(airports, by = c("dest" = "faa")) |>
  rename(airport_name = name)

named_dests |> 
  top_n(n = 10, wt = num_flights) |> 
  arrange(desc(num_flights))
