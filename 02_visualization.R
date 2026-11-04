# Statistical Inference via Data Science
# https://moderndive.com/v2/
# Chapter 2 - Data visualization
# https://moderndive.com/v2/02-visualization.html


# Packages ----

library(nycflights23)
library(ggplot2)
library(moderndive)
library(tibble)

# Scatterplots ----

ggplot(data = envoy_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_point()

# Changing the transparency

ggplot(data = envoy_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.2)

# Jittering the points

ggplot(data = envoy_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_jitter(width = 30, height = 30)

# Linegraphs ----

ggplot(data = early_january_2023_weather,
       mapping = aes(x = time_hour, y = wind_speed)) +
  geom_line()

# Histograms ----

ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram()

ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(color = "white")

ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(color = "white", fill = "steelblue")

# Adjusting the bins

ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(bins = 20, color = "white")

ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(binwidth = 5, color = "white")

# Facets ----

ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~ month)

ggplot(data = weather, mapping = aes(x = wind_speed)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~ month, nrow = 4)

# Boxplots ----

ggplot(data = weather, mapping = aes(x = month, y = wind_speed)) +
  geom_boxplot()

ggplot(data = weather, mapping = aes(x = factor(month), y = wind_speed)) +
  geom_boxplot()

# Barplots ----