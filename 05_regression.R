# Statistical Inference via Data Science
# https://moderndive.com/v2/
# Chapter 5 - Simple Linear Regression
# https://moderndive.com/v2/05-regression.html


# Packages ----

library(moderndive)
library(tidyverse)

# One numerical explanatory variable ----

## Exploratory data analysis ----

UN_data_ch5 <- un_member_states_2024 |> 
  select(iso,
         life_exp = life_expectancy_2022,
         fert_rate = fertility_rate_2022,
         obes_rate = obesity_rate_2016) |> 
  drop_na()

glimpse(UN_data_ch5)

UN_data_ch5 |> 
  slice_sample(n = 5)

UN_data_ch5 |> 
  summarise(
    mean_life_exp = mean(life_exp),
    mean_fert_rate = mean(fert_rate),
    median_life_exp = median(life_exp),
    median_fert_rate = median(fert_rate)
  )

UN_data_ch5 |> 
  select(fert_rate, life_exp) |> 
  tidy_summary()

UN_data_ch5 |> 
  tidy_summary(columns = c(fert_rate, life_exp))

UN_data_ch5 |> 
  get_correlation(formula = fert_rate  ~ life_exp)

UN_data_ch5 |> 
  summarise(correlation = cor(fert_rate, life_exp))


UN_data_ch5 |> 
  mutate(life_exp_diff = life_exp - mean(life_exp),
         fert_rate_diff = fert_rate  - mean(fert_rate)) |> 
  summarise(
    cor = sum(life_exp_diff * fert_rate_diff) / sqrt(sum(life_exp_diff^2) * sum(fert_rate_diff^2))
  )

ggplot(UN_data_ch5,
       aes(x = life_exp, y = fert_rate)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "Life Expectancy", y = "Fertility Rate",
       title = "Relationship of life expectancy and  fertility rate")

## Simple linear regression ----

# Fit regression model
demographics_model <- lm(fert_rate ~ life_exp,  data = UN_data_ch5)

# Get regression coefficients
coef(demographics_model)

## Observed/fitted values and residuals ----

UN_data_ch5 |> 
  filter(iso== "BIH") |> 
  mutate(fert_rate_pred = coef(demographics_model)[1] + life_exp * coef(demographics_model)[2])

regression_points <- get_regression_points(demographics_model)
regression_points

# One categorical explanatory variable ----

## Exploratory data analysis ----

gapminder2022 <- un_member_states_2024 |>
  select(country, life_exp = life_expectancy_2022, continent, gdp_per_capita) |> 
  drop_na()

glimpse(gapminder2022)

gapminder2022 |> 
  sample_n(size =  3)

gapminder2022 |> 
  select(life_exp, continent) |> 
  tidy_summary()

ggplot(gapminder2022, aes(x = life_exp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy", 
       y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies")

ggplot(gapminder2022, aes(x = life_exp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Life expectancy", 
       y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies") +
  facet_wrap(~ continent, nrow = 2)

ggplot(gapminder2022, aes(x = continent, y = life_exp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent")

life_exp_by_continent <- gapminder2022 |>
  group_by(continent) |>
  summarize(median = median(life_exp), mean = mean(life_exp))
life_exp_by_continent

life_exp_by_continent |> 
  mutate(diff_vs_Africa = mean - life_exp_by_continent$mean[life_exp_by_continent$continent == "Africa"])

## Linear  regression ----

life_exp_model <- lm(life_exp ~ continent, data = gapminder2022)
coef(life_exp_model)

## Observed/fitted values and residuals ----

get_regression_points(life_exp_model, ID = "country")

# Related topics ----

## Correlation is not necessarily causation ----

ggplot(data = un_member_states_2024,
       aes(x = hdi_2022, y = life_expectancy_2022)) +
  geom_point() +
  labs(x = "Human Development Index (HDI)", y = "Life Expectancy")

ggplot(data = un_member_states_2024,
       aes(x = hdi_2022, y = fertility_rate_2022)) +
  geom_point() +
  labs(x = "Human Development Index (HDI)", y = "Fertility Rate")

un_member_states_2024 |> 
  get_correlation(life_expectancy_2022 ~ hdi_2022, na.rm = TRUE)

un_member_states_2024 |> 
  get_correlation(fertility_rate_2022 ~ hdi_2022, na.rm = TRUE)

## Best-fitting line ----

# Fit regression model and regression points
demographics_model <- lm(fert_rate ~ life_exp, data = UN_data_ch5)
regression_points <- get_regression_points(demographics_model)

# Compute sum of squared residuals
regression_points |>
  mutate(squared_residuals = residual^2) |>
  summarize(sum_of_squared_residuals = sum(squared_residuals))

## get_regression_x() functions ----

library(broom)
library(janitor)
demographics_model |>
  augment() |>
  clean_names() |>
  select(-c("std_resid", "hat", "sigma", "cooksd", "std_resid"))
