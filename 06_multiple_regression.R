# Statistical Inference via Data Science
# https://moderndive.com/v2/
# Chapter 6 - Multiple Regression
# https://moderndive.com/v2/06-multiple-regression.html


# Packages ----

library(tidyverse)
library(moderndive)
library(ISLR2)

# One numerical and one categorical explanatory variable ----

## Exploratory Data Analysis ----

UN_data_ch6 <- un_member_states_2024 |> 
  select(country, life_exp = life_expectancy_2022,
         fert_rate = fertility_rate_2022,
         income = income_group_2024) |> 
  mutate(income = factor(
    income, 
    levels = c("Low income", "Lower middle income", 
               "Upper middle income", "High income"))) |> 
  drop_na()

glimpse(UN_data_ch6)

UN_data_ch6 |> sample_n(size = 10)

UN_data_ch6 |> 
  select(life_exp, fert_rate, income) |> 
  tidy_summary()

UN_data_ch6 |>
  get_correlation(formula = fert_rate ~ life_exp)

ggplot(UN_data_ch6, aes(x = life_exp, y = fert_rate, color = income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
  labs(x = "Life Expectancy", y = "Fertility Rate", color = "Income group")

## Model with interactions ----

one_factor_model <- lm(fert_rate ~ income, data = UN_data_ch6)
coef(one_factor_model)

# Fit regression model and get the coefficients of the model
model_int <- lm(fert_rate ~ life_exp * income, data = UN_data_ch6)
coef(model_int)

## Model without interactions ----

ggplot(UN_data_ch6, aes(x = life_exp, y = fert_rate, color = income)) +
  geom_point() +
  geom_parallel_slopes(se = FALSE) +
  labs(x = "Life expectancy", y = "Fertility rate", color = "Income group")

# Fit regression model:
model_no_int <- lm(fert_rate ~ life_exp + income, data = UN_data_ch6)

# Get the coefficients of the model
coef(model_no_int)

## Observed responses, fitted values, and residuals ----

model_int

regression_points <- get_regression_points(model_int)
regression_points

# Two numerical explanatory variables ----

## Exploratory data analysis ----

credit_ch6 <- Credit |> as_tibble() |> 
  select(debt = Balance, credit_limit = Limit, 
         income = Income, credit_rating = Rating, age = Age)

glimpse(credit_ch6)

credit_ch6 |> sample_n(size = 5)

credit_ch6 |> select(debt, credit_limit, income) |> tidy_summary()

ggplot(credit_ch6, aes(x = credit_limit, y = debt)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Credit card debt (in $)", 
       title = "Debt and credit limit") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(credit_ch6, aes(x = income, y = debt)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Credit card debt (in $)", 
       title = "Debt and income") +
  geom_smooth(method = "lm", se = FALSE)

credit_ch6 |> get_correlation(debt ~ credit_limit)
credit_ch6 |> get_correlation(debt ~ income)

credit_ch6 |> select(debt, credit_limit, income) |> cor()

credit_ch6 |> get_correlation(debt ~ 1000 * income)  # the correlation coefficient is invariant to linear transformations

## Multiple regression with two numerical regressors ----

debt_model <- lm(debt ~ credit_limit + income, data = credit_ch6)
coef(debt_model)

# Fit regression model and get the coefficients of the model
simple_model <- lm(debt ~ income, data = credit_ch6)
coef(simple_model)

## Observed/fitted values and residuals ----

get_regression_points(debt_model)
