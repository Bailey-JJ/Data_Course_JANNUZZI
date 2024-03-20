# Assignment 8 ####

library(tidyverse)
library(dplyr)
library(janitor)
library(patchwork)
library(easystats)
library(performance)
library(modelr)

## loads the “/Data/mushroom_growth.csv” data set ####

mushroom <- read_csv("./mushroom_growth.csv")

mushroom <- janitor::clean_names(mushroom)


##creates several plots exploring relationships between the response and predictors ####

# temp vs growthrate 
mushroom %>% 
  ggplot(aes(x = temperature, y = growth_rate)) +
  geom_jitter()


# humidity vs growthrate
mushroom %>% 
  ggplot(aes(x = humidity, y = growth_rate)) +
  geom_boxplot()


# nitrogen vs growthrate
mushroom %>% 
  ggplot(aes(x = nitrogen, y = growth_rate, col = species)) +
  geom_smooth(method = 'glm')


# light vs growthrate
mushroom %>% 
  ggplot(aes(x = light, y = growth_rate)) +
  geom_jitter() +
  scale_x_continuous(breaks = c(0, 10, 20))


## defines at least 4 models that explain the dependent variable “GrowthRate” ####
 # calculates the mean sq. error of each model

mod1 <- 
  mushroom %>% 
  glm(formula = growth_rate ~ (light * temperature) + species,
      family = 'gaussian')

performance(mod1)
performance(mod1)$RMSE


mod2 <- 
  mushroom %>% 
  glm(formula = growth_rate ~ (light * temperature * species) + humidity,
      family = 'gaussian')

performance(mod2)
performance(mod2)$RMSE


mod3 <- 
  mushroom %>% 
  glm(formula = growth_rate ~ (light * temperature * species) + humidity + nitrogen,
      family = 'gaussian')

performance(mod3)
performance(mod3)$RMSE


mod4 <- 
  mushroom %>% 
  glm(formula = growth_rate ~ (light * temperature * species * humidity) + nitrogen,
      family = 'gaussian')

performance(mod4)
performance(mod4)$RMSE

mod5 <- 
  mushroom %>% 
  glm(formula = growth_rate ~ light * temperature * species * humidity * nitrogen,
      family = 'gaussian')

performance(mod5)
performance(mod5)$RMSE

  # mod5 has the lowest RMSE

## selects the best model you tried ####
compare_performance(mod1, mod2, mod3, mod4, mod5)
compare_performance(mod1, mod2, mod3, mod4, mod5) %>% plot

  # mod4 is the best model, it has the lowest AIC and BIC, the second lowest RMSE, and the second highest R^2.


## adds predictions based on new hypothetical values for the independent variables used in your model ####

unique(mushroom$species)

new_mushroom <- data.frame(
  species = c("P.ostreotus", "P.ostreotus", "P.ostreotus","P.cornucopiae", "P.cornucopiae", "P.cornucopiae"),
  light = c(0, 10, 20, 0, 10, 20),
  nitrogen = c(0, 45, 35, 20, 10, 20),
  humidity = c('High', 'High', 'Low', 'Low', 'High', 'Low'),
  temperature = c(20, 25, 25, 20, 25, 20))


new_mushroom_preds <- gather_predictions(new_mushroom, mod1, mod2, mod3, mod4, mod5)

mushroom$model = "Actual"

## plots these predictions alongside the real data ####
  

mushroom$PredictionType <- "Real"
new_mushroom_preds$PredictionType <- "Hypothetical"

new_mushroom_preds <- 
new_mushroom_preds %>% 
  mutate(growth_rate = pred)

fullpreds <- full_join(mushroom, new_mushroom_preds)


fullpreds %>% 
  ggplot(aes(x = light, y = pred, color = model)) +
  geom_jitter() +
  geom_jitter(aes(x = light, y = growth_rate)) +
  scale_x_continuous(breaks = c(0, 10, 20)) +
  theme_bw() +
  facet_wrap(~model)


# Are any of your predicted response values from your best model scientifically meaningless? Explain.

predict(mod4, new_mushroom)

  # all the values from the best model have some scientific meaning, there are no 
  # impossible values and all response values overlap areas where the Actual Model
  # has points plotted as well.


# In your plots, did you find any non-linear relationships? Do a bit of research online and give a link to at least one resource explaining how to deal with modeling non-linear relationships in R.
  # Yes, some did not have linear relationships.
  # https://www.r-bloggers.com/2023/11/quadratic-regression-in-r-unveiling-non-linear-relationships/


# Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this)

nonlinear <- read.csv("./non_linear_relationship.csv")

summary(nonlinear)

nonlinear %>% 
  ggplot(aes(x = predictor, y = response)) +
  geom_point() +
  stat_smooth(method = "nls", formula = "y ~ (a*x^b)", method.args = list(start = c(a = 1, b = 1)), se = FALSE)