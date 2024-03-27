# Assignment 9 
library(tidyverse)
library(easystats)
library(patchwork)
library(GGally)

dat <- read_csv("./GradSchool_Admissions.csv")

ggpairs(dat, columns = 1:4)

mod <- 
glm(data = dat,
    formula = as.logical(admit) ~ gpa * rank,
    family = 'binomial')

dat$pred <- predict(mod, dat, type = 'response')

dat %>% 
  ggplot(aes(x = gpa, y = pred, color = factor(rank))) +
  geom_point() +
  geom_smooth(alpha = 0.25) +
  labs(x = "GPA", y = "Admission Prediction")

mod2 <- 
  glm(data = dat,
      formula = as.logical(admit) ~ (gpa + gre) * rank,
      family = 'binomial')

dat$pred_mod2 <- predict(mod2, dat, type = 'response')

dat %>% 
  ggplot(aes(x = gre, y = pred_mod2, color = factor(rank))) +
  geom_point() +
  geom_smooth(alpha = 0.25) +
  labs(x = "GRE", y = "Admission Prediction")

mod3 <- 
  glm(data = dat,
      formula = as.logical(admit) ~ gpa * gre * rank,
      family = 'binomial')

dat$pred_mod3 <- predict(mod3, dat, type = 'response')

dat %>% 
  ggplot(aes(x = gre, y = pred_mod3, color = factor(rank))) +
  geom_point() +
  geom_smooth(alpha = 0.25) +
  labs(x = "GRE", y = "Admission Prediction")

mod4 <- 
  glm(data = dat,
      formula = as.logical(admit) ~ (gpa + gre + rank),
      family = 'binomial')

dat$pred_mod4 <- predict(mod4, dat, type = 'response')

dat %>% 
  ggplot(aes(x = gre, y = pred_mod4, color = factor(rank))) +
  geom_point() +
  geom_smooth(alpha = 0.25) +
  labs(x = "GRE", y = "Admission Prediction") +
  guides(color = guide_legend(title = "Rank"))


mod5 <- 
  glm(data = dat,
      formula = as.logical(admit) ~ gre * rank,
      family = 'binomial')

dat$pred_mod5 <- predict(mod5, dat, type = 'response')

dat %>% 
  ggplot(aes(x = gre, y = pred_mod5, color = factor(rank))) +
  geom_point() +
  geom_smooth(alpha = 0.25) +
  labs(x = "GRE", y = "Admission Prediction")

compare_performance(mod, mod2, mod3, mod4, mod5)

# mod3 has the lowest RMSE score, with mod4 having the second lowest.
# mod4 has the lowest AIC score, with mod3 having the second lowest.
# Due to mod4 having the lower AIC value, this will be determined as the best model.