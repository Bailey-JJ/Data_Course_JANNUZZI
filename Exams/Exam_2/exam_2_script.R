# Exam 2 ####
library(tidyverse)
library(readxl)
library(patchwork)
library(skimr)
library(janitor)
library(ggpubr)
library(easystats)

# [1] Read in the unicef data (10 pts) ####

unicef <- read_csv("unicef-u5mr.csv")

# [2] Get it into tidy format (10 pts) ####


unicef1 <-  
unicef %>% 
  pivot_longer(starts_with("U5MR."),
               names_to = "Year",
               values_to = "U5MR",
               names_prefix = "U5MR.",
               names_transform = as.numeric)


# [3] Plot each country’s U5MR over time (20 points) ####

unicef_plot_1 <-  
unicef1 %>% 
  ggplot(aes(x = Year, y = U5MR)) +
  geom_path() +
  facet_wrap(~Continent) +
  theme_bw()

unicef_plot_1

# [4] Save this plot as LASTNAME_Plot_1.png (5 pts) 

ggsave("JANNUZZI_Plot_1.png",
       plot = unicef_plot_1,
       width = 6.29, height = 2.94,
       units = "in")

# [5] Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts) ####

unicef_plot_2 <- 
unicef1 %>% 
  filter(!is.na(U5MR), !is.na(Continent), !is.na(Year)) %>% 
  group_by(Year, Continent) %>%
  summarise(Mean_U5MR = mean(U5MR)) %>% 
  ggplot(aes(x = Year, y = Mean_U5MR, group = Continent, color = Continent)) +
  geom_line(size = 1.5) +
  theme_bw()

unicef_plot_2

# [6] Save that plot as LASTNAME_Plot_2.png (5 pts)

ggsave("JANNUZZI_Plot_2.png",
       plot = unicef_plot_2,
       width = 6.29, height = 2.94,
       units = "in")

# [7] Create three models of U5MR (20 pts) ####
# - mod1 should account for only Year
mod1 <- glm(data = unicef1, formula = U5MR ~ Year)
summary(mod1)

# - mod2 should account for Year and Continent
mod2 <- glm(data = unicef1, formula = U5MR ~ Year + Continent)
summary(mod2)

# - mod3 should account for Year, Continent, and their interaction term
mod3 <- glm(data = unicef1, formula = U5MR ~ Year * Continent)
summary(mod3)


# [8] Compare the three models with respect to their performance
compare_performance(mod1, mod2, mod3)
compare_performance(mod1, mod2, mod3) %>% plot

# mod3 has the highest R^2, meaning it can explain the variation in the 
# data the most. It also have the lowest RMSE, meaning it is the closest
# to reality. mod2 is only 0.04 lower than mod3 so it can still explain 
# the variation well. mod1 has the lowest R^2, meaning it has the 
# lowest explanatory power. 
# In conclusion, mod3 is the best model.

# [9] Plot the 3 models’ predictions like so: (10 pts) ####

unicef1$mod_1 <- predict(mod1, unicef1)

unicef1$mod_2 <- predict(mod2, unicef1)

unicef1$mod_3 <- predict(mod3, unicef1)

unicef1 %>%
  pivot_longer(cols = c(mod_1, mod_2, mod_3),
               names_to = "model",
               values_to = "Predicted_U5MR") %>% 
  ggplot(aes(x = Year, y = Predicted_U5MR, color = Continent)) +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~model) +
  theme_bw()
  


 # extra credit ####
# Using your preferred model, predict what the U5MR would be for Ecuador in
# the year 2020. The real value for Ecuador for 2020 was 13 under-5 deaths
# per 1000 live births. How far off was your model prediction???

mod3_predict <- predict(mod3, data.frame(Year = 2020, Continent = "Americas", CountryName = "Ecuador"))

mod3_residual <- 13 - mod3_predict
mod3_residual

mod4 <- glm(data = unicef1, formula = U5MR ~ Year * Continent + CountryName)
mod4_predict <- predict(mod4, data.frame(Year = 2020, Continent = "Americas", CountryName = "Ecuador", Region = "South America"))

mod4_residual <- 13 - mod4_predict
mod4_residual

# mod4s prediction was 1.938492 higher than reality
