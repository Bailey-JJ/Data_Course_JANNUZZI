# Assignment 7 ####
library(tidyverse)
library(easystats)
library(janitor)
library(patchwork)
library(ggpubr)
library(dplyr)

# Import the Assignment_7/Utah_Religions_by_County.csv data set

ut_religions <- read_csv('Utah_Religions_by_County.csv')

 
# Clean it up into “tidy” shape ####

clean_ut <- 
  ut_religions %>% 
  pivot_longer(cols = 5:17, names_to = "religion", values_to = "religion_proportion") %>%
  clean_names()

clean_ut <- clean_ut[, c("county", "pop_2010", "religious", "non_religious", "religion", "religion_proportion")]
  
  
# Explore the cleaned data set with a series of figures (I want to see you exploring the data set) ####

clean_ut$religion %>% unique
# viewing all religion types

pop_religious <- 
  clean_ut %>% 
  group_by(county) %>% 
  mutate(total_religious = pop_2010*religious,
         total_not_religious = pop_2010*non_religious)

pop_religious$total_religious %>% unique
pop_religious$total_not_religious %>% unique
# I was wanting to see the religious vs non-religious proportions in actual 
# population numbers


pop_religious2 <- 
pop_religious %>% 
  pivot_longer(starts_with("total"), names_to = "religious_vs_non", values_to = "count") %>% 
  mutate(religious_vs_non = case_when(religious_vs_non == "total_religious" ~ "religious",
                                      TRUE ~ "not religious"))

pop_religious2

plot1 <- 
pop_religious2 %>% 
  filter(county == c("Utah County", "Salt Lake County", "Wasatch County", "Davis County", "Morgan County", "Summit County")) %>% 
  ggplot(aes(x = religious_vs_non, y = count)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~county) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Non-Religious vs. Religious",
       y = "Count")

ggsave("Plot_1.png",
       plot = plot1,
       width = 6.29, height = 2.94,
       units = "in")

# here I wanted to see the difference in religious vs non-religious populations,
# within 6 different counties around Utah County and Salt Lake County.
# As expected, there is a high population of religious people in Utah County, with
# higher non-religious in Salt Lake.


## “Does population of a county correlate with the proportion of any specific religious group in that county?” ####

county_correlation <- 
  clean_ut %>% 
  group_by(religion) %>% 
  summarize(correlation = cor(religion_proportion, pop_2010))

county_correlation

plot2 <- 
clean_ut %>% 
  ggplot(aes(x = religion_proportion, y = pop_2010)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~religion, scales = 'free') +
  theme(axis.text.x = element_text(angle = 45))

ggsave("Plot_2.png",
       plot = plot2,
       width = 6.29, height = 2.94,
       units = "in")

# Based on the correlations there is a positive correlation with county population
# and specific religions. Muslims and United Methodist Church had the highest 
# correlation with county, and Southern Bapist Convention and Episcopal Church had the
# lowest correlation.


## “Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?” ####

nonreligious_correlation <- 
  clean_ut %>%  
  group_by(religion) %>% 
  summarize(correlation = cor(religion_proportion, non_religious))

plot3 <- 
clean_ut %>% 
  ggplot(aes(x = religion_proportion, y = non_religious)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  lims(y = 0:1) +
  facet_wrap(~religion, scales = 'free') +
  theme(axis.text.x = element_text(angle = 45))

ggsave("Plot_3.png",
       plot = plot3,
       width = 6.29, height = 2.94,
       units = "in")
# As the proportion of certain religions goes up, the proportion of non-religious 
# people goes down. The proportion of religions like Buddhism, Greek Orthodox, LDS, 
# Muslim, Non-Denominational, Orthodox, and Pentecostal have a stronger correlation 
# with a lower number of non-religious people. Most of these religions and their 
# correlation with non-religious proportion supports what I would think based on
# observation.