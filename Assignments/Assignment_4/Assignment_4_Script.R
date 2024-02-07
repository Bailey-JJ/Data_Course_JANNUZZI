# Assignment 4

library(tidyverse)

meteor <- read_csv("./Assignments/Assignment_4/meteorites.csv")

# Looking at the data
meteor %>%
  filter(!is.na(lat),
         !is.na(long),
         !is.na(geolocation),
         !is.na(mass),
         year > 2000, mass < 150000,
         lat > 0) %>%
  print(n = 200)


# Mass vs. Year
meteor %>%
  filter(!is.na(lat),
         !is.na(long),
         !is.na(geolocation),
         !is.na(mass),
         year > 2000, mass < 150000,
         lat > 0) %>%
  group_by(year, mass) %>%
  ggplot(mapping = aes(x = year,
                       y = mass,
                       color = class)) + 
  geom_point() +
  theme(legend.position = "none")


# Mass vs. Class
meteor %>%
  filter(!is.na(lat),
         !is.na(long),
         !is.na(geolocation),
         !is.na(mass),
         year > 2000, mass < 150000,
         lat > 0) %>%
  group_by(year, mass) %>%
  ggplot(mapping = aes(x = class,
                       y = mass,
                       color = year)) + 
  geom_point() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90))

  
  
  
