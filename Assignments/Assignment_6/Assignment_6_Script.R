# Assignment 6 ####
library(tidyverse)
library(patchwork)
library(gganimate)
library(readxl)
library(janitor)

dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

# Cleans this data into tidy (long) form ####
# Creates a new column specifying whether a sample is from soil or water

dat <- 
  dat %>% 
  clean_names()

dat <- 
  dat %>% 
  mutate(type = case_when(sample_id == "Clear_Creek" | sample_id == "Waste_Water" ~ "Water",
                          TRUE ~ "Soil"))

dat <- dat[, c("sample_id", "type", "rep", "well", "dilution", "substrate", "hr_24", 'hr_48', "hr_144")]


dat <- 
  dat %>% 
  clean_names() %>% 
  pivot_longer(starts_with("hr_"),
               names_to = "Time",
               values_to = "Absorbance",
               names_prefix = "hr_",
               names_transform = as.numeric)


# Generates a plot that matches this one (note just plotting dilution == 0.1): ####

just_0.1 <- 
dat %>% 
  filter(dilution == 0.1) %>% 
  ggplot(aes(x = Time, y = Absorbance, color = type)) +
  geom_smooth(se = FALSE, size = 0.5) +
  facet_wrap(~substrate) +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  labs(color = "Type") +
  ggtitle("Just Dilution 0.1")

just_0.1

ggsave("just_dilution_0.1.jpg",
       plot = just_0.1,
       width = 6.29, height = 2.94,
       units = "in",
       dpi = 500)


# Generates an animated plot that matches this one (absorbance values are mean of all 3 replicates for each group): ####
# This plot is just showing values for the substrate “Itaconic Acid”

itaconic_acid <- 
dat %>% 
  filter(substrate == "Itaconic Acid") %>% 
  group_by(sample_id, dilution, Time) %>% 
  summarize(Mean_absorbance = mean(Absorbance))


itaconic_over_time <- 
itaconic_acid %>% 
  ggplot(aes(x = Time,
             y = Mean_absorbance,
             color = sample_id,
             group = sample_id)) +
  geom_line() +
  scale_y_continuous(breaks = c(0.0, 0.5, 1, 1.5, 2.0, 2.5)) +
  facet_wrap(~dilution) +
  theme_minimal() +
  labs(color = "Sample ID") +
  transition_reveal(Time)

itaconic_over_time

anim_save("itaconic_animation.gif")

