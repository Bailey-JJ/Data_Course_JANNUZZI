# Exam 1
library(tidyverse)


# I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts) ####

df <- read_csv("./data/cleaned_covid_data.csv")

# II. Subset the data set to just show states that begin with "A" and save this as an object called A_states. (20 pts) ####
  
A_states <- filter(df ,grepl("A", df$Province_State))

# III. Create a plot _of that subset_ showing Deaths over time, with a separate facet for each state. (20 pts) ####

A_states %>% 
  ggplot(mapping = aes(x = as.Date(Last_Update),
                       y = Deaths,
                       color = Province_State,
                       line.width = 5)) +
  geom_point(pch = 1, size = .5) +
  guides(color = guide_legend((title = "Province State"))) +
  facet_wrap(~Province_State, scales = 'free') +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  geom_jitter(height = .5, width = 2) +
  xlab("Last Update") +
  ylab("Deaths")



# IV.(Back to the full dataset) Find the "peak" of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts) ####

state_max_fatality_rate <- df %>%
  filter(!is.na(Case_Fatality_Ratio)) %>% 
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))


# V. Use that new data frame from task IV to create another plot. (20 pts) ####

state_max_fatality_rate %>% 
  ggplot(mapping = aes(x = factor(as.vector(Province_State), Province_State),
                       y = Maximum_Fatality_Ratio)) +
  geom_col(fill = "#78AE6D") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Province_State") +
  ylab("Maximum_Fatality_Ratio")


# VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time ####

total_deaths <- df %>% 
  group_by(Last_Update) %>%
  summarize(TotalDeaths = sum(Deaths))

total_deaths %>% 
  ggplot(mapping = aes(x = Last_Update, y = TotalDeaths)) +
    geom_point() +
    xlab("Last Update") +
    ylab("Total Deaths")

  
  

# + You'll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.