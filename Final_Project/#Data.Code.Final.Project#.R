#Final.Project#
library(tidyverse)
library(janitor)
library(ggplot2)
# Manually create dataframe####
germ_data <- tribble(
  ~Population, ~Experiment, ~Percent_Germinated,
  "All", "Control", 0.15,
  "All", "Chemical", 0.19,
  "All", "Water Room", 0.006,
  "All", "Hot Bath", 0.006,
  "All", "Sand Paper", 0.08,
  "All", "Cold strat", 0,
  
  "ISRO", "Control", 0.08,
  "ISRO", "Chemical", 0.25,
  "ISRO", "Water Room", 0.04,
  "ISRO", "Hot Bath", 0.02,
  "ISRO", "Sand Paper", 0.08,
  "ISRO", "Cold strat", 0,
  
  "MCLA", "Control", 0.24,
  "MCLA", "Chemical", 0.18,
  "MCLA", "Water Room", 0.06,
  "MCLA", "Hot Bath", 0,
  "MCLA", "Sand Paper", 0.08,
  "MCLA", "Cold strat", 0,
  
  "EVER", "Control", 0.10,
  "EVER", "Chemical", 0.20,
  "EVER", "Water Room", 0.08,
  "EVER", "Hot Bath", 0,
  "EVER", "Sand Paper", 0.08,
  "EVER", "Cold strat", 0,
  
  "PBAY", "Control", 0,
  "PBAY", "Chemical", 0.1,        
  "PBAY", "Water Room", 0,
  "PBAY", "Hot Bath", 0.1,      
  "PBAY", "Sand Paper", 0,
  "PBAY", "Cold strat", 0.3,    
  
  "HOME", "Control", 0,
  "HOME", "Chemical", 0,
  "HOME", "Water Room", 0,
  "HOME", "Hot Bath", 0,
  "HOME", "Sand Paper", 0,
  "HOME", "Cold strat", 0.5      
)

#1. Barplot: Germination by Treatment####
ggplot(germ_data, aes(x = Experiment, y = Percent_Germinated, fill = Population)) +
  geom_col(position = "dodge") +
  labs(title = "Germination Rate by Treatment and Population") +
  theme_minimal() +
  coord_flip()

#2. Boxplot: How treatments compare overall####
ggplot(germ_data, aes(x = Experiment, y = Percent_Germinated, fill = Experiment)) +
  geom_boxplot() +
  labs(title = "Distribution of Germination Across Treatments") +
  theme_minimal() +
  theme(legend.position = "none")

#3. Heatmap: Germination by Population and Treatment####
ggplot(germ_data, aes(x = Experiment, y = Population, fill = Percent_Germinated)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Heatmap of Germination Rates") +
  theme_minimal()

#Average germination by treatment####
germ_data %>%
  group_by(Experiment) %>%
  summarize(Average_Germ = mean(Percent_Germinated)) %>%
  arrange(desc(Average_Germ))

#Best treatment per population####
germ_data %>%
  group_by(Population) %>%
  filter(Percent_Germinated == max(Percent_Germinated)) %>%
  arrange(Population)

#ANOVA: Do treatments differ significantly?####
aov_result <- aov(Percent_Germinated ~ Experiment, data = germ_data)
summary(aov_result)

#Interaction: Does the effect of treatment vary by population?####
aov_interaction <- aov(Percent_Germinated ~ Experiment * Population, data = germ_data)
summary(aov_interaction)

#Faceted plots to see each population separately:####
ggplot(germ_data, aes(x = Experiment, y = Percent_Germinated)) +
  geom_col(fill = "skyblue") +
  facet_wrap(~Population) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#For fun####
install.packages("gifski")     
install.packages("transformr") 
library(gganimate)
library(gifski)
library(transformr)
library(dplyr)
ggplot(germ_data, aes(x = Experiment, y = Percent_Germinated, fill = Experiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = 'Germination Rates by Treatment: {closest_state}', y = '% Germinated') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  transition_states(Population, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out')





