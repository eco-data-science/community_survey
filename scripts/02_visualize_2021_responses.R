######################################################
#02_visualize_2021_responses.R
######################################################
# 
# Reads in the clean dataset and produces some grpahs
#
######################################################

# SET UP #######################################################################

## Load packages
library(here)
library(ggsankey)
library(tidyverse)

## Load data
data <- read_csv(file = here("clean_data", "2021_community_survey_data.csv"))

# VISUALIZATIONS ###############################################################

## Sankey plot
sankey_data <- data %>% 
  make_long(where_from, where_at, affiliation)


sankey <-
  ggplot(data = sankey_data,
         mapping = aes(x = x, 
                       next_x = next_x, 
                       node = node, 
                       next_node = next_node,
                       label = node)) +
  geom_sankey(color = "black", fill = "steelblue", alpha = 0.8) +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  theme_sankey(base_size = 18) +
  scale_x_discrete(labels = c("Where are you from?", "Where are you based?", "What is your affiliation?")) +
  theme(legend.position = "None") +
  ggtitle("Member provenance",
          subtitle = "Source: 2021 EDS Community Survey") +
  labs(x = "")




# EXPORT FIGURES ###############################################################

ggsave(plot = sankey,
       filename = here("figures", "2021_sankey.png"),
       width = 9,
       height = 9)
