library(tidyverse)

# Read in managers dataset
mngrs_clean <- read_csv("output/lfc_managers.csv") %>% 
  # Create time interval
  mutate(span = interval(From, To))

# Subset Liverpool home and away games
lfc_mngrs <- as.tibble(england) %>%
  filter(home == "Liverpool" | visitor == "Liverpool") %>% 
  mutate(Date = as.Date(Date)) %>%
  # Create a new column for managers
  arrange(Date)

#------------------------------------------------------------------------------

# Add managers to the games dataset 

# Create a vector of managers to add to the games dataset
add_mngrs <- function(game_date) {
  mngrs_clean$Name[game_date %within% mngrs_clean$span]
}

# Need to create the manager list separately because a NULL value breaks map_chr
game_mngrs <- as.character(map(lfc_mngrs$Date, add_mngrs))

lfc_mngrs$mngr <- game_mngrs

#------------------------------------------------------------------------------

# Store records with no manager here
no_mngr <- filter(lfc_mngrs, mngr == "")

nrow(no_mngr)

#------------------------------------------------------------------------------

# New goals column and at Anfield column
lfc_mngrs <- lfc_mngrs %>% 
  mutate(
    lfc_goals = 0, 
    lfc_goals = ifelse(home == "Liverpool", hgoal, vgoal), 
    at_anf = "", 
    at_anf = ifelse(home == "Liverpool", "Anfield", "Away")
  )