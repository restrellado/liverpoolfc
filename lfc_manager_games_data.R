library(tidyverse)

# Read in managers dataset
mngrs_clean <- read_csv("output/lfc_managers.csv")

# Subset Liverpool home and away games
lfc_mngrs <- as.tibble(england) %>%
  filter(home == "Liverpool" | visitor == "Liverpool") %>% 
  mutate(Date = as.Date(Date)) %>%
  # Create a new column for managers
  arrange(Date) %>% 
  mutate(mngr = "")

# TODO rework this as a function
# Loop to add the rest of the managers
for (i in 1:nrow(mngrs_clean)) {
  lfc_mngrs <- lfc_mngrs %>% 
    mutate(mngr = ifelse(
      Date >= mngrs_clean$From[i] & Date <= mngrs_clean$To[i], 
      mngrs_clean$Name[i], 
      mngr
    ))
}

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

#------------------------------------------------------------------------------