library(tidyverse)

games <- read_csv("output/lfc_manager_games.csv", col_types = cols(Season = col_number()))

# Subset for 2010 season 

season <- games %>% 
  select(-c(FT, division:goaldif, lfc_goals, at_anf)) %>% 
  # Use only seasons that used three points for a win
  filter(Season %in% c(1982:2017)) %>% 
  mutate(points = 0, 
         points = ifelse(home == "Liverpool" & result == "H", 3, points), 
         points = ifelse(visitor == "Liverpool" & result == "A", 3, points), 
         points = ifelse(result == "D", 1, points)) %>%
  group_by(Season) %>%
  mutate(run_total = cumsum(points)) %>% 
  ungroup() %>% 
  split(.$Season) %>% 
  map(~mutate(., game = 1:nrow(.))) %>% 
  bind_rows()

#------------------------------------------------------------------------------

ggplot(data = season, aes(x = game, y = run_total)) + 
  geom_freqpoly(stat = "identity", aes(group = Season))