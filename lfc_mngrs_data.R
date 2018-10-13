library(engsoccerdata)
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

#------------------------------------------------------------------------------

# Clean LFC manager dataset from Wikipedia

site <- read_html(
  "https://en.wikipedia.org/wiki/List_of_Liverpool_F.C._managers"
  )

mtable <- site %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(fill = T) %>%
  as_tibble()

mngrs_clean <- mtable %>%
  select(-Notes) %>%
  filter(row_number() != 1) %>% 
  rename(win_perc = `Win %`)

# Remove zeros
mngrs_clean$P <- str_sub(mngrs_clean$P, 21)
mngrs_clean$W <- str_sub(mngrs_clean$W, 21)
mngrs_clean$D <- str_sub(mngrs_clean$D, 21)
mngrs_clean$L <- str_sub(mngrs_clean$L, 21)
mngrs_clean$win_perc <- str_sub(mngrs_clean$win_perc, 21)

# Convert formats and clean
mngrs_clean <- mngrs_clean %>%
  mutate(From = dmy(as.character(From)), 
         To = dmy(as.character(To)), 
         # Fix NAs for Kay and Taylor 
         To = if_else(Name == "George Kay", ymd("1951-03-22"), To), 
         From = if_else(Name == "Phil Taylor", ymd("1956-05-05"), From), 
         # Fix text
         Name = if_else(
           Name == "William Edward BarclayJohn McKenna", "John McKenna", Name
           ), 
         Nationality = if_else(
           Nationality == "Ireland Ireland", "Ireland", "Nationality"
           ), 
         # Remove spade symbol
         win_perc = str_sub(win_perc, 2))

# Convert manager names to last, first format
s <- str_split_fixed(mngrs_clean$Name, " ", n = Inf)
newstrings_names <- paste0(s[, 2], ", ", s[, 1])
mngrs_clean$Name <- newstrings_names