library(engsoccerdata)
library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

#------------------------------------------------------------------------------

# Clean LFC manager dataset 

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
  filter(row_number() != 1)

# Remove zeros
# mngrs_clean$From <- str_sub(mngrs_clean$From, 9, 18)
# mngrs_clean$To <- str_sub(mngrs_clean$To , 9, 18)
mngrs_clean$P <- str_sub(mngrs_clean$P, 21)
mngrs_clean$W <- str_sub(mngrs_clean$W, 21)
mngrs_clean$D <- str_sub(mngrs_clean$D, 21)
mngrs_clean$L <- str_sub(mngrs_clean$L, 21)
mngrs_clean$`Win %` <- str_sub(mngrs_clean$`Win %`, 21)

# Convert From and To to dates
mngrs_clean <- mngrs_clean %>%
  mutate(From = dmy(as.character(From)), 
         To = dmy(as.character(To)), 
         # Fix NAs for Kay and Taylor 
         To = if_else(Name == "George Kay", ymd("1951-03-22"), To), 
         From = if_else(Name == "Phil Taylor", ymd("1956-05-05"), From))

# Convert manager names to last, first format
s <- str_split_fixed(mngrs_clean$Name, " ", n = Inf)
s2 <- str_sub(s[, 2], 1, .5 * nchar(s[, 2]))
newstrings <- cbind(s, s2)
newstrings_names <- paste(newstrings[, 1], newstrings[,"s2"])

mngrs_clean$Name <- newstrings_names