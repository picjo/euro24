library(rvest)
library(tidyr)
library(dplyr)

# information about players and managers participating in the euro 2024 can be
# found on Wikipedia:

SQUADS_URL <- "https://en.wikipedia.org/wiki/UEFA_Euro_2024_squads"
html_page <- read_html(SQUADS_URL)

# get player data from html tables
player_tables <- html_page %>% html_elements("table.sortable") %>% html_table()
player_tables <- player_tables[-c(25,26)]

# get the list of countries
country_list <- html_page %>% html_elements("h3") %>% html_text() %>%
    stringr::str_remove("\\[.*\\]")
country_list <- country_list[-c(25:29)]

# list of corresponding groups
group_list <- html_page %>% html_elements(("h2")) %>% html_text() %>%
  stringr::str_remove("\\[.*\\]")
group_list <- group_list[-c(1,8:11)] %>% rep(each = 4)

# list of managers
manager_list <- html_page %>% html_elements("p") %>% html_text()
manager_list <- grep("Manager", manager_list, value = TRUE, fixed = TRUE) %>%
  stringr::str_remove(".*: ") %>% stringr::str_remove("\\\n")

# construct data.frames
manager_data <- tibble::tibble(country = country_list, 
                               manager = manager_list)
group_data <- tibble::tibble(country = country_list, 
                             group = group_list)
player_data <- tibble::tibble(country = country_list, 
                              players = player_tables) %>% unnest(players)
# tidy up data and row names
player_data <- player_data %>% mutate(.keep = "unused",
  Position = stringr::str_replace(Pos., "[:digit:]",""),
  Birthday = stringr::str_extract(`Date of birth (age)`, "\\d{4}-\\d{2}-\\d{2}"),
)
player_data <- player_data %>% mutate(
  is_captain = grepl(Player, pattern = "captain", fixed = TRUE),
  Player = stringr::str_remove(Player, "(\\(captain\\))")
)

# export data
save(manager_data, group_data, player_data, file = "data/squad_data.RData")