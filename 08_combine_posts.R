library(readr)
library(dplyr)
library(jsonlite)

debate <- read_rds("data/debate.rds")

x <- nrow(debate)
# gov ---------------------------------------------------------------------


gov_changes_days <- read_rds("data/day_changes/gov_changes_days.rds") %>% 
  rename("government_posts" = "data")

gov_changes_days_json <- toJSON(gov_changes_days)

write(gov_changes_days_json, "release/government_posts.json")

# opp ---------------------------------------------------------------------


opp_changes_days <- read_rds("data/day_changes/opp_changes_days.rds") %>% 
  rename("opposition_posts" = "data")

opp_changes_days_json <- toJSON(opp_changes_days)

write(opp_changes_days_json, "release/opposition_posts.json")

# parl --------------------------------------------------------------------
parl_changes_days <- read_rds("data/day_changes/parl_changes_days.rds") %>% 
  rename("parliamentary_posts" = "data")

parl_changes_days_json <- toJSON(parl_changes_days)

write(parl_changes_days_json, "release/parliamentary_posts.json")

debate <- debate %>% 
  left_join(gov_changes_days) %>% 
  left_join(opp_changes_days)  %>% 
  left_join(parl_changes_days)

y <- nrow(debate)

x == y

write_rds(debate, "release/hansard-speeches-v301.rds", compress = "gz")

write_csv(debate, "release/hansard-speeches-v301.csv")
