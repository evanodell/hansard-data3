library(purrr)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)

names_list <- read_rds("data/names_list.rds")

date_df <- read_rds("data/date_df.rds")

# Opposition posts ---------------------------------------------------------
opp_posts <- map(names_list, "opposition_posts") %>% 
  map("OppositionPost")

opp_posts <- map2(opp_posts, names(opp_posts), ~cbind(.x, mnis_id = .y))

opp_posts <- keep(opp_posts, is.data.frame)

for (i in 1:length(opp_posts)) {
  
  if ("http://www.w3.org/2001/XMLSchema-instance" %in% opp_posts[[i]]$EndDate[[1]]) {
    opp_posts[[i]]$EndDate[[1]] <- NA
    opp_posts[[i]]$EndDate <- as.character(opp_posts[[i]]$EndDate)
  }
}

opp_posts <- bind_rows(opp_posts)  %>%
  clean_names() %>% 
  as_tibble()

opp_changes_days <- opp_posts %>% 
  mutate(end_date = if_else(is.na(end_date), "2020-09-01", end_date)) %>% 
  pivot_longer(cols = start_date:end_date,
               names_to = "key", values_to = "date") %>% 
  mutate(date = as.Date(date)) %>% 
  relocate(mnis_id, key, date) %>% 
  arrange(mnis_id, name) %>% 
  group_by(mnis_id, name) %>%
  complete(date = seq.Date(min(date), max(date), by="day")) %>% 
  filter(date >= as.Date("1979-05-03"),
         key != "end_date"| is.na(key), 
         date %in% date_df$date) %>% 
  group_by(name) %>% 
  fill(key, id, hansard_name) %>% 
  #mutate(opposition_post = "opposition_post") %>% 
  rename("oppo_post_name" = "name",
         "oppo_post_id" = "id",
         "oppo_hansard_name" = "hansard_name")  %>% 
  select(-key, -note, -end_note, -is_joint, -is_unpaid, -email,
         -oppo_hansard_name ) %>%
  nest(data = c(oppo_post_name, oppo_post_id))

write_rds(opp_changes_days, "data/day_changes/opp_changes_days.rds")