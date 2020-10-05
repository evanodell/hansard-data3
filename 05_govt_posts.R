library(purrr)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)

names_list <- read_rds("data/names_list.rds")

date_df <- read_rds("data/date_df.rds")
# Gov posts ---------------------------------------------------------

gov_posts <- map(names_list, "government_posts") %>% 
  map("GovernmentPost")

gov_posts <- map2(gov_posts, names(gov_posts), ~cbind(.x, mnis_id = .y))

gov_posts <- purrr::keep(gov_posts, is.data.frame)

for (i in 1:length(gov_posts_df)) {
  
  if ("http://www.w3.org/2001/XMLSchema-instance" %in% gov_posts[[i]]$EndDate[[1]]) {
    gov_posts[[i]]$EndDate[[1]] <- NA
    gov_posts[[i]]$EndDate <- as.character(gov_posts[[i]]$EndDate)
  }
}

gov_posts <- bind_rows(gov_posts) %>%
  clean_names() %>% 
  as_tibble()


gov_changes_days <- gov_posts %>% 
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
  #mutate(government_post = "government_post") %>% 
  rename("gov_post_name" = "name",
         "gov_post_id" = "id",
         "gov_hansard_name" = "hansard_name") %>% 
  select(-key, -note, -end_note, -is_joint, -is_unpaid, -email,
         -laying_minister_name, -gov_hansard_name) %>%
  nest(data = c(gov_post_name, gov_post_id))

write_rds(gov_changes_days, "data/day_changes/gov_changes_days.rds")
