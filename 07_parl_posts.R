library(purrr)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)

names_list <- read_rds("data/names_list.rds")

date_df <- read_rds("data/date_df.rds")


## other parliamentary posts ---------------------

parl_posts <- map(names_list, "parliamentary_posts") %>%
  map("ParliamentaryPost")

parl_posts <- map2(parl_posts, names(parl_posts), ~cbind(.x, mnis_id = .y))

parl_posts <- keep(parl_posts, is.data.frame)

for (i in 1:length(parl_posts)) {
  
  if ("http://www.w3.org/2001/XMLSchema-instance" %in% parl_posts[[i]]$EndDate[[1]]) {
    parl_posts[[i]]$EndDate[[1]] <- NA
    parl_posts[[i]]$EndDate <- as.character(parl_posts[[i]]$EndDate)
  }
}

parl_posts <- bind_rows(parl_posts)  %>%
  janitor::clean_names() %>% 
  as_tibble()


parl_changes_days <- parl_posts %>% 
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
  #mutate(parliamentary_post = "parliamentary_post")  %>% 
  rename("parl_post_name" = "name",
         "parl_post_id" = "id",
         "parl_hansard_name" = "hansard_name")  %>% 
  select(-key, -note, -end_note, -is_joint, -is_unpaid, -email,
         -laying_minister_name, -parl_hansard_name) %>%
  nest(data = c(parl_post_name, parl_post_id))

write_rds(parl_changes_days, "data/day_changes/parl_changes_days.rds")
