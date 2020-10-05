
## committees ---------------------
comm_posts <- map(names_list, "committees") %>% map("Committee")

comm_posts2 <- map2(comm_posts, names(comm_posts), ~cbind(.x, mnis_id = .y))

comm_posts2 <- purrr::keep(comm_posts2, is.data.frame)

for (i in 1:length(comm_posts2)) {
  
  if("EndDate" %in% colnames(comm_posts2[[i]])) {
    
    comm_posts2[[i]]$EndDate <- as.character(comm_posts2[[i]]$EndDate)
  } else {
    comm_posts2[[i]]$EndDate <- NA
  }
  
  if("ChairDates.ChairDate.EndDate" %in% colnames(comm_posts2[[i]])) {
    
    comm_posts2[[i]]$ChairDates.ChairDate.EndDate <- as.character(comm_posts2[[i]]$ChairDates.ChairDate.EndDate)
  } else {
    comm_posts2[[i]]$ChairDates.ChairDate.EndDate <- NA
  }
  
}

comm_posts_df <- bind_rows(comm_posts2)  %>%
  janitor::clean_names() %>% 
  as_tibble()

names(comm_posts_df) <- str_remove(names(comm_posts_df), "chair_dates_")

comm_posts_df <- comm_posts_df %>% 
  unnest_wider(col = c(chair_date), names_sep = "_") %>% 
  janitor::clean_names() %>%
  mutate(chair_date_end_date_2 = as.character(map(chair_date_end_date_2, 1)),
         chair_date_start_date_2 = as.character(map(chair_date_start_date_2, 1)),
         chair_date_end_date_2 = na_if(chair_date_end_date_2, "NULL"),
         chair_date_start_date_2 = na_if(chair_date_start_date_2, "NULL"),
         chair_date_end_date = if_else(
           is.na(chair_date_end_date), chair_date_end_date_2,
           chair_date_end_date),
         chair_date_start_date = if_else(
           is.na(chair_date_start_date),
           chair_date_start_date_2, chair_date_start_date),
         chair_date_end_date = as.Date(chair_date_end_date),
         chair_date_start_date = as.Date(chair_date_start_date)) %>% 
  select(-end_date_xmlns_xsi, -end_date_xsi_nil, -chair_dates,
         -chair_date_end_date_xmlns_xsi, 
         -chair_date_end_date_xsi_nil, -chair_date_start_date_2, 
         -chair_date_end_date_2, -end_note, -is_co_opted) %>% 
  rename("committee_name" = "name",
         committee_id = id)

comm_changes_days <- comm_posts_df %>% 
  select(-chair_date_start_date,
         -chair_date_end_date) %>% 
  mutate(end_date = if_else(end_date=="list(`@xsi:nil` = \"true\", `@xmlns:xsi` = \"http://www.w3.org/2001/XMLSchema-instance\")", "2020-09-01", end_date),
         end_date = if_else(is.na(end_date), "2020-09-01", end_date)) %>% 
  pivot_longer(cols = start_date:end_date,
               names_to = "key", values_to = "date") %>% 
  mutate(date = as.Date(date))  %>% 
  relocate(mnis_id, key, date) %>% 
  arrange(mnis_id, committee_name) %>% 
  group_by(mnis_id, committee_name) %>%
  complete(date = seq.Date(min(date), max(date), by="day")) %>% 
  filter(date >= as.Date("1979-05-03"),
         #key != "end_date"| is.na(key), 
         date %in% date_df$date) %>% 
  group_by(mnis_id, committee_name) %>% 
  fill(committee_id, is_ex_officio, is_alternate) %>% 
  mutate(committee_member = "committee_member") %>% 
  select(-key)



comm_chairs <- comm_posts_df %>% 
  select(-start_date, -end_date) %>% 
  filter(!is.na(chair_date_start_date)) %>% 
  mutate(chair_date_end_date = if_else(is.na(chair_date_end_date),
                                       as.Date("2020-09-01"),
                                       chair_date_end_date)) %>% 
  pivot_longer(cols = chair_date_start_date:chair_date_end_date,
               names_to = "key", values_to = "date") %>%
  mutate(date = as.Date(date)) %>% 
  ungroup() %>% 
  distinct() %>% 
  relocate(mnis_id, key, date) %>% 
  arrange(mnis_id, committee_name) %>% 
  group_by(mnis_id, committee_name) %>%
  complete(date = seq.Date(min(date), max(date), by="day")) %>% 
  filter(date >= as.Date("1979-05-03"),
         #key != "end_date"| is.na(key), 
         date %in% date_df$date) %>% 
  group_by(mnis_id, committee_name) %>% 
  fill(committee_id, is_ex_officio, is_alternate) %>% 
  #mutate(committee_chair = "committee_chair") %>% 
  select(-key)




x2 <- x %>% group_by(date) %>% 
  summarise(min_date = min(date), 
            max_date = max(date))


committees <- comm_changes_days %>% full_join(comm_chairs)

# con_changes <- con_changes %>%
#   filter(mnis_id %in% con_changes_df2$mnis_id) %>%
#   mutate(start_year = lubridate::year(start_date),
#          end_year = lubridate::year(end_date),
#          parliament = case_when(
#            end_date <= as.Date("1979-05-03") ~ "Pre-1979",
#            end_date <= as.Date("1983-06-09") ~ "Thatcher1",
#            end_date <= as.Date("1987-06-11") ~ "Thatcher2",
#            end_date <= as.Date("1992-04-09") ~ "Thatcher3",
#            end_date <= as.Date("1997-05-01") ~ "Major",
#            end_date <= as.Date("2001-06-07") ~ "Blair1",
#            end_date <= as.Date("2005-05-05") ~ "Blair2",
#            end_date <= as.Date("2010-05-06") ~ "Blair3",
#            end_date <= as.Date("2015-03-30") ~ "Cameron1",
#            end_date <= as.Date("2017-05-03") ~ "Cameron2",
#            end_date <= as.Date("2019-11-06") ~ "May",
#            TRUE ~ "Johnson")) %>% 
#   filter(parliament != "Pre-1979")
# 
# library(fuzzyjoin)


## this seems very slow, how can it be sped up?
## the subset is not that useful, still mostly leaves things blank
# Possibly match and then fill down by year?
# 
# system.time(
# year2 <- year %>% 
#   fuzzy_left_join(con_changes %>%
#                     ungroup() %>% 
#                     filter(
#                       start_year == year$year[[1]] | end_year == year$year[[1]]
#                       ), 
#                   by = c("mnis_id" = "mnis_id",
#        "parliament" = "parliament",
#        "date" = "start_date",
#        "date" = "end_date"),
#                   match_fun = list(`==`,`==`, `>=`, `<`))
# )
# 
# x <- year2 %>% filter(!is.na(constituency_id))