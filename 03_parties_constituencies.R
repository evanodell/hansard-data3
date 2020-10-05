library(readr)
library(dplyr)
library(progress)
library(stringr)
library(tidyr)
library(purrr)
library(janitor)


members <- read_rds("data/members.rds")

date_df <- read_rds("data/date_df.rds")

names_df <- read_rds("data/names_df.rds")


# MNIS info ---------------------------------------------------------------

mem_time <- as.numeric(
  difftime(Sys.time(), file.info("names_list.rds")$ctime, units = "days")
)

if (mem_time >= 5) {
  pb <- progress_bar$new(total = nrow(names_df),
                       format = ":current/:total :percent (:eta) :bar :spin")

library(mnis)

names_list <- vector(mode = "list", length = nrow(names_df))

names_list <- setNames(names_list, names_df$mnis_id)

for (i in 1:nrow(names_df)) {

  pb$tick()

  names_list[[i]] <- mnis_full_biog(names_df$mnis_id[[i]])

}

write_rds(names_list, "data/names_list.rds")

} else {
  names_list <- read_rds("data/names_list.rds")
}

# Party ---------------------------------------------------------

party <- map(names_list, "parties") %>% map("Party")

party <- party %>% discard(is.null)

party <- map2(party, names(party), ~cbind(.x, mnis_id = .y))

party_dfs <- party %>% keep(is.data.frame)

party_others <- party %>% 
  discard(is.data.frame) %>%
  discard(is.null)

others_parties_df <- list()

for (i in 1:length(party_others)) {
  if (length(party_others[[i]]) > 1) {
    
    others_parties_df[[i]] <- as.data.frame(t(party_others[[i]]))
    others_parties_df[[i]]$mnis_id <- names(party_others)[[i]]
    
  } else {
    others_parties_df[[i]] <- as.data.frame(party_others[[i]])
    
    others_parties_df[[i]]$mnis_id <-names(party_others)[[i]]
    
  }
}

others_parties_df <- bind_rows(others_parties_df) %>%
  distinct(mnis_id, .keep_all = TRUE) %>%
  janitor::clean_names() 

others_parties_df[others_parties_df == 'NULL'] <- NA

#no_info <- others_parties_df %>% filter(is.na(name))

others_parties_df <- others_parties_df %>% 
  filter(!is.na(name)) %>% 
  mutate(end_date = as.character(end_date)) %>%
  mutate_all(unlist) %>% 
  as_tibble() %>% 
  mutate(end_date = if_else(end_date == "list(`@xsi:nil` = \"true\", `@xmlns:xsi` = \"http://www.w3.org/2001/XMLSchema-instance\")",
                            "2020-09-07T00:00:00", end_date),
         end_date = as.Date(end_date),
         start_date = as.Date(start_date))

party_missing2 <- map(names_list, "party")

party3_df <- map2(party_missing2, names(party_missing2), ~rbind(.x, mnis = .y))

for (i in 1:length(party3_df)) {
  party3_df[[i]] <- as.data.frame(party3_df[[i]])
  
  party3_df[[i]]$mnis_id <-  names(party3_df)[[i]]
}


party3_df <- bind_rows(party3_df) %>%
  distinct(mnis_id, .keep_all = TRUE) %>%
  janitor::clean_names() %>% 
  #select(-id, -v1) %>%
  #filter(mnis_id %in% no_info$mnis_id) %>%
  mutate_all(unlist) %>% 
  as_tibble() %>% 
  rename(name = number_text) %>% 
  select(-id)


for (i in 1:length(party_dfs)) {
  
  if ("http://www.w3.org/2001/XMLSchema-instance" %in% party_dfs[[i]]$EndDate[[1]]) {
    party_dfs[[i]]$EndDate[[1]] <- NA
    party_dfs[[i]]$EndDate <- as.character(party_dfs[[i]]$EndDate)
  }
}

party_dfs <- bind_rows(party_dfs) %>% 
  clean_names() %>% 
  as_tibble() %>% 
  mutate(
    end_date = as.Date(end_date),
    start_date = as.Date(start_date))

party_changes_days <- party_dfs %>% 
  group_by(mnis_id, name) %>% 
  # summarise(start_date = min(start_date),
  #           end_date = max(end_date)) %>%
  ## removing duplicates 
  bind_rows(others_parties_df %>% 
              filter(!(mnis_id %in% party_dfs$mnis_id))) %>% 
  bind_rows(party3_df %>% 
              filter(!(mnis_id %in% party_dfs$mnis_id),
                     !(mnis_id %in% others_parties_df$mnis_id))) %>%  
  left_join(members %>%
              select(mnis_id, house_start_date, house_end_date)) %>% 
  mutate(house_end_date =  as.Date(house_end_date),
         house_start_date =  as.Date(house_start_date),
         start_date = if_else(start_date == as.Date("1900-01-01"),
                              house_start_date, start_date),
         start_date = if_else(
           is.na(start_date), house_start_date, start_date),
         # start_date = case_when(
         #   start_date < house_start_date ~ start_date,
         #   abs(
         #     as.numeric(as.character(difftime(start_date, house_start_date)))
         #     )==1 ~ min(start_date, house_start_date),
         #   TRUE ~ start_date),
         end_date = if_else(is.na(end_date),
                            house_end_date, end_date)) %>% 
  select(-sits_opposite_side_to_party, -id, -note) %>% 
  mutate(end_date = if_else(is.na(end_date), as.Date("2020-09-01"), end_date),
         sub_type = if_else(str_detect(name, "(Co-op)"), "Co-op", sub_type),
         name = str_replace(name, " \\(Co-op\\)", "")) %>% 
  filter(name != "Non-affiliated") %>% 
  group_by(mnis_id) %>% 
  arrange(start_date)  %>%
  add_tally() %>%
  mutate(group = cur_group_id()) %>%## need to assign a unique value for each group?
  group_modify(~ {
    .x %>%
      mutate(ord = paste0(group, "-",c(1:.x$n)))
  })  %>% 
  group_by(mnis_id, name, sub_type, ord) %>%
  summarise(
    start_date = min(start_date),
    end_date = min(end_date)) %>% 
  fill(sub_type) %>% 
  arrange(mnis_id) %>% 
  filter(mnis_id != "1477" || name != "Independent Conservative") %>% 
  filter(mnis_id != "683" || name != "Independent Conservative") %>% 
  filter(mnis_id != "535" || name != "Independent") %>%   
  filter(!(mnis_id %in% c("653", "904", "293", "1213", "4388", "4377",
                          "878", "800", "1354", "1317", "867", "1018",
                          "923", "714", "1190", "677", "1580", "4048",
                          "1272", "863", "830", "43", "4115"))) %>%
  mutate(end_date = if_else(mnis_id == "3938" & name == "Independent", 
                            as.Date("2019-04-18"), end_date),
         end_date = if_else(
           mnis_id == "54" & end_date == as.Date("2020-07-16"), 
           as.Date("2020-07-15"), end_date)
         
  ) %>% bind_rows(
    tibble(
      mnis_id = c("653"),
      name =  c("Democratic Unionist Party"),
      start_date = c(as.Date("1970-06-18")),
      end_date = c(as.Date("2010-04-12"))
    ),
    tibble(
      mnis_id = c("677"),
      name =  c("Ulster Unionist Party"),
      start_date = c(as.Date("1983-06-09")),
      end_date = c(as.Date("2000-04-30"))
    ),
    tibble(
      mnis_id = c("293"),
      name =  c("Conservative"),
      start_date = c(as.Date("1983-06-09")),
      end_date = c(as.Date("2010-04-12"))
    ),
    tibble(
      mnis_id = c("863"),
      name =  c("Conservative"),
      start_date = c(as.Date("1983-06-09")),
      end_date = c(as.Date("1992-06-09"))
    ),
    tibble(
      mnis_id = c("830"),
      name =  c("Conservative"),
      start_date = c(as.Date("1970-06-09")),
      end_date = c(as.Date("1997-05-01"))
    ),
    tibble(
      mnis_id = c("1018"),
      name =  c("Labour"),
      start_date = c(as.Date("1964-10-15")),
      end_date = c(as.Date("1983-06-09"))
    ),
    tibble(
      mnis_id = c("43"),
      name =  c("Conservative"),
      start_date = c(as.Date("1970-06-18")),
      end_date = c(as.Date("2017-06-08"))
    ),
    tibble(
      mnis_id = c("4388"),
      name =  c("Scottish National Party"),
      start_date = c(as.Date("2015-05-07")),
      end_date = c(as.Date("2017-06-08"))
    ),
    tibble(
      mnis_id = c("1317"),
      name =  c("Labour"),
      start_date = c(as.Date("1966-03-31")),
      end_date = c(as.Date("1980-04-30"))
    ),
    tibble(
      mnis_id = c("4377"),
      name =  c("Scottish National Party"),
      start_date = c(as.Date("2015-05-07")),
      end_date = c(as.Date("2017-06-08"))
    ),
    tibble(
      mnis_id = c("878"),
      name =  c("Conservative"),
      start_date = c(as.Date("1955-01-01")),
      end_date = c(as.Date("1983-06-09"))
    ),
    tibble(
      mnis_id = c("4592"),
      name =  c("Conservative"),
      start_date = c(as.Date("2016-12-08")),
      end_date = c(as.Date("2020-12-12")))
    ,
    tibble(
      mnis_id = c("904"),
      name =  c("Labour"),
      start_date = c(as.Date("1966-03-31")),
      end_date = c(as.Date("1997-05-01"))
    ),
    tibble(
      mnis_id = c("1079"),
      name =  c("Labour"),
      start_date = c(as.Date("1961-04-20")),
      end_date = c(as.Date("1981-05-29")),
      sub_type = c("Co-Op")
    ),
    tibble(
      mnis_id = c("1354"),
      name =  c("Conservative"),
      start_date = c(as.Date("1950-02-23")),
      end_date = c(as.Date("1979-12-26"))
    ),
    tibble(
      mnis_id = c("800"),
      name =  c("Conservative"),
      start_date = c(as.Date("1974-02-28")),
      end_date = c(as.Date("1979-10-24"))
    ),
    tibble(
      mnis_id = c("867"),
      name =  c("Conservative"),
      start_date = c(as.Date("1983-06-09")),
      end_date = c(as.Date("1987-06-11"))
    ),
    tibble(
      mnis_id = c("923"),
      name =  c("Ulster Unionist Party"),
      start_date = c(as.Date("1970-06-18")),
      end_date = c(as.Date("1997-05-01"))
      ),
    tibble(
      mnis_id = c("714"),
      name =  c("Ulster Unionist Party"),
      start_date = c(as.Date("1974-02-28")),
      end_date = c(as.Date("1990-02-02"))
    ),
    tibble(
      mnis_id = c("1190"),
      name =  c("Conservative"),
      start_date = c(as.Date("1970-06-18")),
      end_date = c(as.Date("1997-05-01"))
    ),
    tibble(
      mnis_id = c("1272"),
      name =  c("Conservative"),
      start_date = c(as.Date("1970-06-18")),
      end_date = c(as.Date("1997-05-01"))
    ),
    tibble(
      mnis_id = c("1213"),
      name =  c("Liberal Democrat"),
      start_date = c(as.Date("1991-03-07")),
      end_date = c(as.Date("1992-04-09"))
      ),
    tibble(
      mnis_id = c(rep("1580", 3)),
      name =  c("Conservative", "Independent",
                "Conservative"),
      start_date = c(as.Date("2005-05-05"), as.Date("2019-09-03"),
                     as.Date("2019-10-30")),
      end_date = c(as.Date("2019-09-04"),
                   as.Date("2019-10-30"), as.Date("2019-11-06"))
    ),
    tibble(
      mnis_id = c(rep("4048", 3)),
      name =  c("Conservative", "Independent",
                "Conservative"),
      start_date = c(as.Date("2010-05-06"), as.Date("2019-09-04"),
                     as.Date("2019-10-30")),
      end_date = c(as.Date("2019-09-04"),
                   as.Date("2019-10-30"), Sys.Date())
    ),
    tibble(
      mnis_id = c(rep("4115", 3)),
      name =  c("Conservative", "Independent",
                "Conservative"),
      start_date = c(as.Date("2010-05-06"), as.Date("2019-09-03"),
                     as.Date("2019-10-30")),
      end_date = c(as.Date("2019-09-03"),
                   as.Date("2019-10-30"), Sys.Date())
    )
  ) %>% 
  # filter(end_date >= as.Date("1979-05-05")) %>% 
  gather(key = "key", value="date", -mnis_id, -name, -sub_type, -ord) %>% 
  arrange(mnis_id, ord, name) %>% 
  group_by(mnis_id) %>%
  complete(date = seq.Date(min(date), max(date), by="day"))  %>% 
  rename("party" = "name") %>% 
  ungroup() %>% 
  group_by(mnis_id) %>% 
  arrange(date) %>%
  fill(party) %>% 
  fill(sub_type) %>% 
  distinct() %>% 
  filter(date >= as.Date("1979-05-03"),
         key != "end_date"| is.na(key)) %>%
  select(-key, -ord)

## can use this to report on switchers and stuff

# Constituencies ---------------------------------------------------------

cons <- map(names_list, "constituencies") %>% map("Constituency")

cons <- map2(cons, names(cons), ~cbind(.x, mnis_id = .y))

cons_df <- purrr::keep(cons, is.data.frame)


# 1091, 783

for (i in 1:length(cons_df)) {
  
  if ("http://www.w3.org/2001/XMLSchema-instance" %in% cons_df[[i]]$EndDate[[1]]) {
    cons_df[[i]]$EndDate[[1]] <- NA
    cons_df[[i]]$EndDate <- as.character(cons_df[[i]]$EndDate)
  }
  
}

cons_others <- cons %>% 
  discard(is.data.frame) %>%
  discard(is.null)

others_df <- list()

for (i in 1:length(cons_others)) {
  if (length(cons_others[[i]]) > 1) {
    
    others_df[[i]] <- as.data.frame(t(cons_others[[i]]))
    others_df[[i]]$mnis_id <- names(cons_others)[[i]]
    
  } else {
    others_df[[i]] <- as.data.frame(cons_others[[i]])
    
    others_df[[i]]$mnis_id <-names(cons_others)[[i]]
    
  }
}

others_df <- bind_rows(others_df) %>%
  distinct(mnis_id, .keep_all = TRUE) %>%
  janitor::clean_names() 

others_df[others_df == 'NULL'] <- NA

#no_info <- others_df %>% filter(is.na(name))

others_df <- others_df %>% 
  filter(!is.na(name)) %>% 
  mutate(end_date = as.character(end_date)) %>% 
  unnest_wider(election, names_sep = "_") %>% 
  mutate_all(unlist) %>% 
  as_tibble() %>% 
  mutate(end_date = if_else(end_date == "list(`@xsi:nil` = \"true\", `@xmlns:xsi` = \"http://www.w3.org/2001/XMLSchema-instance\")",
                            "2020-09-07T00:00:00", end_date),
         end_date = as.Date(end_date),
         start_date = as.Date(start_date))

con_missing <- cons %>% keep(is.null)

con_missing2 <- map(names_list, "member_from")

con3 <- map2(con_missing2, names(con_missing2), ~rbind(.x, mnis = .y))

for (i in 1:length(con3)) {
  con3[[i]] <- as.data.frame(con3[[i]])
  
  con3[[i]]$mnis_id <-  names(con3)[[i]]
}


con3_df <- bind_rows(con3) %>%
  distinct(mnis_id, .keep_all = TRUE) %>%
  janitor::clean_names() %>% 
  #filter(mnis_id %in% no_info$mnis_id) %>%
  mutate_all(unlist) %>% 
  as_tibble() %>% 
  rename(name = v1) 
#select(-id)

cons_df <- bind_rows(cons_df) %>%
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(
    end_date = as.Date(end_date),
    start_date = as.Date(start_date),
    end_date = if_else(is.na(end_date), Sys.Date(), end_date)) %>% 
  rename("constituency_id" = "id")

con_changes_days <- cons_df %>% 
  group_by(mnis_id, name) %>% 
  # summarise(start_date = min(start_date),
  #           end_date = max(end_date)) %>%
  ## removing duplicates 
  bind_rows(others_df %>% 
              filter(!(mnis_id %in% cons_df$mnis_id))) %>% 
  bind_rows(con3_df %>% 
              filter(!(mnis_id %in% cons_df$mnis_id),
                     !(mnis_id %in% others_df$mnis_id))) %>%  
  left_join(members %>%
              select(mnis_id, house_start_date, house_end_date)) %>% 
  mutate(start_date = if_else(start_date == as.Date("1900-01-01"),
                              as.Date(house_start_date), start_date),
         start_date = if_else(is.na(start_date),
                              as.Date(house_start_date), start_date),
         end_date = if_else(is.na(end_date),
                            as.Date(house_end_date), end_date)) %>% 
  select(mnis_id, id, name, start_date, end_date) %>% 
  mutate(end_date = if_else(
    is.na(end_date), as.Date("2020-09-01"), end_date)) %>% 
  group_by(mnis_id, name, id) %>% 
  summarise(start_date = min(start_date),
            end_date = max(end_date)) %>% 
  mutate(name = recode(name, "Rutherglen" = "Glasgow, Rutherglen"),
         name = ifelse(mnis_id == "46" && 
                         start_date == as.Date("1997-05-01") &&
                         end_date == as.Date("2010-05-06"),
                       "West Chelmsford", name)
  ) %>% 
  filter(!(mnis_id %in% c("375", "46", "597", "923", "653", "904", "293",
                          "1213", "4499", "4388", "4377", "4592", "878",
                          "1079", "800", "1354", "1317", "867", "1018",
                          "1190", "677", "1580", "1272", "43"))) %>% 
  bind_rows(
    tibble(
      mnis_id = c(rep("375", 3)),
      name =  c("Kingston upon Hull North", "Kingston upon Hull Central",
                "Kingston upon Hull North"),
      start_date = c(as.Date("1966-01-27"), as.Date("1974-02-28"),
                     as.Date("1983-06-09")),
      end_date = c(as.Date("1974-02-28"),
                   as.Date("1983-06-09"), as.Date("2005-05-05"))
      ),
    tibble(
      mnis_id = c(rep("46", 3)),
      name =  c("Chelmsford", "West Chelmsford", "Chelmsford"),
      start_date = c(as.Date("1987-06-11"), as.Date("1997-05-01"),
                     as.Date("2010-05-06")),
      end_date = c(as.Date("1997-05-01"),
                   as.Date("2010-05-06"), as.Date("2017-05-03"))),
    tibble(
      mnis_id = c(rep("597", 3)),
      name =  c("Edinburgh East", "Edinburgh East and Musselburgh",
                "Edinburgh East"),
      start_date = c(as.Date("1970-06-18"), as.Date("1997-05-01"),
                     as.Date("2005-05-05")),
      end_date = c(as.Date("1997-05-01"),
                   as.Date("2005-05-05"), as.Date("2010-05-06"))),
    tibble(
      mnis_id = c(rep("923", 2)),
      name =  c("South Antrim", "Lagan Valley"),
      start_date = c(as.Date("1970-06-18"),
                     as.Date("1983-06-09")),
      end_date = c(as.Date("1983-06-09"),
                   as.Date("1997-05-01"))
      ),
    tibble(
      mnis_id = c(rep("43", 2)),
      name =  c("Middleton and Prestwich", "Saffron Walden"),
      start_date = c(as.Date("1970-06-18"),
                     as.Date("1977-07-07")),
      end_date = c(as.Date("1974-02-28"),
                   as.Date("2017-06-08"))
    ),
    tibble(
      mnis_id = c(rep("904", 2)),
      name =  c("Eton and Slough", "Lagan Valley"),
      start_date = c(as.Date("1966-03-31"),
                     as.Date("1987-06-09")),
      end_date = c(as.Date("1983-06-09"),
                   as.Date("1997-04-08"))),
    tibble(
      mnis_id = c(rep("293", 2)),
      name =  c("Lewisham West", "Stratford-on_avon"),
      start_date = c(as.Date("1983-06-09"),
                     as.Date("1997-05-01")),
      end_date = c(as.Date("1992-03-16"),
                   as.Date("2010-04-12"))
      ),
    tibble(
      mnis_id = c(rep("1190", 2)),
      name =  c("Aberdeen South", "Harwich"),
      start_date = c(as.Date("1970-06-18"),
                     as.Date("1992-04-09")),
      end_date = c(as.Date("1983-06-09"),
                   as.Date("1997-05-01"))
      ),
    tibble(
      mnis_id = c(rep("1272", 2)),
      name =  c("Aberdeen South", "Winchester"),
      start_date = c(as.Date("1983-06-09"),
                     as.Date("1992-04-09")),
      end_date = c(as.Date("1987-06-11"),
                   as.Date("1997-05-01"))
    ),
    
    tibble(
      mnis_id = c("4388"),
      name =  c("Coatbridge, Chryston and Bellshill"),
      start_date = c(as.Date("2015-05-07")),
      end_date = c(as.Date("2017-06-08"))
    ),
    tibble(
      mnis_id = c("867"),
      name =  c("Strathkelvin and Bearsden"),
      start_date = c(as.Date("1983-06-09")),
      end_date = c(as.Date("1987-06-11"))
    ),
    tibble(
      mnis_id = c("1079"),
      name =  c("Warrington"),
      start_date = c(as.Date("1961-04-20")),
      end_date = c(as.Date("1981-05-29"))
    ),
    tibble(
      mnis_id = c("1018"),
      name =  c("Heywood and Royton"),
      start_date = c(as.Date("1964-10-15")),
      end_date = c(as.Date("1983-06-09"))
    ),
    tibble(
      mnis_id = c("1580"),
      name =  c("Wantage"),
      start_date = c(as.Date("2005-05-05")),
      end_date = c(as.Date("2019-11-12"))
    ),
    tibble(
      mnis_id = c("4377"),
      name =  c("West Aberdeenshire and Kincardine"),
      start_date = c(as.Date("2015-05-07")),
      end_date = c(as.Date("2017-06-08"))
    ),
    tibble(
      mnis_id = c("1213"),
      name =  c("Ribble Valley"),
      start_date = c(as.Date("1991-03-07")),
      end_date = c(as.Date("1992-04-09"))),
    tibble(
      mnis_id = c("4499"),
      name =  c("Gower"),
      start_date = c(as.Date("2015-05-07")),
      end_date = c(as.Date("2017-05-03"))
      ),
    tibble(
      mnis_id = c("878"),
      name =  c("Rye"),
      start_date = c(as.Date("1955-01-01")),
      end_date = c(as.Date("1983-06-09"))
    ),
    tibble(
      mnis_id = c("800"),
      name =  c("South West Hertfordshire"),
      start_date = c(as.Date("1974-02-28")),
      end_date = c(as.Date("1979-10-24"))
    ),
    tibble(
      mnis_id = c("1354"),
      name =  c("Southend East"),
      start_date = c(as.Date("1950-02-23")),
      end_date = c(as.Date("1979-12-26"))
    ),
    tibble(
      mnis_id = c("1317"),
      name =  c("Glasgow Central"),
      start_date = c(as.Date("1966-03-31")),
      end_date = c(as.Date("1980-04-30"))
    ),
    tibble(
      mnis_id = c("677"),
      name =  c("South Antrim"),
      start_date = c(as.Date("1983-06-09")),
      end_date = c(as.Date("2000-04-30"))
    ),
    tibble(
      mnis_id = c("653"),
      name =  c("North Antrim"),
      start_date = c(as.Date("1970-06-18")),
      end_date = c(as.Date("2010-04-12"))),
    tibble(
      mnis_id = c("4592"),
      name =  c("Sleaford and North Hykeham"),
      start_date = c(as.Date("2016-12-08")),
      end_date = c(as.Date("2020-12-12")))
  ) %>% 
  filter(end_date >= as.Date("1979-05-05")) %>%
  group_by(mnis_id) %>%
  arrange(start_date)  %>%
  add_tally() %>%
  mutate(group = cur_group_id()) %>%## need to assign a unique value for each group?
  group_modify(~ {
    .x %>%
      mutate(ord = paste0(group, "-",c(1:.x$n)))
  }) %>%
  select(-n, -group) %>% 
  gather(key = "key", value="date", -mnis_id, -name, -id, -ord) %>% 
  arrange(mnis_id, ord, desc(key), date, name, id) %>% 
  group_by(mnis_id) %>%
  complete(date = seq.Date(min(date), max(date), by="day")) %>% 
  # filter(date >= as.Date("1979-05-03"),
  #        key != "end_date"| is.na(key)) %>% 
  rename("constituency" = "name") %>% 
  ungroup() %>% 
  group_by(mnis_id) %>% 
  arrange(date) %>%
  fill(constituency) %>% 
  group_by(mnis_id) %>%
  distinct(date, .keep_all = TRUE) %>% 
  #filter(date >= as.Date("1979-05-03")) %>%
  select(-key)

# Combining MP data -------------------------------------------------------

nrow(party_changes_days)
nrow(con_changes_days)

post_parties <- party_changes_days %>%
  full_join(con_changes_days) %>%
  distinct()

nrow(post_parties)

write_rds(post_parties, "data/post_parties.rds")
