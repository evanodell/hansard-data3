library(readr)
library(dplyr)
library(progress)
library(stringr)
library(tidyr)

y_list <- list.files("debate-single-years")
pb <- progress_bar$new(total = length(y_list))

year_list <- list()

for (i in y_list) {
  
  pb$tick()
  
  year_list[[i]] <- read_rds(paste0("debate-single-years/", i))
}

debate <- bind_rows(year_list)



rm(year_list)
nrow(debate)

summary(is.na(debate$mnis_id))
x2 <- debate %>%
  filter(is.na(mnis_id))


#debate <- debate %>% left_join(post_parties %>% select(-id)) 

#write_rds(debate, "debate.rds")

#rm(post_parties)

#debate <- read_rds("debate.rds")

## creates too many rows, figure out why sometime



# identifying missing people ----------------------------------------------

mem_vec <- c("Several Hon. Members", "Hon. Members", "Several Hon Members",
             "Hon Members","Several Hen. Members", "Several Hon, Members",
             "Several Hon. Members rose—", "Horn. Members",
             "Several hon. Members", "Several Hon.Members",
             "Serveral Hon. Members", "Serveral Hon. Members","An Hon. Member",
             "HON. MEMBERS","Other Hon. Members", "Several/ Hon. Members",
             "Hon. Members:", "Several Don. Members","Hon. Member",
             "Hon.Members", "An Hon Member","Several Members", 
             "Several lion. Members","Hon. Member's", "Several Hon. Member",
             "[Hon. Members", "Several hon.Members", "Several hon Members",
             "Several hon. Members.","Several hon, Members",
             "Several- hon. Members","Several hon. Members rose",
             "several hon. Members","[Hon. Members]",
             "Members of the House of Commons",
             "Several hon. Members rose\u0097","An hon. Member:")

debate <- debate %>% tibble::rownames_to_column()

missing_df <- debate %>%
  filter(is.na(mnis_id), !is.na(speech), !is.na(speakername),
         !(speakername %in% c(mem_vec)))

not_missing <- debate %>% filter(!rowname %in% missing_df$rowname)

#old <- read_rds("hansard-1979-2018-v261.rds")
# 
# old <- old %>%
#   select(pp_id, mnis_id) %>% 
#   mutate(mnis_id = as.character(mnis_id))
# 
# write_rds(old, "old_match.rds")

old <- read_rds("data/old_match.rds")

missing_df <- missing_df %>% mutate(
  pp_id = str_remove(id, "uk.org.publicwhip/debate/"),
  pp_id = str_remove(pp_id, "[a-z]")) %>% 
  select(-mnis_id) %>%
  left_join(old) %>% 
  distinct()


## still a handful need correcting
debate <- missing_df %>% bind_rows(not_missing) %>% 
  arrange(date, sort1, sort2)# %>% 
#  select(-pp_id, -gender, -party_id, -party_text)


nrow(debate)

rm(missing_df, not_missing)


alan_fix <- debate %>%
  filter(mnis_id %in% c("536", "569"), date >= as.Date("1987-06-11"), 
         date <= as.Date("2001-06-07")) %>% 
  mutate(mnis_id = "")

alan_fix <- alan_fix %>% 
  mutate(mnis_id = case_when(
    speakername %in% c("Mr. Alan Williams", "Mr. Alan Willians",
                       "Mr. Alan Williams (Swansea, West)",
                       "Mr. Alan Pilliams") ~ "569",
    speakername %in% c("Mr. Alan w. Williams", "Mr. Alan W. Williams:",
                       "Mr. Alan W.Williams", "Dr. Alan W. Williams",
                       "Dr. Williams", "Mr. Alan W. Williams", 
                       "Mr. Allan W. Williams")  ~ "536",
    id == "uk.org.publicwhip/debate/1988-11-01a.982.0" ~ "288",
    TRUE ~ mnis_id
  ),
  mnis_id = na_if(mnis_id, "")) %>% 
  arrange(date, sort1, sort2) %>% 
  group_by(date) %>%
  fill(mnis_id) %>% 
  ungroup()

## need to do another one of these for Mark Hughes/RObert Hughes, and possibly for the two ron browns
hughes_fix <- debate %>%
  ungroup() %>% 
  filter(mnis_id %in% c("1051", "1075", "980")) %>%
  select(rowname,speech, id, hansard_membership_id, speakerid,
         person_id, speakername, mnis_id, everything()) %>% 
  mutate(mnis_id = "") %>%
  mutate(
    mnis_id = case_when(
      str_detect(speakername, "Mark Hughes") ~ "1075",
      speakername %in% c("Mr. Robert Hughes", "Mr. Robert Hughes:",
                         "Mr. Robert Hughes (Aberdeen, North)",
                         "Mr. Robert. Hughes",  "Mr. Robet Hughes",
                         "Mr. Robert Hughes ()", "Mr. Robert Huges" ,
                         "Mr. Robert. Hughes", "Robert Hughes",
                         "Mr Robert Hughes", "6. Mr. Robert Hughes") ~ "1051",
      speakername %in% c("Mr. Robert -G. Hughes", "Mr. Robert -G. Hughes",
                         "Mr. Robert G. Hughes", "Mr. Robert C. Hughes",
                         "Mr. Rogert G. Hughes", "Mr. Robert G. Hughes:",
                         "Mr. Robert G. Hughes: T", "Mr.Robert G. Hughes" ,
                         "Mr. Robert G. Hughes (Harrow, West)",
                         ". Mr. Robert Hughes", "Robert G. Hughes",
                         "Mr Robert G. Hughes", "Mr. Robert G.Hughes",
                         "The Parliamentary Secretary, Office of Public Service and Science (Mr. Robert G. Hughes)",
                         "The Parliamentary Secretary, Office of Public Service..and..Science..(Mr...Robert..G...Hughes)") ~ "980",
      TRUE ~ mnis_id),
    mnis_id = na_if(mnis_id, ""),
    mnis_id = if_else(id=="uk.org.publicwhip/debate/1982-07-26a.728.1",
                      "1051", mnis_id)
    ) %>% 
  group_by(date) %>% 
  fill(mnis_id) %>% 
  ungroup()

cunningham_fix <- debate %>%
  filter(mnis_id %in% c("308", "496", "1100", "633"),
         date <= as.Date("2005-05-05")) %>% 
  mutate(mnis_id = "") %>% 
  mutate(
    speech = case_when(
    speakername == "In the time th" ~ paste0(speakername, speech),
      TRUE ~ speech
        ),
    mnis_id = case_when(
    speakername %in% 
      c("The Minister of Agriculture, Fisheries and Food (Dr. John Cunningham)",
        "The Minister of Agriculture, Fisheries and Food (Dr. Jack Cunningham)",
        "Dr. Cunningham:", "Dr. Jack Cunningham", "Dr. Cunnigham" ,
        "The Minister for the Cabinet Office (Dr. Jack Cunningham)",
        "Dr.Cunningham", "Jack Cunningham", "Dr. John Cunninham",
        "Dr. John Cunningham(by private notice)", "Dr. John. Cunningham", 
        "Mr. John Cunningham", "Dr. Cunningham", "Dr. John Cunningham"   
                       ) ~ "496",
    str_detect(speakername, "The same Secretary of State who opposes dog registration") ~ "496",
    speakername %in% c("Mr. Jim Cunningham", "Jim Cunningham")  ~ "308",
    speakername %in% c("Mr. George Cunningham", "Mr. Gorge Cunningham",
                       "Mr. George Cunningham (Islington, South and Finsbury)",
                       "Mr, George Cunningham",
                       "Mr.George Cunningham") ~ "1100",
    speakername %in% c("Ms Roseanna Cunningham", "Ms Cunningham") ~ "633",
    TRUE ~ mnis_id
  ),
  mnis_id = na_if(mnis_id, "")) %>% 
  arrange(date, sort1, sort2) %>% 
  group_by(date) %>%
  fill(mnis_id) %>% 
  ungroup() %>% 
  mutate(mnis_id = ifelse(speakername == "An hon. Member", NA, mnis_id))

## need a fix for chris davies (2 people with same name)

debate <- debate %>% 
   filter(!rowname %in% alan_fix$rowname, 
          !rowname %in% cunningham_fix$rowname, 
          !rowname %in%  hughes_fix$rowname) %>% 
   bind_rows(alan_fix, cunningham_fix, hughes_fix) 

#summary(is.na())


f_missing_df <- debate %>%
  filter(is.na(mnis_id), speakername != "Unknown",
         !str_detect(speakername, fixed("Member", ignore_case = TRUE)))

## need to re-run this

#write_excel_csv(f_missing_df, "unidentified-speakers2.csv")

final_missing <- read_csv("unidentified-speakers2.csv")

f_missing_df <- f_missing_df %>%
  select(-mnis_id) %>%
  left_join(final_missing %>%
              select(mnis_id, id) %>% 
              mutate_all(as.character)) %>% 
  select(-display_as)


debate <- debate %>% 
  filter(!rowname %in% f_missing_df$rowname) %>% 
  bind_rows(f_missing_df)


members <- read_rds("data/members.rds")

post_parties <- read_rds("data/post_parties.rds")

## final combining -----------
debate <- debate %>% 
  ungroup() %>%
  #select(-gender, -party_id, -party_text, -display_as) %>% 
  arrange(date, sort1, sort2) %>% 
  mutate(
    mnis_id = case_when(
      mnis_id == "401" & date >= as.Date("2015-05-07") ~ "4384",
      mnis_id == "4384" & date < as.Date("2015-05-07") ~ "401",
      mnis_id == "697" & date >= as.Date("2015-05-07") ~ "4376",
      mnis_id == "4376" & date < as.Date("2015-05-07") ~ "697",
      mnis_id == "1404" & date >= as.Date("2015-05-07") ~ "4803",
      mnis_id == "4803" & date < as.Date("2015-05-07") ~ "1404",
      mnis_id == "536" & date >= as.Date("2001-06-07") ~ "569",
      mnis_id == "536" & date <= as.Date("1987-06-11") ~ "569",
    speakername == "Neil Carmichael" & date >= as.Date("2010-01-01") ~ "4104",
    speakername == "Julie Morgan" ~ "561",
      id == "uk.org.publicwhip/debate/1984-02-13a.11.5" ~ "990",
      is.na(mnis_id) & str_detect(speakername, "Tracey") ~ "1260",
      is.na(mnis_id) & str_detect(speakername, "Davies") &
        date == as.Date("1984-02-13") ~ "552",
      is.na(mnis_id) & str_detect(speakername, "Jones") &
        date == as.Date("1984-02-13") ~ "1147",
      is.na(mnis_id) & str_detect(speakername, "Robinson") &
        date == as.Date("1984-02-13") ~ "1154",
      is.na(mnis_id) & str_detect(speakername, "Powley") ~ "1377",
      is.na(mnis_id) & str_detect(speakername, "Hanley") ~ "734",
      TRUE ~ mnis_id
    )) %>% 
  left_join(post_parties %>% select(-id)) %>% 
  select(-display_as) %>% 
  left_join(members %>% select(mnis_id, display_as)) %>% 
  mutate(
    party = case_when(
      mnis_id == "17" & date >= as.Date("2009-06-22") ~ "Speaker",
      mnis_id == "3899" & date >= as.Date("2000-10-23") ~ "Speaker",
      mnis_id == "679" & date >= as.Date("1992-04-27") ~ "Speaker",
      mnis_id == "960" & date >= as.Date("1983-06-15") ~ "Speaker",
      mnis_id == "1300" & date >= as.Date("1976-03-03") ~ "Speaker",
      mnis_id == "4062" & date == as.Date("2016-10-25") ~ "Independent",
      mnis_id == "311" ~ "Labour",
      TRUE ~ party )
  ) %>% 
  mutate(
    sub_type = recode(sub_type, 
                      "Co-Op" = "Co-op"),
    party = if_else(!is.na(sub_type) & party == "Labour", 
                    paste0(party, " (", sub_type, ")"), party),
    speech_class = case_when(
      speech_class == "list()" ~ "Speech",
      speech_class == "division" ~ "Division",
      speech_class == "unparsed_division" ~ "Unparsed Division",
      speech_class == "table" ~ "Table",
      TRUE ~ speech_class
      ),
    speech = case_when(
      speakername == "Hon. Members: Order, order." ~ "Order, order.",
      speakername == "Several Hon. Members rose—" ~ "rose — ",
      TRUE ~ speech
      )
    )  

x2 <- debate %>% filter(is.na(mnis_id))

debate <- debate %>% filter(!is.na(mnis_id))

x2 <- x2 %>% 
  mutate(display_as = case_when(
    speakername == "Hon. Members" ~ "Several Hon. Members",
    speakername == "Several Hon. Members" ~ "Several Hon. Members",
    speakername == "Several hon. Members" ~ "Several Hon. Members",
    speakername == "Several Hon. Member" ~ "Several Hon. Members",
    speakername == "Several Hon." ~ "Several Hon. Members",
    speakername == "Several Hon Member" ~ "Several Hon. Members",
    speakername == "Hon Members" ~ "Several Hon. Members",
    speakername == "An hon. Member" ~ "An Hon. Member",
    speakername == "Several Hon Members" ~ "Several Hon. Members",
    speakername == "Several Hen. Members" ~ "Several Hon. Members",
    speakername == "An Hon. Member" ~ "An Hon. Member",
    speakername == "Several Hon, Members" ~ "Several Hon. Members",
    speakername == "Several Hon. Members rose‚Äî" ~ "Several Hon. Members",
    speakername == "Several Hon. members" ~ "Several Hon. Members",
    speakername == "Amendments made" ~ "Amendments made",
    speakername == "The Clerk Assistant" ~ "The Clerk Assistant",
    speakername == "Several HON. Members" ~ "Several Hon. Members",
    speakername == "Horn. Members" ~ "Several Hon. Members",
    speakername == "Several Hon.Members" ~ "Several Hon. Members",
    speakername == "Serveral Hon. Members" ~ "Several Hon. Members",
    speakername == "HON. MEMBERS" ~ "Several Hon. Members",
    speakername == "Other Hon. Members" ~ "Several Hon. Members",
    speakername == "Several hon." ~ "Several Hon. Members",
    speakername == "Several Hon. Members rose—" ~ "Several Hon. Members",
    speakername == "Several Hon" ~ "Several Hon. Members",
    speakername == "Several/ Hon. Members" ~ "Several Hon. Members",
    speakername == "Hon. Members:" ~ "Several Hon. Members",
    speakername == "Several Don. Members" ~ "Several Hon. Members",
    speakername == "Hon. Member" ~ "An Hon. Member",
    speakername == "Hon.Members" ~ "Several Hon. Members",
    speakername == "An Hon Member" ~ "An Hon. Member",
    speakername == "Several Members" ~ "Several Hon. Members",
    speakername == "Several lion. Members" ~ "Several Hon. Members",
    speakername == "Hon. Member's" ~ "Several Hon. Members",
    speakername == "[Hon. Members" ~ "Several Hon. Members",
    speakername == "Several hon.Members" ~ "Several Hon. Members",
    speakername == "Several hon Members" ~ "Several Hon. Members",
    speakername == "Several hon. Members." ~ "Several Hon. Members",
    speakername == "Several hon, Members" ~ "Several Hon. Members",
    speakername == "Several- hon. Members" ~ "Several Hon. Members",
    speakername == "Several hon. Members rose" ~ "Several Hon. Members",
    speakername == "Several hon. Member" ~ "Several Hon. Members",
    speakername == "Several hon" ~ "Several Hon. Members",
    speakername == "several hon. Members" ~ "Several Hon. Members",
    speakername == "Hon. Members: Order, order." ~ "Several Hon. Members",
    speakername == "[Hon. Members]" ~ "Several Hon. Members",
    speakername == "My Lords and Members of the House of Commons." ~ "My Lords and Members of the House of Commons",
    speakername == "An hon. Member:" ~ "An Hon. Member",
    TRUE ~ speakername
  )
  )

debate <- debate %>%
  bind_rows(x2) %>% 
  arrange(date, sort1, sort2) %>%
  select(id, speech, display_as, party, constituency, mnis_id, date,
         time, colnum, speech_class, major_heading, minor_heading,
         oral_heading, year, everything()) %>% 
  select(-sort1, -sort2, -ord, -pp_id, -sub_type, -nospeaker, -rowname)


write_rds(debate, "data/debate.rds")
