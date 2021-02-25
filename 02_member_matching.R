
library(readr)
library(dplyr)
library(progress)
library(stringr)
library(tidyr)

# Members -----------------------------------------------------------------

mem_time <- as.numeric(
  difftime(Sys.time(), file.info("data/members.rds")$ctime, units = "days")
  )

if (mem_time >= 5) {

members <- mnis::mnis_all_members()

members <- members %>% 
  select(-house_membership) %>%
  filter(!is.na(display_as)) %>% 
  rename("mnis_id" = "member_id") %>% 
  mutate(
    display_as = str_remove(display_as, "Dr "),
    display_as = str_remove(display_as, "Mr "),
    display_as = str_remove(display_as, "Sir "),
    display_as = str_remove(display_as, "Dame "),
    display_as = str_remove(display_as, "Ms "),
    display_as = str_remove(display_as, "Mrs "),
    display_as = str_remove(display_as, "Miss "),
    display_as = if_else(mnis_id == "770", "Jim Callaghan", display_as),
    display_as = if_else(mnis_id == "1399", "John Thurso", display_as),
    display_as = if_else(mnis_id == "536", "Alan W. Williams", display_as),  
    display_as = if_else(mnis_id == "980", "Robert G. Hughes", display_as),  
    display_as = recode(
      display_as,
      "John D. Taylor" = "John M. Taylor",
      "William Cash" = "Bill Cash",
      "Mary Kelly Foy" = "Mary Foy",
      "Marsha De Cordova" = "Marsha de Cordova",
      "Lord Clarke of Nottingham" = "Kenneth Clarke",
      "Vince Cable" = "Vincent Cable",
      "Liz Saville Roberts" = "Liz Saville-Roberts",
      "Angus Brendan MacNeil" = "Angus MacNeil",
      "Nick Boles" = "Nicholas Boles",
      "Matt Hancock" = "Matthew Hancock",
      "Tom Tugendhat" = "Thomas Tugendhat",
      "Diana Johnson"  = "Diana R. Johnson",
      "Jonathan Ashworth" = "Jon Ashworth",
      "Edward Miliband" = "Ed Miliband",
      "Tanmanjeet Singh Dhesi" = "Tan Dhesi",
      "Baroness Morgan of Cotes" = "Nicky Morgan",
      "David T C Davies" = "David Davies",
      "Thérèse Coffey"  = "Therese Coffey",
      "Nicholas Brown" = "Nick Brown",
      "Stewart Malcolm McDonald" = "Stewart McDonald",
      "Joseph  Johnson" = "Jo Johnson",
      "Lady Hermon" = "Sylvia Hermon",
      "Lord Mann" = "John Mann",
      "Robert Neill" = "Bob Neill",
      "Lord Herbert of South Downs" = "Nick Herbert",
      "Lord Walney" = "John Woodcock",
      "Ian C. Lucas" = "Ian Lucas",
      "Tanmanjeet Singh Dhesi" = "Tan Dhesi",
      "Lord Goldsmith of Richmond Park" = "Zac Goldsmith",
      "Lord Austin of Dudley" = "Ian Austin",
      "Graham P Jones" = "Graham Jones",
      "Lord Vaizey of Didcot" = "Ed Vaizey",
      "Lord Brooke of Sutton Mandeville" = "Peter Brooke",
      "Viscount Macmillan of Ovenden" = "Maurice Macmillan",
      "Lord Waddington" = "David Waddington",
      "Lord Wakeham" = "John Wakeham",
      "The Lord Lyell of Markyate" = "Nicholas Lyell",
      "Lord Steel of Aikwood" = "David Steel",
      "Lord Elliott of Morpeth" = "Robert Elliott",
      "Lord Baker of Dorking" = "Kenneth Baker",
      "Lord Tebbit" = "Norman Tebbit",
      "Lord Kinnock" = "Neil Kinnock",
      "Lord Clinton-Davis" = "Stanley Clinton-Davis",
      "Lord Healey" = "Denis Healey",
      "Lord Jenkin of Roding" = "Charles Jenkin",
      "Lord Haselhurst" = "Alan Haselhurst",
      "Lord Hughes of Woodside" = "Robert Hughes",
      "Lord Brittan of Spennithorne" = "Leon Brittan",
      "Lord St John of Fawsley" = "Norman St John-Stevas",
      "Lord Ashley of Stoke" = "Jack Ashley",
      "Lord Beith" = "Alan Beith",
      "Lord Hattersley" = "Roy Hattersley",
      "Lord Eden of Winton" = "John Eden",
      "Lord Rooker" = "Jeffrey Rooker",
      "Lord Lamont of Lerwick" = "Norman Lamont",
      "Lord Maclennan of Rogart" = "Robert Maclennan",
      "Lord Heseltine" = "Michael Heseltine",
      "Lord Cormack" = "Patrick Cormack",
      "Lord Fraser of Carmyllie" = "Peter Fraser",
      "Lord Maxton" = "John Maxton",
      "Lord Woolmer of Leeds" = "Kenneth Woolmer",
      "Lord Alton of Liverpool" = "David Alton",
      "Lord Martin of Springburn" = "Michael Martin",
      "Lord Owen" = "David Owen",
      "Lord Roper" = "John Roper",
      "The Lord Walker of Worcester" = "Peter Walker",
      "Lord Parkinson" = "Cecil Parkinson",
      "Lord Robertson of Port Ellen" = "George Robertson",
      "Lord Renton of Mount Harry" = "Ronald Renton",
      "Lord Shaw of Northstead" = "Michael Shaw",
      "Lord O'Neill of Clackmannan" = "Martin O'Neill",
      "Lord Jones" = "Stephen Jones",
      "Lord Prior" = "James Prior",
      "Lord Temple-Morris" = "Peter Temple-Morris",
      "Lord Snape" = "Peter Snape",
      "Lord Lawson of Blaby" = "Nigel Lawson",
      "Lord Howe of Aberavon" = "Richard Howe",
      "Lord Morris of Aberavon" = "John Morris",
      "Lord Dixon" = "Donald Dixon",
      "Lord Foulkes of Cumnock" = "George Foulkes",
      "Lord McNally" = "Tom McNally",
      "Lord Jopling" = "Thomas Jopling",
      "Lord Janner of Braunstone" = "Greville Janner",
      "Lord Luce" = "Richard Luce",
      "Lord Howell of Guildford" = "David Howell",
      "Lord Wigley" = "Dafydd Wigley",
      "Lord Graham of Edmonton" = "Thomas Graham",
      "Lord Crickhowell" = "Roger Edwards",
      "Lord Anderson of Swansea" = "Donald Anderson",
      "Lord Rowlands" = "Edward Rowlands",
      "Lord Mason of Barnsley" = "Roy Mason",
      "Lord Wrigglesworth" = "Ian Wrigglesworth",
      "Lord Barnett" = "Joel Barnett",
      "Lord Goodlad" = "Alastair Goodlad",
      "Lord Moore of Lower Marsh" = "John Moore",
      "Lord Cunningham of Felling" = "John Cunningham",
      "Lord Spicer" = "William Spicer",
      "Lord Dykes" = "Hugh Dykes",
      "Lord Stoddart of Swindon" = "David Stoddart",
      "Lord Garel-Jones" = "William Garel-Jones",
      "Lord Hayhoe" = "Bernard Hayhoe",
      "Lord Gilbert" = "John Gilbert",
      "Lord Dubs" = "Alfred Dubs",
      "Lord Naseby" = "Michael Morris",
      "Lord Hurd of Westwell" = "Douglas Hurd",
      "Lord Higgins" = "Terence Higgins",
      "Lord Foster of Bishop Auckland" = "Derek Foster",
      "Lord Field of Birkenhead" = "Frank Field",
      "Lord Young of Cookham" = "George Young",
      "Lord Fowler" = "Peter Fowler",
      "Lord Rodgers of Quarry Bank" = "William Rodgers",
      "Lord Lee of Trafford" = "John Lee",
      "Lord Patten of Barnes" = "Christopher Patten",
      "Lord Soley" = "Clive Soley",
      "Lord Roberts of Conwy" = "Ieuan Roberts",
      "Lord Hammond of Runnymede" = "Philip Hammond",
      "Lord Sheldon" = "Robert Sheldon",
      "Lord Mayhew of Twysden" = "Patrick Mayhew",
      "Lord Evans of Parkside" = "John Evans",
      "Lord Horam" = "John Horam",
      "Lord Lang of Monkton" = "Ian Lang",
      "Lord Morris of Manchester" = "Alfred Morris",
      "Lord Clark of Windermere" = "David Clark",
      "Lord Hunt of Wirral" = "David Hunt",
      "Lord Archer of Sandwell" = "Peter Archer",
      "Lord King of Bridgwater" = "Thomas King",
      "Lord Mawhinney" = "Brian Mawhinney",
      "Lord Pendry" = "Thomas Pendry",
      "Lord Campbell-Savours" = "Dale Campbell-Savours",
      "Lord Kimball" = "Marcus Kimball",
      "Lord Prescott" = "John Prescott",
      "Lord Radice" = "Giles Radice",
      "Lord Hamilton of Epsom" = "Archibald Hamilton",
      "The Marquess of Lothian" = "Michael Ancram",
      "Lord Patten" = "John Patten",
      "Lord Waldegrave of North Hill" = "William Waldegrave",
      "Lord Lofthouse of Pontefract" = "Geoffrey Lofthouse",
      "Lord MacGregor of Pulham Market" = "John MacGregor",
      "Lord Stewartby" = "Bernard Stewart",
      "Lord Selkirk of Douglas" = "James Douglas-Hamilton",
      "Lord Cope of Berkeley" = "John Cope",
      "Lord Hoyle" = "Eric Hoyle",
      "Lord Newton of Braintree" = "Antony Newton",
      "Lord Randall of St Budeaux" = "Stuart Randall",
      "Lord Bruce of Bennachie" = "Malcolm Bruce",
      "Lord Smith of Finsbury" = "Christopher Smith",
      "Lord Carlile of Berriew" = "Alexander Carlile",
      "Lord McCrea of Magherafelt and Cookstown" = "Robert McCrea",
      "Lord Howard of Lympne" = "Michael Howard",
      "Lord Corbett of Castle Vale" = "Robin Corbett",
      "Lord Wallace of Tankerness" = "James Wallace",
      "Lord Howarth of Newport" = "Alan Howarth",
      "Lord Ashdown of Norton-sub-Hamdon" = "Jeremy Ashdown",
      "Lord Freeman" = "Roger Freeman",
      "Lord Kirkwood of Kirkhope" = "Archibald Kirkwood",
      "Lord Forsyth of Drumlean" = "Michael Forsyth",
      "Lord Hayward" = "Robert Hayward",
      "Lord Maginnis of Drumglass" = "Kenneth Maginnis",
      "Lord Moynihan" = "Colin Moynihan",
      "Lord Moynihan" = "Antony Moynihan",
      "Lord Kilclooney" = "John D. Taylor",
      "Lord Maude of Horsham" = "Francis Maude",
      "Lord Framlingham" = "Michael Lord",
      "Lord Lilley" = "Peter Lilley",
      "Lord Ryder of Wensum" = "Richard Ryder",
      "Lord Blencathra" = "David Maclean",
      "Lord Livsey of Talgarth" = "Richard Livsey",
      "Lord Taylor of Goss Moor" = "Matthew Taylor",
      "Lord Blunkett" = "David Blunkett",
      "Lord Boateng" = "Paul Boateng",
      "Lord Reid of Cardowan" = "John Reid",
      "Lord Moonie" = "Lewis Moonie",
      "Lord Grocott" = "Bruce Grocott",
      "Lord Darling of Roulanish" = "Alistair Darling",
      "Lord Kirkhope of Harrogate" = "Timothy Kirkhope",
      "Lord Bilston" = "Dennis Turner",
      "Lord Carrington of Fulham" = "Matthew Carrington",
      "Lord Fearn" = "Ronald Fearn",
      "Lord Murphy of Torfaen" = "Paul Murphy",
      "Lord Boswell of Aynho" = "Timothy Boswell",
      "Lord Davies of Stamford" = "Quentin Davies",
      "Lord Bradley" = "Keith Bradley",
      "Lord Campbell of Pittenweem" = "Menzies Campbell",
      "Lord Arbuthnot of Edrom" = "James Arbuthnot",
      "Lord Hague of Richmond" = "William Hague",
      "Lord Watson of Invergowrie" = "Michael Watson",
      "Lord Trimble" = "William Trimble",
      "Lord Hain" = "Peter Hain",
      "Lord Hutton of Furness" = "John Hutton",
      "Lord Tyler" = "Paul Tyler",
      "Lord Risby" = "Richard Spring",
      "Lord Foster of Bath" = "Donald Foster",
      "Lord Jones of Cheltenham" = "Nigel Jones",
      "Lord Mandelson" = "Peter Mandelson",
      "Lord Robathan" = "Andrew Robathan",
      "Lord Garnier" = "Edward Garnier",
      "Lord Davies of Oldham" = "Bryan Davies",
      "Lord Pickles" = "Eric Pickles",
      "Lord Bates" = "Michael Bates",
      "Lord Willetts" = "David Willetts",
      "Lord Coe" = "Sebastian Coe",
      "Lord Chidgey" = "David Chidgey",
      "Lord Touhig" = "James Touhig",
      "Lord Wills" = "Michael Wills",
      "Lord Allan of Hallam" = "Richard Allan",
      "Lord Stunell" = "Robert Stunell",
      "Lord Lansley" = "Andrew Lansley",
      "Lord Watts" = "David Watts",
      "Lord Tyrie" = "Andrew Tyrie",
      "Lord Prior of Brampton" = "David Prior",
      "Lord Browne of Ladyton" = "Desmond Browne",
      "Lord Burnett" = "John Burnett",
      "Lord Randall of Uxbridge" ="Alexander Randall",
      "Lord Barker of Battle" = "Gregory Barker",
      "Lord Barwell" = "Gavin Barwell",
      "Baroness Thatcher" = "Margaret Thatcher",
      "Baroness Taylor of Bolton" = "Winifred Taylor",
      "Baroness Fookes" = "Janet Fookes",
      "Baroness Boothroyd" = "Betty Boothroyd",
      "Baroness Chalker of Wallasey" = "Lynda Chalker",
      "Baroness Williams of Crosby" = "Shirley Williams",
      "Baroness Bottomley of Nettlestone" = "Virginia Bottomley",
      "Baroness Golding" = "Llinos Golding",
      "Baroness Quin" = "Joyce Quin",
      "Baroness Nicholson of Winterbourne" = "Emma Nicholson",
      "Baroness Shephard of Northwold" = "Gillian Shephard",
      "Baroness Primarolo" = "Dawn Primarolo",
      "Baroness Adams of Craigielea" = "Katherine Adams",
      "Baroness Corston" = "Jean Corston",
      "Baroness Jowell" = "Tessa Jowell",
      "Baroness Browning" = "Angela Browning",
      "Lord Davies of Gower" = "Byron Davies",
      "Baroness Maddock" = "Diana Maddock",
      "Baroness Liddell of Coatdyke" = "Helen Liddell",
      "Baroness Clark of Calton" = "Lynda Clark",
      "Baroness Morris of Yardley" = "Estelle Morris",
      "Baroness King of Bow" = "Oona King",
      "Baroness McIntosh of Pickering" = "Anne McIntosh",
      "Baroness Tonge" = "Jennifer Tonge",
      "Baroness Hughes of Stretford" = "Beverley Hughes",
      "Baroness Kramer" = "Susan Kramer",
      "Baroness Burt of Solihull" = "Lorely Burt",
      "Baroness Featherstone" ="Lynne Featherstone",
      "Baroness Clark of Kilwinning" = "Katy Clark",
      "Baroness Blackwood of North Oxford" = "Nicola Blackwood",
      "Baroness Ritchie of Downpatrick" = "Margaret Ritchie",
      "Baroness Stuart of Edgbaston" =  "Gisela Stuart",
      "Baroness Hoey" = "Kate Hoey",
      "Lord Wharton of Yarm" = "James Wharton",
      "Lord McLoughlin" = "Patrick McLoughlin",
      "Lord Dodds of Duncairn" = "Nigel Dodds",
      "Lord Lancaster of Kimbolton" = "Mark Lancaster",
      "Baroness Hayman of Ullock" = "Sue Hayman",
      "William Cash" = "Bill Cash",
      "Tanmanjit Singh Dhesi" = "Tan Dhesi"
    ),
    display_as = str_trim(display_as)) %>% 
  filter(!(mnis_id %in% c("1784", "1649")))

write_rds(members, "data/members.rds")

} else {
  members <- read_rds("data/members.rds")
}

# members2 <- members %>%
#   group_by(display_as) %>%
#   summarise(n=n()) %>%
#   arrange(desc(n))
# 
# members2_dupes <- members2 %>% filter(n>1)
# nnn <- members2_dupes$display_as
# 
# members_unique <- members %>%
#   filter(!(display_as %in% members2_dupes$display_as))
# 

# load CSVs -----------------------------------------------------------


match1979 <- read_csv("completed-matches/197.csv") %>%
  #select(-eo_id) %>% 
  rename(speakerid = speaker_id) %>%
  distinct() %>% 
  mutate(speakerid = paste0("uk.org.publicwhip/member/", speakerid)) %>%
  mutate_all(as.character) %>%
  left_join(members %>% select(mnis_id, display_as))

match80s <- read_csv("completed-matches/198.csv") %>%
  select(-eo_id, -pp_id, -speaker_office, -person_id, -speaker_id) %>% 
  #rename(speakerid = speaker_id) %>%
  distinct() %>% 
  filter(!is.na(hansard_membership_id)) %>%
  mutate_all(as.character)

match80s <- match80s %>% left_join(members %>% select(mnis_id, display_as))

match90s <- read_csv("completed-matches/199.csv") %>% 
  rename(speakerid = speaker_id) %>% 
  select(hansard_membership_id, speakerid, person_id, mnis_id) %>%
  distinct() %>% 
  mutate(speakerid = paste0("uk.org.publicwhip/member/", speakerid))%>%
  mutate_all(as.character)

match90s <- match90s %>% left_join(members %>% select(mnis_id, display_as))

match00s <- read_csv("completed-matches/200.csv") %>% 
  rename(speakerid = speaker_id) %>% 
  select(hansard_membership_id, speakerid, person_id, mnis_id) %>%
  distinct() %>% 
  mutate(speakerid = paste0("uk.org.publicwhip/member/", speakerid))%>%
  mutate_all(as.character)

match00s <- match00s %>% left_join(members %>% select(mnis_id, display_as))

match10s <- read_csv("completed-matches/210.csv") %>% 
  rename(speakerid = speaker_id) %>% 
  select(hansard_membership_id, speakerid, person_id, mnis_id) %>%
  distinct() %>% 
  mutate(speakerid = paste0("uk.org.publicwhip/member/", speakerid))%>%
  mutate_all(as.character)

match10s <- match10s %>% left_join(members %>% select(mnis_id, display_as))

#  loop -----------------------------------------------------------

speaker_vector <- c("Speaker|chairman|Chaiman")

y_list <- list.files("debate-single-years")

names_df <- list()

date_df <- list()

pb <- progress_bar$new(total = length(y_list))

for (i in y_list) {
  
  year <- read_rds(paste0("debate-single-years/", i))
  
  if (i=="1979.rds") {
    year <- year %>% filter(date >= as.Date("1979-05-03"))
  }
  
  year <- year %>% 
    mutate(
      speech_class = case_when(
      is.na(speakername) & speech_class == "list()" ~ "Procedural",
      TRUE ~ speech_class
      ),
      speakername = case_when(
        is.na(speakername) ~ "Unknown",
        TRUE ~ speakername
      )
    )
  
  
  if (i=="1983.rds") {
    ## incorrect year is listed in Hansard
    year <- year %>% 
      mutate(
        id = if_else(date == as.Date("1983-02-13"),
                     str_replace_all(id, "1983", "1984"), id),
        year = if_else(date == as.Date("1983-02-13"),
                       1984, year),
        date = if_else(date == as.Date("1983-02-13"),
                       as.Date("1984-02-13"), date)
      )
    
  }
  
  if (any(str_detect(year$speech, "^[0-9]{1,2}\\.?[0-9]{0,2} [a,p]\\.m\\.$|\\n [0-9]{1,2}\\.?[0-9]{0,2} [a,p]m\\n"),
          na.rm = TRUE)) {
    
    #message(paste0("Fixing time for year ", i))
    
    time_fix1 <- year %>%
      filter(
        str_detect(speech,
                   "^[0-9]{1,2}\\.?[0-9]{0,2} [a,p]\\.m\\.$") 
      )
    
    time_fix2 <- year %>%
      filter(
        str_detect(speech,
                   "\\n [0-9]{1,2}\\.?[0-9]{0,2} [a,p]m\\n"))
    
    year <- year %>%
      filter(!(id %in% c(time_fix1$id, time_fix2$id)))
    
    # Fixing Time -------------------------------------------------------------
    ## need to extract time embedded in the text, with str_extract
    time_fix1 <- time_fix1 %>% 
      separate(speech, c("speech1", "am_pm"), sep = " ") %>%
      separate(speech1, c("hour", "min"), sep = "\\.") %>%
      mutate(hour = as.numeric(hour),
             hour = if_else(am_pm == "a.m." & hour ==12, hour-12, hour),
             hour = if_else(am_pm == "p.m.", hour+12, hour),
             min = if_else(is.na(min), "00", min),
             min = if_else(str_count(min)==1, paste0("0", min), min),
             time = paste0(hour, ":", min)) %>% 
      select(-hour, -min, -am_pm)
    
    
    time_fix2 <- time_fix2 %>% 
      mutate(
        time = str_extract_all(
          speech, "\\n [0-9]{1,2}\\.?[0-9]{0,2} [a,p]m\\n"),
        time = unlist(purrr::map(time, 1)),
        time = str_trim(str_remove_all(time, "\\n"))
      ) %>% 
      separate(time, c("time2", "am_pm"), sep = " ", remove=FALSE) %>%
      separate(time2, c("hour", "min"), sep = "\\.", remove=FALSE) %>% 
      mutate(hour = as.numeric(hour),
             hour = if_else(am_pm == "am" & hour ==12, hour-12, hour),
             hour = if_else(am_pm == "pm", hour+12, hour),
             min = if_else(is.na(min), "00", min),
             min = if_else(str_count(min)==1, paste0("0", min), min),
             time = paste0(hour, ":", min),
             speech = str_replace_all(
               speech, "\\n [0-9]{1,2}\\.?[0-9]{0,2} [a,p]m\\n", "\n")) %>% 
      select(-hour, -min, -am_pm, -time2)
    
    year <- year %>% bind_rows(time_fix1, time_fix2) %>% 
      arrange(date, sort1, sort2) %>% 
      group_by(date) %>% 
      fill(time) %>%
      ungroup() %>% 
      filter(!is.na(speech))
    
  }
  
  # Fixing Names ------------------------------------------------------------
  year <- year %>% 
    mutate(
      speakername = recode(speakername,
                           "Stephen Barclay" = "Steve Barclay",
                           "Andrew Slaughter" = "Andy Slaughter",
                           "Nicholas Dakin" = "Nic Dakin",
                           "Steven Baker" = "Steve Baker",
                           "Steve Pound" = "Stephen Pound",
                           "Chris Matheson" = "Christian Matheson",
                           "Stuart McDonald" = "Stuart C McDonald",
                           "Jeffrey M. Donaldson" = "Jeffrey M Donaldson",
                           "Rebecca Long-Bailey" = "Rebecca Long Bailey",
                           "Mary Kelly Foy" = "Mary Foy",
                           "Edward Balls" = "Ed Balls",
                           "Robert Wilson" = "Rob Wilson",
                           "Robin John Millar" = "Robin Millar",
                           "John Martin McDonnell" = "John McDonnell",
                           "Nicholas Fletcher" = "Nick Fletcher",
                           "Robert Alexander Courts" = "Robert Courts",
                           "Nicola Faye Richards" = "Nicola Richards",
                           "Sarah Elizabeth Dines" = "Sarah Dines",
                           "Darren George Henry" = "Darren Henry",
                           "Jonathan Edward Gullis" = "Jonathan Gullis",
                           "Richard Gordon Thomson" = "Richard Thomson",
                           "Jack Edgar Brereton" = "Jack Brereton",
                           "Duncan Charles Baker" = "Duncan Baker",
                           "Fay Alicia Jones" = "Fay Jones",
                           "Stuart Paul Anderson" = "Stuart Anderson",
                           "Angela Joy Richardson" = "Angela Richardson",
                           "Scott Lloyd Benton" = "Scott Benton",
                           "Julia Dockerill" = "Julia Lopez",
                           "Shaun Stephen Bailey" = "Shaun Bailey",
                           "Emma Little-Pengelly" = "Emma Little Pengelly",
                           "Imran Nasir Ahmad Khan" = "Imran Ahmad Khan",
                           "Jamie Hamilton Wallis" = "Jamie Wallis",
                           "Mary Kelly Foy" = "Mary Foy",
                           "Tanmanjeet Singh Dhesi" = "Tan Dhesi",
                           "Ian Paisley Jnr" = "Ian Paisley",
                           "Jonathan Edward Gullis" =  "Jonathan Gullis",
                           "Naseem Shah" = "Naz Shah",
                           "Daniel Poulter" = "Dan Poulter",
                           "Michael Crockart" = "Mike Crockart",
                           "Gerard Killen" = "Ged Killen",
                           "Suella Fernandes" = "Suella Braverman",
                           "Rob Flello" = "Robert Flello",
                           "Phil Boswell" = "Philip Boswell",
                           "Michael Weir" = "Mike Weir",
                           "Stuart Donaldson" = "Stuart Blair Donaldson",
                           "Lee Benjamin Rowley" = "Lee Rowley",
                           "Martin Docherty" =  "Martin Docherty-Hughes",
                           "Edward Davey" = "Ed Davey"
      ),
      speakername = str_remove(speakername, "Dr "),
      speakername = str_remove(speakername, "Sir "),
      speakername = str_remove(speakername, "Dame ")
    )
  
# matching files ----------------------------------------------------------

  if (i=="1979.rds") {
    
    year <- year %>% 
      left_join(match1979, 
                by = c("hansard_membership_id", "speakerid", "person_id"))
    
  } else if (i %in% y_list[2:11]) {
    year <- year %>%
      left_join(match80s, by = "hansard_membership_id")
  } else if (i %in% y_list[12:21]) {
    year <- year %>%
      left_join(match90s,
                by = c("hansard_membership_id", "speakerid", "person_id"))
  } else if (i %in% y_list[22:31]) {
    year <- year %>%
      left_join(match00s,
                by = c("hansard_membership_id", "speakerid", "person_id"))
  } else if (i %in% y_list[32:36]) { ## doesn't include 2019 or 2020
    year <- year %>%
      left_join(match10s, 
                by = c("hansard_membership_id", "speakerid", "person_id"))
  } else { #if (i %in% y_list[37:42]) {
    year <- year %>%
      left_join(members %>%
                  select(mnis_id, display_as),
                by = c("speakername" = "display_as")) %>% 
      mutate(display_as = speakername)
  }
  

# Name Corrections --------------------------------------------------------
  year <- year %>%
    distinct(id, .keep_all = TRUE) %>%
    mutate(
      mnis_id = case_when(
        str_detect(speakername, "Enoch Powell") ~ "1023",
        str_detect(speakername, "Godman Irvine") ~ "878",
        str_detect(speakername,  "God-man Irvine") ~ "878",
        str_detect(speakername,  "Fookes") ~ "830",
       
        str_detect(speakername, "Ernest Armstrong") ~ "968",
        str_detect(speakername, "Kenneth Clarke") ~ "366",
        str_detect(speakername, "R. C. Mitchell") ~ "1353",
        str_detect(speakername, "R. C.Mitchell") ~ "1353",
        str_detect(speakername, "R. C Mitchell") ~ "1353",
        str_detect(speakername, "loan Evans") ~ "1012",
        str_detect(speakername, "Rhys Williams") ~ "702",
        
        
        str_detect(speakername, "Öpik") ~ "559",
        str_detect(speakername, "Alun Cairns") ~ "4086",
        str_detect(speakername, "John M. Taylor") ~ "313",
        str_detect(speakername, "John Mark Taylor") ~ "313",
        str_detect(speakername, "John D. Taylor") ~ "657",
        str_detect(speakername, "John David Taylor") ~ "657",
        str_detect(speakername, "Alan W. Williams") ~ "536",
        str_detect(speakername, "Robert G. Hughes") ~ "980",
        str_detect(speakername, "Heiler") ~ "725",
        str_detect(speakername, "Heller") ~ "725",
        str_detect(speakername, "Helfer") ~ "725",
        str_detect(speakername, "Heifer") ~ "725",
        str_detect(speakername, "Hefter") ~ "725",
        str_detect(speakername, "Hafer") ~ "725",
        str_detect(speakername, "Haffer") ~ "725",
        str_detect(speakername, "Fave11") ~ "823",
        str_detect(speakername, "Fayell") ~ "823",
        str_detect(speakername, "Nellist") ~ "1199",
        str_detect(speakername, "Mark Robinson") ~ "1154",
        str_detect(speakername, "Norman A. Goclman") ~ "616",
        str_detect(speakername, "Bermingham") ~ "490",
        str_detect(speakername, "Marek") ~ "535",
        str_detect(speakername, "Straubenezee") ~ "1371",
        str_detect(speakername, "Bravo") ~ "759",
        str_detect(speakername, "Lestor") ~ "904",
        str_detect(speakername, "dalye") ~ "622",
        str_detect(speakername, "Weatheril") ~ "960",
        str_detect(speakername, "Wetherill") ~ "960",
        str_detect(speakername, "Weatberill") ~ "960",
        str_detect(speakername, "Wriggles worth") ~ "965",
        str_detect(speakername, "Good hew") ~ "847",    
        str_detect(speakername, "Crawsliaw") ~ "793",
        str_detect(speakername, "Crawstaw") ~ "793",        
        str_detect(speakername, "Heseltime") ~ "94",
        str_detect(speakername, "George Younger") ~ "969",
        str_detect(speakername, "Mr. Younger") ~ "969",
        str_detect(speakername, "George Young$") ~ "57",
        str_detect(speakername, "George Young\\)") ~ "57",
        str_detect(speakername, "Mtchell") ~ "372",
        str_detect(speakername,  "Michael Cocks") ~ "782",
       
        str_detect(speakername, "William O'Brien") ~ "419",
        str_detect(speakername, "Rober Ainsworth") ~ "306",
        str_detect(speakername, " Milian") ~ "704",
        str_detect(speakername, "Gamier") ~ "337",
        str_detect(speakername, "Bowman") ~ "886",
        str_detect(speakername, "Kellet") ~ "886",
        str_detect(speakername, "Crawshaw") ~ "793",
        str_detect(speakername, "Ottaway") ~ "157",
        str_detect(speakername, "Mr. Torrey") ~ "1177",
        str_detect(speakername, "Mr.Burden") ~ "766",
        str_detect(speakername, "Edwina Currie") ~ "1140",
        str_detect(speakername, "Mrs. Currie") ~ "1140",
        str_detect(speakername, "Meadowcroft") ~ "1376",
        str_detect(speakername, "Thurso") ~ "1399",
        str_detect(speakername, "Eadie") ~ "811",
        str_detect(speakername, "Hubbard") ~ "873",
        str_detect(speakername, "Raffan") ~ "1280",
        str_detect(speakername, "Terlezki") ~ "1378",
        str_detect(speakername, "Milan") ~ "704",
        str_detect(speakername, "Hatter") ~ "858",
        str_detect(speakername, "Willott") ~ "1497",
        str_detect(speakername, "Amery") ~ "739",
        str_detect(speakername, "Jannner") ~ "880",
        str_detect(speakername, "Janner") ~ "880",
        str_detect(speakername, "Stevas") ~ "946",
        str_detect(speakername, "Savours") ~ "499",
        str_detect(speakername, "Hyslop") ~ "1296",
        str_detect(speakername, "Lomond") ~ "894",
        str_detect(speakername, "Ronald W. Brown") ~ "1061",
        str_detect(speakername, "Dickson") ~ "909", 
        str_detect(speakername, "Dr. Mahon") ~ "909",
        str_detect(speakername, "Dickson Mahon") ~ "909",
        str_detect(speakername, "Alice Mahon") ~ "409",
        str_detect(speakername, "Mrs. Mahon") ~ "409",
        str_detect(speakername, "Mrs Mahon") ~ "409",
        str_detect(speakername, "Mrs.Mahon") ~ "409",
        str_detect(speakername, "Ms. Mahon") ~ "409",
        str_detect(speakername, "Jim McMahon") ~ "4569",
        str_detect(speakername, "Mrs. Ann Win.terton") ~ "425",
        str_detect(speakername, "Fitt") ~ "828",
        str_detect(speakername, "Malcolm Rifkhid") ~ "1191",
        str_detect(speakername, "Ritkind") ~ "1191",
        str_detect(speakername, "Rifland") ~ "1191",
        str_detect(speakername, "Rikfind") ~ "1191",
        str_detect(speakername, "Rilkind") ~ "1191",
        str_detect(speakername, "Rifking") ~ "1191",
        str_detect(speakername, "Rifldnd") ~ "1191",
        str_detect(speakername, "Rifkind") ~ "1191",
        str_detect(speakername, "Francis Pym") ~ "1001",
        str_detect(speakername, "Buchanan-Smith") ~ "727",   
        str_detect(speakername, "George Cunningham") ~ "1100",   
        str_detect(speakername, "Martin Smyth") ~ "644",
        str_detect(speakername, "John Patten") ~ "1137",
        str_detect(speakername, "Peter Bottomley") ~ "117",
        str_detect(speakername, "Virginia Bottomley") ~ "106",
        str_detect(speakername, "Mrs. Bottomley") ~ "106",
        str_detect(speakername, "Mrs Bottomley") ~ "106",
        str_detect(speakername, "Phillip Oppenheim") ~ "928",
        str_detect(speakername, "David Steel") ~ "949",
        str_detect(speakername, "Tebbit") ~ "952",
        str_detect(speakername, "Tebblt") ~ "952",
        str_detect(speakername, "Grifflths") ~ "1098", 
        str_detect(speakername, "Brocklebank") ~ "762",
        str_detect(speakername, "Roger Monte") ~ "922",
        str_detect(speakername, "Brocldebank") ~ "762",   
        str_detect(speakername, "John Meddle") ~ "713",   
        str_detect(speakername, "Nicholas Winterton") ~ "430",
        str_detect(speakername, "George Thomas") ~ "1300",
        str_detect(speakername, "Marcus Fox") ~ "1025",
        str_detect(speakername,  "Paul Bryan") ~ "1042",
        str_detect(speakername,  "I. Gilmour") ~ "844",
        str_detect(speakername, "Robert Maclennan") ~ "578",
        str_detect(speakername, "MacIennan") ~ "578",
        str_detect(speakername, "Greville Banner") ~ "880",
        str_detect(speakername, "Francis Maude") ~ "115",
        str_detect(speakername, "Eddie McCrady") ~ "656",
        str_detect(speakername, "Ian McCartney") ~ "448",
        str_detect(speakername, "Portillo") ~ "187",
        str_detect(speakername, "Mowlam") ~ "503",
        str_detect(speakername, "Douglas-Hamilton") ~ "802",
        str_detect(speakername, "Douglas Hurd") ~ "875",
        str_detect(speakername, "James Molyneaux") ~ "923",
        str_detect(speakername, "Leon Britton") ~ "709",
        str_detect(speakername, "Brirtan") ~ "709",
        str_detect(speakername, "Britton") ~ "709",
        str_detect(speakername, "Brittan") ~ "709",
        str_detect(speakername, "Evving") ~ "1103",
        str_detect(speakername, "Speed") ~ "1234",
        str_detect(speakername, "Frank Holey") ~ "869",
        str_detect(speakername, "Frank Hooky") ~ "1234",
        str_detect(speakername, "Joel Burnett") ~ "1018",
        str_detect(speakername, "Alexander Fletcher") ~ "983",
        str_detect(speakername, "James Thin") ~ "1361",
        str_detect(speakername, "Mr. Thin") ~ "1361",
        str_detect(speakername, "John-Stevan") ~ "946",
        str_detect(speakername,  "Cwilym Roberts") ~ "1185",  
        str_detect(speakername, "Gard-Jones") ~ "840",
        str_detect(speakername,  "Carel-Jones") ~ "840",  
       
 
        TRUE ~ mnis_id
      ))
  
  summary(is.na(year$mnis_id))
  
  year <- year %>% 
    mutate(mnis_id = case_when(
      str_detect(speakername, "Speaker") &
        mnis_id == "1300" & 
        date == as.Date("1984-02-13") ~ "960",
      speakername == "John Thurso" ~ "1399",
      speakername == "Mr. Speaker-Elect" & 
        date == as.Date("1979-05-09") ~ "1300",
      TRUE ~ mnis_id)
    )
  
  speaker_fix <- year %>%
    filter(str_detect(speakername,
                      regex(speaker_vector, ignore_case = TRUE))) %>% 
    group_by(date) %>% 
    fill(mnis_id)
  
  without_speaker <- year %>%
    filter(!str_detect(speakername,
                       regex(speaker_vector, ignore_case = TRUE)))
  
  year <- speaker_fix %>% bind_rows(without_speaker) %>% 
    arrange(date, sort1, sort2) 
  
  names_df[[i]] <- unique(year$display_as)
  
  date_df[[i]] <- unique(year$date)
  
  #year <- year %>% mutate(mnis_id = if_else(mnis_id == "4537", "127", mnis_id))
  ## need to check the status of this 
  save_name <- paste0("debate-single-years/", i)
  
  write_rds(year, file = save_name)
  
  pb$tick()
  
}

names_df <- tibble(display_as=unique(unlist(names_df))) %>%
  left_join(members %>%
              select(mnis_id, display_as, gender, party_id, party_text)) %>%
  filter(!is.na(mnis_id), !(mnis_id %in% c(2276, 1784, 2150, 2276, 2989)))

date_df <- tibble(date=as.Date(unique(unlist(date_df)),
                               origin = "1970-01-01"))

write_rds(date_df, "data/date_df.rds")

write_rds(names_df, "data/names_df.rds")
