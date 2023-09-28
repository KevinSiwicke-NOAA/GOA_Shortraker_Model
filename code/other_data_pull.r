library(dplyr)
library(DBI)
library(keyring)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username,
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username))

lls.sr.len <- dbGetQuery(channel_akfin, 
                          "select    *
                from      afsc.lls_length_rpn_by_area_all_strata
                where     species_code = '30576' 
                order by  year asc") %>% 
  rename_all(tolower) %>% 
  filter(year > 1991, year < YEAR + 1, !council_sablefish_management_area == "Aleutians",
         !council_sablefish_management_area == "Bering Sea")

bts.sr.len <- dbGetQuery(channel_akfin, 
                          "select    *
                from      afsc.race_sizestratumaigoa 
                where     species_code = '30576'") %>% 
  rename_all(tolower) %>% 
  filter(year > 1989, year < YEAR + 1, survey == "GOA")

# get age data to population
bts.sr.age <- dbGetQuery(channel_akfin, 
                          "select    *
                from      afsc.race_agecomptotalaigoa 
                where     species_code = '30576'") %>% 
  rename_all(tolower) %>% 
  filter(survey == "GOA")

# Age sample sizes
bts.sr.spec <- dbGetQuery(channel_akfin, 
                         "select    *
                from      afsc.race_specimenaigoa 
                where     species_code = '30576'") %>% 
  rename_all(tolower) %>% 
  filter(region == "GOA", age > 0) %>% 
  mutate(survey_year = (cruise - 1) / 100) %>% 
  group_by(survey_year) %>% 
  summarize(Num = n())
  
#Fishery Lengths
fsh.sr.len <- dbGetQuery(channel_akfin,
                          "select    *
                from      norpac.debriefed_length
                where     species = 326 and
                          year >=1989") %>% 
  rename_all(tolower) %>% 
  filter(year < YEAR + 1)
  