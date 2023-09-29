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
         !council_sablefish_management_area == "Bering Sea") %>% 
  write_csv(paste0(dat_path, "/goa_sr_lls_lengths", YEAR, ".csv"))

bts.sr.len <- dbGetQuery(channel_akfin, 
                          "select    *
                from      afsc.race_sizestratumaigoa 
                where     species_code = '30576'") %>% 
  rename_all(tolower) %>% 
  filter(year > 1989, year < YEAR + 1, survey == "GOA") %>% 
  write_csv(paste0(dat_path, "/goa_sr_bts_lengths", YEAR, ".csv"))

# get age data to population
bts.sr.age <- dbGetQuery(channel_akfin, 
                          "select    *
                from      afsc.race_agecomptotalaigoa 
                where     species_code = '30576'") %>% 
  rename_all(tolower) %>% 
  filter(survey == "GOA") %>% 
  write_csv(paste0(dat_path, "/goa_sr_bts_ages", YEAR, ".csv"))

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
                          nmfs_area > 609 and
                          nmfs_area < 651 and 
                          year >= 1989") %>% 
  rename_all(tolower) %>% 
  filter(year < YEAR + 1) %>% 
  write_csv(paste0(dat_path, "/goa_sr_fishery_lengths", YEAR, ".csv"))
  
# Fishery Catch
fsh.sr.cat <- dbGetQuery(channel_akfin, 
                  "select    *
                  from      council.comprehensive_norpac
                  where     species_name = 'SHORTRAKER ROCKFISH' and
                            nmfs_area > 609 and
                            nmfs_area < 651 and
                            year >= 2010 
                ") %>% 
  rename_all(tolower) %>% 
  filter(gear_type == 1 | gear_type == 8, !nmfs_area == 649, extrapolated_number > 0) %>% 
  mutate(region = ifelse(nmfs_area == 610, 'WGOA', 
                         ifelse(nmfs_area == 620 | nmfs_area == 630, 'CGOA',
                                ifelse(nmfs_area == 640 | nmfs_area == 650, 'EGOA', NA)))) %>% 
  write_csv(paste0("data/", YEAR, "/shortraker_catch.csv"))
