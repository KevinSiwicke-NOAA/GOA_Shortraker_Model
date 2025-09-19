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
                from      gap_products.akfin_sizecomp
                where     species_code = '30576' and
                          SURVEY_DEFINITION_ID = 47 and 
                          year > 1989 and
                          length_mm > 0") %>% 
  rename_all(tolower)

# get age data to population
bts.sr.age <- dbGetQuery(channel_akfin, 
                          "select    *
                from      gap_products.akfin_agecomp 
                where     species_code = '30576' and
                          SURVEY_DEFINITION_ID = 47 and 
                          year > 1989") %>% 
  rename_all(tolower) %>% 
  write_csv(paste0(dat_path, "/goa_sr_bts_ages", YEAR, ".csv"))

# Age sample sizes
bts.sr.spec <- dbGetQuery(channel_akfin, 
                         "select    *
                from      gap_products.akfin_specimen 
                where     species_code = '30576' and
                          SURVEY_DEFINITION_ID = 47 and 
                          age > 0") %>% 
  rename_all(tolower) %>% 
  filter(age > 0) %>% 
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
  mutate(region = ifelse(nmfs_area == 610, 'WGOA', 
                         ifelse(nmfs_area == 620 | nmfs_area == 630, 'CGOA',
                                ifelse(nmfs_area == 640 | nmfs_area == 650, 'EGOA', NA)))) %>% 
  write_csv(paste0(dat_path, "/goa_sr_fishery_lengths", YEAR, ".csv"))
  
# Fishery Catch
fsh.sr.cat <- dbGetQuery(channel_akfin, 
                  "select    *
                  from      council.comprehensive_blend_ca
                  where     species_group_code = 'SRKR' and 
                            fmp_area = 'GOA' and
                            year >= 2010
                                  ") %>% 
  rename_all(tolower) %>% 
  filter(weight_posted > 0) %>% 
  mutate(region = ifelse(reporting_area_code == 610, 'WGOA', 
                         ifelse(reporting_area_code == 620 | reporting_area_code == 630, 'CGOA',
                                ifelse(reporting_area_code == 640 | reporting_area_code == 650, 'EGOA', NA)))) %>% 
  write_csv(paste0("data/", YEAR, "/shortraker_catch.csv"))
