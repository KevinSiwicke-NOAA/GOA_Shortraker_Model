library(tidyverse)
library(DBI)
library(keyring)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username,
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username))

lls.sst.len <- dbGetQuery(channel_akfin, 
                          "select    *
                from      afsc.lls_length_rpn_by_area_all_strata
                where     species_code = '30576' 
                order by  year asc") %>% 
  rename_all(tolower) %>% 
  filter(year > 1991, !council_sablefish_management_area == "Aleutians",
         !council_sablefish_management_area == "Bering Sea")

bts.sst.len <- dbGetQuery(channel_akfin, 
                          "select    *
                from      afsc.race_sizestratumaigoa 
                where     species_code = '30576'") %>% 
  rename_all(tolower) %>% 
  filter(year > 1989, survey == "GOA")

#Fishery Lengths
fsh.sst.len <- dbGetQuery(channel_akfin,
                          "select    *
                from      norpac.debriefed_length
                where     species = 326 and
                          year >=1989") %>% 
  rename_all(tolower)