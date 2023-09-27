library(dplyr)
library(DBI)
library(keyring)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                dsn = db,
                uid = keyring::key_list(db)$username,
                pwd =  keyring::key_get(db, keyring::key_list(db)$username))

# Get longline survey RPWs
rpw <- dbGetQuery(channel_akfin, 
                "select    *
                from      afsc.lls_area_rpn_all_strata
                where     species_code = '21230' and 
                          fmp_management_area = 'GOA' and 
                          exploitable = 1 and 
                          country = 'United States'
                order by  year asc
                ") %>% 
  rename_all(tolower)

cpue <- rpw %>% 
  filter(year > 1991, year < YEAR + 1) %>% 
  group_by(year, strata = council_management_area) %>% 
  mutate(strata = ifelse(strata == 'Western Gulf of Alaska', 'WGOA',
                         ifelse(strata == 'Central Gulf of Alaska', 'CGOA',
                                ifelse(strata == 'Eastern Gulf of Alaska', 'EGOA', NA)))) %>% 
  summarize(cpue = sum(rpw, na.rm = TRUE),
            cv = sqrt(sum(rpw_var, na.rm = TRUE)) / cpue)

cpue_dat <- left_join(data.frame('year' = rep(unique(cpue$year), each = 3), 'strata' = rep(c('WGOA', 'CGOA', 'EGOA'), length(unique(cpue$year)))), cpue, by = c('year', 'strata')) 

# ggplot(cpue_dat, aes(year, cpue)) +
#    geom_line() +
#    facet_wrap(~strata)

# Get bottom trawl survey biomass data
biom <- dbGetQuery(channel_akfin, 
                "select    *
                from      afsc.race_biomassstratumaigoa
                where     species_code = '21230' and 
                          survey = 'GOA' 
                order by  year asc
                ") %>% 
  rename_all(tolower)

strata <- dbGetQuery(channel_akfin, 
                    "select    *
                from      afsc.race_goastrataaigoa
                where     survey = 'GOA'
                ") %>% 
  rename_all(tolower)

biom2 <- left_join(biom, strata, by = c("stratum"))  

# Using all data like previous model
biomass <- biom %>% 
  mutate(strata = ifelse(stratum %in% c(10:13, 110:112, 210, 310), 'WGOA (0-500 m)',
                         ifelse(stratum %in% c(20:35, 120:134, 220:232, 32, 320, 330), 'CGOA (0-500 m)',
                                ifelse(stratum %in% c(40:50, 140:151, 240:251, 340:351), 'EGOA (0-500 m)',
                                       ifelse(stratum == 410, 'WGOA (501-700 m)',
                                              ifelse(stratum == 510, 'WGOA (701-1000 m)',
                                                     ifelse(stratum %in% c(420, 430), 'CGOA (501-700 m)',
                                                            ifelse(stratum %in% c(520, 530), 'CGOA (701-1000 m)',
                                                                   ifelse(stratum %in% c(440, 450), 'EGOA (501-700 m)',
                                                                          ifelse(stratum %in% c(540, 550), 'EGOA (701-1000 m)', NA)))))))))) %>% 
  group_by(year, strata) %>% 
  summarize(n = n(), biomass = sum(stratum_biomass, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

biomass_dat <- left_join(data.frame('year' = rep(unique(biomass$year), each = 9), 'strata' = rep(unique(biomass$strata), length(unique(biomass$year)))), biomass, by = c('year', 'strata')) %>% 
  mutate(cv = ifelse (cv == 0, 0.1, cv)) 

model_yrs <- 1990:YEAR

# biomass_alt <- biom2 %>% 
#   filter(stratum_type == 'SLOPE' | stratum_type == 'GULLY') %>% 
#   filter(min_depth > 200, min_depth < 701) %>% 
#   mutate(strata = ifelse(regulatory_area_name == "WESTERN GOA", "WGOA",
#                          ifelse(regulatory_area_name == "EASTERN GOA", "EGOA", "CGOA"))) %>% 
#   group_by(year, strata) %>% 
#   summarize(n = n(), biomass = sum(stratum_biomass, na.rm = TRUE),
#             cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 
# 
# biomass_alt_dat <- left_join(data.frame('year' = rep(unique(biomass_alt$year), each = 3), 'strata' = rep(unique(biomass_alt$strata), length(unique(biomass_alt$year)))), biomass_alt, by = c('year', 'strata'))

# ggplot(biomass_dat, aes(year, biomass)) +
#   geom_line() +data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
#   facet_wrap(~strata)

# This is the data that is brought into rema
model_dat <- list('biomass_dat' = biomass_dat, 'cpue_dat' = cpue_dat, 
                  'model_yrs' = model_yrs)
