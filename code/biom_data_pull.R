library(dplyr)
library(DBI)
library(keyring)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username[1],
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username[1]))

# Get longline survey RPWs
rpw <- dbGetQuery(channel_akfin, 
                  "select    *
                from      afsc.lls_area_rpn_all_strata
                where     species_code = '30576' and 
                          fmp_management_area = 'GOA' and 
                          exploitable = 1 and 
                          country = 'United States'
                order by  year asc
                ") %>% 
  rename_all(tolower)

readr::write_csv(rpw, here::here("data", "sr_area_rpn_all_strata.csv"))

# for katy 10/26/2023
cpue <- rpw %>% 
  filter(year > 1991, year < YEAR + 1) %>% 
  group_by(year, strata = council_sablefish_management_area) %>% 
  mutate(strata = ifelse(strata == 'Western Gulf of Alaska', 'WGOA',
                         ifelse(strata == 'Central Gulf of Alaska', 'CGOA',
                         ifelse(strata == 'West Yakutat', 'WY',
                                ifelse(strata == 'East Yakutat/Southeast', 'EY/SE', NA))))) %>% 
  summarize(cpue = sum(rpw, na.rm = TRUE),
            cv = sqrt(sum(rpw_var, na.rm = TRUE)) / cpue)

readr::write_csv(cpue, here::here("data", "sr_rpw_split_egoa.csv"))

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
                from      gap_products.akfin_biomass
                where     species_code = '30576' and 
                          survey_definition_id = 47
                order by  year asc
                ") %>% 
  rename_all(tolower)


# Using all data like previous model
biomass <- biom |> 
  mutate(biomass_var = ifelse(is.na(biomass_var), (0.5 * biomass_mt) ^ 2, 
                              ifelse(biomass_var == 0 & biomass_mt > 0, (0.5 * biomass_mt) ^ 2, biomass_var)),
         strata = ifelse(area_id %in% c(10:15, 110:113, 210:211, 310, 410, 510:511), 'WGOA',
                         ifelse(area_id %in% c(20:38, 120:136, 220:232, 32, 320:321, 330, 420, 430, 520:521, 530:531), 'CGOA',
                                ifelse(area_id %in% c(40:51, 140:152, 240:252, 340:352, 440, 450, 540:541, 550:551), 'EGOA', NA)))) %>% 
  filter(!is.na(strata)) %>% 
  group_by(year, strata) %>% 
  summarize(n = sum(n_haul), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

biomass_dat <- left_join(data.frame('year' = rep(unique(biomass$year), each = 3), 'strata' = rep(unique(biomass$strata), length(unique(biomass$year)))), biomass, by = c('year', 'strata'))

biomass_old <- biom |> 
         mutate(strata = ifelse(area_id %in% c(210, 310, 410, 510), 'WGOA',
                                ifelse(area_id %in% c(220:232, 32, 320, 330, 420, 430, 520, 530), 'CGOA',
                                       ifelse(area_id %in% c(240:251, 340:351, 440, 450, 540, 550), 'EGOA', NA)))) %>% 
  filter(!is.na(strata)) %>% 
  group_by(year, strata) %>% 
  summarize(n = sum(n_haul), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

biomass_old_dat <- left_join(data.frame('year' = rep(unique(biomass_old$year), each = 3), 'strata' = rep(unique(biomass_old$strata), length(unique(biomass_old$year)))), biomass, by = c('year', 'strata')) %>% 
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
                  'biomass_dat_old' = biomass_old_dat, 'model_yrs' = model_yrs)

DBI::dbDisconnect(channel_akfin)
