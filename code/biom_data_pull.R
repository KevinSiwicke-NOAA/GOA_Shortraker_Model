library(tidyverse)
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
                where     species_code = '30576' and 
                          fmp_management_area = 'GOA' and 
                          exploitable = 1 and 
                          country = 'United States'
                order by  year asc
                ") %>% 
  rename_all(tolower)

cpue <- rpw %>% 
  filter(year > 1991) %>% 
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
                where     species_code = '30576' and 
                          survey = 'GOA' 
                order by  year asc
                ") %>% 
  rename_all(tolower)

biomass <- biom %>% 
  mutate(strata = ifelse(stratum %in% c(10:13, 110:112, 210, 310, 410, 510), 'WGOA',
                         ifelse(stratum %in% c(20:35, 120:134, 220:232, 32, 320, 330, 420, 430, 520, 530), 'CGOA',
                                ifelse(stratum %in% c(40:50, 140:151, 240:251, 340:351, 440, 450, 540, 550), 'EGOA', NA)))) %>% 
  group_by(year, strata) %>% 
  summarize(n = n(), biomass = sum(stratum_biomass, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

biomass_dat <- left_join(data.frame('year' = rep(unique(biomass$year), each = 3), 'strata' = rep(unique(biomass$strata), length(unique(biomass$year)))), biomass, by = c('year', 'strata'))

model_yrs <- 1984:YEAR

# ggplot(biomass_dat, aes(year, biomass)) +
#   geom_line() +
#   facet_wrap(~strata)

# This is the data that is brought into rema
model_dat <- list('biomass_dat' = biomass_dat, 'cpue_dat' = cpue_dat, 
                  'model_yrs' = model_yrs)

