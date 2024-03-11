library(dplyr)
library(DBI)
library(keyring)
library(ggplot2)

YEAR = 2023

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username,
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username))

# Get bottom trawl survey biomass data
biom <- dbGetQuery(channel_akfin, 
                   "select    *
                from      afsc.race_biomassstratumaigoa
                where     species_code = '30576' and 
                          survey = 'GOA' and 
                          year > 1989
                order by  year asc
                ") %>% 
  rename_all(tolower)

# strata <- dbGetQuery(channel_akfin, 
#                      "select    *
#                 from      afsc.race_goastrataaigoa
#                 where     survey = 'GOA'
#                 ") %>% 
#   rename_all(tolower)

biomass <- biom %>% 
  mutate(strata = ifelse(stratum %in% c(10:13, 110:112, 210, 310, 410, 510), 'WGOA',
                         ifelse(stratum %in% c(20:35, 120:134, 220:232, 32, 320, 330, 420, 430, 520, 530), 'CGOA',
                                ifelse(stratum %in% c(40:50, 140:151, 240:251, 340:351, 440, 450, 540, 550), 'EGOA', NA)))) %>% 
  group_by(year, strata) %>% 
  summarize(n = n(), total_catch = sum(catch_count), biomass = sum(stratum_biomass, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

biomass_dat <- left_join(data.frame('year' = rep(unique(biomass$year), each = 3), 'strata' = rep(unique(biomass$strata), length(unique(biomass$year)))), biomass, by = c('year', 'strata'))

# Now using gap_products...
biom_new <- dbGetQuery(channel_akfin, 
                       "select    *
                from      gap_products.akfin_biomass
                where     species_code = '30576' and 
                          survey_definition_id = 47 and 
                          year < 2024
                order by  year asc
                ") %>% 
  rename_all(tolower)

# strata_new <- dbGetQuery(akfin, 
#                 "select    *
#                 from      gap_products.akfin_area
#                 where     survey_definition_id = 47 and
#                           area_type = 'STRATUM' and
#                           design_year = 1984
#                 ") %>% 
#   rename_all(tolower)

biomass_new <- biom_new %>% 
  mutate(strata = ifelse(area_id %in% c(10:13, 110:112, 210, 310, 410, 510), 'WGOA',
                         ifelse(area_id %in% c(20:35, 120:134, 220:232, 32, 320, 330, 420, 430, 520, 530), 'CGOA',
                                ifelse(area_id %in% c(40:50, 140:151, 240:251, 340:351, 440, 450, 540, 550), 'EGOA', NA)))) %>% 
  filter(!is.na(strata)) %>% 
  group_by(year, strata) %>% 
  summarize(n = n(), total_catch = sum(n_count), biomass = sum(biomass_mt, na.rm = TRUE),
            cv = sqrt(sum(biomass_var, na.rm = TRUE))/biomass) 

biomass_dat_new <- left_join(data.frame('year' = rep(unique(biomass_new$year), each = 3), 'strata' = rep(unique(biomass_new$strata), length(unique(biomass_new$year)))), biomass_new, by = c('year', 'strata'))

model_yrs <- 1990:YEAR

# Combine and compare
compare <- biomass_dat_new %>% 
  mutate(schema = "akfin.gap_products") %>% 
  select(schema, year, strata, n, total_catch, biomass, cv) %>% 
  bind_rows(biomass_dat %>% 
              mutate(schema = "akfin.afsc") %>% 
              select(schema, year, strata, n, total_catch, biomass, cv)) 

ggplot(compare, aes(x = factor(year), y = biomass, fill = schema)) +
  geom_bar(stat = 'identity', position = position_dodge2(width = 0.5, preserve = "single", padding = 0)) +
  geom_errorbar(aes(ymin = biomass - cv * biomass, ymax = biomass + cv * biomass),
                position = position_dodge2(width = 0.5, preserve = "single", padding = 0)) +
  facet_grid(~strata, scale = 'free_y') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL)
