# GOA shortraker biomass estimation using the bottom trawl and longline survey indices

# Model naming conventions:
# m23.3 (xtra LLS OE), accepted in 2023

# Set up ----

# assessment year
YEAR <- 2026

# Consider whether the rema package needs to be 'updated' - will need to update to get new extra_cv fxns
# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE) #, force = TRUE

libs <- c('rema', 'readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 15) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# Read data ----
source("code/biom_data_pull.r")

# bottom trawl survey
biomass_dat <- model_dat$biomass_dat
biomass_dat %>% 
  write_csv(paste0(dat_path, "/goa_sr_biomass_", YEAR, ".csv"))
biomass_dat %>%
  tidyr::expand(year = min(biomass_dat$year):(YEAR),
                strata) %>%
  left_join(biomass_dat %>%
              mutate(value = ifelse(is.na(biomass), NA,
                                    paste0(prettyNum(round(biomass, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) %>%
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) %>%
  arrange(year) %>%
  write_csv(paste0(out_path, '/biomass_data_wide.csv'))

# longline survey rpws
cpue_dat <- model_dat$cpue_dat 
cpue_dat %>% 
  write_csv(paste0(dat_path, "/goa_sr_rpw_", YEAR, ".csv"))

# Model 23.3 is Model 23.1 with additional obs error for LLS -----
input <- prepare_rema_input(model_name = 'Model 23.3',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            start_year = 1990,
                            end_year = YEAR + 2,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 2, 3),
                              pointer_q_cpue = c(1, 2, 3)),
                            extra_cpue_cv = list(assumption = 'extra_cv'))

m23.3 <- fit_rema(input)
out23.3 <- tidy_rema(m23.3)
out23.3 <- tidy_extra_cv(out23.3) # adds a few new columns to biomass_by_strata and/or cpue_by_strata with the 'total' observation error (assumed from design-based + estimated)

out23.3$cpue_by_strata %>% select(model_name, strata, year, obs, contains(c('obs_lci','obs_uci')))

plot_extra_cv(out23.3)$cpue_by_strata # bold error bar w/ whiskers = assumed CV from design-based survey estimates, full error bar w/ no whiskers = total (assumed + estimated)
out23.3$parameter_estimates
compare <- compare_rema_models(rema_models = list(m23.3))

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     theme(legend.position = 'top', legend.text = element_text(size = 16)) +
                     facet_wrap(~factor(strata, levels=c('WGOA', 'CGOA', 'EGOA')), ncol = 3) +
                     geom_line(linewidth = 1.2) +
                     labs(x = NULL, y = 'Biomass (t)',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF')) +
                     coord_cartesian(ylim=c(0, 80000)) +
                     scale_color_discrete(type = c('#440154FF')) +
                     scale_x_continuous(limits = c(1989, 2028)) +
                     scale_y_continuous(expand = c(0,0), limits = c(0, 130000)), 
                   compare$plots$cpue_by_strata  +
                     theme(legend.position = 'none') +
                     facet_wrap(~factor(strata, levels=c('WGOA', 'CGOA', 'EGOA')), ncol = 3) +
                     geom_line(linewidth = 1.2) +
                     labs(x = NULL, y = 'Relative Population Weights',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     scale_fill_discrete(type = c('#440154FF')) +
                     # coord_cartesian(ylim=c(0, 60000)) +
                     scale_color_discrete(type = c('#440154FF')) +
                     scale_x_continuous(limits = c(1990, 2028)) +
                     scale_y_continuous(expand = c(0,0), limits = c(0, 50000)),
                   ncol = 1,
                   rel_heights = c(0.9, 1))

ggsave(filename = paste0(out_path, '/M23.3_strata_fits.png'),
       dpi = 300, bg = 'white', units = 'in', height = 9, width = 14)

compare$plots$total_predicted_biomass +
  theme(legend.position = 'top', legend.text = element_text(size = 16)) +
  labs(y = 'Biomass (t)',
       fill = NULL, colour = NULL) +
  scale_fill_discrete(type = c('#440154FF', '#FDE725FF')) +
  scale_color_discrete(type = c('#440154FF', '#FDE725FF')) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 52000)) +
  geom_line(linewidth = 1.2)

ggsave(filename = paste0(out_path, '/M23.3_totalbiomass.png'),
       dpi = 300, bg = 'white', units = 'in', height = 6, width = 13)

params <- compare$output$parameter_estimates %>% 
  write_csv(paste0(out_path, '/parameter_values.csv'))

pred_strata_biom <- compare$output$biomass_by_strata %>% 
  write_csv(paste0(out_path, '/pred_strata_biomass.csv'))

pred_tot_biom <- compare$output$total_predicted_biomass %>% 
  write_csv(paste0(out_path, '/pred_total_biomass.csv'))

# apportionment ----
appo_std <- compare$output$biomass_by_strata %>%
  mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
                         ifelse(grepl('EGOA', strata), 'EGOA',
                                'WGOA'))) %>%
  group_by(model_name, strata, year) %>%
  summarize(stratum_biomass = sum(pred)) %>%
  group_by(model_name, year) %>%
  mutate(total_biomass = sum(stratum_biomass)) %>%
  ungroup() %>%
  mutate(proportion_std = stratum_biomass / total_biomass) %>%
  mutate(strata = factor(strata, labels = c('EGOA', 'CGOA', 'WGOA'), levels = c('EGOA', 'CGOA', 'WGOA'), ordered = TRUE)) %>%
  arrange(year, strata)

appo_std %>%
  pivot_wider(id_cols = c(strata, year), names_from = model_name, values_from = proportion_std) 

ggplot(appo_std, aes(year, proportion_std)) + 
  geom_col(aes(fill = strata)) + 
  facet_wrap(~model_name) +
  coord_flip() +
  labs(x = NULL, y = 'Proportion', fill = 'Region')

ggsave(filename = paste0(out_path, '/m23.3_appo_std.png'),
       dpi = 600, bg = 'white', units = 'in', height = 8.5, width = 10)

# These are just for comparison to BTS only apportionment
full_sumtable_std <- appo_std %>%
  filter(year == YEAR + 1) %>%
  mutate(natmat = 0.03,
         OFL = natmat * total_biomass,
         maxABC = 0.75 * natmat * total_biomass,
         ABC = maxABC, stratum_abc = ABC * proportion_std)

sumtable_std <- full_sumtable_std %>%
  distinct(model_name, year, biomass = total_biomass, OFL, maxABC) %>%
  select(model_name, year, biomass, OFL, maxABC) %>%
  write_csv(paste0(out_path, '/abc_ofl_summary_std.csv'))

# # percent changes -----
biomass_dat %>% filter(year %in% c(2023,2025)) %>%
  mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
                         ifelse(grepl('EGOA', strata), 'EGOA',
                                'WGOA'))) %>%
  group_by(year, strata) %>%
  summarise(biomass = sum(biomass, na.rm = T)) %>%
  pivot_wider(id_cols = c(strata), names_from = year, values_from = biomass) %>%
  mutate(percent_change = (`2025`-`2023`)/`2023`)

cpue_dat %>% filter(year %in% c(2023, 2025)) %>%
  pivot_wider(id_cols = c(strata), names_from = year, values_from = cpue) %>%
  mutate(percent_change = (`2025`-`2023`)/`2023`)

old <- 28768
new <- out23.3$total_predicted_biomass %>% filter(year == 2025) %>% pull(pred)
(new-old)/old
# 
# # Table for predicted biomass by strata with total LCI/UCI
# strata_table <- compare$output$biomass_by_strata %>%
#   filter(model_name == "Model 23") %>% 
#   mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
#                          ifelse(grepl('EGOA', strata), 'EGOA',
#                                 'WGOA'))) %>%
#   group_by(year, strata) %>%
#   summarise(biomass = sum(pred, na.rm = T), lci = sum(pred, na.rm = T), uci = sum(pred, na.rm = T) ) %>%
#   pivot_wider(id_cols = c(year), names_from = strata, values_from = biomass) %>% 
#   mutate(WGOA = round(WGOA, 0), CGOA = round(CGOA, 0), EGOA = round(EGOA, 0)) %>% 
#   write.csv(paste0(out_path, '/M23_Table_strata_biomass_pred.csv'))
# 
# goa_table <- compare$output$total_predicted_biomass %>% 
#   filter(model_name == "Model 23") %>% 
#   select(year, pred, pred_lci, pred_uci) %>% 
#   mutate(GOA_Total = round(pred, 0), LCI = round(pred_lci, 0), UCI = round(pred_uci, 0)) %>% 
#   write.csv(paste0(out_path, '/M23_Table_goa_biomass_pred.csv'))

# Pull length data and make figures
source("code/Other_Figures.r")
source("code/survey_footprints.r")
source("code/map_plots.r")

# Survey comparisons with figures; uncomment to run or open file and run separate
# source("code/survey_comparison.r")
