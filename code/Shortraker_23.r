# GOA shortraker biomass estimation using the bottom trawl and longline survey indices

# Stock assessment in 2023 (projected to 2024) based on Sept 2023 GPT
# recommendations (endorsed by SSC at Oct 2022 SSC mtg):
# https://meetings.npfmc.org/CommentReview/DownloadFile?p=36b50cbe-9340-4170-bba0-347da1b0054d.pdf&fileName=C5%20GOA%20Groundfish%20Plan%20Team%20Minutes.pdf
# (1) The Team recommended excluding BTS data from 1984 and 1987 due to different survey
# methodology and to continue utilizing a two-survey model.
# (2) The Team recommended simplifying the model naming convention where Model 19 represents the
# status quo model, Model 19* is the corrected model in TMB with new data, and Model 22 is the
# model with additional observation error on BTS and LLS.
# (3) The Team recommended discontinuing the misspecified status quo model (Model 19) and bringing
# forward both the corrected model (Model 19*) and the mod

# Model naming conventions:
# Model 19* = m19s: 1990-pres. corrected version of status quo model
# Model 19* w/ 1984/97 = m19b: 1990-pres. corrected version of status quo model
# Model 23 = m23: 1990-pres. xtra observation error for BTS and LLS
# Model 23 w/ 1984/87 = m23b: 1990-pres. xtra observation error for BTS and LLS

# Set up ----

# assessment year
YEAR <- 2022

# Consider whether the rema package needs to be 'updated'
# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)

libs <- c('rema', 'readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 12) +
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
  
# Model 19* ----
input <- prepare_rema_input(model_name = 'Model 19* w/ 84/87',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            # RPWs are summable
                            sum_cpue_index = TRUE,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(
                              # LLS strata (n=3) indexed as follows for the
                              pointer_biomass_cpue_strata = c(1, 2, 3),
                              pointer_q_cpue = c(1, 2, 3)))
m19b <- fit_rema(input)
out19b <- tidy_rema(m19b)
out19b$parameter_estimates

# Model 19* no 1984/87 ----
input <- prepare_rema_input(model_name = 'Model 19*',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            # start at 1990 instead of 1984
                            start_year = 1990,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 2, 3),
                              pointer_q_cpue = c(1, 2, 3)))
m19s <- fit_rema(input)
out19s <- tidy_rema(m19s)
out19s$parameter_estimates

# Model 23 extra biomass + LLS RPW obs error -----
input <- prepare_rema_input(model_name = 'Model 23 w/ 84/87',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 2, 3),
                              pointer_q_cpue = c(1, 2, 3)),
                            # ESTIMATE EXTRA biomass CV
                            extra_biomass_cv = list(assumption = 'extra_cv'),
                            # ESTIMATE EXTRA LLS RPW CV
                            extra_cpue_cv = list(assumption = 'extra_cv'))

m23b <- fit_rema(input)
out23b <- tidy_rema(m23b)
out23b$parameter_estimates

# Model 23 no 1984/87 -----
input <- prepare_rema_input(model_name = 'Model 23',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = TRUE,
                            # start at 1990 instead of 1984
                            start_year = 1990,
                            end_year = YEAR + 1,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(
                              pointer_biomass_cpue_strata = c(1, 2, 3),
                              pointer_q_cpue = c(1, 2, 3)),
                            extra_biomass_cv = list(assumption = 'extra_cv'),
                            extra_cpue_cv = list(assumption = 'extra_cv'))

m23 <- fit_rema(input)
out23 <- tidy_rema(m23)
out23$parameter_estimates

# Compare M19* and M23 ----
compare <- compare_rema_models(rema_models = list(m19b, m23b))
compare$aic

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     theme(legend.position = 'none') +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Trawl survey biomass (t)',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     ggplot2::scale_fill_viridis_d(direction = -1) +
                     ggplot2::scale_colour_viridis_d(direction = -1),
                   compare$plots$cpue_by_strata  +
                     facet_wrap(~strata, ncol = 1)  +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Longline survey RPW',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     ggplot2::scale_fill_viridis_d(direction = -1) +
                     ggplot2::scale_colour_viridis_d(direction = -1),
                   ncol = 2,
                   rel_widths = c(1.5, 1))

ggsave(filename = paste0(out_path, '/M19b_M23b_fits.png'),
       dpi = 400, bg = 'white', units = 'in', height = 9, width = 14)

compare$plots$total_predicted_biomass +
  labs(subtitle = 'Total predicted biomass (t)',
       fill = NULL, colour = NULL) +
  ggplot2::scale_fill_viridis_d(direction = -1) +
  ggplot2::scale_colour_viridis_d(direction = -1)

ggsave(filename = paste0(out_path, '/M19b_M23b_totalbiomass.png'),
       dpi = 400, bg = 'white', units = 'in', height = 3.5, width = 8)

# Compare M19* and M23 no 1984/87----
compare <- compare_rema_models(rema_models = list(m19s, m23))
compare$aic %>% write_csv(paste0(out_path, '/m19s_m23_aic.csv'))

cowplot::plot_grid(compare$plots$biomass_by_strata +
                     theme(legend.position = 'none') +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Trawl survey biomass (t)',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     ggplot2::scale_fill_viridis_d(direction = -1) +
                     ggplot2::scale_colour_viridis_d(direction = -1),
                   compare$plots$cpue_by_strata  +
                     facet_wrap(~strata, ncol = 1)  +
                     geom_line() +
                     labs(x = NULL, y = NULL, subtitle = 'Longline survey RPW',
                          fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
                     ggplot2::scale_fill_viridis_d(direction = -1) +
                     ggplot2::scale_colour_viridis_d(direction = -1),
                   ncol = 2,
                   rel_widths = c(1.5, 1))

ggsave(filename = paste0(out_path, '/m19s_m23_fits.png'),
       dpi = 400, bg = 'white', units = 'in', height = 9, width = 14)

compare$plots$total_predicted_biomass +
  labs(x = NULL, y = NULL, subtitle = 'Total predicted biomass (t)',
       fill = NULL, colour = NULL) +
  ggplot2::scale_fill_viridis_d(direction = -1) +
  ggplot2::scale_colour_viridis_d(direction = -1)

ggsave(filename = paste0(out_path, '/m19s_m23_totalbiomass.png'),
       dpi = 400, bg = 'white', units = 'in', height = 3.5, width = 8)

# compare short and long time series ----
compare <- compare_rema_models(rema_models = list(m19b, m19s, m23b, m23))

compare$plots$total_predicted_biomass +
  labs(x = NULL, y = NULL, subtitle = 'Total predicted biomass (t)',
       fill = NULL, colour = NULL) +
  ggplot2::scale_fill_viridis_d(direction = -1) +
  ggplot2::scale_colour_viridis_d(direction = -1)

ggsave(filename = paste0(out_path, '/M19b_M19s_M23b_M23_totalbiomass.png'),
       dpi = 400, bg = 'white', units = 'in', height = 3.5, width = 8)

# param estimates
compare$output$parameter_estimates %>%
  write_csv(paste0(out_path, '/M19b_M19s_M23b_M23_parameters.csv'))

# predicted biomass by strata and total for each model
biom <- compare$output$biomass_by_strata %>%
  pivot_wider(id_cols = c(strata, year), names_from = model_name, values_from = pred) %>%
  bind_rows(compare$output$total_predicted_biomass %>%
              mutate(strata = 'Total') %>%
              pivot_wider(id_cols = c(strata, year), names_from = model_name, values_from = pred)) %>%
  write_csv(paste0(out_path, '/M19b_M19s_M23b_M23_biomass_pred.csv'))

# apportionment
appo <- compare$output$biomass_by_strata %>%
  mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
                         ifelse(grepl('EGOA', strata), 'EGOA',
                                'WGOA'))) %>%
  group_by(model_name, strata, year) %>%
  summarize(stratum_biomass = sum(pred)) %>%
  group_by(model_name, year) %>%
  mutate(total_biomass = sum(stratum_biomass)) %>%
  ungroup() %>%
  mutate(proportion = stratum_biomass / total_biomass) %>%
  mutate(strata = factor(strata, labels = c('EGOA', 'CGOA', 'WGOA'), levels = c('EGOA', 'CGOA', 'WGOA'), ordered = TRUE)) %>%
  arrange(year, strata)

appo %>%
  pivot_wider(id_cols = c(strata, year), names_from = model_name, values_from = proportion) %>%
  write_csv(paste0(out_path, '/M19b_M19s_M23b_M23_apportionment.csv'))

full_sumtable <- appo %>%
  filter(year == YEAR + 1) %>%
  mutate(natmat = 0.03,
         OFL = natmat * total_biomass,
         maxABC = 0.75 * natmat * total_biomass,
         ABC = maxABC)

sumtable <- full_sumtable %>%
  distinct(model_name, year, biomass = total_biomass, OFL, maxABC) %>%
  select(model_name, year, biomass, OFL, maxABC) %>%
  write_csv(paste0(out_path, '/abc_ofl_summary.csv'))

# percent changes -----
biomass_dat %>% filter(year %in% c(2019,2021)) %>%
  mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
                         ifelse(grepl('EGOA', strata), 'EGOA',
                                'WGOA'))) %>%
  group_by(year, strata) %>%
  summarise(biomass = sum(biomass, na.rm = T)) %>%
  pivot_wider(id_cols = c(strata), names_from = year, values_from = biomass) %>%
  mutate(percent_change = (`2021`-`2019`)/`2019`)

cpue_dat %>% filter(year %in% c(2021, 2022)) %>%
  pivot_wider(id_cols = c(strata), names_from = year, values_from = cpue) %>%
  mutate(percent_change = (`2022`-`2021`)/`2021`)

old <- out19s$total_predicted_biomass %>% filter(year == 2022) %>% pull(pred)
new <- out23$total_predicted_biomass %>% filter(year == 2022) %>% pull(pred)
(new-old)/old

# Table for predicted biomass by strata with total LCI/UCI
strata_table <- compare$output$biomass_by_strata %>%
  filter(model_name == "Model 23") %>% 
  mutate(strata = ifelse(grepl('CGOA', strata), 'CGOA',
                         ifelse(grepl('EGOA', strata), 'EGOA',
                                'WGOA'))) %>%
  group_by(year, strata) %>%
  summarise(biomass = sum(pred, na.rm = T), lci = sum(pred, na.rm = T), uci = sum(pred, na.rm = T) ) %>%
  pivot_wider(id_cols = c(year), names_from = strata, values_from = biomass) %>% 
  mutate(WGOA = round(WGOA, 0), CGOA = round(CGOA, 0), EGOA = round(EGOA, 0)) %>% 
  write.csv(paste0(out_path, '/M23_Table_strata_biomass_pred.csv'))

goa_table <- compare$output$total_predicted_biomass %>% 
  filter(model_name == "Model 23") %>% 
  select(year, pred, pred_lci, pred_uci) %>% 
  mutate(GOA_Total = round(pred, 0), LCI = round(pred_lci, 0), UCI = round(pred_uci, 0)) %>% 
  write.csv(paste0(out_path, '/M23_Table_goa_biomass_pred.csv'))

# Pull length data and make figures
source("code/Length_figures.r")