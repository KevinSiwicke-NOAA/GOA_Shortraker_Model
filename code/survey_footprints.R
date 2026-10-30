library(dplyr)
library(akgfmaps)
library(sf)
library(ggplot2)

# Load in Stratum data from akgfmaps package
goa_trl <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")
goa_trl_strata <- goa_trl$survey.strata |> mutate(Area = ifelse(STRATUM %in% c(10:15, 110:113, 210:211, 310, 410, 510:511), 'WGOA',
                                                                ifelse(STRATUM %in% c(20:38, 120:136, 220:232, 32, 320:321, 330, 420, 430, 520:521, 530:531), 'CGOA',
                                                                       ifelse(STRATUM %in% c(40:51, 140:152, 240:252, 340:352, 440, 450, 540:541, 550:551), 'EGOA', NA))))

goa_ll <- akgfmaps::get_base_layers(select.region = "ll.goa", set.crs = "auto") 

goa_ll_strata <- goa_ll$survey.strata |> 
  mutate(Area = ifelse(AREA_DESC == 'Shumagin', 'WGOA',
                       ifelse(AREA_DESC %in% c('Kodiak', 'Chirikof'), 'CGOA',
                              ifelse(AREA_DESC %in% c('West Yakutat', 'East Yakutat', 'Southeast'), 'EGOA', NA))))

# gullies in the EGOA are not included in the akgfmaps package, so add them
gully_aa <- read_sf("data/lls_gullies.shp") |> mutate(Area = 'EGOA')

ggplot() +
  geom_sf(data = goa_ll$akland) +
  geom_sf(data = goa_trl_strata, aes(fill = factor(Area, levels = c('WGOA', 'CGOA', 'EGOA'))), color = NA, alpha = 0.2) +
  geom_sf(data = goa_ll_strata, aes(fill = Area), color = NA) +
  geom_sf_text(data = goa_trl$akland |> filter(COUNTRY == 'US'), aes(label = STATE_PROVINCE), size = 6) +
  geom_sf(data = gully_aa, aes(fill = Area), color = NA) +
  geom_sf(data = goa_trl$bathymetry |> filter(DEPTH_M %in% c(200, 500, 1000)), col = 'grey50', size = 0.2, aes(lty = factor(DEPTH_M))) +
  labs(x = 'Longitude', y = 'Latitude', lty = 'Isobath (m)', fill = 'Area') +
  coord_sf(xlim = c(goa_ll$plot.boundary$x[1], goa_ll$plot.boundary$x[2]), 
           ylim = c(goa_ll$plot.boundary$y[1], goa_ll$plot.boundary$y[2])) +
  scale_x_continuous(name = "Longitude",
                     breaks = goa_ll$lon.breaks) +
  scale_y_continuous(name = "Latitude",
                     breaks = goa_ll$lat.breaks) +
  theme_bw()

ggsave(paste0(out_path, '/survey_footprints.png'), height = 4, width = 12, dpi = 300)