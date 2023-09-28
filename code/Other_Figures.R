# =========================================================================================
#
# Shortraker assessment Length figures
#
# =========================================================================================
# Pull length data
source("code/other_data_pull.r")

lls.sr.len %>% 
  write_csv(paste0(dat_path, "/goa_sr_lls_lengths", YEAR, ".csv"))
bts.sr.len %>% 
  write_csv(paste0(dat_path, "/goa_sr_bts_lengths", YEAR, ".csv"))
bts.sr.age %>% 
  write_csv(paste0(dat_path, "/goa_sr_bts_ages", YEAR, ".csv"))
fsh.sr.len %>% 
  write_csv(paste0(dat_path, "/goa_sr_fishery_lengths", YEAR, ".csv"))

####################
# Group the BTS numbers by year/age
age = bts.sr.age %>% 
  filter(age > 0) %>% 
  group_by(age, survey_year) %>% 
  summarize(freq = sum(agepop)) %>% 
  mutate(calc = freq * age)

means = age %>% group_by(survey_year) %>% 
  summarize(tot = sum(freq), l_calc = sum(calc)) %>% 
  mutate(mean = l_calc / tot) %>% 
  left_join(bts.sr.spec)

age = merge(age, means, by=c("survey_year"))

age %>%  
  ggplot(aes(x=age, y=after_stat(density), weight = freq)) +
  # theme_linedraw() +
  geom_histogram(alpha=0.25, binwidth=1, col="black") +
  facet_wrap(~survey_year, ncol = 1) +
  theme(legend.position = "top") +
  xlab("Age") +
  ylab("Proportion of trawl survey population") +
  geom_text(aes(x=100, y = 0.05, label = paste0("n = ", Num))) +
  geom_text(aes(x=100, y=0.04, label = paste0("Mean = ", format(round(mean, digits=1), nsmall = 1)))) +
  # scale_y_continuous(expand=c(0,0), limits=c(0,0.09)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0, 150, 25)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()) 

ggsave(file = paste0("results/", YEAR, "/SR_BTS_Ages.png"), height = 10, width = 5, dpi=600)

# Group the LL numbers by year/length
ll.len = lls.sr.len %>% 
  group_by(year, length) %>% 
  summarize(freq = sum(rpn, na.rm = T))

ll.len$calc = ll.len$freq*ll.len$length

ll.means = ll.len %>% group_by(year) %>% 
  summarize(tot = sum(freq), l_calc = sum(calc))

ll.means$mean = ll.means$l_calc/ll.means$tot
ll.len = merge(ll.len, ll.means, by=c("year"))

ll.len %>%  
  ggplot(aes(x=length, y=after_stat(density), weighted.mean=freq)) +
  geom_histogram(alpha=0.25, binwidth=1, col="black") +
  facet_wrap(~year, ncol=3) +
  theme(legend.position = "top") +
  xlab("Length (cm)") +
  ylab("Proportion of LL survey RPNs") +
  geom_text(aes(x=30, y=0.075, label=year)) +
  geom_text(aes(x=30, y=0.055, label=paste0("(", format(round(mean, digits=1), nsmall = 1) , " cm)"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.099)) +
  scale_x_continuous(breaks=seq(25,115,10), limits=c(22,118)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file=paste0("results/", YEAR, "/SR_LLS_Lengths.png"), height = 15, width = 12, dpi=600)

ggplot(ll.len, aes(year, mean)) + geom_point() + geom_line()

ll.len$survey = "LLS"

#####################
# Group the BTS numbers by year/length
len = bts.sr.len %>% 
  mutate(length = length / 10) %>% 
  group_by(year, length) %>% 
  summarize(freq = sum(total)) 
  
len$calc = len$freq*len$length

means = len %>% group_by(year) %>% 
  summarize(tot = sum(freq), l_calc = sum(calc))
  
means$mean = means$l_calc/means$tot
len = merge(len, means, by=c("year"))

len %>%  
        ggplot(aes(x=length, y=after_stat(density), weighted.mean=freq)) +
        # theme_linedraw() +
        geom_histogram(alpha=0.25, binwidth=1, col="black") +
        facet_wrap(~year, ncol=2) +
        theme(legend.position = "top") +
        xlab("Length (cm)") +
        ylab("Proportion of trawl survey population") +
        geom_text(aes(x=30, y=0.065, label=year)) +
        geom_text(aes(x=30, y=0.045, label=paste0("(", format(round(mean, digits=1), nsmall = 1) , " cm)"))) +
        scale_y_continuous(expand=c(0,0), limits=c(0,0.09)) +
        scale_x_continuous(expand=c(0,0), breaks=seq(25,115,10)) +
        theme_bw() +
        theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
                panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file = paste0("results/", YEAR, "/SR_BTS_Lengths.png"), height = 10, width = 7, dpi=600)

len$survey = "BTS"

# Look at a time series of the mean length by surveys
ggplot(ll.means, aes(year, mean))  + 
  geom_point(size=4) +
  geom_point(data=means, aes(x=year, y=mean), size=4, pch=21) +
  ylab("Mean length (cm)") +
  scale_x_continuous(breaks=seq(1990,2020,5)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file = paste0("results/", YEAR, "/SR_Time_Series_length_Comp.png"), height = 3.5, width = 6, dpi=600)

# IS there a relationship between mean of LL lengths and mean of BTS lengthS?
comb = merge(ll.means, means, by="year")

ggplot(comb, aes(mean.x, mean.y)) + 
  geom_point()

# NO relationship present

# Summary of entire survey datasets
all.bts = bts.sr.len %>% 
  mutate(length = length / 10) %>% 
  group_by(length) %>% 
  summarize(freq = sum(total))

all.bts$calc = as.numeric(all.bts$freq)*as.numeric(all.bts$length)
all.bts$survey = "BTS"

# all.bts = all.bts[ , c(3,2,4,5)]

bts.mean = all.bts %>% summarize(tot = sum(freq), l_calc = sum(calc))

bts_mean = data.frame("Mean" = bts.mean$l_calc/bts.mean$tot)

all.bts %>%  
  ggplot(aes(x=length, y=after_stat(density), weighted.mean=freq)) +
  geom_histogram(alpha=0.25, binwidth=1, col="black") +
  theme(legend.position = "top") +
  xlab("Length (cm)") +
  ylab("Proportion of trawl survey population") +
  geom_text(aes(x=10, y=0.05, label=paste0("(", format(round(bts_mean, digits=1), nsmall = 1) , " cm)"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.07)) +
  scale_x_continuous(breaks=seq(10,110,10)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

all.ll = lls.sr.len %>% group_by(length) %>% 
  summarize(freq = sum(rpn, na.rm = TRUE))

all.ll$calc = as.numeric(all.ll$freq)*as.numeric(all.ll$length)
all.ll$survey = "LLS"

ll.mean = all.ll %>% 
  summarize(tot = sum(freq), l_calc = sum(calc))

ll_mean = data.frame("Mean" = ll.mean$l_calc/ll.mean$tot)

all.ll %>%  
  ggplot(aes(x=length, y=after_stat(density), weighted.mean=freq)) +
  geom_histogram(alpha=0.25, binwidth=1, col="black") +
  theme(legend.position = "top") +
  xlab("Length (cm)") +
  ylab("Proportion of LL survey RPNs") +
  geom_text(aes(x=10, y=0.06, label=paste0("(", format(round(ll_mean, digits=1), nsmall = 1) , " cm)"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.085)) +
  scale_x_continuous(breaks=seq(10,110,10)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

# Combine for plot
all = rbind(all.bts, all.ll)

ll_mean$survey = "LLS"
bts_mean$survey = "BTS"
all.mean = rbind(ll_mean, bts_mean)

ggplot() +
  geom_histogram(data=all, aes(x=length, y=after_stat(density), weighted.mean=freq, fill = survey),
                 position = 'identity', alpha=0.5, binwidth=1, col="black") +
  theme(legend.position = "top") +
  scale_fill_discrete(type = c('red', 'blue')) +
  xlab("Length (cm)") +
  ylab("Length composition by survey") +
  geom_text(data=all.mean, aes(x=c(23,23),  y=c(0.045, .04), label=paste0(survey, " mean length: ", format(round(Mean, digits=1), nsmall = 1) , " cm"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.055)) +
  scale_x_continuous(expand=c(0,5), breaks=seq(5,115,10)) +
  # facet_wrap(~survey, ncol=1) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

ggsave(file = paste0("results/", YEAR, "/all_len.png"), height = 4, width = 6, dpi=600)
# Alternative using ggridges

# Fishery lengths
lengths <- fsh.sr.len %>% 
  filter(gear == 1 | gear == 8) %>%
  filter(nmfs_area > 609, nmfs_area < 651, !nmfs_area == 649) %>% 
  mutate(Gear = ifelse(gear == 1, "Trawl", "Longline"))

mean_lengths <- lengths %>% 
  group_by(Gear) %>% 
  summarize(Mean = mean(length))

ggplot(lengths) + 
  geom_histogram(aes(x=length, y=after_stat(density), weighted.mean=frequency, fill = Gear),
                 position = 'identity', alpha=0.5, binwidth=1, col="black") +
  scale_fill_discrete(type = c('blue', 'red')) +
  xlab("Length (cm)") +
  ylab("Length composition by gear type") +
  geom_text(data=mean_lengths, aes(x=c(85,85),  y=c(0.055, .05), label=paste0(Gear, " mean length: ", format(round(Mean, digits=1), nsmall = 1) , " cm"))) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.06)) +
  scale_x_continuous(expand=c(0,5), breaks=seq(5,115,10)) +
  theme_bw() +
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), strip.background=element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), strip.text=element_blank()) 

    
ggsave(file = paste0("results/", YEAR, "/all_fish_len.png"), height = 4, width = 7, dpi=600)
