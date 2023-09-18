rm(list =ls())

library("readxl")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")

dat <- read_excel("../data/dat_comp.xlsx", sheet = "analysis", skip = 1, col_names = T)
dat_isot <- read_excel("../data/dat_comp.xlsx", sheet = "isotope", col_names = T)

# removed rows with time-zero samples
rem_rows <- which(dat$id <= 9)
dat <- dat[-c(rem_rows), ]

T <- 298
R <- 0.082057
M_CH4 <- 16.04 
BG <- min(dat$CH4_ppm, na.rm = T)

for(i in (1:16)){
  dat$dm[dat$reactor == i] <- approx(x = dat$day[!is.na(dat$dm) & dat$reactor == i], y = dat$dm[!is.na(dat$dm) & dat$reactor == i], xout = dat$day[dat$reactor == i])$y
  dat$vs[dat$reactor == i] <- approx(x = dat$day[!is.na(dat$vs) & dat$reactor == i], y = dat$dm[!is.na(dat$vs) & dat$reactor == i], xout = dat$day[dat$reactor == i])$y
}  

dat_select <- dat %>% select(c('reactor', 'day', 'temp','CH4_ppm', 'flow', 'slurry_before_sample', 'sample_removed','dm', 'vs')) %>% 
  mutate(CH4_rate_g_day = (CH4_ppm - BG)/10^6 * flow/(R * T) * M_CH4 * 60 * 24) %>% mutate(CH4_rate_g_dat_kg_vs = CH4_rate_g_day / (slurry_before_sample * dm/100 * vs/100 / 1000))

dat_select <- dat %>% select(c('reactor', 'day', 'temp','CH4_ppm', 'flow', 'sample_removed', 'slurry_start', 'vs0','dm0')) %>% 
  mutate(CH4_rate_g_day = (CH4_ppm - BG)/10^6 * flow/(R * T) * M_CH4 * 60 * 24,
         norm_vs0 = (slurry_start - sample_removed)/1000 * (dm0/100 * vs0) / 1000, 
         CH4_rate_g_dat_kg_vs0 = CH4_rate_g_day / norm_vs0)

dat_grouped <- dat_select %>% group_by(temp, day) %>% summarise(across(c('CH4_rate_g_dat_kg_vs0'), .fns = list(Mean = mean, SD = sd), na.rm = TRUE))
dat_grouped2 <- dat_select %>% group_by(reactor, day, temp) %>% summarise(across(c('CH4_rate_g_dat_kg_vs0'), .fns = list(Mean = mean, SD = sd), na.rm = TRUE))

write.csv(dat_grouped, '../output/CH4_rates.csv', row.names = F)
write.csv(dat_grouped2, '../output/CH4_rates2.csv', row.names = F)

dat_isot$ppm_bg <- as.numeric(dat_isot$ppm_bg)
dat_isot$dCH4_bg <- as.numeric(dat_isot$dCH4_bg)

dat_isot.mod <- dat_isot %>% filter(reactor != 'bg', ppm_read > 5) %>% mutate(ppm_man = ppm_read - ppm_bg, delta_man = (ppm_read * dCH4_read - ppm_bg * dCH4_bg)/ppm_man)

dat_isot_grouped <- dat_isot.mod %>% group_by(temp, day) %>% summarise(delta_man_mean = mean(delta_man, na.rm = T), delta_man_SD = sd(delta_man, na.rm = T))

write.csv(dat_isot_grouped, '../output/delta_values.csv', row.names = F)
 
