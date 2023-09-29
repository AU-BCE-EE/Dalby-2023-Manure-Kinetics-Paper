rm(list =ls())

library("readxl")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")
library('ggpmisc')

dat <- read_excel("../data/dat_comp.xlsx", sheet = "analysis", skip = 1, col_names = T)

fresh <- dat[1:6,]
fresh$source <- c('urine', 'urine', 'urine', 'feces', 'feces', 'feces')
names <- c('vfa','tn','tan','dm','vs','hem','cel','ndf','indf','lig','lip', 'c_dry','o_dry','n_dry','h_dry','s_dry','source')
fresh <- fresh[,  colnames(fresh) %in% names]
fresh.mod <- fresh %>% group_by(source) %>% summarise(across(, .fns = list(Mean = mean, SD = sd), na.rm = TRUE))

write.csv(fresh.mod, '../output/fresh_conc_table.csv', row.names = F)

# removed rows with time-zero samples
rem_rows <- which(dat$id <= 9)
dat <- dat[-c(rem_rows), ]

# interpolate for day 20, where it is missing. 

for(i in (1:16)){
  dat$vs[dat$reactor == i] <- approx(x = dat$day[!is.na(dat$vs) & dat$reactor == i & dat$day < 86], y = dat$vs[!is.na(dat$vs) & dat$reactor == i & dat$day < 86], xout = dat$day[dat$reactor == i])$y
}  

dat$vs[dat$day %in% c(29, 36, 50, 57, 71, 78)] <- NA

dat$dm_calc <- dat$dm[dat$day == 2] * dat$vs/100 + dat$ash[dat$day == 2]/100 * dat$dm[dat$day == 2]

# use calculated dm
dat$dm_used <- dat$dm_calc

# organic matter components
comps_dm <- c('lip', 'cel', 'hem', 'lig', 'ndf', 'indf', 'pro.n_dry', 'n_dry', 'c_dry', 'vs')
comps_slurry <- c('vfa', 'pro', 'tan')
pH <- 'pH'

dat_mass <- dat %>% mutate(pro.n_dry = n_dry * 6.25, across(comps_dm, function(x) x/100 * dm_used/100)) %>% 
  mutate(across(comps_slurry, function(x) x * (slurry_before_sample + sample_removed)/(slurry_before_sample[day == 0])/1000))
dat_mass$RFd <- dat_mass$ndf - dat_mass$indf
comps_dm <- c(comps_dm, 'RFd') 

dat_grouped <- dat_mass %>% filter(reactor != 1) %>% select(c(comps_dm, comps_slurry, dm_used, dm_calc, day, reactor, temp, pH)) %>% group_by(temp, day) %>% 
  summarise(across(-c('reactor'), .fns = list(Mean = mean, SD = sd), na.rm = TRUE))


## CONSIDER THIS: UPS REMEMBER TO REMOVE DAY 0 FOR KJELDAHL N, ELEM N and VS ## 

dat_grouped_log <- dat_grouped %>% filter(day > 0) %>% select(-ends_with('SD'), -starts_with('pH'), -starts_with('urea')) %>% 
  ungroup() %>% mutate(across(-c('day', 'temp'), function(x) log(x[day == 2]/x)))

# rate constants at different temperatures and for different compounds in units of 1/day

dat_grouped_log_fit <- dat_grouped_log %>% group_by(temp) %>% mutate(across(-c('day'), function(x) lm(x ~ 0 + day)$coefficients)) %>% distinct(temp, .keep_all = T) %>% ungroup() %>% select(-c('vfa_Mean'))

write.csv(dat_grouped_log_fit, '../output/comps_rate_constants.csv', row.names = F)

R <- 0.008314 # KJ /(mol * K)

Arr_dat <- dat_grouped_log_fit %>% mutate(T_inv = 1/(temp + 273.15)) %>% 
  mutate(across(-c('temp','T_inv','day'), log)) 


keep <- apply(Arr_dat, 2, function(x) isTRUE(all(x)))

Ea<- as.data.frame(Arr_dat[, keep] %>% mutate(across(-c('temp','T_inv'), function(x) lm(x ~ T_inv)$coefficients[2]*R))) # KJ/mol
A <- as.data.frame(Arr_dat[, keep] %>% mutate(across(-c('temp','T_inv'), function(x) exp(lm(x ~ T_inv)$coefficients[1])))) # unit less?

write.csv(data.frame(rbind(Ea, A)), '../output/Arrh_constants.csv', row.names = F)

dat.mass <- dat %>% mutate(corr_mass = slurry_before_sample + sample_removed) %>% select(day, corr_mass, temp, dm) %>% group_by(temp, day) %>% summarise(mass.mean = mean(corr_mass, na.rm = T), dm = mean(dm, na.rm = T))
dat.mass$mass.mean[dat.mass$day >= 20] <- dat.mass$mass.mean[dat.mass$day >= 20] - 100
dat.mass$mass.mean[is.na(dat.mass$dm)] <- NA

dat.mass$temp <- as.factor(dat.mass$temp) 

write.csv(dat.mass, '../output/mass_dat.csv', row.names = F)


