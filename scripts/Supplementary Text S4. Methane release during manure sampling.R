rm(list =ls())

library("readxl")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")

dat <- read_excel("../data/dat_comp.xlsx", sheet = "analysis", skip = 1, col_names = T)

dat.m <- dat %>% filter(day %in% c(2,4,7,14,20,43,64,85)) %>% 
  group_by(day, temp) %>% 
  summarise(mass = mean(slurry_before_sample/1000, na.rm = T), 
            mean_ppm = mean(CH4_ppm, na.rm=T), norm_vs0 = mean((slurry_start - sample_removed)/1000 * (dm0/100 * vs0) / 1000))

dat.m$kH <- 0.0014 * exp(1600 * ((1/(dat.m$temp + 273.15))-(1/298.15))) # mol/L pr bar
dat.m$atm <- dat.m$mean_ppm/10^6 # pressure of methane in headspace, atm
dat.m$M_CH4 <- 16.04 # molar mass of methane g/mol
dat.m$CH4_sol <- dat.m$kH * dat.m$atm * dat.m$M_CH4 * dat.m$mass # saturated concentration of methane, g/L

CH4_release <- dat.m %>% group_by(temp) %>% summarise(sum = sum(CH4_sol)) # cumulative methane released during all sampling events (g)
CH4_release_vs0 <- dat.m %>% mutate(CH4_vs0_released = CH4_sol/norm_vs0) %>% 
  group_by(temp) %>% summarise(sum_vs0 = sum(CH4_vs0_released)) # cumulative methane released during all sampling events pr kg VS0 (g/(kgvs))

CH4_release_vs0
