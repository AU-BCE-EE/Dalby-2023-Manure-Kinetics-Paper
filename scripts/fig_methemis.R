rm(list =ls())

library("readxl")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")
library("pracma")

dat.org <- read.csv('../output/CH4_rates.csv')

dat.delta.org <- read.csv('../output/delta_values.csv')
dat.org$temp <- as.factor(dat.org$temp)
dat.delta.org$temp <- as.factor(dat.delta.org$temp)

fig_emis_rate <- ggplot(dat.org, aes(x = day, y = CH4_rate_g_dat_kg_vs0_Mean, col = temp)) + geom_point() + geom_line() + 
  geom_errorbar(aes(ymin = CH4_rate_g_dat_kg_vs0_Mean - CH4_rate_g_dat_kg_vs0_SD, ymax = CH4_rate_g_dat_kg_vs0_Mean + CH4_rate_g_dat_kg_vs0_SD)) + 
  labs(y = expression('Emission rate (g CH'[4]~'d'^'-1'~'kg'[VS]^'-1'~')'), tag = "a") + 
  geom_vline(xintercept = 85, lty = 2, col = 'gray65') + theme_bw() + theme(legend.position="none")

dat.org2 <- read.csv('../output/CH4_rates2.csv')
colnames(dat.org2 ) <- c('reactor','day','temp','emis','sd')
dat.intp <- dat.org2
dat.intp$emis[is.na(dat.intp$emis)] <- 0

sum_emis <- dat.intp %>% group_by(reactor) %>% filter(day <= 85) %>% summarise(cum = trapz(day, emis), temp = temp, day = day) %>% 
  group_by(temp) %>% summarise(mean = mean(cum), sd = sd(cum))

sum_emis_curve <- dat.intp %>% group_by(reactor) %>% summarise(cum = cumtrapz(day, emis), temp = temp, day = day) %>% 
  group_by(temp, day) %>% summarise(mean = mean(cum), sd = sd(cum))

sum_emis_curve$temp <- as.factor(sum_emis_curve$temp)

fig_emis_cum <- ggplot(sum_emis_curve, aes(x = day, y = mean, col = temp)) + geom_point() + geom_line() + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) + 
  labs(y = expression('Cumulative emission (g CH'[4]~'kg'[VS]^'-1'~')'), tag = "a") + 
  geom_vline(xintercept = 85, lty = 2, col = 'gray65') + theme_bw() + theme(legend.position="none")

fig_emis_cum
  
fig_delta <- ggplot(dat.delta.org, aes(x = day, y = delta_man_mean, col = temp)) + geom_point() + geom_line() + 
  geom_errorbar(aes(ymin = delta_man_mean - delta_man_SD, ymax = delta_man_mean + delta_man_SD)) + 
  labs(y = expression(delta^{13}~'C'[CH4]~"(‰)"), tag = "b", col = expression("Temp " ( degree*C))) + 
  geom_vline(xintercept = 85, lty = 2, col = 'gray65') + theme_bw()

# rate plots
png('../figures/fig_emis.png',  width = 18/2.54, height = 10/2.54, units = 'in', res = 600)
grid::grid.draw(cbind(ggplotGrob(fig_emis_cum), ggplotGrob(fig_delta)))
dev.off()


svg('../figures/fig_emis.svg',  width = 18/2.54, height = 10/2.54)
grid::grid.draw(cbind(ggplotGrob(fig_emis_cum), ggplotGrob(fig_delta)))
dev.off()

pdf('../figures/fig_emis.pdf',   width = 17/2.54, height = 10/2.54)
grid::grid.draw(cbind(ggplotGrob(fig_emis_cum), ggplotGrob(fig_delta)))
dev.off()

