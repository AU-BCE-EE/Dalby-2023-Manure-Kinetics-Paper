rm(list =ls())

library("readxl")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")

dat <- read.csv('../output/comps_grouped.csv')

ifelse (!isFALSE(any(dat$dm_used_Mean == dat$dm_calc_Mean)), y.title <- expression('Concentration '(g~kg[slurry]^'-1')), y.title <- 'Sample corrected mass (g)')

dat_long <- dat %>% select(-c('dm_used_Mean', 'dm_used_SD', 'dm_calc_Mean', 'dm_calc_SD', 'n_dry_Mean', 'n_dry_SD', 'RFd_Mean', "RFd_SD", 'tan_Mean', 'tan_SD')) %>% pivot_longer(-c('temp', 'day'), names_to = "comp", values_to = "value") 
dat_long.mean <- dat_long[grepl('Mean$', dat_long$comp),]
dat_long.sd <- dat_long[grepl('SD$', dat_long$comp),]
dat_long_both <- cbind(dat_long.mean, SD = dat_long.sd$value)

dat_long_both$temp <- as.factor(dat_long_both$temp)
dat_long_both$comp <- textclean::mgsub(dat_long_both$comp, c("_Mean", "pro",'pro.n_dry', 'hem','lig','lip','ndf','c_dry', 'vfa','vs'), 
                            c("", "CP wet",'CP dry', 'Hemicellulose','Lignin','Lipids','NDF','Carbon dry','VFA','Volatile solids'))
dat_long_both$comp <- gsub('cel$', 'Cellulose', dat_long_both$comp)

dat_plot <- dat_long_both %>% filter(comp != 'pH', comp != 'VFA', day > 0)

dat_plot$comp <- factor(dat_plot$comp, levels = c('Volatile solids', 'Carbon dry', 'NDF', 'Hemicellulose', 'Cellulose', 'CP wet', 'CP dry',
                                                            'Lipids', 'Lignin'))
dat_plot$value <- dat_plot$value * 100
dat_plot$SD <- dat_plot$SD * 100

fig_conc <- ggplot(dat_plot, aes(x = day, y = value, col = temp)) + geom_point() + geom_line(data = dat_plot[!is.na(dat_plot$value),]) + 
  geom_errorbar(aes(ymin = value - SD, ymax = value + SD)) + facet_wrap(~comp, scales = "free_y") + 
  labs(y = y.title, x = 'Time (days)', tag = 'a', col = expression("Temp " ( degree*C))) + theme_bw()

dat <- read_excel("../data/dat_comp.xlsx", sheet = "analysis", skip = 1, col_names = T) %>% filter(day <=2 & !is.na(reactor)) %>% 
  select(c('day', 'temp', 'reactor', 'vfa','tn','tan','dm','vs','pro','lip','lig','hem','ndf','cel','n_dry','c_dry','s_dry','o_dry','h_dry','ash')) %>% 
  group_by(day, temp) %>% summarise(across(, mean)) 

dat$day <- as.factor(dat$day)

cols_r <- c('c_dry','n_dry','s_dry','h_dry','o_dry','ash','pro','vfa','tan')

dat.long <- dat %>% group_by(temp) %>% mutate(across(cols_r, function(x) x - x[day == 0])) %>% pivot_longer(cols = c('c_dry','n_dry','s_dry','h_dry','o_dry','ash','pro','vfa','tan'), names_to = 'comp', values_to = 'value') %>% filter(day == 2)

dat.long$comp<- gsub('c_dry','C (%DM)', dat.long$comp)
dat.long$comp<- gsub('n_dry','N (%DM)', dat.long$comp)
dat.long$comp<- gsub('h_dry','H (%DM)', dat.long$comp)
dat.long$comp<- gsub('s_dry','S (%DM)', dat.long$comp)
dat.long$comp<- gsub('o_dry','O (%DM)', dat.long$comp)
dat.long$comp<- gsub('pro','CP wet (g/kg)', dat.long$comp)

dat.long$comp<- gsub('tan','TAN (g/kg)', dat.long$comp)
dat.long$comp<- gsub('vfa','VFA (g/kg)', dat.long$comp)
dat.long$comp<- gsub('ash','Ash (%DM)', dat.long$comp)

dat.long$temp <- as.factor(dat.long$temp)

fig_ini_change <- ggplot(dat.long, aes(x = comp, y = value, fill = temp)) + geom_bar(stat = 'identity', position = 'dodge') + 
  labs(y = 'Change during first two days', x = "", tag = 'b', fill= expression("Temp " ( degree*C))) + scale_y_continuous(breaks = c(-12.5, -10, -5, -2.5, 0, 2.5, 4)) + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

fig_ini_change
comb <- grid.arrange(fig_conc, fig_ini_change, nrow = 2)


png('../figures/fig_conc.png',  width = 18/2.54, height = 18/2.54, units = 'in', res = 600)
grid::grid.draw(comb)
dev.off()

svg('../figures/fig_conc.svg',  width = 18/2.54, height = 18/2.54)
grid::grid.draw(comb)
dev.off()

pdf('../figures/fig_conc.pdf',   width = 17/2.54, height = 12/2.54)
grid::grid.draw(comb)
dev.off()
