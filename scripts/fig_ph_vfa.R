rm(list =ls())

library("readxl")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")
library('ggpmisc')

dat <- read.csv('../output/comps_grouped.csv')

dat_long <- dat %>% select(-c('dm_calc_Mean', 'dm_calc_SD', 'dm_used_Mean', 'dm_used_SD', 'n_dry_Mean', 'n_dry_SD')) %>% mutate(vfa_Mean = vfa_Mean * 1000, vfa_SD = vfa_SD * 1000) %>% pivot_longer(-c('temp', 'day'), names_to = "comp", values_to = "value") 
dat_long.mean <- dat_long[grepl('Mean$', dat_long$comp),]
dat_long.sd <- dat_long[grepl('SD$', dat_long$comp),]
dat_long_both <- cbind(dat_long.mean, SD = dat_long.sd$value)

dat_long_both$temp <- as.factor(dat_long_both$temp)
dat_long_both$comp <- textclean::mgsub(dat_long_both$comp, c("_Mean", "pro",'pro.n_dry', 'hem','lig','lip','ndf','c_dry', 'vfa','vs'), 
                                       c("", "CP (Kjeldahl)",'CP (dry matter)', 'Hemicellulose','Lignin','Lipids','NDF','Carbon (dry matter)','VFA','Volatile solids'))
dat_long_both$comp <- gsub('cel$', 'Cellulose', dat_long_both$comp)

dat_plot <- dat_long_both %>% filter(comp == 'pH'  & !is.na(value) | comp == 'VFA' & !is.na(value))

fig_pH_vfa <- ggplot(dat_plot, aes(x = day, y = value, col = temp)) + geom_point() + geom_line() +  geom_errorbar(aes(ymin = value -SD, ymax = value + SD)) +
  facet_wrap(~comp, scales = "free_y") + 
  labs(y = expression('pH or VFA (g kg'^'-1'~')'), tag = 'a', col = expression("Temp " ( degree*C))) + 
  theme_bw() + theme(legend.position = 'top')

dat <- read_excel("../data/dat_comp.xlsx", sheet = "vfa", skip = 0, col_names = T) %>%
  filter(!is.na(temp))

dat$sum <- rowSums(dat[, -c(1:6)], na.rm = T)
dat.long <- dat %>% mutate(across(acetic:hexanoic, ~./sum * 100)) %>% select(-sum) %>% 
  group_by(temp, day) %>% summarise(across(-c(1:4), mean)) %>% ungroup() %>% pivot_longer(cols = -c('temp', 'day'), values_to = 'value', names_to = 'comp')

new.lab = as_labeller(c("10"= "10~degree~C", "15"= "15~degree~C", "20"= "20~degree~C", "25"= "25~degree~C"), label_parsed)

custom_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#000000", "#808080", "#fb9a99")

fig_vfa_comp <- ggplot(dat.long, aes(x = day, y = value, col = comp, linetype = comp)) + geom_line(size = 0.5) + facet_wrap(~temp, ncol = 4,labeller = new.lab) + theme_bw() +
  labs(y = 'Relative VFA abundance (%)', tag = 'b', plot.tag = TRUE) +
  scale_color_manual(values = custom_palette) +
  theme(axis.text.x = element_text(angle = 90,  vjust = 0.5), legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        legend.text = element_text(size=10),
        plot.tag.position = c(0.01, 1.10)) +
         guides(fill = guide_legend(nrow=4, byrow=TRUE))
  
fig_vfa_comp
#grid::grid.draw(rbind(ggplotGrob(fig_pH_vfa), ggplotGrob(fig_vfa_comp)))

comb <- grid.arrange(fig_pH_vfa, fig_vfa_comp, nrow = 2)

png('../figures/fig_pH_vfa.png',  width = 18/2.54, height = 15/2.54, units = 'in', res = 600)
grid::grid.draw(comb)
dev.off()

svg('../figures/fig_pH_vfa.svg',  width = 18/2.54, height = 15/2.54)
grid::grid.draw(comb)
dev.off()

pdf('../figures/fig_ph_vfa.pdf',  width = 18/2.54, height = 15/2.54)
grid::grid.draw(comb)
dev.off()
