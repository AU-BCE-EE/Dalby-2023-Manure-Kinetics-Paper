rm(list =ls())

library("readxl")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")
library('scales')
library('ggtext')

options(encoding = "UTF-8")

dat.org <- read_excel("../data/dat_qpcr.xlsx", sheet = "R_dat")
dat.seq <- read_excel("../data/dat_seq.xlsx", sheet = "sorted")

dm.dat <- read.csv('../output/comps_grouped.csv') %>% select(temp, day, dm_used_Mean, dm_used_SD) %>% filter(!is.na(dm_used_Mean))
dm.dat$dm_used_Mean[dm.dat$day == 0] <- 23.4
dm.dat$dm_used_SD[dm.dat$day == 0] <- 0.486

dat.merge <- merge(dat.org, dm.dat, by = c('temp', 'day'), all.x = TRUE)
dat.merge$dm_used_Mean[dat.merge$day >85] <- dat.merge$dm_used_Mean[dat.merge$day == 85] 
dat.merge$dm_used_SD[dat.merge$day >85] <- dat.merge$dm_used_SD[dat.merge$day == 85] 
dat.merge$temp <- as.factor(dat.merge$temp)
dat.merge$day <- as.factor(dat.merge$day)
dat.merge$cop_dm <- dat.merge$cop * 10^6 * dat.merge$dm_used_Mean/100 # copies / g DM
dat.merge$cop_sd <- sqrt(((dat.merge$sd * 10^6)/(dat.merge$cop * 10^6) *100)^2 + (dat.merge$dm_used_SD/dat.merge$dm_used_Mean * 100)^2)
dat.merge$cop_sd_abs <- dat.merge$cop_sd/100 * dat.merge$cop_dm


dat.seq.long <- dat.seq %>% pivot_longer(cols = -c('day','temp'), names_to = 'OTU', values_to = 'abundance')
dat.seq.long$day <- as.factor(dat.seq.long$day)
dat.seq.long$temp <- as.factor(dat.seq.long$temp)

new.lab = as_labeller(c("10"= "10~degree~C", "15"= "15~degree~C", "20"= "20~degree~C", "25"= "25~degree~C"), label_parsed)

custom_palette <- c("#808080", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#000000", "#1f78b4", "#fb9a99")


div <- ggplot(dat.seq.long, aes(x = day, y = abundance, fill = OTU)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~temp, labeller = new.lab) + theme_bw() + 
  scale_fill_manual(values = custom_palette)  +
  theme(axis.text.x = element_text(angle = 90,  vjust = 0.5), axis.title.y = element_text(size = 10), legend.text.align = 0, legend.position = "bottom",
        legend.title = element_text(size=8),
        legend.key.size = unit(0.3, 'cm'),
        legend.text = element_text(size =7, margin = margin(r = 5, unit = "pt"), face = 'italic')) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  labs(y = 'Relative mcrA gene abundance (%)')

abun <- ggplot(dat.merge, aes(x = day, y = cop_dm)) + geom_bar(stat = "identity", position = "stack") + 
  geom_errorbar(aes(x = day, ymin = cop_dm - cop_sd_abs, ymax = cop_dm + cop_sd_abs)) + 
  facet_grid(~temp, labeller = new.lab) +
  theme_bw()  +   labs(y = expression('mcrA gene abundance (copies g'[DM]^'-1'~')')) + 
  scale_y_continuous(trans = "log10", oob = rescale_none, limits = c(1000000, 10^10)) + 
  theme(axis.text.x = element_text(angle = 90,  vjust = 0.5), axis.title.y = element_text(size = 10), legend.position = "none")


png('../figures/fig_micro.png',  width = 15/2.54, height = 15/2.54, units = 'in', res = 600)
grid::grid.draw(rbind(ggplotGrob(abun), ggplotGrob(div)))
dev.off()

pdf('../figures/fig_micro.pdf',  width = 15/2.54, height = 15/2.54)
grid::grid.draw(rbind(ggplotGrob(abun), ggplotGrob(div)))
dev.off()

svg('../figures/fig_micro.svg',  width = 15/2.54, height = 15/2.54)
grid::grid.draw(rbind(ggplotGrob(abun), ggplotGrob(div)))
dev.off()
