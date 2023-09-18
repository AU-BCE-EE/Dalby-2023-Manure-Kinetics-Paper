rm(list =ls())

library("readxl")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")
library("biogas")

dat <- read.csv('../output/comps_grouped.csv')

time <- 4

source('cal_funs.R')
source('hydrolysis.R')

new_pars <- data.frame(E_qds = 65 , lnA_qds = 10^9)
pars.cal <- log10(new_pars)

cal <- optim(par = pars.cal, 
             fn = resfun,
             days = time,
             dat = dat,
             method = 'Nelder-Mead',
             hessian = TRUE
)

E_qds <- -10^cal$par['E_qds']
lnA_qds <- 10^cal$par['lnA_qds']

out <- hydrolysis(E_qds, lnA_qds)
out$qds[out$time == 2]

qds_conc <- data.frame(temp = c(10, 15, 20, 25), qds = out$qds[out$time == 2])

write.csv(qds_conc, "../output/qds_conc.csv")

temp_slurry <- c(10, 15, 20, 25)
R <- 0.008314
k <- lnA_qds * exp(E_qds/(R * (temp_slurry + 273.15)))
print(c(k*10^3,lnA_qds, E_qds))

write.csv(c(E_qds = E_qds, lnA_qds = lnA_qds), "../output/qds_const.csv")
