rm(list=ls())

# processing organic matter componenets
source('../scripts/process_comp.R')
# processing methane gas data
source('../scripts/process_meth.R') 

# make Figure 1
source('../scripts/fig_conc.R')
# make Figure 2
source('../scripts/fig_ph_vfa.R')
# make Figure 3
source('../scripts/fig_methemis.R')
# make Figure 4
source('../scripts/fig_micro.R')

# run optimization and get model results
source('../scripts/cal_funs.R')
source('../scripts/hydrolysis.R')
source('../scripts/optimize.R')

# calculate COD concents of feces and urine
source('../scripts/COD_calc.R')

# Supplementary Text S4. Methane release during manure sampling
source('../scripts/Supplementary Text S4. Methane release during manure sampling.R')
