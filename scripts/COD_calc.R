rm(list=ls())

library(biogas)

# urine

c <- 244/12.01
n <- 136/14
h <- 39.8/1
s <- 23.5/32.065
o <- 169/16
urine_dm <- 1.11
urine_vs <- 0.618
urine <- c(c = c, n = n, h = h, s = s, o = o)
COD_vs_urine <- calcCOD(paste("C",urine[['c']],"H",urine[['h']],"N",urine[['n']],"S",urine[['s']],"O",urine[['o']], sep =""))

# feces

c <- 438/12.01
n <- 33.8/14
h <- 64.5/1
s <- 3.4/32.065
o <- 347/16
feces_dm <- 23.6
feces_vs <- 0.887
feces <- c(c = c, n = n, h = h, s = s, o = o)

COD_vs_feces <- calcCOD(paste("C",feces[['c']],"H",feces[['h']],"N",feces[['n']],"S",feces[['s']],"O",feces[['o']], sep =""))

# mix 

c <- 414/12.01
n <- 46.4/14
h <- 61.4/1
s <- 5.9/32.065
o <- 325.7/16
mix_dm <- 6.73
mix_vs <- 0.8537
mix <- c(c = c, n = n, h = h, s = s, o = o)

COD_vs_mix <- calcCOD(paste("C",mix[['c']],"H",mix[['h']],"N",mix[['n']],"S",mix[['s']],"O",mix[['o']], sep =""))

urine_COD_of_total <- urine_dm * urine_vs * 3 * COD_vs_urine /(urine_dm * urine_vs * 3 * COD_vs_urine + feces_dm * feces_vs * 1 * COD_vs_feces)

