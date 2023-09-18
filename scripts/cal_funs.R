resfun <- function(pars.cal = pars.cal, dat = dat, days = days){
  
  p <- 10^pars.cal
  
  E_qds <- -p[['E_qds']]
  lnA_qds <- p[['lnA_qds']]
  
  out <- hydrolysis(E_qds = E_qds, lnA_qds = lnA_qds)
  obs <- dat[, c('day', 'vfa_Mean','temp')] %>% filter(day <= days) %>% mutate(vfa_Mean = vfa_Mean * 1000)
  pred <- out[which(out$time %in% obs$day), c('time', 'vfa', 'temp')]
  res <- sum(abs(obs$vfa_Mean - pred$vfa))/nrow(obs)
  
  return(res)
  
}