hydrolysis <- function(E_qds, lnA_qds){
  
  dat_mass <- read.csv('../output/mass_dat.csv') %>% filter(day == 0)
  dat <- read.csv('../output/comps_grouped.csv')
  dat_Arrh <- read.csv('../output/Arrh_constants.csv')
  dat_Arrh$parm <- c(rep('Ea', 4), rep('LnA', 4))
  dat_Arrh <- unique(select(dat_Arrh, -c('temp', 'T_inv')))
  dat.mod <- dat %>% filter(day == 0)
  
  temp <- dat.mod$temp
  
  out <- NULL 
  
  for(i in temp){
    
    temp_slurry <- i
    
    pars <- list(
      temp_slurry = i,
      slurry_mass0 = dat_mass$mass.mean[dat_mass$temp == temp_slurry], # kg
      lnA = c(CP_elem = dat_Arrh$pro.n_dry_Mean[dat_Arrh$parm == 'LnA'] , 
              lip = dat_Arrh$lip_Mean[dat_Arrh$parm == 'LnA'], 
              cel = dat_Arrh$cel_Mean[dat_Arrh$parm == 'LnA'], 
              RFd = dat_Arrh$RFd_Mean[dat_Arrh$parm == 'LnA'],
              hem = dat_Arrh$hem_Mean[dat_Arrh$parm == 'LnA'], 
              ndf = dat_Arrh$ndf_Mean[dat_Arrh$parm == 'LnA'], 
              CP_kjel = dat_Arrh$pro_Mean[dat_Arrh$parm == 'LnA'], 
              vs = dat_Arrh$vs_Mean[dat_Arrh$parm == 'LnA'],
              C = dat_Arrh$c_dry_Mean[dat_Arrh$parm == 'LnA']), 
      E = c(CP_elem = dat_Arrh$pro.n_dry_Mean[dat_Arrh$parm == 'Ea'], 
            lip = dat_Arrh$lip_Mean[dat_Arrh$parm == 'Ea'], 
            cel = dat_Arrh$cel_Mean[dat_Arrh$parm == 'Ea'], 
            RFd = dat_Arrh$RFd_Mean[dat_Arrh$parm == 'Ea'], 
            hem = dat_Arrh$hem_Mean[dat_Arrh$parm == 'Ea'], 
            ndf = dat_Arrh$ndf_Mean[dat_Arrh$parm == 'Ea'], 
            CP_kjel = dat_Arrh$pro_Mean[dat_Arrh$parm == 'Ea'], 
            vs = dat_Arrh$vs_Mean[dat_Arrh$parm == 'Ea'],
            C = dat_Arrh$c_dry_Mean[dat_Arrh$parm == 'Ea']),
      conv = c(CP_elem = 1.41, lip = 2.65, cel = 1.092, hem = 1.117, CP_kjel = 1.41, qds = 0.9833),
      R = 0.008314,
      lnA_qds = lnA_qds,
      E_qds = E_qds
    )
    
    y0 <- c(CP_elem = dat.mod$pro.n_dry_Mean[dat.mod$temp == temp_slurry] * 1000, 
            lip = dat.mod$lip_Mean[dat.mod$temp == temp_slurry]* 1000,
            cel = dat.mod$cel_Mean[dat.mod$temp == temp_slurry]* 1000,
            RFd = dat.mod$RFd_Mean[dat.mod$temp == temp_slurry]* 1000,
            hem = dat.mod$hem_Mean[dat.mod$temp == temp_slurry]* 1000,
            ndf = dat.mod$ndf_Mean[dat.mod$temp == temp_slurry]* 1000,
            CP_kjel = dat.mod$pro_Mean[dat.mod$temp == temp_slurry] * 1000, 
            vs = dat.mod$vs_Mean[dat.mod$temp == temp_slurry]* 1000,
            C = dat.mod$c_dry_Mean[dat.mod$temp == temp_slurry]* 1000,
            qds = dat.mod$vs_Mean[dat.mod$temp == temp_slurry]*1000 - dat.mod$lip_Mean[dat.mod$temp == temp_slurry]*1000 - dat.mod$ndf_Mean[dat.mod$temp == temp_slurry]*1000 - dat.mod$pro.n_dry_Mean[dat.mod$temp == temp_slurry]*1000,
            vfa =  dat.mod$vfa_Mean[dat.mod$temp == temp_slurry]*1000)
    
    change <- function(t, y, parms = pars) {
      
      temp_slurry = parms$temp_slurry
      conv = parms$conv
      R = parms$R
      lnA = parms$lnA 
      lnA['qds'] <- parms$lnA_qds
      E = parms$E 
      E['qds'] <- parms$E_qds
      
      # extract state variables
      CP_elem <- y['CP_elem']
      lip <- y['lip']
      cel <- y['cel']
      RFd <- y['RFd']
      hem <- y['hem']
      ndf <- y['ndf']
      CP_kjel <- y['CP_kjel']
      vs <- y['vs']
      C <- y['C']
      qds <- y['qds']
      vfa <- y['vfa']
      
      # first order rate constant: Arrhenius equation
      k <- lnA * exp(E/(R * (temp_slurry + 273.15)))
      
      keep <- names(y)[!names(y) %in% names(E)]
      
      k[keep] <- 0
      
      derivatives <- c(
        CP_elem = - k['CP_elem'] * CP_elem, # VS in kg
        lip = - k['lip'] * lip,
        cel = - k['cel'] * cel,
        RFd = -k['RFd'] * RFd,
        hem = - k['hem'] * hem,
        ndf = - k['ndf'] * ndf,
        CP_kjel = - k['CP_kjel'] * CP_kjel,
        vs = - k['vs'] * vs,
        C = - k['C'] * C,
        qds = - k['qds'] * qds,
        vfa = k['lip'] * lip * conv['lip'] + k['cel'] * cel * conv['cel'] + 
          k['hem'] * hem * conv['hem'] + k['CP_elem'] * CP_elem * conv['CP_elem'] + k['qds'] * qds * conv['qds']
      )
      
      return(list(derivatives))
      
    }
    
    t <- 0:85
    
    out1 <- data.frame(deSolve::lsoda(y = y0, times = t, func = change, parms = pars))
    out1 <- cbind(out1, temp = temp_slurry)
    out <- rbind(out, out1)
    
  }
  
  return(out)
  
}
