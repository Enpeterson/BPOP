
calculate_Rhat <- function(par, S, Chains, mcmc_list){

  samp <- array(NA, c(S, Chains))
  for(c in 1:Chains){
    samp[1:S,c] <- mcmc_list[[c]] [1:S, par]
  }
  rr <- Rhat(samp)

  return(rr)
}



RunMCMC <- function(
                    runsetting,
                    output.dir,
                    percentiles ,
                    n,
                    parstosave,
                    nbWB_A = nbWB_A,
                    scale = scale,
                    globalparams,
                    RParams,
                    CParams,
                    Params.ct,
                    validation=F,
                    recent_years=F
                    ){



if(runsetting == "test"){
  Niter = 5
  Nburn = 1
  Nchains = 2
  Nthin = 1
}
if(runsetting == "short"){
  Niter = 1000
  Nburn = 200
  Nchains = 5
  Nthin = 5
}
if(runsetting == "quick"){
  Niter = 4000
  Nburn = 2000
  Nchains = 5
  Nthin = 10
}


if(runsetting == "semlong"){
  Niter = 15000
  Nburn = 8000
  Nchains = 5
  Nthin = 10
}


if(runsetting == "long"){
  Niter = 60000
  Nburn = 40000
  Nchains = 5
  Nthin = 40
}




  rnorm(1)
  set.seed(n)

  model_output <- list()

  K <- max(seq(1:modeldata$nyears))
  W <- matrix(0, nrow = K, ncol = K)

  for(i in 1:(K-1)) W[i,i+1] <- 1

  for(i in 1:(K-1)) W[i+1,i] <- 1
  Tnb <- mat2listw(W)
  Tnb <- nb2WB(nb = Tnb$neighbours)

  #Read in modeldata
  modeldata <- readRDS(paste0(output.dir, 'modeldata.RDS'))
  nyears <- modeldata$nyears


#Create inputs for nimble model
  data_list <- list(
    pep_pop_ctr = modeldata$pep_pop_ctr,
    d_ctr = modeldata$d_ctr,
    decpop_tref_ctr = modeldata$decpop_tref_ctr,
    acspop_cpr = modeldata$acspop_cpr,
    s_cpr = modeldata$s_cpr)


  model_constants <- list(
    adj = nbWB_A$adj,
    weights = nbWB_A$weights,
    num= nbWB_A$num,
    L= length(nbWB_A$weights),
    tref10 = modeldata$tref10,
    tref20  = modeldata$tref20,
    C = modeldata$C,
    R  = modeldata$R,
    p_periods = modeldata$p_periods,
    T_pep = modeldata$T_pep,
    nyears = modeldata$nyears,
    wnh_undercount_2010 = modeldata$wnh_undercount_2010,
    wnh_undercount_2020 = modeldata$wnh_undercount_2020,
    black_undercount_2010 = modeldata$black_undercount_2010,
    black_undercount_2020 = modeldata$black_undercount_2020,
    wnh_sd_2010= modeldata$wnh_sd_2010,
    wnh_sd_2020 = modeldata$wnh_sd_2020,
    black_sd_2010= modeldata$black_sd_2010,
    black_sd_2020 = modeldata$black_sd_2020,
    wnh_index = modeldata$white_index,
    black_index = modeldata$black_index,
    subtract_constant = modeldata$subtract_constant
    )





  #Set inits
C <- modeldata$C
R <- modeldata$R
p_periods <- modeldata$p_periods
rep_inits <- list(
    sigma_eta = 0.01,
    phi = 0.01,
    omega = 0.01,
    etaglobal = 9.14,
    rho = 0.2,
    sigma_ref = 0.01,
    sigma_kappa_r = rep(0.01, times = R),
    eta_c = rep(9, C),
    eta_cr = array(9.14, c(C, R)),
    theta_cr = array(0, c(C, R)),
    kappa_cr = array(0, c(C, R)),
    sigma_nonsamp_cr = array(10, c(C, R)),
    xi_ctr  = array(0, c(C, nyears, R)),
    eta_ctr  = array(0, c(C, nyears, R)),
    gamma_ctr  = array(1000, c(C, nyears, R)),
    sigmasq_pep_ctr  = array(10, c(C, nyears, R)),
    sigma_r_ctr  = array(10, c(C, nyears, R)),
    Sigma_cppr = array(10, c(C, p_periods, p_periods, R)))

#Set initial values
  inits <- list()
  for(i in 1:Nchains){
    inits[[i]] <- rep_inits
  }


  #Read in the write model function and produce nimble model.
  #Rerun with every model change
  source(paste0(getwd(), "/R/WriteModel_spattemp_nimble.R"))
  nimble_code <- WriteModel_spattemp_nimble(output.dir = output.dir)


#Run nimble model
  fit <- nimbleMCMC(code = nimble_code,
                    data = data_list,
                    inits =inits,
                     constants = model_constants,
                    monitors = parstosave,
                    niter = Niter,
                    nburnin = Nburn,
                    thin = Nthin,
                    nchains = Nchains,
                    progressBar = TRUE,
                    summary = TRUE,
                    samplesAsCodaMCMC = TRUE,
                    WAIC = TRUE
  )

  saveRDS(fit, paste0(output.dir, "fit.RDS"))

  mcmc_list <- as.mcmc.list(fit$samples)





pdf(paste0(output.dir, "globalparams_convergence.pdf"))
  MCMCvis::MCMCtrace(object = mcmc_list,
            pdf = FALSE, # no export to PDF
            ind = TRUE, # separate density lines per chain
            params = globalparams)
dev.off()


pdf(paste0(output.dir, "Rparams_convergence.pdf"))
MCMCvis::MCMCtrace(object = mcmc_list,
                   pdf = FALSE, # no export to PDF
                   ind = TRUE, # separate density lines per chain
                   params = RParams)
dev.off()


pdf(paste0(output.dir, "Cparams_convergence.pdf"))
MCMCvis::MCMCtrace(object = mcmc_list,
                   pdf = FALSE, # no export to PDF
                   ind = TRUE, # separate density lines per chain
                   params = CParams)
dev.off()


pdf(paste0(output.dir, "CRParams_convergence.pdf"))
MCMCvis::MCMCtrace(object = mcmc_list,
                   pdf = FALSE, # no export to PDF
                   ind = TRUE, # separate density lines per chain
                   params = c("sigma_nonsamp_cr", "eta_cr", "kappa_cr"))
dev.off()


temp <- Params.ct[-which(Params.ct %in% c("xi_ctr", "sigma_nonsamp_cr", "eta_cr", "kappa_cr", "sigmasq_pep_ctr"))]

pdf(paste0(output.dir, "CTRParams_convergence.pdf"))
MCMCvis::MCMCtrace(object = mcmc_list,
                   pdf = FALSE, # no export to PDF
                   ind = TRUE, # separate density lines per chain
                   params = temp)
dev.off()






CIs <- GetCIsNimble(fit=fit , modeldata =modeldata, parstosave = parstosave, globalparams = globalparams)
saveRDS(CIs, paste0(output.dir, "CIs.RDS"))


}#end function
