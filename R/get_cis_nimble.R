



GetCIsNimble <- function(fit , modeldata, parstosave, globalparams){

  CIout <- list()
  nyears <- modeldata$nyears
  p_periods <- modeldata$p_periods
  years <- seq(1,nyears)
  periods = seq(1, p_periods)
   R = modeldata$R
  tref10 = modeldata$tref10
  tef20=modeldata$tref20
  counties <- modeldata$counties

  summaries <- c("par", "mean", "sd", "LQ", "MQ", "UQ")


  CIsyearperc <- list()



  fixed_summaries <- data.frame(array(NA, c(length(globalparams), length(summaries))))
  names(fixed_summaries) <- summaries
  fixed_summaries$par <- globalparams
  for(p in 1:length(globalparams)){
    fixed_summaries[p, "mean"] <- fit$summary$all.chains[globalparams[p],"Mean"]
    fixed_summaries[p, "sd"] <- fit$summary$all.chains[globalparams[p],"St.Dev."]
    fixed_summaries[p, "LQ"] <- fit$summary$all.chains[globalparams[p],"95%CI_low"]
    fixed_summaries[p, "MQ"] <- fit$summary$all.chains[globalparams[p],"Median"]
    fixed_summaries[p, "UQ"] <- fit$summary$all.chains[globalparams[p],"95%CI_upp"]
  }

  CIsg <- fixed_summaries



  sigma_kappa_rq <- data.frame(array(NA, c(R, length(summaries) +1)))
  names(sigma_kappa_rq) <- c("r",summaries)
  sigma_kappa_rq$par <- paste0("sigma_kappa_r")
  sigma_kappa_rq$r = c(1:R)
  for(r in 1:R){
    sigma_kappa_rq[r, "mean"] <- fit$summary$all.chains[paste0("sigma_kappa_r[", r, "]"),"Mean"]
    sigma_kappa_rq[r, "sd"] <- fit$summary$all.chains[paste0("sigma_kappa_r[", r, "]"),"St.Dev."]
    sigma_kappa_rq[r, "LQ"] <- fit$summary$all.chains[paste0("sigma_kappa_r[", r, "]"),"95%CI_low"]
    sigma_kappa_rq[r, "MQ"] <- fit$summary$all.chains[paste0("sigma_kappa_r[", r, "]"),"Median"]
    sigma_kappa_rq[r, "UQ"] <- fit$summary$all.chains[paste0("sigma_kappa_r[", r, "]"),"95%CI_upp"]
  }



  CIsr <- list()
  CIsr[["sigma_kappa_rq"]] <- sigma_kappa_rq



  Cpars <- c( "eta_c")
  CRpars <- c("eta_cr", "kappa_cr",
            "sigma_nonsamp_cr", "kappa_cr",  "eta_cr")
  CTRpars <- c("xi_ctr", "gamma_ctr", "eta_ctr", "sigmasq_pep_ctr", "sigma_r_ctr")

  CIsc <- list()

  for (c in 1:modeldata$C){
    Clist <-  list()
    Cparlist <- list()
    for(p in 1:length(Cpars)){
      paramname <- paste0(Cpars[p], "[", c, "]")
      Cparlist[[p]]  <- get_nimble_summaries(paramname = paramname, c=c,  summaries = summaries, par = Cpars[p], fit = fit, county = counties[c])
    }
    Clist[["c_params"]] <- data.frame(do.call("rbind", Cparlist)) %>%
      mutate_at(c('c', 'mean', 'sd', 'LQ', 'MQ', 'UQ'), as.numeric)


    CRparlist <- list()
      races <- modeldata$races
        for(p in 1:length(CRpars)){
      Rparlist <- list()
      for(r in 1:R){
       paramname <- paste0(CRpars[p], "[", c, ", ",r,"]")
       race <- races[r]
       Rparlist[[r]]   <- get_nimble_summaries(paramname = paramname, c=c, r=r, summaries = summaries, par = CRpars[p], fit = fit, county = counties[c], race = race)
      }

      CRparlist[[p]] <- do.call("rbind", Rparlist)
    }
    Clist[["cr_params"]] <- data.frame(do.call("rbind", CRparlist)) %>%
      mutate_at(c('c','r', 'mean', 'sd', 'LQ', 'MQ', 'UQ'), as.numeric)


    CTRparlist <- list()

    for(p in 1:length(CTRpars)){
      Rparlist <- list()
      for(r in 1:R){
        Tparlist <- list()
        for(t in 1:nyears){
        paramname <- paste0(CTRpars[p], "[", c, ", ", t, ", ",r,"]")
        race <- races[r]
        year = t +2006-1
        Tparlist[[t]]   <- get_nimble_summaries(paramname = paramname, c=c, r=r, t=t, summaries = summaries, par = CTRpars[p], fit = fit, county = counties[c], race = race, year=year)
      }

      Rparlist[[r]] <- do.call("rbind", Tparlist)
      }
      CTRparlist[[p]] <- do.call("rbind", Rparlist)
    }

    Clist[["ctr_params"]] <- data.frame(do.call("rbind", CTRparlist)) %>%
      mutate_at(c('c','r', 't', 'year', 'mean', 'sd', 'LQ', 'MQ', 'UQ'), as.numeric) %>%
      filter(!is.na(mean)) %>%
      mutate(year = ifelse(par == "xi_ctr" & t==1, 2010, year)) %>%
      mutate(year = ifelse(par == "xi_ctr" & t==2, 2020, year))

CIsc[[modeldata$counties[c]]] <- Clist
  }





  CIout[["CIsc"]] <- CIsc
  CIout[["CIsg"]] <- CIsg
  CIout[["CIsr"]] <- CIsr


  return(CIout)
}

