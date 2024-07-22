
WriteModel_spattemp_nimble<- function(output.dir) {

  if (file.exists(here::here(output.dir, "model.txt"))) {
    file.remove(here::here(output.dir, "model.txt"))
  }



code <- nimbleCode({


    for(r in 1:R){
      for(c in 1:C){

        #census data model
        decpop_tref_ctr[c,1,r] ~ dnorm(gamma_ctr[c,tref10,r] * (1- (xi_ctr[c,1,r]/100)), sd = sigma_nonsamp_cr[c,r])
        decpop_tref_ctr[c,2,r] ~ dnorm(gamma_ctr[c,tref20,r] * (1- (xi_ctr[c,2,r]/100)), sd = sigma_nonsamp_cr[c,r])

        #PEP data model
      for(t in tref10:T_pep){
        pep_pop_ctr[c,t,r] ~ dnorm(gamma_ctr[c,t,r], sd = sqrt(sigmasq_pep_ctr[c,t,r]))
      }

       # Percent population change data model, tau_r <- precision of annual change.
      for(t in (tref10+1):T_pep){
        d_ctr[c,t,r]~ dnorm(0,  sd = sigma_r_ctr[c,t,r])
      }

      #acs data model
       acspop_cpr[c,1:p_periods,r] ~ dmnorm( tildegamma_cpr[c,1:p_periods,r], cov =  Sigma_cppr[c,1:p_periods,1:p_periods,r])


      for(t in 1:nyears){
        gamma_ctr[c,t,r] <- exp( eta_cr[c,r] + eta_ctr[c,t,r] + kappa_cr[c,r])
        sigma_r_ctr[c,t,r] ~ T(dnorm(0,  sd =  10),1,)
      }

      # PEP errors:
      #In decennial year <- nonsampling error
      #Outside decennial year <- nonsampling error + sum of var(d.ctr's)
      for(t in (tref10+1):(tref20-1)){
        sigmasq_pep_ctr[c,t,r] <- sigma_nonsamp_cr[c,r]^2 + sum(sigma_r_ctr[c,(tref10+1):t,r]^2)
      }
      for(t in (tref20+1):nyears){
        sigmasq_pep_ctr[c,t,r] <- sigma_nonsamp_cr[c,r]^2 + sum(sigma_r_ctr[c,(tref20+1):t,r]^2)
      }


      # ACS var-cov (error) matrix
      for(p in 1:p_periods){
        tildegamma_cpr[c,p,r] <-  (1/5) * sum(gamma_ctr[c,p:(p+ subtract_constant),r])
        Sigma_cppr[c,p,p,r] <- sigma_nonsamp_cr[c,r]^2 + s_cpr[c,p,r]^2

        for(s in (p+1):p_periods){
          Sigma_cppr[c,p,s,r] <- rho * sqrt( sigma_nonsamp_cr[c,r]^2 + s_cpr[c,p,r]^2) * sqrt(sigma_nonsamp_cr[c,r]^2 + s_cpr[c,s,r]^2)
          Sigma_cppr[c,s,p,r] <- Sigma_cppr[c,p,s,r]
        }
      }

      eta_ctr[c,tref10,r] ~ dnorm(0, sd = sigma_ref)

      for(t in (tref10 +1):nyears){
        eta_ctr[c,t,r] ~ dnorm(eta_ctr[c,t-1,r], sd = sigma_eta)
      }
      for(t in 2:tref10){
        eta_ctr[c,t-1,r] ~ dnorm(eta_ctr[c,t,r], sd = sigma_eta)
      }

      eta_cr[c,r] ~ dnorm(eta_c[c], sd = phi)

      sigma_nonsamp_cr[c,r] ~ T(dnorm(0, sd  = 10), 1,)


      mukappa_cr[c,r] <- 0

      theta_cr[c,r] ~ dnorm(0, sd = 0.1)

    }#end C

      tau.kappa.r[r] <- 1/(sigma_kappa_r[r]^2)
      sigma_kappa_r[r] ~ T(dnorm(0,sd =0.1), 0,1)

      kappa_cr[1:C, r] ~ dcar_normal(adj[1:L], weights[1:L], num[1:C], tau = tau.kappa.r[r], zero_mean = 1)

  } # end R



  for(c in 1:C){
    eta_c[c] ~ dnorm(etaglobal, sd = omega)


    # Under-reporting adjustment 2010
    xi_ctr[c,1,wnh_index] ~ T(dnorm(wnh_undercount_2010, sd = wnh_sd_2010), 0,)
    xi_ctr[c,1,black_index] ~ T(dnorm(black_undercount_2010, sd = black_sd_2010), ,0)

    # Under-reporting adjustment 2020
    xi_ctr[c,2,wnh_index] ~ T(dnorm(wnh_undercount_2020, sd = wnh_sd_2020), 0,)
    xi_ctr[c,2,black_index] ~ T(dnorm(black_undercount_2020, sd  = black_sd_2020), 0,)

  }#end C loop



    # global level of population
    etaglobal ~ dnorm(9.15, sd =  0.1)


    sigma_eta ~ T(dnorm(0, sd = 0.1), 0,1)
    sigma_ref ~ T(dnorm(0, sd = 0.1), 0,)


    #across [c,r] variation in log(population), var(eta[c,r])
    phi ~ T(dnorm(0, sd  = 1), 0,)

   #across [c] variation in log(population), var(eta[c])
    omega ~ T(dnorm(0, sd = 1), 0,1)

    rho ~ dunif(0,1)
})

write_lines(code, file<- here::here(output.dir, "model.txt"))


return(code)
}


