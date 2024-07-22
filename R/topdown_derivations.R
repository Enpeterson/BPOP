
get_topdown_counts <- function(data.dir, output.dir, county_data, startyear){
  modeldata <- readRDS(paste0(output.dir, "modeldata.RDS"))
  fit <- readRDS(paste0(output.dir, "fit.RDS"))


mcmc_list <- as.mcmc.list(fit$samples)
S = dim(mcmc_list$chain1)[1]
C <- modeldata$C
counties <- modeldata$counties
R <- modeldata$R
startyear <- modeldata$startyear
nyears <- modeldata$nyears
Nchains = length(fit$samples)

propdata <- readRDS(paste0(data.dir, "age_gender_proportions.RDS")) %>%
  filter(!is.na(PEP_prop)) %>%
  mutate(race_int = as.numeric(race_f)) %>%
  mutate(county_int = as.numeric(factor(county_name, levels = counties)),
         tindex = year - 2006+1)

propdata$mean <- propdata$sd <- propdata$median <-  propdata$lower <- propdata$upper <- NA

for(i in  1:length(propdata$GEOID)){
  cindex <- propdata$county_int[i]
  tindex <- propdata$tindex[i]
  rindex <- propdata$race_int[i]

  fit_samples <- c(fit$samples$chain1[,paste0("gamma_ctr[", cindex, ", ", tindex, ", ", rindex, "]")],fit$samples$chain2[,paste0("gamma_ctr[", cindex, ", ", tindex, ", ", rindex, "]")],
    fit$samples$chain3[,paste0("gamma_ctr[", cindex, ", ", tindex, ", ", rindex, "]")], fit$samples$chain4[,paste0("gamma_ctr[", cindex, ", ", tindex, ", ", rindex, "]")],
    fit$samples$chain5[,paste0("gamma_ctr[", cindex, ", ", tindex, ", ", rindex, "]")])

  if(propdata$PEP_prop[i] !=0){
   propdatasamp <-  propdata$PEP_prop[i]
  }else{
  propdatasamp <- 0.00001
  }

  samples <- fit_samples * propdatasamp
  propdata$mean[i] <- mean(fit_samples * propdata$PEP_prop[i])
  propdata$sd[i] <- sd(samples)
  propdata$median[i] <- median(samples)
  propdata$lower[i] <- quantile(samples, 0.025)
  propdata$upper[i] <- quantile(samples, 0.975)
}

saveRDS(propdata, paste0(output.dir, "finaldata.RDS"))





}
