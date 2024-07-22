get_model_data_revised <- function(output.dir,
                                   data.dir,
                          county_data,
                          graph_filename,
                          startyear,
                          endyear,
                          tref_rw,
                          validation = F, recent_years = F, seed){

  out <- list()

   temp = subset(county_data, county_data$endyear  & !is.na(county_data$GEOID))


  #Obtain main meta inputs
  counties <- unique(temp$county_name)
  C = length(counties)
  R = length(unique(temp$race_f))
  Pep_endyear = max(temp$endyear[which(!is.na(temp$pep_est))])
  acs_endyear = max(temp$endyear[which(!is.na(temp$estimate))])
  acs_startyear = min(temp$endyear[which(!is.na(temp$estimate))])
  p_periods = length(seq(acs_startyear, acs_endyear))
  years = seq(startyear, endyear)
  Nyears = length(years)
  T_pep = Pep_endyear-startyear+1
  N_codat = dim(temp)[1]
  tref = tref_rw-startyear +1




  getyear.j <- as.numeric(temp$endyear) - startyear +1

  getc.j <-  ind.j <- gett1.j <- gett2.j <- int.j <- rep(NA, N_codat)
  geta.j <- getr.j <- rep(NA, N_codat)


# Transform MOEs into standard errors using ACS formula.

    sigma.j <- temp$moe/1.645

  for(j in 1:N_codat){
    getc.j[j] = which(counties == temp$county_name[j])
    ind.j[j] = ifelse(getyear.j[j] == (tref - startyear +1), 0, 1)
    getr.j[j] = as.numeric(temp$race_f[j])
  }


  #Get observed population counts by indices i and j
  peppop.j = c(temp$pep_est)
  acspop.j = temp$estimate
  s.j = ifelse(!is.na(temp$moe), temp$moe/1.645, 0)
  decpop.j = c(temp$dec_est)




  #Re-index j into county-time-race
    acspop.cpr  =s.cpr =  array(NA, c(C, p_periods, R))
    dec_pop.ctr = array(NA, c(C,2,R))
    peppop.ctr =  pep_props.ctr = ratio.ctr =  array(NA, c(C, Nyears, R))
    pep_diff.ctr =  array(NA, c(C, Nyears, R))

    for(c in 1:C){
        for(r in 1:R){
            int1 = which(getc.j == c & getyear.j == 2010-startyear+1 & getr.j == r )
            dec_pop.ctr[c,1,r] = ifelse(length(int1) >0, decpop.j[which(getc.j == c & getyear.j == 2010-startyear+1  & getr.j == r )], NA)

            int2 = which(getc.j == c & getyear.j == 2020-startyear+1 & getr.j == r )
            dec_pop.ctr[c,2,r] = ifelse(length(int2) >0, decpop.j[which(getc.j == c & getyear.j == 2020-startyear+1 & getr.j == r )], NA)
        }}


  subtract_constant <- 4 #Number of year to add to get startyear for ACS
  #These runs take awhile with lots of data.

for(c in 1:C){
  for(r in 1:R){
    for(p in 1:p_periods){
      # gett2.cp[c,p] = p
      # gett1.cp[c,p] = p
      int2 = which(getc.j == c & getyear.j == (p+subtract_constant) & getr.j == r )
      acspop.cpr[c,p,r] = ifelse(length(int2)>0, acspop.j[which(getc.j == c & getyear.j == (p+subtract_constant) & getr.j == r )], NA)


      tt = ifelse(length(int2)>0, sigma.j[which(getc.j == c & getyear.j == (p+subtract_constant) & getr.j == r )], median(sigma.j[which(getc.j == c &  getr.j == r)], na.rm=T))
      # s.cpr[c,p,r] <- ifelse(p %in% c(1,11), 0, tt)
      s.cpr[c,p,r] <-tt
}}}





for(c in 1:C){
  for(r in 1:R){
        for(t in tref:T_pep){
          int3 = which(getc.j == c & getyear.j == t & getr.j == r )
          peppop.ctr[c,t,r] = ifelse(length(int3)>0, peppop.j[which(getc.j == c & getyear.j == t & getr.j == r )], NA)
        }
        for(t in tref:T_pep){
          if(t >tref){
          pep_diff.ctr[c,t,r] = peppop.ctr[c,t,r] - peppop.ctr[c,t-1,r]
          }else{
            pep_diff.ctr[c,t,r]=0
          }
        }
        }
      }#end C loop




#Get race index
  race_ind <- unique(county_data$race_f)
  race_vars <- c("black_index", "white_index")
  R = length(race_vars)

    out[["acspop_cpr"]] = acspop.cpr
    out[["s_cpr"]] = s.cpr
    out[["pep_pop_ctr"]] = peppop.ctr
    out[["T_pep"]] = T_pep
    out[["decpop_tref_ctr"]] = dec_pop.ctr
    out[["d_ctr"]] = pep_diff.ctr

    out[["C"]] = C
    out[["R"]] =R
    out[["nyears"]] = Nyears
    out[["startyear"]] = startyear
    out[["endyear"]] = endyear
    out[["counties"]] = counties
  out[["tref10"]] = tref
  out[["tref20"]] = 2020-startyear+1
  out[["p_periods"]] = p_periods
 out[["subtract_constant"]] <- subtract_constant


  #####################################################################
  ######### WARNING
  #These undercounts need to be hard-coded from the USCB Post Enumeration Survey
  #If these under-counts change based on population definitions, these must be updated.
  #####################################################################

  out[["races"]] <- unique(county_data$race)
  out[[race_vars[1]]] <- 1
  out[[race_vars[2]]] <- 2


  out[["wnh_undercount_2020"]] <- 1.64
  out[["black_undercount_2020"]] <- 3.30

  out[["wnh_sd_2020"]] <- 0.21
  out[["black_sd_2020"]] <- 0.61

  out[["wnh_undercount_2010"]] <- -0.83
  out[["black_undercount_2010"]] <- 2.06

  out[["wnh_sd_2010"]] <- 0.15
  out[["black_sd_2010"]] <- 0.50




    saveRDS(out, paste(output.dir, "modeldata.RDS", sep=""))
  return(out)

  }
