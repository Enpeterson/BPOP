create_plot_data <- function(CIs, modeldata, county_data){
  CIscounty<- CIs[["CIsc"]]
  nyears <- modeldata$nyears
  names.c <- modeldata$counties
  C = length(names.c)
  p_periods = modeldata$p_periods
  tref = modeldata$tref
  startyear = modeldata$startyear
  endyear = modeldata$endyear
  R = modeldata$R
  county_data$startyear = as.numeric(county_data$endyear)-5
  county_data$endyear = as.numeric(county_data$endyear)


  county_data = subset(county_data, county_data$endyear  & !is.na(county_data$GEOID)) %>%
    mutate(endyear = as.numeric(endyear))


  plot_data = data.frame(array(NA, c(length(county_data$GEOID), 12)))
  names(plot_data) = c("name.i", "startyear.i", "endyear.i", "race.i", "log_acspop.i", "acspop.i", "year", "decpop.i",
                       "log_decpop.i", "peppop.i", "log_peppop.i", "s.i")
  plot_data[, "name.i"] = county_data$county_name
  plot_data[, "startyear.i"] = county_data$endyear -4
  plot_data[, "endyear.i"] = county_data$endyear

  plot_data[, "log_acspop.i"] = log(county_data$estimate)
  plot_data[, "acspop.i"] = county_data$estimate
  plot_data[, "year"] = county_data$endyear
  plot_data[, "decpop.i"] = county_data$dec_est
  plot_data[, "log_decpop.i"] = log(county_data$dec_est)
  plot_data[, "peppop.i"] = county_data$pep_est
  plot_data[, "log_peppop.i"] = log(county_data$pep_est)
  plot_data[, "s.i"] =county_data$moe/1.645
  plot_data[, "race.i"] = county_data$race_f

  gamma_data <- list()
  for(c in 1:C){
    gamma_data[[c]] <- CIs$CIsc[[c]]$ctr_params %>% filter(par == "gamma_ctr")
  }
  gamma_df <- bind_rows(gamma_data)


  combi_data <- gamma_df %>%
    left_join(plot_data, by  = c("county"= "name.i", "race" = "race.i", "year" = "endyear.i" )) %>%
    pivot_longer(cols = c("MQ", "decpop.i", "peppop.i", "acspop.i"), names_to = c("type")) %>%
    mutate(LQ = ifelse(type != "MQ", NA, LQ),
           UQ = ifelse(type != "MQ" , NA, UQ),
           mean = ifelse(type != "MQ" , NA, mean),
           sd = ifelse(type != "MQ" , NA, sd),
           s.i = ifelse(type != "acspop.i", NA, s.i),
           lowererr = value -s.i,
           uppererr = value + s.i)



  combi_data$race = as.factor(combi_data$race)

  return(combi_data)
}
