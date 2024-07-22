get_nimble_summaries <- function(paramname, c, r=NULL,t=NULL, summaries, par, fit, county, race=NA, year=NA){


if(is.null(r) & is.null(t)){
  dat <- rep(NA, c(length(summaries) +2))
  names(dat) <- c("c", "county", summaries)

dat[["c"]] <- c
dat[["county"]] <- county
dat[["par"]] = par

dat["mean"] <- fit$summary$all.chains[paramname,"Mean"]
dat["sd"] <- fit$summary$all.chains[paramname,"St.Dev."]
dat["LQ"] <- fit$summary$all.chains[paramname,"95%CI_low"]
dat["MQ"] <- fit$summary$all.chains[paramname,"Median"]
dat["UQ"] <- fit$summary$all.chains[paramname,"95%CI_upp"]
}





  if(!is.null(r) & is.null(t)){
    dat <- rep(NA, c(length(summaries) +4))
    names(dat) <- c("c", "county", "r", "race", summaries)

    dat[["c"]] <- c
    dat[["county"]] <- county
    dat[["r"]] <- r
    dat[["race"]] <- race
    dat[["par"]] = par

    dat["mean"] <- fit$summary$all.chains[paramname,"Mean"]
    dat["sd"] <- fit$summary$all.chains[paramname,"St.Dev."]
    dat["LQ"] <- fit$summary$all.chains[paramname,"95%CI_low"]
    dat["MQ"] <- fit$summary$all.chains[paramname,"Median"]
    dat["UQ"] <- fit$summary$all.chains[paramname,"95%CI_upp"]
  }


  if(!is.null(r) & !is.null(t)){
    dat <- rep(NA, c(length(summaries) +6))
    names(dat) <- c("c", "county", "r", "race","t", "year", summaries)

    dat[["c"]] <- c
    dat[["county"]] <- county
    dat[["r"]] <- r
    dat[["race"]] <- race
    dat[["par"]] = par
    dat[["t"]] <- t
    dat[["year"]] <- year

    dat["mean"] <- fit$summary$all.chains[paramname,"Mean"]
    dat["sd"] <- fit$summary$all.chains[paramname,"St.Dev."]
    dat["LQ"] <- fit$summary$all.chains[paramname,"95%CI_low"]
    dat["MQ"] <- fit$summary$all.chains[paramname,"Median"]
    dat["UQ"] <- fit$summary$all.chains[paramname,"95%CI_upp"]
  }


return(dat)
}
