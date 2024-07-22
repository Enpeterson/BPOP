
get_estimates_i <- function(plotdata_c, CIs,startyear, tract=F, modeldata){
  plotdata_c$lowerci = plotdata_c$upperci = plotdata_c$est = plotdata_c$countylevel = NA

if(!tract){
for(i in 1:length(plotdata_c$name.i)){
  c = which(names.c == plotdata_c$name.i[i])
  r.int = ifelse(plotdata_c$race.i[i] == "black_only", 1, 2)

  plotdata_c$lowerci[i] =  CIs$CIsc[[c]]$gamma_ctr[plotdata_c$year[i] - startyear +1,r.int,1]
  plotdata_c$upperci[i] =  CIs$CIsc[[c]]$gamma_ctr[plotdata_c$year[i] - startyear +1,r.int,3]
  plotdata_c$est[i] =  CIs$CIsc[[c]]$gamma_ctr[plotdata_c$year[i] - startyear +1,r.int,2]

  #gamma.crs = exp(mcmc[, paste0("eta.cr[", c,",", r.int, "]")])
  #plotdata_c$gamma.cr = median(gamma.crs)
}

}else{

    for(i in 1:length(plotdata_c$name.i)){
      c = which(names.c == plotdata_c$name.i[i])
      r.int = ifelse(plotdata_c$race.i[i] == "black_only", 1, 2)
      t.int = plotdata_c$endyear.i[i] - 2006 +1


      trcs=modeldata$tracts[which(modeldata$getc_g ==c)]

      g.int = which(trcs == plotdata_c$tract.i[i])
      if(length(g.int)>0){
      plotdata_c$lowerci[i] = CIs$CIsc[[c]]$gamma_gtr[g.int,t.int,  r.int, 1]
      plotdata_c$upperci[i] = CIs$CIsc[[c]]$gamma_gtr[g.int,t.int, r.int, 3]
      plotdata_c$est[i] = CIs$CIsc[[c]]$gamma_gtr[g.int,t.int, r.int, 2]
      }
    }
}




return(plotdata_c)

}
