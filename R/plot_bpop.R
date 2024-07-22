

plot_bpop <- function(plotdata_c, subplot=F, plotsize, nrow_for_facet, size_pt, size_line, tract_cond=F, co_name=NA,validation = F){

plotdata_c$race.i = as.factor(plotdata_c$race.i)
  race.labs = ifelse(plotdata_c$race.i == "white_only", "WHITE ALONE", "BLACK ALONE")
  names(race.labs) = plotdata_c$race.i

  miny = max(0, plotdata_c$lowererr, plotdata_c$dec_lowererr, na.rm = T)
  maxy = max(plotdata_c$uppererr, plotdata_c$dec_uppererr, na.rm = T)

  if(validation){
  plotdata_c$acs_validate = ifelse(plotdata_c$endyear.i %in% c( 2020, 2021), plotdata_c$acspop.i, NA)
  plotdata_c$lowererr_validate = ifelse(plotdata_c$endyear.i %in% c( 2020, 2021), plotdata_c$lowererr, NA)
  plotdata_c$upperr_validate = ifelse(plotdata_c$endyear.i %in% c(  2020, 2021), plotdata_c$uppererr, NA)
  plotdata_c$pep_validate = ifelse(plotdata_c$endyear.i %in% c( 2020, 2021), plotdata_c$peppop.i, NA)

  change_obs = which(plotdata_c$endyear.i %in% c(c( 2020, 2021)))
plotdata_c$acspop.i[change_obs] = plotdata_c$peppop.i[change_obs] = plotdata_c$lowererr[change_obs] = plotdata_c$uppererr[change_obs] = NA
  }

  shapes<- c("census" = 1, "ACS" = 4, "PEP" = 23, "BPop estimate" = 20, "Removed ACS" = 18,"Removed PEP" = 17)
  colors<- c("census" = "black", "ACS" = "red", "PEP" = "green4", "BPop estimate" = "blue", "Removed PEP" = "orange", "Removed ACS" = "orange")

plotdata_c$endyear.i <- as.integer(plotdata_c$endyear.i)

countyplot <- ggplot(data = plotdata_c) +
  scale_x_continuous(name = "Year", limits = c(2010,2022), breaks = seq(2010, 2022, by=5)) +
  scale_y_continuous(name = "Population") +
  geom_point(aes(x = endyear.i, y = decpop.i, shape= "census", col = "census"), size = size_pt, stroke=4) +
  geom_point(aes(x = endyear.i, y = acspop.i, shape= "ACS", col = "ACS"),  size = size_pt, stroke=4) +
  geom_line(aes(x = endyear.i, y = est), col = "blue", alpha = 1, size = size_line) +
  geom_point(aes(x = endyear.i, y = est, shape = "BPop estimate", col= "BPop estimate"),  size = size_pt) +
  geom_segment(aes(x= endyear.i, y=lowererr, xend= endyear.i, yend= uppererr), col = "red" ,alpha = 0.6, size = size_line, linetype = "longdash") +
   geom_ribbon(aes(x= endyear.i, ymin=lowerci, ymax= upperci),  col="blue", alpha = 0.3, fill = "blue") +
   # geom_segment(aes(x= endyear.i, y=dec_lowererr, xend= endyear.i, yend= dec_uppererr),  col="black", alpha = 1, size =size_line, linetype = "dashed") +
  theme_grey(base_size = plotsize) +
  theme(title = element_text(size = plotsize),
        axis.title=element_text(size= plotsize ),
        strip.text.x = element_text(size = plotsize, colour = "blue"))

if(!tract_cond ){
  countyplot = countyplot +
    geom_point(aes(x = endyear.i, y = peppop.i, shape= "PEP", col = "PEP"), size = size_pt, stroke =4) +
  scale_shape_manual(name = "Data Source",
                     values = shapes)  +
    scale_color_manual(name = "Data Source",
                       values = colors)

  if(!validation){
    countyplot = countyplot + geom_segment(aes(x= endyear.i, y=pep_lowererr, xend= endyear.i, yend= pep_uppererr),  col="green4", alpha = 1, size = size_line, linetype = "dashed")
  }

}

if(validation){
  countyplot = countyplot +
    geom_point(aes(x = endyear.i, y = acs_validate, col = "Removed ACS", shape = "Removed ACS"),  size = size_pt) +
    geom_segment(aes(x= endyear.i, y=lowererr_validate, xend= endyear.i, yend= upperr_validate), col = "orange" ,alpha = 0.3, size = size_line, linetype = "solid") +
    geom_point(aes(x = endyear.i, y = pep_validate, col = "Removed PEP", shape = "Removed PEP"), size = size_pt,  stroke =4)

}


if(!subplot){
  countyplot = countyplot + labs(title = co_name) + facet_wrap(~ race.i, scales = "free",  nrow = 1, labeller = labeller(race.i= race.labs))+
    scale_shape_manual(name = "Data Source",
                       values = shapes)  +
    scale_color_manual(name = "Data Source",
                       values = colors)



}else{
  if(!tract_cond){

    countyplot = countyplot + facet_wrap(plotindexf ~ race.i , ncol = 4,scales = "free", labeller = labeller(race.i= race.labs))

    }else{


    tract1 = gsub(" County, Georgia", "", plotdata_c$tract.i)
    tract.labs = gsub("Census ", "", tract1)
    names(tract.labs) = plotdata_c$tract.i


    countyplot = countyplot + facet_wrap(tract.i ~ race.i , scales = "free", ncol = 4, labeller = labeller(tract.i = tract.labs, race.i= race.labs))+
      scale_x_continuous(name = "Year", limits = c(2010,2022),breaks = seq(2010, 2022, by=5))+
      scale_shape_manual(name = "Data Source",
                         values = shapes)  +
      scale_color_manual(name = "Data Source",
                         values = colors)

  }
}

return(countyplot)

}
