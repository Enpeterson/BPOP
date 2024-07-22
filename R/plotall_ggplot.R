



plotall_ggplot <- function( # props from jasgdata to be added eventually for checking it
  output.dir,
  modeldata, CIs,
  plotdata){


shapes<- c("census" = 1, "ACS" = 4, "PEP" = 23, "BPop estimate" = 20, "Removed ACS" = 18,"Removed PEP" = 17)
colors<- c("census" = "black", "ACS" = "red", "PEP" = "green4", "BPop estimate" = "blue", "Removed PEP" = "orange", "Removed ACS" = "orange")

CIscounty<- CIs[["CIsc"]]
nyears <- modeldata$nyears
names.c <- modeldata$counties
C = length(names.c)
p_periods = modeldata$p_periods
tref = modeldata$tref
startyear = modeldata$startyear
endyear = modeldata$endyear
R = modeldata$R



  co_list <- list()

  inds <- seq(1, C, by = 20)

  for(i in 1:(length(inds)-1)){
    co_list[[i]] <- names.c[inds[i]: (inds[i+1]-1)]
  }

  co_list[[length(inds)]] <- names.c[inds[length(inds)]: C]

  combi_data <- plotdata

  size_pt = 15
  size_line = 3
  linewidth = 5
  plotsize = 20
  textsize=10

  pdf(paste(output.dir, "countyplots.pdf",sep=""),  height = 45, width= 60)
  for(i in 1:length(co_list)){
  subnames = co_list[[i]]
  plotdata_c <- combi_data[ which(combi_data$county %in% subnames),]


  countyplot <- ggplot(data = plotdata_c) +
    scale_x_continuous(name = "Year", limits = c(startyear, endyear), breaks = seq(startyear, endyear, by=5)) +
    scale_y_continuous(name = "Population") +
    geom_point(aes(x = year, y = value, col = type), size = size_pt, stroke=1, shape = 19, alpha = 0.4) +
    geom_segment(aes(x= year, y=lowererr, xend= year, yend= uppererr), col = "red" ,alpha = 0.3, size = size_line, linetype = "solid") +
    geom_segment(aes(x= year, y=LQ, xend= year, yend= UQ),  col="blue", alpha = 0.5, size = size_line) +
    scale_color_manual(values  = c("red", "black", "green", "blue")) +
    facet_wrap(county ~ race, ncol = 10, scales = "free") +
    theme_grey(base_size = plotsize) +
    theme(title = element_text(size = plotsize),
          axis.title=element_text(size= plotsize ),
          strip.text.x = element_text(size = plotsize, colour = "blue"),
          legend.text = element_text(size = textsize))
    print(countyplot)
  }
  dev.off()

  return(NULL)
}
