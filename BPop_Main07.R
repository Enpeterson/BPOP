###################################################################################
# BPOP Main Script
###################################################################################
#Emily Peterson
#This is the main script to run the BPOP model.
#All functions needed to run the BPOP model are held in the R folder.
#Any issues in completing a model run, email emily.nancy.peterson@emory.edu


library(devtools)

load_all()


#Read in all needed libraries. Make sure all needed libraries have been installed prior to model runs.
#To view all the needed libraries, open the R folder and open the read_libs.R function.
read_libs()



###################################################################################
# USER SETTINGS AND CUSTOM DIRECTORIES TO BE ADJUSTED FOR EACH USER
###################################################################################
#Runsetting determines length of iterations run. For quick runs, iters = 4000, burnin = 2000.
#Go to RunMCMC to see other settings. Set to "test" for a very quick run to get model running.
runsetting = "test"

#The start year is set to 2006 when the first ACS data point is 2010.
#Change the start year to be 4 years prior to the first ACS data point.
startyear = 2006 #start year of estimates
endyear = 2024 #end year of estimates


#Runname describes the output folder.
#Output folder is named "test_run" given below.
runname = "test"


#Create output directories
output.dir <- paste(getwd(), "/",runname, "/",sep="")
dir.create(output.dir)

#Create data directory
#The data directory should lead to a "data" folder that contains the following:
#1: A .shp file that contains the geographic shapefile of the boundaries.
#2: A data file that contains county-race group specific data on population counts
data.dir <- paste0(getwd(), "/data/")
graph_filename ="County.shp"

#Read in county data.Columns include county, endyear, PEP count, race, GEOID, ACS estimate, ACS, MOEs, and decennial count.
#This data was extracted using tidycensus and compiled using other code.
#Created factors for race and county.
#Endyear denotes the endyear of the 5-year ACS interval, and is merged with PEP and Decennial count.
#Start year is the beginning of the 5-year interval. Note: We use the end year and the corresponding year with PEP and Dec.
#Current data inputs assume data include years 2010 to 2022.

county_data <- readRDS(paste0(data.dir, "ga_county_data.RDS")) %>%
  mutate(startyear = as.numeric(endyear-4))%>%
  filter(endyear > 2009 )
#This gives the columns of the county level data.
  #pep_est = PEP count
  #dec_est = census count
  #estimate = ACS count plus moe = margin of error
colnames(county_data)
head(county_data)


###################################################################################
# Model set-up and run
###################################################################################

#Run this as is.
#List all parameters to be saved from model runs.
#Params are broken down into global parameters
globalparams = c("sigma_eta","sigma_ref", "phi", "omega", "etaglobal","rho")
#County specific parameters
CParams = c("eta_c")
#Race specific parameters
RParams = c("sigma_kappa_r")
#County-year-race specific parameters
Params.ct =  c( "gamma_ctr",  "eta_ctr",  "sigma_r_ctr", "xi_ctr", "sigma_nonsamp_cr","sigmasq_pep_ctr", "eta_cr", "kappa_cr")
parstosave = c(globalparams, RParams, CParams, Params.ct)



#Creating spatial structure using sf, spdep, and INLA packages
gen <- read_sf(paste0(data.dir, graph_filename))
Co_nb <- poly2nb(gen)
nbWB_A <- nb2WB(nb=Co_nb)#Create list of neighbors
W.scale <- nb2mat(Co_nb, zero.policy = TRUE, style = "B")
W.scale <- -W.scale
diag(W.scale) <- abs(apply(W.scale, 1, sum))
Dw<-diag(rowSums(W.scale))

unique_geos <- unique(gen$FIPS)

#GEOIDs not included in the gen graph.
excluded <- unique(county_data$county_name[which(county_data$GEOID %!in% unique_geos)])
#Exclude counties not included in the geography file otherwise model will not run.
county_data <- county_data %>%
  filter(county_name %!in% excluded)


#Create modeldata and save to output folder.
#Set the reference year for the random walk to be 2010.
# Find the undercounty for 2020 and 2010 census at:
# https://www2.census.gov/programs-surveys/decennial/coverage-measurement/pes/national-census-coverage-estimates-by-demographic-characteristics.pdf«˚‘=]]]]]]]]

#####################################################################
  ######### WARNING !!!
  #These undercounts need to be hard-coded from the USCB Post Enumeration Survey
  #If these under-counts change based on population definitions, these must be updated within the function.
# Make sure to update the percent undercount within the get_model_data_revised.R function before running.
#####################################################################

modeldata <- get_model_data_revised(output.dir, data.dir,
                                        county_data = county_data,
                                        graph_filename = graph_filename,
                                        startyear = startyear,
                                        endyear = endyear,
                                          seed = 1,
                                    tref_rw = 2010 #Set the reference year to be the 2010 census year
                                    )

#Function to run the nimble code.
## Within this function initial values are set for the unknown parameters.
## It reads in the model file WriteModel_spattemp_nimble.R. Tp use a different model file, you must update within the function.
## Saved in the output directory is the following:
### 1. posterior samples and summaries created from the nimble code in fit.RDS
### 2. Traceplots of parameters broken down into groups, global, country pars, race pars, county-race-year pars.
### 3. CIs.RDS is a data set of summaries of parameters including uncertainty bounds.
##Note this run will take a while.
RunMCMC(
        runsetting = runsetting, #Length of run
        output.dir = output.dir, #Output directory
        percentiles = c(0.025, 0.5, 0.975), #Percentailes for uncertainty bounds
        n = 1,
        parstosave,
        nbWB_A = nbWB_A,
        scale = scale,
        globalparams = globalparams,
        RParams = RParams,
        CParams = CParams,
        Params.ct=Params.ct,
        validation = F,
        recent_years = F)


modeldata <- readRDS(paste(output.dir, "modeldata.RDS", sep=""))
CIs <- readRDS(paste(output.dir, "CIs.RDS", sep=""))


#Function to create BPop plot ready data. Saved to output directory.
plotdata <- create_plot_data(CIs = CIs, modeldata = modeldata, county_data = county_data)

#Function to output BPOP Plots with posterior estimates and uncertainties.
##Plots ACS, PEP, Census against BPOP from start year to end year for all counties in faceted plots.
##Plots are saved to output directory.
plotall_ggplot (
  output.dir = output.dir,
  modeldata = modeldata,
  CIs = CIs,
  plotdata = plotdata)

