
# Load constants, functions, etc
source("./b03/scripts/0-constants.R")


options(tibble.width = Inf, tibble.print_min = Inf)

#### Inputs ####
# Input parameters
resultTag <- c("BAU_85")
numScenarios <- length(resultTag)
numIterations <- 10
timeInterval <- 100
timesteps <- seq(from=2010, to=2110, by=timeInterval)
numTimesteps = length(timesteps)
speciesCodes <- c("BLBR", "RANA", "MAAM", "URAM", "PLCI")
numSpecies <- length(speciesCodes)


# Input and output files and folders
stsimDir1 <- b03Libraries
outDir <- b03ResultsMapsDir
natAreasMaskFilename <-file.path(b03ProcessedMapsDir, "b03-natural-areas-2010.tif")

#### Create clipping mask
natAreasMask <- raster(natAreasMaskFilename)

#Shapefile with ecoregions
bt4 <- st_read(file.path(b03RawMapsDir, "CR_CERQ_NIV_04_S_b03.shp"))
bt6 <- raster(file.path(b03RawMapsDir, "CR_CERQ_NIV_04_S_b03.tif"))

#Saint-Lawrence Lowlands
bt=st_read(file.path(b03ProcessedMapsDir, "b03-studyarea.shp"))
btp=as(bt, "Spatial")

# STSim
modelDir <- c(stsimDir1)
resultScenarioNumber <- c(218) 
modelFile <- "BTSL_stconnect.ssim"

# MAPPING FUNCTIONS ---------------------------------------------------------
#Assemble all the pieces and map them together
#Define the map theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Arial", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      #panel.border = element_rect(colour = "grey", fill=NA, size=1),
      legend.position="bottom",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-10,-10,-10,-10),
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    )
}

# Habitat suitability all species ---------------------------------------------
for(scn in 1:numScenarios){
  print(paste("Working on scenario", resultTag[scn]))

  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  for(year in timesteps){
    print(paste("Working on scenario", resultTag[scn], "year", year))
    #### Create maps of habitat suitability summed across species and iterations ####
    mapName <- "stconnect_HSOutputHabitatSuitability"
    
    # Get habitat suitability map for year, iteration 1 for all species
    habSuit <- datasheetRaster(myScenario, mapName, iteration = 1, timestep = year)
    # Sum all species
    habSuit <- calc(habSuit, sum)
    # Crop to focal stratum
    #habSuit <- habSuit * clippingMask
    
    # Get habitat suiability maps for year and all other iterations
    for(i in 2:numIterations){
      # Read in habitat suitability map for all iterations for all species 
      habSuitTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
      # Sum all species
      habSuitTemp <- calc(habSuitTemp, sum)
      # Crop to focal stratum
      #  habSuitTemp <- habSuitTemp * clippingMask
      
      habSuit <- habSuit + habSuitTemp
    }    
    
    # Divide by num iterations to get probability
    habSuit <- habSuit / (numIterations * numSpecies * 100)    
    
    # Set NA outside study area
    habSuit<-habSuit*clippingMask
    
    # Set NA outside BTSL
    habSuit_btsl<-habSuit*btslMask
    
    # Write rasters
    # writeRaster(habSuit, paste0(workingDir, resultTag[scn], "_", year, "_habitatSuitabilityALL.tif"), overwrite=T)
    # BTSL
    writeRaster(habSuit_btsl, paste0(workingDir, resultTag[scn], "_", year, "_habitatSuitabilityALL_btsl.tif"), overwrite=T)
  }
}

# Maps of habitat suitability for each focal species -------------------------------------------
for(scn in 1:numScenarios){
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  
  for(year in timesteps){
    print(paste("Working on scenario", resultTag[scn], "year", year))
    mapName <- "stconnect_HSOutputHabitatSuitability"
    
    for(speciesIndex in numSpecies){
      
      # Get habitat suitability map for year, iteration 1 for all species
      habSuit <- datasheetRaster(myScenario, mapName, iteration = 1, timestep = year)
      # Get map for focal species
      habSuit <- habSuit[[speciesIndex]]
      # Sum all species
      habSuit <- calc(habSuit, sum)
      # Crop to focal stratum
      #habSuit <- habSuit * clippingMask
      
      # Get habitat suitability maps for year and all other iterations
      for(i in 2:numIterations){
        # Read in habitat suitability map for all iterations for all species 
        habSuitTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
        # Get map for focal species
        habSuitTemp <- habSuitTemp[[speciesIndex]]
        # Sum all species
        habSuitTemp <- calc(habSuitTemp, sum)
        # Crop to focal stratum
        #habSuitTemp <- habSuitTemp * clippingMask
        
        habSuit <- habSuit + habSuitTemp
      }    
      
      # Divide by num iterations to get probability
      habSuit <- habSuit / (numIterations * 100)   
      
      # Set NA outside study area
      habSuit<-habSuit*clippingMask
      
      # Write rasters
      writeRaster(habSuit, paste0(workingDir, resultTag[scn], "_", mapName, "_", speciesRow, "_", year, ".tif"), overwrite=T)
    }
  }
}




# Habitat patch all species all years ---------------------------------------------
for(scn in 1:numScenarios){
  print(paste("Working on scenario", resultTag[scn]))
  
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
#  for(year in timesteps){
    #### Create maps of habitat suitability summed across species and iterations ####
    mapName <- "stconnect_HSOutputHabitatPatch"

    print(paste("Working on scenario", resultTag[scn], "iteration 1"))    
    # Get habitat patch map for year, iteration 1 for all species
    habPatch <- datasheetRaster(myScenario, mapName, iteration = 1)#, timestep = year)
    # Sum all species
    habPatch <- calc(habPatch, sum)
    # Crop to focal stratum
    #habPatch <- habPatch * clippingMask
    
    # Get habitat patch maps for year and all other iterations
    for(i in 2:numIterations){
      print(paste("Working on scenario", resultTag[scn], "iteration", i))
      # Read in habitat Patchability map for all iterations for all species 
      habPatchTemp <- datasheetRaster(myScenario, mapName, iteration = i)#, timestep=year)
      # Sum all species
      habPatchTemp <- calc(habPatchTemp, sum)
      # Crop to focal stratum
      #  habPatchTemp <- habPatchTemp * clippingMask
      
      habPatch <- habPatch + habPatchTemp
    }    
    
    # Divide by num iterations to get probability
    habPatch <- habPatch / (numIterations * length(timesteps))    
    
    # Set NA outside BTSL
    habPatch_btsl<-habPatch*btslMask
    
    # Write rasters
    writeRaster(habPatch_btsl, paste0(outDir, resultTag[scn], "_allYears_habitatPatchALL_btsl.tif"), overwrite=T)
#  }
}



# Habitat patch all species per year ---------------------------------------------
for(scn in 1:numScenarios){
  print(paste("Working on scenario", resultTag[scn]))
  
  # Setting up st-sim library, project, scenario
  mySession <- session()
  myLibrary <- ssimLibrary(file.path(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  for(year in timesteps){
    print(paste("Working on scenario", resultTag[scn], "year", year))
    #### Create maps of habitat suitability summed across species and iterations ####
    mapName <- "stconnect_HSOutputHabitatPatch"
    
    # Get habitat patch map for year, iteration 1 for all species
    habPatch <- datasheetRaster(myScenario, mapName, iteration = 1, timestep = year)
    # Sum all species
    habPatch <- calc(habPatch, sum)
    # Crop to focal stratum
    #habPatch <- habPatch * clippingMask
    
    # Get habitat patch maps for year and all other iterations
    for(i in 2:numIterations){
      # Read in habitat Patchability map for all iterations for all species 
      habPatchTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
      # Sum all species
      habPatchTemp <- calc(habPatchTemp, sum)
      # Crop to focal stratum
      #  habPatchTemp <- habPatchTemp * clippingMask
      
      habPatch <- habPatch + habPatchTemp
    }    
    
    # Divide by num iterations to get probability
    habPatch <- habPatch / (numIterations)    
    
    # Set NA outside BTSL
    habPatch_btsl<-habPatch*btslMask
    
    # Write rasters
    writeRaster(habPatch_btsl, paste0(outDir, resultTag[scn], "_", year, "_habitatPatchALL_btsl.tif"), overwrite=T)
  }
}


# Habitat patch per species all years ---------------------------------------------
for(scn in 1:numScenarios){
  print(paste("Working on scenario", resultTag[scn]))
  
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  for(year in timesteps){
    print(paste("Working on scenario", resultTag[scn], "year", year))
    #### Create maps of habitat suitability summed across species and iterations ####
    mapName <- "stconnect_HSOutputHabitatPatch"
    
    for(sp in 1:numSpecies){
    # Get habitat patch map for year, iteration 1 for all species
    habPatch <- datasheetRaster(myScenario, mapName, iteration = 1, timestep = year)
    # Sum all species
    habPatch <- calc(habPatch, sum)
    # Crop to focal stratum
    #habPatch <- habPatch * clippingMask
    
    # Get habitat patch maps for year and all other iterations
    for(i in 2:numIterations){
      # Read in habitat Patchability map for all iterations for all species 
      habPatchTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
      # Sum all species
      habPatchTemp <- calc(habPatchTemp, sum)
      # Crop to focal stratum
      #  habPatchTemp <- habPatchTemp * clippingMask
      
      habPatch <- habPatch + habPatchTemp
    }    
    }
    # Divide by num iterations to get probability
    habPatch <- habPatch / (numIterations)    
    
    # Set NA outside BTSL
    habPatch_btsl<-habPatch*btslMask
    
    # Write rasters
    writeRaster(habPatch_btsl, paste0(workingDir, resultTag[scn], "_", year, "_habitatPatchALL_btsl.tif"), overwrite=T)
  }
}


# Habitat suitability all species per year ---------------------------------------------
for(scn in 1:numScenarios){
  print(paste("Working on scenario", resultTag[scn]))
  
  # Setting up st-sim library, project, scenario
  mySession <- session(SyncroSimDir)
  myLibrary <- ssimLibrary(paste0(modelDir[scn], modelFile), session=mySession)
  myProject <- project(myLibrary, "Definitions")  # Assumes there is only one default project per library
  myScenario <- scenario(myProject, resultScenarioNumber[scn])
  #datasheet(myScenario)
  
  for(year in timesteps){
    print(paste("Working on scenario", resultTag[scn], "year", year))
    #### Create maps of habitat suitability summed across species and iterations ####
    mapName <- "stconnect_HSOutputHabitatSuitability"
    
    # Get habitat patch map for year, iteration 1 for all species
    habPatch <- datasheetRaster(myScenario, mapName, iteration = 1, timestep = year)
    # Sum all species
    habPatch <- calc(habPatch, sum)
    # Crop to focal stratum
    #habPatch <- habPatch * clippingMask
    
    # Get habitat patch maps for year and all other iterations
    for(i in 2:numIterations){
      # Read in habitat Patchability map for all iterations for all species 
      habPatchTemp <- datasheetRaster(myScenario, mapName, iteration = i, timestep=year)
      # Sum all species
      habPatchTemp <- calc(habPatchTemp, sum)
      # Crop to focal stratum
      #  habPatchTemp <- habPatchTemp * clippingMask
      
      habPatch <- habPatch + habPatchTemp
    }    
    
    # Divide by num iterations to get probability
    habPatch <- habPatch / (numIterations*100)    
    
    # Set NA outside BTSL
    habPatch_btsl<-habPatch*btslMask
    
    # Write rasters
    writeRaster(habPatch_btsl, paste0(outDir, resultTag[scn], "_", year, "_habitatSuitabilityALL_btsl.tif"), overwrite=T)
  }
}


# Ecoregion level-4 summary Habitat Patch all species per year ------------------------------------------------
#scenario <- "NC_NC"
scenario <- "BAU_85" 
year<-2110
#habPatch_btsl <- raster(paste0(outDir, scenario, "_allYears_habitatPatchALL_btsl.tif"))
habPatch_btsl <- raster(paste0(outDir, scenario, "_", year, "_habitatPatchALL_btsl.tif"))

values(habPatch_btsl)[is.na(values(bt6))]=NA
mns<-data.frame(zonal(habPatch_btsl, bt6, fun="mean"))
#sms<-data.frame(zonal(habPatch_btsl, bt6, fun="sum"))
names(mns)<-c("FID04", "mean")
ecr2=merge(bt4, mns[c("FID04", "mean")])
st_write(ecr2, paste0(outDir, scenario, "_", year, "_HabitatPatches_BTSL_Ecoregion.shp"))

#write.csv(ecv, paste0(workingDir, scenario, "_HabitatPatch_BTSL_Ecoregion.csv"), row.names = F)

# Ten quantiles
qn <- quantile(
  ecv$mean,
  probs=seq(0, 1, length.out=10))

# Min and max vlaues for legend
mnV <- 0 #as.numeric(min(ecv$mean))
mxV <- 5 #as.numeric(max(ecv$mean))

q=ggplot() +
  geom_polygon(data = btp, aes(x = long+2000, #shadow
                               y = lat-800,
                               group=group),
               color = "grey", size = 1, fill="grey") +
  geom_sf(data = bt, size = 1, fill="white", color="black") + #btsl
  geom_sf(data = ecr2, aes(fill=mean), size = 0.03, color="black") +
  coord_sf() +
  theme_map() +
  labs(x = NULL,
       y = NULL) +
  scale_fill_distiller(palette="Greens",
                       direction = 1,
                       # here we use guide_colourbar because it is still a continuous scale
                       guide = guide_colorbar(
                         title = "Probability of habitat patch",
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         # some shifting around
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       ))

ggsave(
  paste0(outDir, scenario, "_", year, "_HabitatPatches_BTSL_Ecoregion1.png"), q, height=4, width=11)


# Ecoregion level-4 summary Habitat Suitability all species per year ----------------------------------------
scenario <- "NC_NC"
#scenario <- "BAU_85" 
year <- 2010
habSuit_btsl <- raster(paste0(outDir, scenario, "_", year, "_habitatSuitabilityALL_btsl.tif"))

values(habSuit_btsl)[is.na(values(bt6))]=NA
mns <- data.frame(zonal(habSuit_btsl, bt6, fun="mean"))
#sms<-data.frame(zonal(habSuit_btsl, bt6, fun="sum"))
names(mns) <- c("FID04", "mean")
ecr2 <- merge(bt4, mns[c("FID04", "mean")])
st_write(ecr2, paste0(outDir, scenario, "_", year, "_HabitatSuitability_BTSL_Ecoregion.shp"))

#write.csv(ecv, paste0(workingDir, scenario, "_HabitatPatch_BTSL_Ecoregion.csv"), row.names = F)

# Ten quantiles
qn <- quantile(
  ecr2$mean,
  probs=seq(0, 1, length.out=10))

# Min and max vlaues for legend
mnV <- 0 #as.numeric(min(ecv$mean))
mxV <- 5 #as.numeric(max(ecv$mean))

q=ggplot() +
  geom_polygon(data = btp, aes(x = long+2000, #shadow
                               y = lat-800,
                               group=group),
               color = "grey", size = 1, fill="grey") +
  geom_sf(data = bt, size = 1, fill="white", color="black") + #btsl
  geom_sf(data = ecr2, aes(fill=mean), size = 0.03, color="black") +
  coord_sf() +
  theme_map() +
  labs(x = NULL,
       y = NULL) +
  scale_fill_distiller(palette="Reds",
                       values=qn,
                       direction = 1,
                       # here we use guide_colourbar because it is still a continuous scale
                       guide = guide_colorbar(
                         title = "Probability of habitat patch",
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         # some shifting around
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       ))

ggsave(
  paste0(outDir, scenario, "_", year, "_HabitatSuitability_BTSL_Ecoregion.png"), q, height=4, width=11)




