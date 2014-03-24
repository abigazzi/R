######################################################
#
# Alex Bigazzi,  abigazzi@pdx.edu
# Extracting elevation from GPS for PDX
#
# v1: 11/12/2013
#
########################################################
# Function to extract elevation data for Portland from DEM or DSM
# Data sources are 1-m LIDAR data, housed in the PSU Geography Dept. (thanks to J. Broach for the link)
# Input the Lat and Lon as individual vectors, in the form of standard GPS data (default datum is WGS 84)
# Select the surface or elevation model with "Model" as "DEM" or "DSM"
# Output is just a string of elevations (in Int'l Feet)

  getElevation <- function(Lat,Lon,Model="DEM",GpsDatum="WGS84"){
    require(httr)
    require(sp)
    require(rgdal)

    if(length(Lat)!=length(Lon)) return ("Error: unequal Lat/Lon vector lengths")
    Output <- rep(NULL,length(Lat))

    # Handle NA's
      Skip <- is.na(Lat) | is.na(Lon)
      if(any(Skip)) Output[Skip] <- NA
      if(all(Skip)) return(Output)
      
    # Convert Lat/Lon to DEM space
      Sp <- SpatialPoints(cbind(Lon[!Skip],Lat[!Skip]), proj4string=CRS(paste0("+proj=longlat +datum=",GpsDatum)))
      # CRS: http://spatialreference.org/ref/?search=state+plane+oregon+north
      StatePlaneOR <- "+proj=lcc +lat_1=44.33333333333334 +lat_2=46 +lat_0=43.66666666666666 +lon_0=-120.5 +x_0=2500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +units=ft +no_defs"
      Sp <- spTransform(Sp, CRS(StatePlaneOR))
      X <- coordinates(Sp)[,1]
      Y <- coordinates(Sp)[,2]

    # Connect to Geography server          # http://atlas.geog.pdx.edu/arcgis/rest/services/Portland_Metro_DEM/
      DemServer <- 'http://atlas.geog.pdx.edu/arcgis/rest/services/Portland_Metro_DEM/DEM_1M/ImageServer/getSamples' # Elevation model: ground-level
      DsmServer <- 'http://atlas.geog.pdx.edu/arcgis/rest/services/Portland_Metro_DEM/DSM_1M/ImageServer/getSamples' # Surface model: includes trees, buildings, bridges, etc.
      Server <- switch(Model,
                  "DEM"=DemServer,
                  "DSM"=DsmServer,
                  "Error")
      QueryStart <- paste0('geometry={','"points"',':')
      QueryEnd <- '}&geometryType=esriGeometryMultipoint&returnFirstValueOnly=true&f=json'

    # Break into sequences of 50
      N <- ceiling(length(X)/50)
      if(N==1) Seg <- factor(rep(N,length(X)-(N-1)*50))  else {
        Seg <- factor(c(rep(1:(N-1),each=50),rep(N,length(X)-(N-1)*50))) }
      for(i in 1:N){
        Points <- paste0('[',paste('[', X[Seg==i],',',Y[Seg==i],']',sep='', collapse=','),']')
        Query <- paste0(QueryStart,Points,QueryEnd)
        Return <- extractValues(GET(Server, query=Query))
        Output[!Skip][Seg==i] <- Return
      }    # Close for loop

      return(Output)

  }   # Close function

  # Used to extract the value output from the url query return from the DEM or DSM
  extractValues <- function(Output) sapply(strsplit(strsplit(content(Output), '"value":"')[[1]][-1],'"'), function(x) as.numeric(x[1]))  # "Output" is the return from a GET url query

########################################################################
# END
########################################################################
