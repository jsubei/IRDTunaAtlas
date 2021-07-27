#Taha IMZILEN, JULIEN BARDE UPDATE 2021
#####This Generic script allow to transform Tuna Atlas data (stored in SARDARA) to NetCDF respecting a naming conventions described in this guidelines (url below):
## Guidelines Doc (Naming conventions): https://docs.google.com/document/d/1pS7X3bP669nX0Ikk45fTaE5bL-mR5CgMx-J1_sfAPJI/edit?usp=sharing
# https://pjbartlein.github.io/REarthSysSci/netCDF.html#reshaping-from-raster-to-rectangular
# https://community.rstudio.com/t/how-do-i-map-netcdf-files-in-r/35859/3

########################
rm(list=ls())
########################
#Used Packages:
#######################
library(DBI)
library(sf)
library(sp)
require(raster)
require(ncdf4)
library(leaflet)
library(chron)
# require(IndicatorsForFisheries)
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/wkt2spdf.R")
########################
#Database requests:
#######################
# SQLrequest <- "SELECT year,month,species ,gear,flag, sum(value) AS catches,ST_asText(geom) AS geom_wkt 
# from tunaatlas_indicators.tunaatlas_catches_by_quadrant55_year_month_gear_species_flag 
# group by year, month,species,gear,flag, geom_wkt"

##Database Identifiers: tunaatlas / db-tunaatlas
# source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")

# query<-"SELECT species,schooltype,gear,flag,catchtype,time_start,time_end,v_catch,ST_asText(geom) AS geom_wkt FROM tunaatlas_ird.global_catch_tunaatlasird_5deg_1m_1950_01_01_2016_01_01_l0 where catchunit IN ('MT','MTNO')"
# , EXTRACT(YEAR FROM time_start) as year,EXTRACT(MONTH FROM time_end) as month
# query<-"SELECT species,schooltype,gear,flag,catchtype,time_start,time_end,value AS v_catch,geom_wkt AS geom_wkt
# FROM fact_tables.global_catch_5deg_1m_firms_level0
# WHERE unit IN ('t')  AND species IN ('ALB') AND gear='09.32' AND flag='TWN' AND extract(year from time_start) > 2000 AND extract(year from time_start) < 2002
# ORDER BY time_start ASC"

query<-"SELECT ST_asText(geom) AS geom_wkt, DATE(year || '-01-01') AS time_start, DATE(year || '-12-31') AS time_end, species, country as flag, value AS v_catch FROM public.i6i7i8
WHERE species IN ('YFT') AND country IN ('747') AND year > 2000
;"

# query<-"SELECT* FROM public.i6i7i8 LIMIT 100;"

res_dimensions_and_variables<-dbGetQuery(con,query)
head(res_dimensions_and_variables)
nrow(res_dimensions_and_variables)
unique(res_dimensions_and_variables$year)
unique(res_dimensions_and_variables$species)
unique(res_dimensions_and_variables$flag)

# res_dimensions_and_variables$time <- as.Date((df$time))
# res_dimensions_and_variables$year <- as.numeric(format(res_dimensions_and_variables$year, "%Y"))
# res_dimensions_and_variables$month <- as.numeric(format(df$time, "%m"))
# res_dimensions_and_variables$day <- as.numeric(format(df$time, "%d"))

########################
#Script parameters:
#######################
sp_resolution=5   # 1 or 5 (deg)
# t_resolution= 'year'  # year or month 
variables ='v_catch'   # One Variable // to do: combinations of these variables : catches, effort and size_class
dimensions= "all"  # combinations of these dimensions : species, flag, gear, ...! all for to include all dimensions and empty "" 



t_resolution <- as.numeric(diff(c(as.Date(res_dimensions_and_variables$time_start[1]),as.Date(res_dimensions_and_variables$time_end[1])),unit='day'))
# t_resolution <- as.numeric(diff(c(as.Date(origin=Sys.Date(),res_dimensions_and_variables$year[1]),as.Date(origin=Sys.Date(),res_dimensions_and_variables$year[1])),unit='day'))
# switch (t_resolution,
#         'year' = aggBy <- c(aggBy,'year'),
#         'month' = aggBy <- c(aggBy,c('year','month')))

##################
##aggregate data:
##################
aggBy <- c("geom_wkt","time_start")
# aggBy <- c("geom_wkt","year")

if(tolower(dimensions)=='all' ){
  # dimensions <- names(res_dimensions_and_variables)[-which( names(res_dimensions_and_variables) %in% c('v_catch','geom_wkt','year'))]
  dimensions <- names(res_dimensions_and_variables)[-which( names(res_dimensions_and_variables) %in% c('v_catch','geom_wkt','time_start','time_end'))]
}
if(nchar(dimensions[1]) != 0 & dimensions[1]!='all' ){
  aggBy <- c(aggBy,dimensions)
  }


switch (variables,
        'v_catch' = aggFun <- 'sum',
        'efforts' = aggFun <- 'sum',
        'sizeclass' = aggFun <- 'mean')

res_dimensions_and_variables <- aggregate(res_dimensions_and_variables[variables],res_dimensions_and_variables[aggBy],FUN=aggFun)
head(res_dimensions_and_variables)
vars <-res_dimensions_and_variables[[variables]]
geom_wkt <- res_dimensions_and_variables$geom_wkt

# year <-res_dimensions_and_variables$year
# if(t_resolution=="month"){month <- res_dimensions_and_variables$month}


#####test exact box:
test <- data.frame(wkt=unique(geom_wkt),id=1:length(unique(geom_wkt)))
sptest <- wkt2spdf(df = test,wkt_col_name = 'wkt',id_col_name = 'id')
box <- bbox(sptest)

##################
##DEFINE DIMENSIONS FOR NETCDF:
##################

##### TEMPORAL DIMENSION  ##########

##Function to Find the last day
# fld <- function(date) {
#   # date character string containing POSIXct date
#   date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
#   mon <- date.lt$mon + 2 
#   year <- date.lt$year
#   year <- year + as.integer(mon==13) # if month was December add a year
#   mon[mon==13] <- 1
#   iso = ISOdate(1900+year, mon, 1, hour=0, tz=attr(date,"tz"))
#   result = as.POSIXct(iso) - 86400 # subtract one day
#   result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
# }

# switch (t_resolution,
#         'year' = {t_year<- unique(year)
#         dateVector <- as.Date(paste(t_year,"12","31",sep="/"))},
#         'month' = {t_year<- year
#         t_month <-month
#         t_cbind <- array(cbind(t_year,t_month),c(length(t_year),2) )
#         dateVector <- as.POSIXct((paste(t_cbind[,1],t_cbind[,2],"01",sep="/")))
#         dateVector <- fld(dateVector)
#         dateVector <- as.Date(dateVector)}
# )


# dateVector1 <- sort(unique((dateVector)))
# dateVector <- sort(unique(as.numeric(dateVector)))

dateVector1 <- sort(unique((res_dimensions_and_variables$time_start)))
# dateVector1 <- sort(unique((res_dimensions_and_variables$year)))

class(dateVector1)
dateVector <- julian(x=as.numeric(format(dateVector1,'%m')),d=as.numeric(format(dateVector1,'%d')),y=as.numeric(format(dateVector1,'%Y')),origin.=c(month = 1, day = 1, year = 1950))


##### SPATIAL DIMENSION  ##########

longitudeVector = seq(from=box['x','min']+(sp_resolution/2), to=box['x','max']-(sp_resolution/2), by=sp_resolution)
latitudeVector = seq(from=box['y','min']+(sp_resolution/2), to=box['y','max']-(sp_resolution/2), by=sp_resolution)


######################################################################
##### DEFINE SPATIAL and TEMPORAL DIMENSIONS  ##########
######################################################################
# XYT dimensions
dimX <- ncdim_def("longitude", "degrees_east", longitudeVector)
dimY <- ncdim_def("latitude", "degrees_north", latitudeVector)
dimT <- ncdim_def("time", "days since 1950-01-01 00:00:00", dateVector, unlim = FALSE)


listDims <- list(dimX,dimY)

names <- list()
ref=list()
values <- list()
meaningValue= list(names=list(names),ref=list(ref),values=list(values))
######################################################################
##### DEFINE OTHERS DIMENSIONS  ##########
######################################################################
if(nchar(dimensions[1])!=0){
  for(dim in dimensions){
    #@julien add a condition to keep only dimensions whose length > 1
    dimvals <- unique(res_dimensions_and_variables[[dim]])
    if(length(dimvals) > 1){
      if(grepl('_',dim)){
        dimVector = dimvals
        unit=unlist(strsplit(dim,'_'))[2]
        dim=unlist(strsplit(dim,'_'))[1]
      } else {
        dimVector = 1:length(dimvals)
        unit =""
        meaningValue$names[[match(dim,dimensions)]] <- dim
        meaningValue$ref[[match(dim,dimensions)]] <- dimVector
        meaningValue$values[[match(dim,dimensions)]] <- dimvals
        ##meaningvalue to add automatically as attribut for the character dimensions 
      }
      dimns <- ncdim_def(dim, unit, dimVector)
      listDims <- c(listDims,list(dimns))
    }

  }
}
listDims <- c(listDims,list(dimT))

######################################################################
##### DEFINE VARIABLES  ##########
######################################################################
nonAvailable <- -9999
varproj <-  ncvar_def(name="crs", units="", dim=NULL, missval=nonAvailable, prec="integer")
switch (variables,
        'v_catch' = unite <- 'tons',
        'efforts' = unite <- '???',
        'size_class' = unite <- '???')
varXd <- ncvar_def(name=variables, units=unite, dim=listDims, missval=nonAvailable, prec="float")
######################################################################
##### CREATE EMPTY NETCDF FILE  ##########  # create netCDF file and put arrays
######################################################################
# Create file name
netCDF_CF_filename <- paste("SARDARA_",variables,"_",paste(dimensions,collapse = "-"),"_",sp_resolution,"deg_",t_resolution,'D',dateVector1[1],"_",dateVector1[length(dateVector1)],".nc",sep="")
nc <- nc_create(netCDF_CF_filename,list(varproj,varXd))
cat("netCDF File created")

######################################################################
##### ADD VALUES TO VARIABLES  ##########
######################################################################
#data to the ncdf file
# switch (t_resolution,
#         'year'= ye <- as.numeric(format(dateVector1,'%Y')),
#         'month'= {ye <- as.numeric(format(dateVector1,'%Y')) 
#         mo <- as.numeric(format(dateVector1,'%m'))})




###Généric loops for The dimensions
####################################
list1 <- list()
list2 <- list()
l1 <- 1
#check the length of each dimension by selecting distinct values to create indices
for(indd in length(dimensions):1){
  # cat(indd)
  if(length(unique(res_dimensions_and_variables[[dimensions[indd]]]))>1){
    #count distinct values and set length of related dimension with related indices
    gridDim<-unique(res_dimensions_and_variables[[dimensions[indd]]]) ; gridDimInd<-1:length(unique(res_dimensions_and_variables[[dimensions[indd]]]))
    list1[[l1]] <- gridDim ; list2[[l1]] <- gridDimInd
    l1 <- l1+1
    gridDim <- expand.grid(gridDim,unique(res_dimensions_and_variables[[dimensions[indd]]])); gridDimInd <- expand.grid(gridDimInd,1:length(unique(res_dimensions_and_variables[[dimensions[indd]]])))
  }

 }
# }


gridDim<- do.call(expand.grid,list1);gridDimInd<- do.call(expand.grid,list2)
if(nrow(gridDim)==0){gridDimInd <- data.frame()
}else {
if(length(ncol(gridDim))==0){gridDim <- as.data.frame(gridDim); gridDimInd <- as.data.frame(gridDimInd)
} else {gridDim <- gridDim[,ncol(gridDim):1]
gridDimInd <- gridDimInd[,ncol(gridDimInd):1]
}
}

####
######
num_time <- julian(x=as.numeric(format(res_dimensions_and_variables$time_start,'%m')),d=as.numeric(format(res_dimensions_and_variables$time_start,'%d')),y=as.numeric(format(res_dimensions_and_variables$time_start,'%Y')),origin.=c(month = 1, day = 1, year = 1950))
res_dimensions_and_variables$num_time <- num_time
numLayer=1
gridDim <- as.data.frame(gridDim)
gridDimInd <- as.data.frame(gridDimInd)
for(i in 1:length(dateVector)){
    resT <- subset(res_dimensions_and_variables,res_dimensions_and_variables$num_time==dateVector[i])
    resT$num_time=NULL
  if(length(list1)>=1){
    nrwgrid <- nrow(gridDim)
  }else{nrwgrid<- 1}
  
  for(j in 1:nrwgrid){
    
    resTD <- resT
  
    if(nrow(gridDim)==0){
      cat("La grille est de dimension nulle")
      print(paste('couche ',numLayer,' sur ',length(dateVector),sep=''))
    } else {
      for(jc in 1:ncol(gridDim)){
        resTD <- subset(resTD, resTD[[dimensions[jc]]]==gridDim[j,jc])
      }
      print(paste('couche ',numLayer,' sur ',nrow(gridDim)*length(dateVector),sep=''))
    }
    numLayer <- numLayer+1
    if(nrow(resTD)==0){
      # ncvar_put(nc=nc, varid=var3d, vals=as.matrix(rep(nonAvailable,72*36)), start = c(1, 1, p, e, s, r, g, i),  count = c(-1, -1, 1, 1, 1, 1, 1, 1))
      next()
    } else {
      resTD$fake_id <- seq(1:nrow(resTD))
      spDF <- wkt2spdf(resTD, "geom_wkt", "fake_id")
      # head(spDF)
      # rast <- rasterize(spDF, raster(ncol=72, nrow=36, xmn=-180, xmx=180, ymn=-90, ymx=90, crs=CRS("+init=epsg:4326")), "v_catch")
      #resolution a automatisé
      rast <- rasterize(spDF, raster(ncol=as.numeric(diff(box['x',]))/sp_resolution, nrow=as.numeric(diff(box['y',]))/sp_resolution, xmn=box['x','min'], xmx=box['x','max'], ymn=box['y','min'], ymx=box['y','max'], crs=CRS("+init=epsg:4326")), variables)
      data <- as.matrix(rast)
      data <- t(data[nrow(data):1,])
      
      if(nrow(gridDim)==0){strt <- NULL}  
      if(nrow(gridDim)!=0){strt <- as.numeric(gridDimInd[j,])}
      
      ncvar_put(nc=nc, varid=varXd, vals=data, start = c(1, 1, strt,   i),  count = c(-1, -1, rep(1,ncol(gridDim)),  1))
    }
  }
}


###add a flags meaning Automatically to Non-numerical dimensions
################################################################
for(indD in 1: length(dimensions)){
  loc <-  match(dimensions[indD], unlist(meaningValue$names))
  if(!is.na(loc)){
    ncatt_put(nc,dimensions[indD],"flag_values",paste((meaningValue$ref[[loc]]),collapse = ","))
    ncatt_put(nc,dimensions[indD],"flag_meanings",paste(gsub(" ","_",as.character(meaningValue$values[[loc]])),collapse=" "))
    ncatt_put(nc,dimensions[indD],"valid_range",paste(c(min(meaningValue$ref[[loc]]),max(meaningValue$ref[[loc]])),collapse = ","))}
}




######################################################################
#####  DIM VAR ATTRIBUTES  ##########
######################################################################
ncatt_put(nc,"longitude","standard_name","longitude")
ncatt_put(nc,"longitude","axis","X") 
ncatt_put(nc,"longitude","_CoordinateAxisType","longitude")
ncatt_put(nc,"longitude","valid_min",box['x','min'])
ncatt_put(nc,"longitude","valid_max",box['x','max'])

ncatt_put(nc,"latitude","standard_name","latitude")
ncatt_put(nc,"latitude","axis","Y")
ncatt_put(nc,"latitude","_CoordinateAxisType","latitude")
ncatt_put(nc,"latitude","valid_min",box['y','min'])
ncatt_put(nc,"latitude","valid_max",box['y','max'])

ncatt_put(nc,"time","standard_name","time")
ncatt_put(nc,"time","axis","T")
ncatt_put(nc,"time","_CoordinateAxisType","time")
ncatt_put(nc,"time","valid_min",min(dateVector))
ncatt_put(nc,"time","valid_max",max(dateVector))
ncatt_put(nc,"time","calendar",'standard')

ncatt_put(nc,"crs","grid_mapping_name","latitude_longitude")
ncatt_put(nc,"crs","longitude_of_prime_meridian",0.0)
ncatt_put(nc,"crs","semi_major_axis",6378137.0)
ncatt_put(nc,"crs","inverse_flattening",298.257223563)

ncatt_put(nc,"v_catch","grid_mapping","crs")


##### GLOBAL ATTRIBUTES  ##########
#################################

##Extent Search
ncatt_put(nc,0,"geospatial_lat_min",box['y','min'])
ncatt_put(nc,0,"geospatial_lat_max",box['y','max'])
ncatt_put(nc,0,"geospatial_lon_min",box['x','min'])
ncatt_put(nc,0,"geospatial_lon_max",box['x','max'])

ncatt_put(nc,0,"time_coverage_start",as.character(min(dateVector1)))
ncatt_put(nc,0,"time_coverage_end",as.character(max(dateVector1)))
ncatt_put(nc,0,"julian_day_unit","days since 1970-01-01 00:00:00")
ncatt_put(nc,0,"time_min",min(dateVector))
ncatt_put(nc,0,"time_max",max(dateVector))

##Extent Information
ncatt_put(nc,0,"geospatial_lat_units","degrees_north")
ncatt_put(nc,0,"geospatial_lat_resolution",sp_resolution)
ncatt_put(nc,0,"geospatial_lon_units","degrees_east")
ncatt_put(nc,0,"geospatial_lon_resolution",sp_resolution)
YY <- format(as.Date(max(dateVector) - min(dateVector),origin = "0000-01-01 00:00:00"),'%Y')
MM <- format(as.Date(max(dateVector) - min(dateVector),origin = "0000-01-01 00:00:00"),'%m')
DD <- format(as.Date(max(dateVector) - min(dateVector),origin = "0000-01-01 00:00:00"),'%d')
ncatt_put(nc,0,"time_coverage_duration",paste("P",YY,"Y",as.numeric(MM)-1,"M",as.numeric(DD)-1,"D",sep=""))
# YY <- format(as.Date(dateVector[2] - min(dateVector),origin = "0000-01-01 00:00:00"),'%Y')
# MM <- format(as.Date(dateVector[2] - min(dateVector),origin = "0000-01-01 00:00:00"),'%m')
# DD <- format(as.Date(dateVector[2] - min(dateVector),origin = "0000-01-01 00:00:00"),'%d')
YY <- 0
MM <- 0
ncatt_put(nc,0,"time_coverage_resolution",paste("P",YY,"Y",MM,"M",t_resolution,"D",sep=""))

## Text search
title <- paste("Global",variables,"of tuna",sep=" ")
ncatt_put(nc,0,"title",title)
ncatt_put(nc,0,"summary",paste("This file is a time series of the global",variables,"of tuna aggregated by time and by space",sep=" "))
ncatt_put(nc,0,"keywords",paste("tuna, fisheries, ",variables,",tuna RFMOs",sep=""))
ncatt_put(nc,0,"history","This dataset has been produced by merging geo-spatial data of tuna ,catches coming from four tuna regional fisheries management organizations: IOTC, ICCAT, IATTC, WCPFC. Some processes have been applied by the French Research institute for sustainable development (IRD) to the raw data: mainly conversion from number of fishes catched to weight of fishes (for catches originally expressed in number), and raisings.")
ncatt_put(nc,0,"comment","Data extracted from SARDARA database")
ncatt_put(nc,0,"source","SARDARA database")
ncatt_put(nc,0,"Conventions","CF 1.6")

##creator search
ncatt_put(nc,0,"creator_email","taha.imzilen@ird.fr julien.barde@ird.fr paul.taconet@ird.fr")
ncatt_put(nc,0,"creator_name","IMZILEN.T BARDE.J TACONET.P")
ncatt_put(nc,0,"date_created", as.character(Sys.time()))
ncatt_put(nc,0,"date_modified", as.character(Sys.time()))
ncatt_put(nc,0,"institution","Institut de Recherche pour le Developpement")
ncatt_put(nc,0,"contributor_name","ICCAT, IOTC, IATTC, WCPFC")
ncatt_put(nc,0,"contributor_role","data provider")


nc_close(nc)


# Load raster and set projection and lat/lon extent
pr <- raster(netCDF_CF_filename, varname="v_catch")

# Project to the leaflet lat/long grid and visualize
r <- projectRasterForLeaflet(pr, method = "bilinear")
plot(r)


# set color palette
color_pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                          na.color = "transparent")

# underlay basic leaflet map, overlayed with JSON boundary and raster grid
leaflet() %>% addTiles() %>%
  addRasterImage(r, colors = color_pal, opacity = .7) %>%
  addLegend(pal = color_pal, values = values(r),
            title = "Les captures de thon") 

