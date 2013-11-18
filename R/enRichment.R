
enRichment <- function(data, opendapUrl, varName, 
                       geomMethod="closest", geomDistMethod="dirty", 
                       timeMethod="closest", 
                       longitudeVarName="longitude",
                       latitudeVarName="latitude",
                       timeVarName="time",
                       xName="x",
                       yName="y",
                       timeName="time",
                       verbose=FALSE)
{  
  library(ncdf)
  library(sp)
  library(fpc)
  
  #test the indput data
  if (missing(data)) {
    stop("no data parameter")
  }
  
  if (missing(varName)) {
    stop("no varName parameter")
  }
  
  if (missing(opendapUrl)) {
    stop("no opendapUrl parameter")
  }
  
  if (inherits(data, what="data.frame"))  {
    if (! xName %in% names(data)) {
      stop("x data not found")
    }
    if (! yName %in% names(data)) {
      stop("y data not found")
    }
    x <- data[, xName]
    y <- data[, yName]
    if (exists("timeName") && timeName %in% names(data)) {
      d <- data[, timeName]
    }    
  } else {
    if (inherits(data, what="SpatialPointsDataFrame")) {
      x <- data@coords[, 1]
      y <- data@coords[, 2]
      if (exists("timeName") && timeName %in% names(data@data)) {
        d <- data@data[, timeName]
      }
    }    
  }

  #id of rows
  data.df.id <- 1:length(x)
    
  #open the netCDF
  if (verbose) {
    cat("Opening the netCDF\n")
  }  
  nc <- open.ncdf(opendapUrl)
  
  if (verbose) {
    cat("Check variables and dimensions\n")
  }
  #find the requested var and store the indice
  netcdf.var.id <- which(unlist(lapply(X=nc$var, FUN=function(var) {var$name == varName || att.get.ncdf(nc, var$name, "standard_name")$value == varName})))
  if (length(netcdf.var.id) == 0) {
    stop(paste("Cannot find requested var:", varName))
  }
  
  netcdf.var <- nc$var[[netcdf.var.id]]
  
  #check the dimensions
  longitude.var.name <- longitudeVarName
  longitude.var.units <- "degrees_east"
  longitude.dim.index <- NA
  latitude.var.name <- latitudeVarName
  latitude.var.units <- "degrees_north"
  latitude.dim.index <- NA
  time.var.name <- timeVarName
  time.dim.index <- NA
  
  for (current.dim.index in 1:length(netcdf.var$dim)) {
    #check for longitude dimension
    if (netcdf.var$dim[[current.dim.index]]$name == longitude.var.name ||
          att.get.ncdf(nc, netcdf.var$dim[[current.dim.index]]$name, "standard_name")$value == longitude.var.name) {
      if (netcdf.var$dim[[current.dim.index]]$units == longitude.var.units) {
        longitude.dim.index <- current.dim.index
        longitude.var.values <- get.var.ncdf(nc=nc, varid=netcdf.var$dim[[current.dim.index]]$name)
      } else {
        print(paste("Bad units value for longitude dimension: found ", netcdf.var$dim[[current.dim.index]]$units, " but expected ", longitude.var.units, sep=""))
      }        
    }
    #check for latitude dimension
    if (netcdf.var$dim[[current.dim.index]]$name == latitude.var.name ||
          att.get.ncdf(nc, netcdf.var$dim[[current.dim.index]]$name, "standard_name")$value == latitude.var.name) {
      if(netcdf.var$dim[[current.dim.index]]$units == latitude.var.units) {
        latitude.dim.index <- current.dim.index
        latitude.var.values <- get.var.ncdf(nc=nc, varid=netcdf.var$dim[[current.dim.index]]$name)
      } else {
        print(paste("Bad units value for latitude dimension: found ", netcdf.var$dim[[current.dim.index]]$units, " but expected ", latitude.var.units, sep=""))
      }        
    }
    #check for time dimension
    if (netcdf.var$dim[[current.dim.index]]$name == time.var.name ||
          att.get.ncdf(nc, netcdf.var$dim[[current.dim.index]]$name, "standard_name")$value == time.var.name) {
      time.var.units <- netcdf.var$dim[[current.dim.index]]$units
      time.var.units.increment <- unlist(strsplit(time.var.units, " since "))[1]
      if (time.var.units.increment == "seconds") {
        time.var.units.increment <- "secs"
      }
      time.var.units.reference <- strptime(unlist(strsplit(time.var.units, " since "))[2], format="%Y-%m-%d %H:%M:%S")
      if (is.na(time.var.units.reference)) {
        time.var.units.reference <- strptime(unlist(strsplit(time.var.units, " since "))[2], format="%Y-%m-%dT%H:%M:%S")
      }
      if (is.na(time.var.units.reference)) {
        time.var.units.reference <- strptime(unlist(strsplit(time.var.units, " since "))[2], format="%Y-%m-%dT%H:%M:%SZ")
      }
      if (is.na(time.var.units.reference)) {
        time.var.units.reference <- strptime(unlist(strsplit(time.var.units, " since "))[2], format="%Y-%m-%d")
      }
      if (is.na(time.var.units.reference)) {
        stop(paste("Cannot read time units: ", time.var.units))
      }
      
      #test for climatology
      if (att.get.ncdf(nc, netcdf.var$dim[[current.dim.index]]$name, "climatology")$hasatt) {
        #yes, this is a climatology. Read the climatology var
        time.climatology.var.name <- att.get.ncdf(nc, netcdf.var$dim[[current.dim.index]]$name, "climatology")$value
        time.climatology.bounds <- get.var.ncdf(nc=nc, varid=time.climatology.var.name)
        
      }
      
      time.dim.index <- current.dim.index
      time.var.values <- get.var.ncdf(nc=nc, varid=netcdf.var$dim[[current.dim.index]]$name)
      time.var.realValues <- time.var.units.reference + as.difftime(time.var.values, units = time.var.units.increment)
    }
  }
  
  if (is.na(longitude.dim.index)) {
    stop("longitude dim not found")
  }
  if (is.na(latitude.dim.index)) {
    stop("latitude dim not found")
  }
  
  if (! is.na(time.dim.index) && exists("d")) {
    withTime <- TRUE
  } else {
    withTime <- FALSE
  }
  
  f2 <- function(v) if(sum(is.na(v)) == length(v)) NA else which.min(v)
  
  if (withTime) {
    #traitement du temps
    if (verbose) {
      cat("Preprocessing time\n")
    }
    
    time.diff <- outer(time.var.realValues, d, "-")
    
    if (timeMethod == "closest") {
      time.indices <- unlist(apply(abs(time.diff), MARGIN=2, FUN=f2))
    }
    
    rm(time.diff)
  }
  
  #traitement de la géographie
  if (verbose) {
    cat("Preprocessing geometry\n")
  }
  
  if (geomDistMethod == "dirty") {
    #build a bbox
    #TODO peut être généralisé ! si xmin = 1 ajouter length(longitude.var.values) et si xmax = length(longitude.var.values) ajouter 1
    bbox.x.min <- max(which.min(abs(longitude.var.values - min(x, na.rm=TRUE))) - 1, 1)
    bbox.x.max <- min(which.min(abs(longitude.var.values - max(x, na.rm=TRUE))) + 1, length(longitude.var.values))
    bbox.y.min <- which.min(abs(latitude.var.values - min(y, na.rm=TRUE)))
    bbox.y.max <- which.min(abs(latitude.var.values - max(y, na.rm=TRUE)))
    if (bbox.y.max < bbox.y.min) {
      #case of inverted latitude axis
      bbox.y.min <- min(bbox.y.min + 1, length(latitude.var.values))
      bbox.y.max <- max(bbox.y.max - 1, 1)
      tmp <- bbox.y.min
      bbox.y.min <- bbox.y.max
      bbox.y.max <- tmp
    } else {
      bbox.y.min <- max(bbox.y.min - 1, 1)
      bbox.y.max <- min(bbox.y.max + 1, length(latitude.var.values))  
    }
    
    longitude.diff <- outer(longitude.var.values[bbox.x.min:bbox.x.max], x, "-")
    latitude.diff <- outer(latitude.var.values[bbox.y.min:bbox.y.max], y, "-")
    
    if (geomMethod == "closest") {      
      longitude.indices <- unlist(apply(abs(longitude.diff), MARGIN=2, FUN=f2)) + bbox.x.min - 1
      latitude.indices <- unlist(apply(abs(latitude.diff), MARGIN=2, FUN=f2)) + bbox.y.min - 1
    }
    rm(longitude.diff)
    rm(latitude.diff)
  } else {
    geom.expand.mat <- expand.grid(x=longitude.var.values, y=latitude.var.values)
    geom.expand.mat <- matrix(c(geom.expand.mat$x, geom.expand.mat$y), ncol=2)
    
    #calcul des distances, découpage par pts
    f1 <- function(x, mat) which.min(spDistsN1(mat, x, longlat=geomDistMethod == "GC"))
    geom.expand.indices <- apply(X=cbind(x,y), MARGIN=1, f1, mat=geom.expand.mat) #TODO : paralléliser ?
    
    #conversion en indice du netCDF
    if(geomMethod == "closest") {
      longitude.indices <- match(geom.expand.mat[geom.expand.indices,1], longitude.var.values)
      latitude.indices <- match(geom.expand.mat[geom.expand.indices,2], latitude.var.values)
    }
    #free the big mat
    rm(geom.expand.mat)
  }
  
  
  if (verbose) {
    cat("Computing data to fetch\n")
  }
  
  indices.df <- data.frame(lon=longitude.indices, lat=latitude.indices)
  if (withTime) {
    indices.df <- cbind(indices.df, time=time.indices)
  }
  
  #on ne va pas demander 2 fois la même cellule !
  unique.id <- rep(x=NA, times=nrow(indices.df))
  indices.df.unique <- na.omit(unique(indices.df))

if (verbose) {
  cat("lon range", max(indices.df.unique$lon) - min(indices.df.unique$lon), "\n")
  cat("lat range", max(indices.df.unique$lat) - min(indices.df.unique$lat), "\n")
  cat("time range", max(indices.df.unique$time) - min(indices.df.unique$time), "\n")
}  
  
  for (i in 1:nrow(indices.df.unique)) {
    unique.id[which(apply(mapply(indices.df, indices.df.unique[i,], FUN="=="), MARGIN=1, FUN=all))] <- i
  }
    
  ####
  #compute a cluster of distance for netCDF index
  ds <- dbscan(indices.df.unique, eps=20, MinPts=2)
  indices.df.unique <- cbind(indices.df.unique, cluster=ds$cluster, id=1:nrow(indices.df.unique))
  
  #fetching procedure
  fetch.df <- data.frame(stringsAsFactors=FALSE)
  start.array <- rep(1, length(netcdf.var$dim))
  count.array <- rep(1, length(netcdf.var$dim))
  
  if (verbose) {    
    fetch.count <- max(indices.df.unique$cluster) + nrow(indices.df.unique[indices.df.unique$cluster == 0,])
    cat("Fetching", fetch.count, "data blocks\n")
    fetch.current <- 0
    progress.bar <- txtProgressBar(min=0, max=fetch.count)
  }
  
  for (current.cluster in unique(indices.df.unique$cluster)) {
    current.indices.df <- indices.df.unique[indices.df.unique$cluster == current.cluster,]
#print(paste("clust", current.cluster))
    if (current.cluster == 0) {
      count.array <- rep(1, length(netcdf.var$dim))
      #points not in a cluster
      for (i in 1:nrow(current.indices.df)) {
        start.array[longitude.dim.index] <- current.indices.df$lon[i]
        start.array[latitude.dim.index] <- current.indices.df$lat[i]
        if (withTime) {
          start.array[time.dim.index] <- current.indices.df$time[i]
        }
        
        #read data from netCDF
        value <- get.var.ncdf(nc=nc, varid=netcdf.var$name, start=start.array, count=count.array)
        
        #build the result DF
        current.fetch.df <- data.frame(lon=current.indices.df$lon[i],
                                 lat=current.indices.df$lat[i],
                                 value=value,
                                 id=current.indices.df$id[i])
        if (withTime) {
          current.fetch.df <- cbind(current.fetch.df, time=current.indices.df$time[i])
        }
  
        fetch.df <- rbind(fetch.df, current.fetch.df)
        
        if (verbose) {
          fetch.current <- fetch.current + 1
          setTxtProgressBar(progress.bar, fetch.current)
        }
      }  
    } else {
      #points in a cluster    
      start.array[longitude.dim.index] <- min(current.indices.df$lon)
      count.array[longitude.dim.index] <- max(current.indices.df$lon) - min(current.indices.df$lon) + 1
      start.array[latitude.dim.index] <- min(current.indices.df$lat)
      count.array[latitude.dim.index] <- max(current.indices.df$lat) - min(current.indices.df$lat) + 1
      if (withTime) {
        start.array[time.dim.index] <- min(current.indices.df$time)
        count.array[time.dim.index] <- max(current.indices.df$time) - min(current.indices.df$time) + 1
      }
      
#print(paste("start.array", paste(start.array, collapse = " ")))
#print(paste("count.array", paste(count.array, collapse = " ")))
      #read data from netCDF
      values <- get.var.ncdf(nc=nc, varid=netcdf.var$name, start=start.array, count=count.array)
      
      values.array <- array(values, dim=count.array)
      for (i in 1:nrow(current.indices.df)) {      
        indices.array <- rep(1, length(netcdf.var$dim))
        indices.array[longitude.dim.index] <- current.indices.df$lon[i] - min(current.indices.df$lon) + 1
        indices.array[latitude.dim.index] <- current.indices.df$lat[i] - min(current.indices.df$lat) + 1
        if (withTime) {
          indices.array[time.dim.index] <- current.indices.df$time[i] - min(current.indices.df$time) + 1
        }
        
        #build the result DF
        current.fetch.df <- data.frame(lon=current.indices.df$lon[i],
                                 lat=current.indices.df$lat[i],
                                 value=values.array[matrix(indices.array,1)],
                                 id=current.indices.df$id[i])
        if (withTime) {
          current.fetch.df <- cbind(current.fetch.df, time=current.indices.df$time[i])
        }
        fetch.df <- rbind(fetch.df, current.fetch.df)      
      }
      
      if (verbose) {
        fetch.current <- fetch.current + 1
        setTxtProgressBar(progress.bar, fetch.current)
      }
    }    
  }
  
  if (verbose) {
    cat("\nFormating result\n")
  }
  
  #we format correctly the output
  result.df <- data.frame(value=rep(NA, length(x)), lon=rep(NA, length(x)), lat=rep(NA, length(x)))
  if (withTime) {
    result.df <- cbind(result.df, time=as.POSIXct(rep(NA, length(x))))
  }
  for (i in 1:nrow(fetch.df)) {
    result.df[which(unique.id == fetch.df[i,]$id),]$value <- fetch.df[i,]$value
    result.df[which(unique.id == fetch.df[i,]$id),]$lon <- longitude.var.values[fetch.df[i,]$lon]
    result.df[which(unique.id == fetch.df[i,]$id),]$lat <- latitude.var.values[fetch.df[i,]$lat]
    if (withTime) {
      result.df[which(unique.id == fetch.df[i,]$id),]$time <- time.var.realValues[fetch.df[i,]$time]
    }
  }
  #remplacing missval with NA
  if ("missval" %in% names(netcdf.var) && 
        ! is.na(netcdf.var$missval) && 
        length(result.df[! is.na(result.df$value) & result.df$value == netcdf.var$missval, ]$value) > 0) {
    result.df[! is.na(result.df$value) & result.df$value == netcdf.var$missval, ]$value <- NA
  }
  
  names(result.df)[1] <- varName
  names(result.df)[2] <- paste(varName,"_longitude", sep="")
  names(result.df)[3] <- paste(varName,"_latitude", sep="")
  if (withTime) {
    names(result.df)[4] <- paste(varName,"_time", sep="")
  }

  if (inherits(data, what="data.frame"))  {
    data <- cbind(data, result.df)    
  } else {
    if (inherits(data, what="SpatialPointsDataFrame")) {
      data@data <- cbind(data@data, result.df)    
    }    
  }
  return(data)
}

