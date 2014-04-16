
# Atlas_i11_CatchesByCountry.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a map of 5° degrees squares catches relative part by countries. An associated RDF file is also produced.
##################################################################
# Norbert Billet - IRD
# 2013/11/04: Norbert - Add RDF export and allow production of multiple maps (i.e. species)
# 2013/09/03: Norbert - Add attributes names as parameters
# 2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
# 2013/07/07: Norbert - First version
##################################################################
# Use example:
#  library(IRDTunaAtlas)
#  library(rgdal)
#  inputFilePath <- "/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i6i7i8.shp"
#  layerName <- ogrListLayers(inputFilePath)[1]
#  sp.df <- readOGR(dsn=inputFilePath, layer=layerName, disambiguateFIDs=TRUE)
#  #sp.df <- sp.df[sp.df$species == "MLS" & sp.df$year == 1990,]
#  Atlas_i11_CatchesByCountry(sp.df, 
#                        geomIdAttributeName="geom_id",
#                        countryAttributeName="country",
#                        speciesAttributeName="species",
#                        valueAttributeName="value")
##################################################################

Atlas_i11_CatchesByCountry <- function(df,
                                       geomIdAttributeName="geom_id",
                                       countryAttributeName="country", 
                                       speciesAttributeName="species",                                         
                                       valueAttributeName="value",
                                       withSparql=TRUE)
{
  sizeX <- 1200
  sizeY <- 600
    
  #check inputs
  if (class(df) != "SpatialPolygonsDataFrame")
  {
    stop(paste("Bad geometrical feature type, must be a SpatialPolygonsDataFrame"))
  }
  
  if(sum(names(df) == geomIdAttributeName) == 0) {
    stop("Cannot found geom id attribute")
  }
  
  if(sum(names(df) == countryAttributeName) == 0) {
    stop("Cannot found country attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot found value attribute")
  }
  
  #format columns
  df@data[, geomIdAttributeName] <- as.character(df@data[, geomIdAttributeName])
  df@data[, countryAttributeName] <- as.factor(df@data[, countryAttributeName])
  df@data[, speciesAttributeName] <- as.factor(df@data[, speciesAttributeName])
  df@data[, valueAttributeName] <- as.numeric(df@data[, valueAttributeName])
  
  #rename columns
  names(df)[which(names(df) == geomIdAttributeName)] <- "geom_id"
  names(df)[which(names(df) == countryAttributeName)] <- "country"  
  names(df)[which(names(df) == speciesAttributeName)] <- "species"
  names(df)[which(names(df) == valueAttributeName)] <- "value"
      
  
  #if more than X countries we don't know how to clearly represent, so we drop
  countriesMax <- 7
  #if country X catches represent less than Y we drop it
  countriesMinthreshold <- 0.01  
  
  draw.pieNono <- function (x, y, z, radius, scale = T, labels = NA, silent = TRUE, ...) 
  {
    nx <- length(x)
    nz <- dim(z)[2]
    if (length(y) != nx) 
      stop("x and y should be vectors of the same length")
    if (length(dim(z)) != 2) 
      stop("z should be a 2-dimensional array")
    if (dim(z)[1] != nx) 
      stop("the number of rows in of z should match as the length of x and y")
    if (sum(z, na.rm = T) == 0) 
      stop("z has no data")
    maxsumz <- max(rowSums(z, na.rm=T), na.rm = T)
    pm <- setProgressMsg(1, nx)
    for (i in 1:nx) {
      xi <- x[i]
      yi <- y[i]
      zi <- z[i, ]
      zi <- ifelse(is.na(zi), 0, zi)
      if (length(radius) > 1) 
        radiusi <- radius[i]
      else radiusi = radius
      if (scale & length(radius) == 1) 
        radiusi <- radius * sqrt(sum(zi, na.rm = T))/sqrt(maxsumz)
      if (sum(zi) > 0) 
        add.pie(zi, xi, yi, labels, radius = radiusi, ...)
      if (!silent) 
        pm <- progressMsg(pm, i)
    }
  }
  
  #load the GSHHS C L1 sp data (coatsline)
  data(gshhs_c_L1)

  #define the result df  
  result.df <- c()
  
  for (species.current in unique(df$species)) {
    
    if (withSparql) {      
      #get species scientific name from ecoscope sparql
      sparqlResult <- getSpeciesFromEcoscope(as.character(species.current))
      if (length(sparqlResult) > 0) {
        species.label <- sparqlResult[1,"scientific_name"]
        species.URI <- sparqlResult[1,"uri"]
      } else {
        species.label <- species.current
        species.URI <- species.current
      } 
    } else {
      species.label <- species.current
      species.URI <- species.current
    }
    
    species.df <- df[df$species == species.current,]
  
    #compute sum per country
    sumPerCountry <- aggregate(value ~ country, data=species.df, FUN=sum)
    sumPerCountry$relative <- sumPerCountry$value / sum(sumPerCountry$value)
    #is there more than one country with catches part < countriesMinthreshold ?
    if (sum(sumPerCountry$relative < countriesMinthreshold) > 1) {
      #yes, we build the "other" group
      species.df$country <- as.character(species.df$country)
      species.df@data[(species.df$country %in% sumPerCountry$country[sumPerCountry$relative < countriesMinthreshold]),]$country <- "Other"
    }
    #is there more than countriesMax country ?
    if (length(unique(species.df$country)) > countriesMax) {
      sumPerCountry <- aggregate(value ~ country, data=species.df, FUN=sum)
      topCountries <- sumPerCountry[order(sumPerCountry$value, decreasing = TRUE), ]$country[1:countriesMax]
      species.df$country <- as.character(species.df$country)
      species.df@data[! (species.df$country %in% topCountries),]$country <- "Other"
    }
    
    species.df$country <- as.factor(species.df$country)
    
    #aggregate data to cut extra columns
    df.data.agg <- aggregate(value ~ geom_id + country + species, data=species.df, FUN=sum)
    srl <- species.df@polygons[match(paste(df.data.agg$geom_id, df.data.agg$country, df.data.agg$species), 
                             paste(species.df@data$geom_id, species.df@data$country, species.df@data$species))]
    aggSp <- SpatialPolygons(srl, proj4string=CRS(proj4string(species.df)))
    species.df <- SpatialPolygonsDataFrame(Sr=aggSp, data=df.data.agg, match.ID=FALSE)
        
    #build the palette
    mypalette <- brewer.pal(length(unique(species.df$country)), "Set1")
    
    bb <- bbox(species.df)
    
    if (missing(sizeX)) {    
      if (diff(bb[1,]) < diff(bb[2,])) {      
        sizeX <- ceiling(sizeY * diff(bb[1,]) / diff(bb[2,]) * 1.25)
      }
    }  
    
    if (missing(sizeY)) {    
      if(diff(bb[1,]) > diff(bb[2,])) {
        sizeY <- ceiling(sizeX * diff(bb[2,]) / diff(bb[1,]) * 1.25) 
      }
    }
    
    base_temp_file <- tempfile(pattern=paste("I11_", gsub(" ", "_", species.label), sep=""))
    plot_file_path <- paste(base_temp_file, ".png", sep="")
  
    png(plot_file_path, width=sizeX, height=sizeY)

    #base map  
    basemap(xlim=bb[1,], ylim=bb[2,], main=paste(species.label, "catches per country"), xlab=NA, ylab=NA, bg=NA)
    
    #add coastline layer
    #map("worldHires", fill=TRUE, col="grey", boundary=FALSE, interior=FALSE, bg=NA, border="grey", add=TRUE)
    plot(gshhs_c_L1, add=T, col="grey85", border=NA)
    
    #add squares
    plot(species.df[match(unique(species.df$geom_id), species.df$geom_id), ], add=TRUE, col=NA, border="grey75")
      
    #pies
    Z <- tapply(X=species.df$value, INDEX=list(paste(coordinates(species.df)[,1], coordinates(species.df)[,2], sep = "; "), species.df$country), FUN=sum, na.rm=TRUE)
    
    XY <- rownames(Z)
    tempfun <- function(XY, i) {
      as.numeric(gsub(",", ".", unlist(lapply(strsplit(XY, "; "), function(x) x[i]))))
    }
    X <- tempfun(XY, 1)
    Y <- tempfun(XY, 2)
    
    draw.pieNono(X, Y, Z, radius=2.5, col=mypalette)
    
    #legend
    legend(x=mean(bb[1,]), y=par()$usr[4], legend=unique(species.df$country), fill=mypalette, border=NA, cex=0.75, xjust=0.5, yjust=1, horiz=TRUE, bg="white", bty="o")
    
    #reference pie size
    maxsumz <- ceiling(max(rowSums(Z, na.rm=T), na.rm = T))  
    add.pie(z=maxsumz, x=par()$usr[1]+5, y=par()$usr[3]+5, labels=paste("[", maxsumz, " tons]", sep=""), radius=2.5, col="black", label.dist=1.25, init.angle=180)
    
    dev.off()
    
    #create the RDF metadata
    rdf_file_path <- paste(base_temp_file, ".rdf", sep="")
    buildRdf(rdf_file_path=paste(base_temp_file, ".rdf", sep=""),
             rdf_subject="http://ecoscope.org/indicatorI11", 
             titles=c("IRD Tuna Atlas: indicator #11 - Map of catches", 
                      "IRD Atlas thonier : indicateur #11 - Carte des captures"),
             descriptions=c(paste(species.label, "catches map"), 
                            paste("Carte des captures de", species.label)),
             subjects=c(species.label),
             processes="&localfile;/processI11",
             spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))",
             withSparql)
    
    result.df <- rbind(result.df, c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
  }
  
  return(result.df)
    
}

