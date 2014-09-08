# Atlas_i6_SpeciesMap.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a map of 5x5 degrees squares aggregated catches. An associated RDF file is also produced.
##################################################################
# Norbert Billet - IRD
# 2013/11/04: Norbert - Add RDF export and allow production of multiple maps (i.e. species)
# 2013/09/03: Norbert - Add attributes names as parameters
# 2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
# 2013/04/22: Norbert - First version
##################################################################
# Use example:
#  library(IRDTunaAtlas)
#  library(rgdal)
#  inputFilePath <- "/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i6i7i8.shp"
#  layerName <- ogrListLayers(inputFilePath)[1]
#  sp.df <- readOGR(dsn=inputFilePath, layer=layerName, disambiguateFIDs=TRUE)
#  sp.df <- sp.df[sp.df$species == "MLS" & sp.df$year == 1990,]
#  Atlas_i6_SpeciesMap(sp.df, 
#                        geomIdAttributeName="geom_id",
#                        yearAttributeName="year",
#                        speciesAttributeName="species",
#                        valueAttributeName="value")
##################################################################

Atlas_i6_SpeciesMap <- function(df,
                                geomIdAttributeName="geom_id",
                                yearAttributeName="year", 
                                speciesAttributeName="species",                                         
                                valueAttributeName="value",
                                withSparql=TRUE)
{
   if (! require(maps)) {
     stop("Missing library")
   }
  
  #check inputs
  if (class(df) != "SpatialPolygonsDataFrame")
  {
    stop(paste("Bad geometrical feature type, must be a SpatialPolygonsDataFrame"))
  }
    
  if(sum(names(df) == geomIdAttributeName) == 0) {
    stop("Cannot found geom id attribute")
  }
  
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot found year attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot found value attribute")
  }
  
  #format columns
  df@data[, geomIdAttributeName] <- as.character(df@data[, geomIdAttributeName])
  df@data[, yearAttributeName] <- as.numeric(df@data[, yearAttributeName])
  df@data[, speciesAttributeName] <- as.factor(df@data[, speciesAttributeName])
  df@data[, valueAttributeName] <- as.numeric(df@data[, valueAttributeName])

  #rename columns
  names(df)[which(names(df) == geomIdAttributeName)] <- "geom_id"
  names(df)[which(names(df) == yearAttributeName)] <- "year"  
  names(df)[which(names(df) == speciesAttributeName)] <- "species"
  names(df)[which(names(df) == valueAttributeName)] <- "value"


  plotFct <- function(subDf, species.label, lims) {
    #aggregate values by 5° CWP square
    aggData <- aggregate(value ~ geom_id, data=subDf, sum)      

    #create a spatial df object from
    aggSp <- SpatialPolygons(species.df[match(aggData$geom_id, species.df$geom_id),]@polygons, proj4string=CRS("+init=epsg:4326"))
    aggSpdf <- SpatialPolygonsDataFrame(Sr=aggSp, data=aggData, match.ID=FALSE)
    
    names(aggSpdf)[names(aggSpdf) == "geom_id"] <- "id"
    aggSpdf.fortify <- fortify(aggSpdf, region="id")
    aggSpdf.df <- join(aggSpdf.fortify, aggSpdf@data, by="id")
    
    world.df <-  fortify(maps::map("world", plot = FALSE, fill = TRUE))
    
    if (missing(lims)) {
      lims <- range(aggSpdf.df$value, na.rm=TRUE)
    }
    
    if (min(subDf$year) == max(subDf$year)) {
      my.title <- paste(species.label , " catches 5x5° for ",  min(subDf$year), sep="")
    } else {
      my.title <- paste(species.label , " catches 5x5° for ",  min(subDf$year), "-",  max(subDf$year), sep="")
    }
    
    resultPlot <- ggplot() +
      geom_polygon(data=aggSpdf.df, mapping=aes(x = long, y = lat, fill=value, group=group)) +
      scale_fill_continuous(low="yellow", high="blue", na.value="grey25", name="Catches in k. tons", limits=lims, 
                          guide=guide_colourbar(direction="horizontal", 
                                             title.position="top",
                                             label.position="bottom",
                                              barwidth=20)) +
      geom_path(data=world.df, mapping=aes(x=long, y=lat, group=group), colour="grey25") +
      coord_equal() +
      theme(legend.position="bottom", axis.title.x=element_blank(), axis.title.y=element_blank()) +
      labs(title=my.title)
    
    #draw the plot
    tempfile.base <- tempfile(pattern=paste("I6_", gsub(" ", "_", species.label), "_", as.character(min(subDf$year)), "_to_", as.character(max(subDf$year)), "_", sep=""))
    plot.filepath <- paste(tempfile.base, ".png", sep="")
    ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)
    
    #create the RDF metadata
    rdf_file_path <- paste(tempfile.base, ".rdf", sep="")
    buildRdf(rdf_file_path=paste(tempfile.base, ".rdf", sep=""),
             rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""),               
             titles=c("IRD Tuna Atlas: indicator #6 - Map of catches", 
                      "IRD Atlas thonier : indicateur #6 - Carte des captures"),
             descriptions=c(paste(species.label, "catches map"), 
                            paste("Carte des captures de", species.label)),
             subjects=c(as.character(species.current)),
             #subjects=c(species.label),
             processes="http://www.ecoscope.org/ontologies/resources/processI4",
             data_output_identifier=plot.filepath,  
             start=as.character(min(subDf$year)),
             end=as.character(max(subDf$year)),
             spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))",
             withSparql)
    
    return(c(plot.file.path=plot.filepath, rdf.file.path=rdf_file_path))
  }
  
  #define the resulr df  
  result.df <- c()
  
  #convert values from tons to thousand tons
  df$value <- df$value / 1000
  
  #fisrt subset by species
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
    
    #plot for all the period
    result.plot.df <- plotFct(species.df, species.label)
    result.df <- rbind(result.df, result.plot.df)
    
    #for each year
    if (length(unique(species.df$year)) > 1)
    {
      for(year.current in unique(species.df$year)) {
        result.plot.df <- plotFct(species.df[species.df$year==year.current,], species.label)
        result.df <- rbind(result.df, result.plot.df)
      }
        
      #for each decade
      species.df$decade <- species.df$year - (species.df$year %% 10)
      if (length(unique(species.df$decade)) > 1)
      {
        for(decade.current in unique(species.df$decade)) {
         result.plot.df <- plotFct(species.df[species.df$decade==decade.current,], species.label)
         result.df <- rbind(result.df, result.plot.df)
        }
      }
    }
  }

  return(result.df)
}
