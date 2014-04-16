
# Atlas_i7_SpeciesMapRelativeCatches.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a map of 5° degrees squares catches percent of total catches for the species. An associated RDF file is also produced.
##################################################################
# Norbert Billet - IRD
# 2013/11/04: Norbert - Add RDF export and allow production of multiple maps (i.e. species)
# 2013/09/03: Norbert - Add attributes names as parameters
# 2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
# 2013/05/28: Norbert - First version
##################################################################
# Use example:
#  library(IRDTunaAtlas)
#  library(rgdal)
#  inputFilePath <- "/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i6i7i8.shp"
#  layerName <- ogrListLayers(inputFilePath)[1]
#  sp.df <- readOGR(dsn=inputFilePath, layer=layerName, disambiguateFIDs=TRUE)
#  sp.df <- sp.df[sp.df$species == "MLS" & sp.df$year == 1990,]
#  Atlas_i7_SpeciesMapRelativeCatches(sp.df, 
#                        geomIdAttributeName="geom_id",
#                        yearAttributeName="year",
#                        speciesAttributeName="species",
#                        valueAttributeName="value")
##################################################################
Atlas_i7_SpeciesMapRelativeCatches <- function(df,
                                               geomIdAttributeName="geom_id",
                                               yearAttributeName="year", 
                                               speciesAttributeName="species",                                         
                                               valueAttributeName="value",
                                               withSparql=TRUE)
{

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
    #transform value of each square in percentage of total catches value
    aggData$value <- aggData$value / sum(aggData$value) * 100
    #create a spatial df object from
    aggSp <- SpatialPolygons(species.df[match(aggData$geom_id, species.df$geom_id),]@polygons, proj4string=CRS("+init=epsg:4326"))
    aggSpdf <- SpatialPolygonsDataFrame(Sr=aggSp, data=aggData, match.ID=FALSE)
    
    names(aggSpdf)[names(aggSpdf) == "geom_id"] <- "id"
    aggSpdf.fortify <- fortify(aggSpdf, region="id")
    aggSpdf.df <- join(aggSpdf.fortify, aggSpdf@data, by="id")
    
    world.df <-  fortify(maps::map("world", plot = FALSE, fill = TRUE))
    
    if (missing(lims)) {
      lims <- range(aggSpdf.df$value)
    }
    
    if (min(subDf$year) == max(subDf$year)) {
      my.title <- paste(species.label , " catches 5x5° square contribution / total catches for ",  min(subDf$year), sep="")
    } else {
      my.title <- paste(species.label , " catches 5x5° square contribution / total catches for ",  min(subDf$year), "-",  max(subDf$year), sep="")
    }
    
    resultPlot <- ggplot() +
      geom_polygon(data=aggSpdf.df, mapping=aes(x = long, y = lat, fill=value, group=group)) +
      scale_fill_continuous(low="yellow", high="blue", na.value="grey25", name="Contribution (%)", limits=lims, 
                            guide=guide_colourbar(direction="horizontal", 
                                                  title.position="top",
                                                  label.position="bottom",
                                                  barwidth=20)) +
      geom_path(data=world.df, mapping=aes(x=long, y=lat, group=group), colour="grey25") +
      coord_equal() +
      theme(legend.position="bottom", axis.title.x=element_blank(), axis.title.y=element_blank()) +
      labs(title=my.title)
    
    #draw the plot
    base_temp_file <- tempfile(pattern=paste("I7_", gsub(" ", "_", species.label), "_", as.character(min(subDf$year)), "-", as.character(max(subDf$year)), "_", sep=""))
    plot_file_path <- paste(base_temp_file, ".png", sep="")
    ggsave(filename=plot_file_path, plot=resultPlot, dpi=100)
    
    #create the RDF metadata
    rdf_file_path <- paste(base_temp_file, ".rdf", sep="")
    buildRdf(rdf_file_path=paste(base_temp_file, ".rdf", sep=""),
             rdf_subject="http://ecoscope.org/indicatorI7", 
             titles=c("IRD Tuna Atlas: indicator #7 - Map of contribution of catches", 
                      "IRD Atlas thonier : indicateur #7 - Carte des contribution aux captures"),
             descriptions=c(paste(species.label, "contribution catches map"), 
                            paste("Carte des contributions aux captures de", species.label)),
             subjects=c(species.label),
             processes="&localfile;/processI7",
             start=as.character(min(subDf$year)),
             end=as.character(max(subDf$year)),
             spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))",
             withSparql)
    
    return(c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
  }
  
  #define the resulr df  
  result.df <- c()
  
  
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
        
    if (length(unique(species.df$year)) > 1)
    {
      #for each year
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