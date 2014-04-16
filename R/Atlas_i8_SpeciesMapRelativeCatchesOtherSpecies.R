
# Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a map of 5° degrees squares catches of selected species percent of total catches for the all species. An associated RDF file is also produced.
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
#  sp.df <- sp.df[sp.df$year >= 1980 & sp.df$year <= 2000,]
#  Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies(sp.df, "MLS",
#                        geomIdAttributeName="geom_id",
#                        yearAttributeName="year",
#                        speciesAttributeName="species",
#                        valueAttributeName="value")
##################################################################
Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies <- function(df, targetedSpecies,
                                                           geomIdAttributeName="geom_id",
                                                           yearAttributeName="year", 
                                                           speciesAttributeName="species",                                         
                                                           valueAttributeName="value",
                                                           withSparql=TRUE)
{
  require(maps)
  
  if (missing(targetedSpecies)) {
    stop("Missing target species")
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
    
  #check if there is only one species
  if (length(unique(df@data[, speciesAttributeName])) < 2) {
    stop("Less than two species found in input data")
  }
  
  #check if targeted species is in the data
  if (all(is.na(match(unique(df@data[, speciesAttributeName]), targetedSpecies)))) {
    stop(paste("Targeted species not found in the dataset"))
  }
    
  plotFct <- function(subDf, species.targeted, species.label, lims) {
    
    #aggregate values by 5° CWP square and species
    aggData <- aggregate(value ~ geom_id + species, data=subDf, sum)    
    #aggregate values by 5° CWP square only
    aggDataAllSpecies <- aggregate(value ~ geom_id, data=aggData, sum)          
    #merge data
    aggData <- merge(aggData, aggDataAllSpecies, by="geom_id")    
    #keep only data for the selected species
    aggData <- aggData[aggData$species == species.targeted, ]    
    #transform value of each square in percentage of total catches value
    aggData$value <- aggData$value.x / aggData$value.y * 100    
    #create a spatial df object from
    aggSp <- SpatialPolygons(subDf[match(aggData$geom_id, subDf$geom_id),]@polygons, proj4string=CRS("+init=epsg:4326"))
    aggSpdf <- SpatialPolygonsDataFrame(Sr=aggSp, data=aggData, match.ID=FALSE)    
    
    names(aggSpdf)[names(aggSpdf) == "geom_id"] <- "id"
    aggSpdf.fortify <- fortify(aggSpdf, region="id")
    aggSpdf.df <- join(aggSpdf.fortify, aggSpdf@data, by="id")
    
    world.df <-  fortify(maps::map("world", plot = FALSE, fill = TRUE))
    
    if (missing(lims)) {
      lims <- range(aggSpdf.df$value)
    }
    
    if (min(subDf$year) == max(subDf$year)) {
      my.title <- paste(species.label , " Catches 5x5° contribution / all species for ",  min(subDf$year), sep="")
    } else {
      my.title <- paste(species.label , " Catches 5x5° contribution / all species for ",  min(subDf$year), "-",  max(subDf$year), sep="")
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
    base_temp_file <- tempfile(pattern=paste("I8_", gsub(" ", "_", species.label), "_", as.character(min(subDf$year)), "-", as.character(max(subDf$year)), "_", sep=""))
    plot_file_path <- paste(base_temp_file, ".png", sep="")
    ggsave(filename=plot_file_path, plot=resultPlot, dpi=100)
    
    #create the RDF metadata
    rdf_file_path <- paste(base_temp_file, ".rdf", sep="")
    buildRdf(rdf_file_path=paste(base_temp_file, ".rdf", sep=""),
             rdf_subject="http://ecoscope.org/indicatorI8", 
             titles=c("IRD Tuna Atlas: indicator #8 - Map of contribution of catches in percent of catches for all species", 
                      "IRD Atlas thonier : indicateur #8 - Carte des contribution aux captures en pourcentage des captures pour toutes les espèces"),
             descriptions=c(paste(species.label, "contribution catches map"), 
                            paste("Carte des contributions aux captures de", species.label)),
             subjects=c(species.label),
             processes="&localfile;/processI8",
             start=as.character(min(subDf$year)),
             end=as.character(max(subDf$year)),
             spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))",
             withSparql)
    
    return(c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
  }
  
  if (withSparql) {      
    #get species scientific name from ecoscope sparql
    sparqlResult <- getSpeciesFromEcoscope(as.character(targetedSpecies))
    if (length(sparqlResult) > 0) {
      species.label <- sparqlResult[1,"scientific_name"]
      species.URI <- sparqlResult[1,"uri"]
    } else {
      species.label <- species
      species.URI <- species
    } 
  } else {
    species.label <- species
    species.URI <- species
  }
  
  #define the result df  
  result.df <- c()
  
  #plot for all the period
  result.plot.df <- plotFct(df, targetedSpecies, species.label)
  result.df <- rbind(result.df, result.plot.df)
  
  if (length(unique(df$year)) > 1)
  {
    #for each year
    for(year.current in unique(df$year)) {
      result.plot.df <- plotFct(df[df$year==year.current,], targetedSpecies, species.label)
      result.df <- rbind(result.df, result.plot.df)
    }
    
    #for each decade
    df$decade <- df$year - (df$year %% 10)
    if (length(unique(df$decade)) > 1)
    {
      for(decade.current in unique(df$decade)) {
        result.plot.df <- plotFct(df[df$decade==decade.current,], targetedSpecies, species.label)
        result.df <- rbind(result.df, result.plot.df)
      }
    }
  }
  
  return(result.df)    
}