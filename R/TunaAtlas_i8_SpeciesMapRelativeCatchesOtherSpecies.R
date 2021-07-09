Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies <- function(df, targetedSpecies,
                                                           geomIdAttributeName="geom_id",
                                                           yearAttributeName="year", 
                                                           speciesAttributeName="species",                                         
                                                           valueAttributeName="value",
                                                           withSparql=FALSE)
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
    # ggsave(filename=plot_file_path, plot=resultPlot, dpi=100)
    
    # return(c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
    return(resultPlot)
  }
  
  species.label <- targetedSpecies
  species.URI <- targetedSpecies
  
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
  # resultPlot<- ggplotly(result.plot.df)
  resultPlot<- result.plot.df
  return(resultPlot)  
  return()    
}