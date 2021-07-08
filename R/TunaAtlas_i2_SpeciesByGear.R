

Atlas_i2_SpeciesByGear <- function(df, 
                                   yearAttributeName="year", 
                                   speciesAttributeName="species",
                                   gearTypeAttributeName="gear",
                                   valueAttributeName="value",
                                   withSparql=FALSE)
{
  if (! require(XML) | ! require(ggplot2) | ! require(RColorBrewer)) {
    stop("Missing library")
  }
  
  if (missing(df)) {
    stop("Input data frame not specified")
  }
  
  #check for input attributeshttps://github.com/juldebar/IRDTunaAtlas/wiki/Indicator-I2-:-Annual-catches-by-gear
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot found year attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
  
  if(sum(names(df) == gearTypeAttributeName) == 0) {
    stop("Cannot found gear attribute")
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot found value attribute")
  }  
  
  #format columns  
  df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  df[, gearTypeAttributeName] <- as.factor(df[, gearTypeAttributeName])
  df[, valueAttributeName] <- as.numeric(df[, valueAttributeName])    
  
  #aggregate to cut other columns
  df <- aggregate(x=df[, valueAttributeName], 
                  by=list(df[, yearAttributeName], df[, speciesAttributeName], df[, gearTypeAttributeName]), 
                  FUN=sum)
  #rename columns
  names(df) <- c("year", "species", "gear_type", "value")
  
  #test if FAO usual gear codes are used
  #if (length(intersect(levels(df$gear_type), c("BB", "GILL", "LL", "PS", "OTHER_I", "OTHER_A", "TROL", "TRAP"))) == length(levels(df$gear_type))) {
  #  df$gear_type <- factor(df$gear_type, levels=c("BB", "GILL", "LL", "PS", "OTHER_I", "OTHER_A", "TROL", "TRAP"), labels=c("Baitboat", "Gillnet", "Longline", "Purse seine", "Unclass. art. Indian O.", "Unclass. art. Atl. O.", "Trol.", "Trap"))
  #}
  
  #setup the palette
  my.colors <- brewer.pal(length(levels(df$gear_type)), "Set1")
  names(my.colors) <- levels(df$gear_type)
  
  #define the result
  result.df <- c()
  
  #TODO : mcforeach ?
  for (species.current in unique(df$species)) {
    current.df <- df[df$species == species.current,]
    
    #aggregate values by years and gear type
    aggData <- aggregate(value ~ gear_type + year, data=current.df, sum)
    
    #convert values from tons to thousand tons
    aggData$value <- aggData$value / 1000
    
    #order factors levels by value
    aggData$gear_type <- factor(aggData$gear_type, levels=rev(levels(reorder(aggData$gear_type, aggData$value))))
    
    
    species.label <- species.current
    species.URI <- species.current
    
    
    #build the plot
    resultPlot <- ggplot(aggData, aes(x=year, y=value, fill=gear_type, order=gear_type)) + 
      geom_bar(stat="identity", width=0.8) + 
      geom_bar(stat="identity", width=0.8, colour="grey20", show.legend=FALSE) + 
      scale_fill_manual(name="Gear type", values=my.colors) +
      xlab("Year") + ylab("Catches in thousand tons") + 
      ggtitle(paste(species.label, "catches by gear type")) +
      theme(legend.position="bottom")
    
  }
  
  return(resultPlot)
  
}
