
Atlas_i1_SpeciesByOcean <- function(df, 
                                    yearAttributeName="year", 
                                    oceanAttributeName="ocean", 
                                    speciesAttributeName="species",
                                    valueAttributeName="value",
                                    withSparql=FALSE)
{
  if (! require(XML) | ! require(ggplot2) | ! require(RColorBrewer)) {
    stop("Missing library")
  }
  
  if (missing(df)) {
    stop("Input data frame not specified")
  }
  
  #check for input attributes
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot found year attribute")
  }
  
  if(sum(names(df) == oceanAttributeName) == 0) {
    stop("Cannot found ocean attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot found value attribute")
  }  
  
  #format columns  
  df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  df[, oceanAttributeName] <- as.factor(df[, oceanAttributeName])
  df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  df[, valueAttributeName] <- as.numeric(df[, valueAttributeName])    
  
  #aggregate to cut other columns
  df <- aggregate(x=df[, valueAttributeName], 
                  by=list(df[, yearAttributeName], df[, oceanAttributeName], df[, speciesAttributeName]), 
                  FUN=sum)
  #rename columns
  names(df) <- c("year", "ocean", "species", "value")
  
  #define the result df  
  result.df <- c()
  
  #test if FAO usual ocean codes are used
  if (length(intersect(levels(df$ocean), c("ATL", "IND", "PAC_E", "PAC_W"))) == length(levels(df$ocean))) {
    df$ocean <- factor(df$ocean, levels=c("ATL", "IND", "PAC_E", "PAC_W"), labels=c("Atlantic O.", "Indian O.", "East Pacific O.", "West Pacific O."))
  }
  
  #setup the palette
  my.colors <- brewer.pal(length(levels(df$ocean)), "Set1")
  names(my.colors) <- levels(df$ocean)
  
  #TODO : mcforeach ?
  for (species.current in unique(df$species)) {
    current.df <- df[df$species == species.current,]
    
    #aggregate values by years and ocean
    aggData <- aggregate(value ~ ocean + year, data=current.df, sum)
    
    #keep only common time extent
    max_year <- min(unlist(lapply(levels(aggData$ocean), function(o) {return(if(length(subset(aggData, ocean==o)$year) > 0) max(subset(aggData, ocean==o)$year) else NA)})), na.rm=TRUE)
    min_year <- max(unlist(lapply(levels(aggData$ocean), function(o) {return(if(length(subset(aggData, ocean==o)$year) > 0) min(subset(aggData, ocean==o)$year) else NA)})), na.rm=TRUE)
    aggData <- subset(aggData, year >= min_year & year <= max_year)
    
    #convert values from tons to thousand tons
    aggData$value <- aggData$value / 1000
    
    
    species.label <- species.current
    species.URI <- species.current
    
    #build the plot
    resultPlot <- ggplot(aggData, aes(x=year, y=value, group=ocean)) + 
      geom_area(aes(fill=ocean), position="stack") + 
      geom_line(position="stack", color="grey20") + 
      scale_fill_manual(name="Ocean", values=my.colors) +
      xlab("Year") + ylab("Catches in thousand tons") + 
      ggtitle(paste(species.label, "catches by Ocean")) +
      theme(legend.position="bottom")
    
    # Turn it interactive with ggplotly
    resultPlot<- ggplotly(resultPlot)
    
    # resultPlot <- plot_ly(x = aggData$year, y = aggData$value, type="scatter", mode="markers", fill = "tozeroy")
    # resultPlot <- add_trace(p, x = year, y = var3, type="scatter", mode="markers", fill = "tonexty")
    
  }
  return(resultPlot)
}
