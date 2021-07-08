
Atlas_i4_SpeciesMonthByOcean <- function(df, 
                                         oceanAttributeName="ocean",
                                         yearAttributeName="year", 
                                         monthAttributeName="month", 
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
  if(sum(names(df) == oceanAttributeName) == 0) {
    stop("Cannot found ocean attribute")
  }
  
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot found year attribute")
  }
  
  if(sum(names(df) == monthAttributeName) == 0) {
    stop("Cannot found month attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot found value attribute")
  }  
  
  #format columns
  df[, oceanAttributeName] <- as.factor(df[, oceanAttributeName])
  df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  df[, monthAttributeName] <- as.numeric(df[, monthAttributeName])
  df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  df[, valueAttributeName] <- as.numeric(df[, valueAttributeName])    
  
  #aggregate to cut other columns
  df <- aggregate(x=df[, valueAttributeName], 
                  by=list(df[, oceanAttributeName], df[, yearAttributeName], df[, monthAttributeName], df[, speciesAttributeName]), 
                  FUN=sum)
  #rename columns
  names(df) <- c("ocean", "year", "month", "species", "value")
  
  #setup month factor
  df$month <- factor(df$month, labels=month.name)
  
  #test if FAO usual gear codes are used
  if (length(intersect(levels(df$ocean), c("ATL", "IND", "PAC_E", "PAC_W"))) == length(levels(df$ocean))) {
    df$ocean <- factor(df$ocean, levels=c("ATL", "IND", "PAC_E", "PAC_W"), labels=c("Atlantic O.", "Indian O.", "East Pacific O.", "West Pacific O."))
  }
  
  #setup the palette
  my.colors <- brewer.pal(length(levels(df$ocean)), "Set1")
  names(my.colors) <- levels(df$ocean)  
  
  #define the resulr df  
  result.df <- c()
  
  for (species.current in unique(df$species)) {
    
    species.label <- species.current
    species.URI <- species.current
    
    
    species.df <- df[df$species == species.current,]
    
    #compute the decade
    species.df$decade <- species.df$year - (species.df$year %% 10)
    decades.factor <- sort(unique(species.df$decade))
    
    species.df$decade <- factor(species.df$decade, 
                                levels=decades.factor,
                                labels=unlist(lapply(X=decades.factor, FUN=function(dec) paste(min(species.df[species.df$decade == dec,]$year), "-", max(species.df[species.df$decade == dec,]$year), sep=""))))
    for (decade.current in unique(species.df$decade)) {
      current.df <- species.df[species.df$decade == decade.current,]
      
      #aggregate values by years and month
      valuesSum <- aggregate(value ~ year + month, data=current.df, FUN=sum)
      names(valuesSum) <- c("year", "month", "valuesSum")
      mergedDf <- merge(current.df, valuesSum)
      
      #build the plot
      #pie plot
      resultPlot <- ggplot(data=mergedDf, mapping=aes(x=valuesSum/2, fill=ocean, y=value, width=valuesSum)) + 
        facet_grid(facets=year ~ month) + 
        geom_bar(aes(order = ocean), position="fill", stat="identity") + 
        scale_fill_manual(name="Ocean", values=my.colors) +
        coord_polar(theta="y") + 
        theme(axis.text.y=element_text(size=6), axis.text.x=element_blank(), panel.grid.minor=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + 
        labs(title=paste(species.label, "monthly catches by ocean for", decade.current))
      
      #bar plot
      #resultPlot <- ggplot(data=mergedDf) + facet_grid(facets=year ~ month) + geom_bar(mapping=aes(x=ocean, y=value, fill=ocean), stat="identity") + theme(axis.text.x=element_blank(), panel.grid.minor=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + labs(title="Montly catches by ocean")
      
      #draw the plot
      tempfile.base <- tempfile(pattern=paste("I4_", gsub(" ", "_", species.label), "_", as.character(decade.current), "_", sep=""))
      plot.filepath <- paste(tempfile.base, ".png", sep="")
      ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)
      
      
    }
    
    #if multiple decade we produce a graph by decade
    if (length(unique(species.df$decade)) > 1) {
      #aggregate values by decade and month
      valuesSum <- aggregate(value ~ decade + month, data=species.df, FUN=sum)
      names(valuesSum) <- c("decade", "month", "valuesSum")
      values <- aggregate(value ~ decade + month + ocean, data=species.df, FUN=sum)
      mergedDf <- merge(values, valuesSum)
      
      #build the plot
      #pie plot
      resultPlot <- ggplot(data=mergedDf, mapping=aes(x=valuesSum/2, fill=ocean, y=value, width=valuesSum)) + 
        facet_grid(facets=decade ~ month) + 
        geom_bar(aes(order = ocean), position="fill", stat="identity") + 
        scale_fill_manual(name="Ocean", values=my.colors) +
        coord_polar(theta="y") + 
        theme(axis.text.y=element_text(size=6), axis.text.x=element_blank(), panel.grid.minor=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) + 
        labs(title=paste(species.label, "monthly catches by ocean and by decade"))
      
      #draw the plot
      tempfile.base <- tempfile(pattern=paste("I4_", gsub(" ", "_", species.current), "_byDecade_", sep=""))
      #plot_file_path <- paste(tempfile.base, ".png", sep="")
      plot.filepath <- paste(tempfile.base, ".png", sep="")
      ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)
      
      
    }
  }
  
  return(resultPlot)
}
