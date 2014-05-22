
# Atlas_i10_RelativeSizeFrequenciesByDecade.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a graph relative contribution of size frequencies in catches for a species by decade. An associated RDF file is also produced.
##################################################################
# Norbert Billet - IRD
# 2013/11/04: Norbert - Add RDF export and allow production of multiple maps (i.e. species)
# 2013/09/03: Norbert - Add attributes names as parameters
# 2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
# 2013/06/14: Norbert - First version
##################################################################
# Use example:
#  library(IRDTunaAtlas)
#  csv.df <- read.csv("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i9i10.csv", stringsAsFactors=FALSE)
#  #csv.df <- csv.df[csv.df$species == "ALB",]
#  Atlas_i10_RelativeSizeFrequenciesByDecade(csv.df, temporalAgg=5,
#                        yearAttributeName="year",
#                        speciesAttributeName="species",
#                        sizeClassLowerBoundAttributeName="class_low",
#                        sizeClassUpperBoundAttributeName="class_up",
#                        fishCountAttributeName="fish_count")
##################################################################

Atlas_i10_RelativeSizeFrequenciesByDecade <- function(df, temporalAgg=10,
                                                      yearAttributeName="ns0:year",
                                                      speciesAttributeName="ns0:species",
                                                      sizeClassLowerBoundAttributeName="ns0:class_low",
                                                      sizeClassUpperBoundAttributeName="ns0:class_up",
                                                      fishCountAttributeName="ns0:fish_count")
{  
  if (! require(ggplot2) | ! require(RColorBrewer)) {
    stop("Missing library")
  }
  
  if (missing(df)) {
    stop("Input data frame not specified")
  }
  
  if (temporalAgg < 2) {
    stop("Invalid parameter value for temporalAgg, must be > 1")
  }
  
  #check for input attributes
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot found year attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
    
  if(sum(names(df) == sizeClassLowerBoundAttributeName) == 0) {
    stop("Cannot found size class lower bound attribute")
  }  
  
  if(sum(names(df) == sizeClassUpperBoundAttributeName) == 0) {
    stop("Cannot found size class upper bound attribute")
  }
  
  if(sum(names(df) == fishCountAttributeName) == 0) {
    stop("Cannot found fish count attribute")
  }  
  
  #format columns
  df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  df[, sizeClassLowerBoundAttributeName] <- as.numeric(df[, sizeClassLowerBoundAttributeName])    
  df[, sizeClassUpperBoundAttributeName] <- as.numeric(df[, sizeClassUpperBoundAttributeName])    
  df[, fishCountAttributeName] <- as.numeric(df[, fishCountAttributeName])    
  
  #rename columns
  names(df)[which(names(df) == yearAttributeName)] <- "year"  
  names(df)[which(names(df) == speciesAttributeName)] <- "species"
  names(df)[which(names(df) == sizeClassLowerBoundAttributeName)] <- "sizeClassLowerBound"
  names(df)[which(names(df) == sizeClassUpperBoundAttributeName)] <- "sizeClassUpperBound"
  names(df)[which(names(df) == fishCountAttributeName)] <- "fishCount"
  

  #compute decades
  df$decade <- df$year - (df$year %% temporalAgg)
  decade.df <- aggregate(list(year=df$year), by=list(decade=df$decade), FUN=range)
  decade.df$decade <- as.factor(decade.df$decade)
  decade.df$label <- paste(decade.df$year[,1], "-", decade.df$year[,2], sep="")
  
  #setup the palette
  my.colors <- rep(brewer.pal(nrow(decade.df), "Set1"), length.out=nrow(decade.df))
  names(my.colors) <- decade.df$label
  
  #function to compute mean and median for frequency data
  calculateMeanMedian <- function(LowerBound, UpperBound, Obs) {  
    cumObs <- cumsum(Obs)
    n_2 <- max(cumObs) / 2
    row.mid <- findInterval(max(cumObs) / 2, cumObs) + 1
    the.median <- LowerBound[row.mid] + ((UpperBound[row.mid] - LowerBound[row.mid]) / Obs[row.mid]) * (n_2 - (cumObs[row.mid] - Obs[row.mid]))
    the.mean <- sum((LowerBound + (UpperBound - LowerBound) / 2) * Obs) / sum(Obs)
    return(c(mean=the.mean, median=the.median))
  }
  
  #define the resulr df  
  result.df <- c()
  
  for (species.current in unique(df$species)) {
    
    #get species scietific name from ecoscope sparql
    sparqlResult <- getSpeciesFromEcoscope(as.character(species.current))
    if (length(sparqlResult) > 0) {
      species.label <- sparqlResult[1,"scientific_name"]
      species.URI <- sparqlResult[1,"uri"]
    } else {
      species.label <- species.current
      species.URI <- species.current
    }
    
    species.df <- df[df$species == species.current,]
    
    species.df.year.min <- min(species.df$year)
    species.df.year.max <- max(species.df$year)
      
    species.df <- aggregate(fishCount ~ sizeClassLowerBound + sizeClassUpperBound + decade, data=species.df, FUN=sum)
    species.df$decade <- factor(species.df$decade, levels=decade.df$decade, labels=decade.df$label)  
    
    #order data
    species.df <- species.df[order(species.df$decade, species.df$sizeClassLowerBound),]
    #compute mean and median by decade
    median.df <- ddply(species.df, .(decade), function(x) calculateMeanMedian(x$sizeClassLowerBound, x$sizeClassUpperBound, x$fishCount))
    
    #compute sum and relative contribution
    species.df <- merge(species.df, aggregate(list(sum=species.df$fishCount), by=list(decade=species.df$decade), FUN=sum))
    species.df$relative <- species.df$fishCount / species.df$sum
    
    #detrmine a little space on the plot btw each class
    
    #build the plot
    plot.result <- ggplot(data=species.df) + 
      geom_rect(mapping=aes(fill=decade, order=decade, xmin = sizeClassLowerBound, xmax = sizeClassUpperBound, ymin = 0, ymax = relative), colour="grey25", show_guide=FALSE) +
      facet_grid(decade ~ .) +
      geom_vline(data=median.df, mapping=aes(xintercept=median), linetype="dashed", colour="grey25") +
      geom_vline(data=median.df, mapping=aes(xintercept=mean), colour="grey25") +
      scale_fill_manual(values=my.colors) +
      labs(x="Size class (in cm). With mean (solid grey line) and median (dashed)", y="Relative contribution", title=paste(species.label, "size frequencies contribution"), fill=NA)
    
    #draw the plot
    tempfile.base <- tempfile(pattern=paste("I10_", gsub(" ", "_", species.label), "_", as.character(species.df.year.min), "-", as.character(species.df.year.max), "_", sep=""))
    plot.filepath <- paste(tempfile.base, ".png", sep="")
    ggsave(filename=plot.filepath, plot=plot.result, dpi=100)
    
    #create the RDF metadata
    rdf_file_path <- paste(tempfile.base, ".rdf", sep="")
    buildRdf(rdf_file_path=paste(tempfile.base, ".rdf", sep=""),
             rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""),               
             titles=c("IRD Tuna Atlas: indicator #10 - Graph relative contribution of size frequencies over decades", 
                      "IRD Atlas thonier : indicateur #10 - Graphique des contributions des classes de tailles par décades"),
             descriptions=c(paste(species.label, "size frequencies contribution catches plot"), 
                            paste("Contributions des classes de tailles aux captures de", species.label)),
             subjects=c(as.character(species.current)),
             processes="http://www.ecoscope.org/ontologies/resources/processI10",
             data_output_identifier=plot.filepath,
             start=as.character(species.df.year.min),
             end=as.character(species.df.year.max),
             spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))")
    
    result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf_file_path))
  }
  return(result.df)  
}




