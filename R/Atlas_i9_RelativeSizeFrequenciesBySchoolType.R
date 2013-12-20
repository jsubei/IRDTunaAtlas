
# Atlas_i9_RelativeSizeFrequenciesBySchoolType.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a graph relative contribution of size frequencies in catches for a species by school type. An associated RDF file is also produced.
##################################################################
# Norbert Billet - IRD
# 2013/11/04: Norbert - Add RDF export and allow production of multiple maps (i.e. species)
# 2013/09/03: Norbert - Add attributes names as parameters
# 2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
# 2013/06/12: Norbert - First version
##################################################################
# Use example:
#  library(IRDTunaAtlas)
#  csv.df <- read.csv("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i9i10.csv", stringsAsFactors=FALSE)
#  csv.df <- csv.df[csv.df$species == "ALB",]
#  Atlas_i9_RelativeSizeFrequenciesBySchoolType(csv.df,                       
#                        yearAttributeName="year",
#                        speciesAttributeName="species",
#                        schoolAttributeName="school",
#                        sizeClassLowerBoundAttributeName="class_low",
#                        sizeClassUpperBoundAttributeName="class_up",
#                        fishCountAttributeName="fish_count")
##################################################################

Atlas_i9_RelativeSizeFrequenciesBySchoolType <- function(df,
                                                         yearAttributeName="ns0:year",
                                                         speciesAttributeName="ns0:species",
                                                         schoolAttributeName="ns0:school",
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
  
  #check for input attributes
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot found year attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
  
  if(sum(names(df) == schoolAttributeName) == 0) {
    stop("Cannot found school type attribute")
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
  df[, schoolAttributeName] <- as.factor(df[, schoolAttributeName])
  df[, sizeClassLowerBoundAttributeName] <- as.numeric(df[, sizeClassLowerBoundAttributeName])    
  df[, sizeClassUpperBoundAttributeName] <- as.numeric(df[, sizeClassUpperBoundAttributeName])
  df[, fishCountAttributeName] <- as.numeric(df[, fishCountAttributeName])    
  
  #rename columns
  names(df)[which(names(df) == yearAttributeName)] <- "year"  
  names(df)[which(names(df) == speciesAttributeName)] <- "species"
  names(df)[which(names(df) == schoolAttributeName)] <- "school"
  names(df)[which(names(df) == sizeClassLowerBoundAttributeName)] <- "sizeClassLowerBound"
  names(df)[which(names(df) == sizeClassUpperBoundAttributeName)] <- "sizeClassUpperBound"
  names(df)[which(names(df) == fishCountAttributeName)] <- "fishCount"
  
  #test if usual school codes are used
  if (length(intersect(levels(df$school), c("IND", "BO", "BL"))) == length(levels(df$school))) {
    df$school <- factor(df$school, levels=c("IND", "BO", "BL"), labels=c("Undefined school", "Log school", "Free school"))
  }
  
  #setup the palette
  my.colors <- brewer.pal(length(levels(df$school)), "Set1")
  names(my.colors) <- levels(df$school)
  
  #plot fct
  plotFct <- function(subDf, species.label, lims=c()) {
    #aggregate values by size class and school type
    valuesSum <- aggregate(fishCount ~ sizeClassLowerBound + sizeClassUpperBound + school, data=subDf, FUN=sum)
    valuesSum$relative <- (valuesSum$fishCount / sum(valuesSum$fishCount)) * 100
#     mergedDf <- data.frame(sizeClass=valuesSum$sizeClass, school=valuesSum$school, relative=valuesSum$fishCount / sum(valuesSum$fishCount))
#     mergedDf$relative <- mergedDf$relative * 100
    
    #plot title
    if (min(subDf$year) == max(subDf$year)) {
      my.title <- paste(species.label , " size frequencies by school type for ",  min(subDf$year), sep="")
    } else {
      my.title <- paste(species.label , " size frequencies by school type for ",  min(subDf$year), "-",  max(subDf$year), sep="")
    }
    
    #build the plot
    plot.result <- ggplot(mapping=aes(fill=school, order=school)) +
      geom_rect(data=valuesSum, mapping=aes(xmin = sizeClassLowerBound, xmax = sizeClassUpperBound, ymin = 0, ymax = relative), colour="grey25") +
      scale_fill_manual(name="School type", values=my.colors) +
      xlab("Size (in cm)") + ylab("Relative contribution (in %)") + 
      labs(colour="School type", title=my.title) +
      theme(legend.position="bottom")
    
    if (length(lims) == 4) {
      plot.result <- plot.result + scale_x_continuous(limits=c(lims[1], lims[2])) + scale_y_continuous(limits=c(lims[3], lims[4]))
    }
    
    #draw the plot
    base_temp_file <- tempfile(pattern=paste("I9_", gsub(" ", "_", species.label), "_", as.character(min(subDf$year)), "-", as.character(max(subDf$year)), "_", sep=""))
    plot_file_path <- paste(base_temp_file, ".png", sep="")
    ggsave(filename=plot_file_path, plot=plot.result, dpi=100)
    
    #create the RDF metadata
    rdf_file_path <- paste(base_temp_file, ".rdf", sep="")
    buildRdf(rdf_file_path=paste(base_temp_file, ".rdf", sep=""),
             rdf_subject="http://ecoscope.org/indicatorI9", 
             titles=c("IRD Tuna Atlas: indicator #9 - Graph relative contribution of size frequencies in catches for a species by school type", 
                      "IRD Atlas thonier : indicateur #9 - Graphique des contributions des classes de tailles aux captures par type de banc"),
             descriptions=c(paste(species.label, "size frequencies contribution catches plot"), 
                            paste("Contributions des classes de tailles aux captures de", species.label)),
             subjects=c(species.label),
             processes="&localfile;/processI9",
             start=as.character(min(subDf$year)),
             end=as.character(max(subDf$year)),
             spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))")
    
    return(c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
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
    
    #species.df <- df[df$species == species.current,]
    species.df <- aggregate(fishCount ~ sizeClassLowerBound + sizeClassUpperBound + school + year, data=df[df$species == species.current,], FUN=sum)
    
    #plot for all the period
    result.plot.df <- plotFct(species.df, species.label)
    result.df <- rbind(result.df, result.plot.df)
  
    years <- unique(species.df$year)
    if (length(years) > 1)
    {      
      contrib.max <- max(unlist(lapply(years, FUN=function(x) {max((species.df[species.df$year == x,]$fishCount / sum(species.df[species.df$year == x,]$fishCount)) * 100)})))
      sizeClass.range <- range(species.df$sizeClassLowerBound, species.df$sizeClassUpperBound)
      #for each year
      for(year.current in years) {
        result.plot.df <- plotFct(species.df[species.df$year==year.current,], species.label, lims=c(sizeClass.range[1], sizeClass.range[2], 0, contrib.max))
        result.df <- rbind(result.df, result.plot.df)
      }
      
      #for each decade
      species.df$decade <- species.df$year - (species.df$year %% 10)
      decades <- unique(species.df$decade)
      if (length(decades) > 1)
      {
        species.decade.df <- aggregate(fishCount ~ sizeClassLowerBound + sizeClassUpperBound + school + decade, data=species.df, FUN=sum)
        contrib.max <- max(unlist(lapply(decades, FUN=function(x) {max((species.decade.df[species.decade.df$decade == x,]$fishCount / sum(species.decade.df[species.decade.df$decade == x,]$fishCount)) * 100)})))
        for(decade.current in decades) {
          result.plot.df <- plotFct(species.df[species.df$decade==decade.current,], species.label, lims=c(sizeClass.range[1], sizeClass.range[2], 0, contrib.max))
          result.df <- rbind(result.df, result.plot.df)
        }
      }
    }
  }
  return(result.df)
}




