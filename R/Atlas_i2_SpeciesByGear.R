#Atlas_i2_SpeciesByGear.R
#Tuna Atlas - IRD / MR EME
#
#This indicator produce a graph of annual catches by gear for each species present in the input data. An associated RDF file is also produced.
##################################################################
#Norbert Billet - IRD
#2013/11/04: V2 version: add RDF export and allow production of multiple graph (i.e. species)
#2013/09/03: Norbert - Add attributes names as parameters
#2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
#2013/03/15: Norbert - Initial version
##################################################################
#Use example:
# library(IRDTunaAtlas)
# csv.df <- read.csv("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i1i2.csv", stringsAsFactors=FALSE)
# Atlas_i2_SpeciesByGear(csv.df, 
#                         yearAttributeName="year", 
#                         gearTypeAttributeName="gear_type", 
#                         speciesAttributeName="species", 
#                         valueAttributeName="value")
##################################################################

Atlas_i2_SpeciesByGear <- function(df, 
                                   yearAttributeName="ns0:year", 
                                   speciesAttributeName="ns0:species",
                                   gearTypeAttributeName="ns0:gear_type",
                                   valueAttributeName="ns0:value")
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
  } else {
    df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  }
  
  if(sum(names(df) == gearTypeAttributeName) == 0) {
    stop("Cannot found gear attribute")
  } else {
    df[, gearTypeAttributeName] <- as.factor(df[, gearTypeAttributeName])
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  } else {
    df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot found value attribute")
  } else {
    df[, valueAttributeName] <- as.numeric(df[, valueAttributeName])
  }
  
  #test if FAO usual gear codes are used
  if (length(intersect(levels(df$gear_type), c("BB", "GILL", "LL", "PS", "OTHER_I", "OTHER_A", "TROL", "TRAP"))) == length(levels(df$gear_type))) {
    df$gear_type <- factor(df$gear_type, levels=c("BB", "GILL", "LL", "PS", "OTHER_I", "OTHER_A", "TROL", "TRAP"), labels=c("Baitboat", "Gillnet", "Longline", "Purse seine", "Unclass. art. Indian O.", "Unclass. art. Atl. O.", "Trol.", "Trap"))
  }
  
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
    
    #get species scietific name from ecoscope sparql
    sparqlResult <- getSpeciesFromEcoscope(as.character(species.current))
    if (length(sparqlResult) > 0) {
      species.label <- sparqlResult[1,"scientific_name"]
      species.URI <- sparqlResult[1,"uri"]
    } else {
      species.label <- species.current
      species.URI <- species.current
    }
    
    #build the plot
    resultPlot <- ggplot(aggData, aes(x=year, y=value, fill=gear_type, order=gear_type)) + 
      geom_bar(stat="identity", width=0.8) + 
      geom_bar(stat="identity", width=0.8, colour="grey20", show_guide=FALSE) + 
      scale_fill_manual(name="Gear type", values=my.colors) +
      xlab("Year") + ylab("Catches in thousand tons") + 
      ggtitle(paste(species.label, "catches by gear type")) +
      theme(legend.position="bottom")

    #draw the plot
    tempfile.base <- tempfile(pattern=paste("I2_", gsub(" ", "_", species.current), "_", sep=""))
    plot.filepath <- paste(tempfile.base, ".png", sep="")
    ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)
    
    #create the RDF metadata
    rdf.filepath <- paste(tempfile.base, ".rdf", sep="")
    buildRdf(rdf_file_path=rdf.filepath,
             rdf_subject="http://ecoscope.org/indicatorI2", 
             titles=c("IRD Tuna Atlas: indicator #2 - catches by species and by gear type", 
                      "IRD Atlas thonier : indicateur #2 - captures par espèces et par type d'engin"),
             descriptions=c(paste(species.label, "catches by gear type"), 
                            paste("Captures de", species.label, "par type d'engin")),
             subjects=c(as.character(species.current), as.character(unique(current.df$gear_type))),
             processes="&localfile;/processI2",
             start=as.character(min(aggData$year)),
             end=as.character(max(aggData$year)),
             spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))")
    
    result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf.filepath))
  }

  return(result.df)
}