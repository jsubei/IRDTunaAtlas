#Atlas_i1_SpeciesByOcean.R
#Tuna Atlas - IRD / MR EME
#
#This indicator produce a graph of annual catches by ocean for each species present in the input data. An associated RDF file is also produced.
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
# Atlas_i1_SpeciesByOcean(csv.df, 
#                         yearAttributeName="year", 
#                         oceanAttributeName="ocean", 
#                         speciesAttributeName="species", 
#                         valueAttributeName="value")
##################################################################

Atlas_i1_SpeciesByOcean <- function(df, 
                                    yearAttributeName="ns0:year", 
                                    oceanAttributeName="ns0:ocean", 
                                    speciesAttributeName="ns0:species",
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
    resultPlot <- ggplot(aggData, aes(x=year, y=value, group=ocean)) + 
      geom_area(aes(fill=ocean), position="stack") + 
      geom_line(position="stack", color="grey20") + 
      scale_fill_manual(name="Ocean", values=my.colors) +
      xlab("Year") + ylab("Catches in thousand tons") + 
      ggtitle(paste(species.label, "catches by Ocean")) +
      theme(legend.position="bottom")
    
    #draw the plot
    tempfile.base <- tempfile(pattern=paste("I1_", gsub(" ", "_", species.label), "_", sep=""))
    plot.filepath <- paste(tempfile.base, ".png", sep="")
    ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)
    
    #create the RDF metadata
    rdf.filepath <- paste(tempfile.base, ".rdf", sep="")
    buildRdf(rdf_file_path=rdf.filepath,
              rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""), 
              #rdf_subject="http://ecoscope.org/indicatorI1", 
              titles=c("IRD Tuna Atlas: indicator #1 - catches by species and by ocean", 
                       "IRD Atlas thonier : indicateur #1 - captures par espèces et par océan"),
              descriptions=c(paste(species.label, "catches by ocean"), 
                       paste("Captures de", species.label, "par océan")),
              subjects=c(as.character(species.current)),
              processes="http://www.ecoscope.org/ontologies/resources/processI1",
              data_output_identifier=plot.filepath,
              start=as.character(min(aggData$year)),
              end=as.character(max(aggData$year)),
             #julien => A ADAPTER AVEC LA CONVEX HULL / ou la collection DE TOUTES LES GEOMETRIES CONCERNEES
             spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))")
        
    result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf.filepath))
  }
  return(result.df)
}
