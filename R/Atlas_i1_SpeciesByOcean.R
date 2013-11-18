#inputFilePath <- "/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i1i2.csv"
#yearAttributeName="year"
#oceanAttributeName="ocean"
#speciesAttributeName="species"
#gearTypeAttributeName="gear_type"
#valueAttributeName="value"

# Atlas_i1_SpeciesByOcean(inputFilePath="/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i1i2.csv", 
#                         yearAttributeName="year", 
#                         oceanAttributeName="ocean", 
#                         speciesAttributeName="species", 
#                         valueAttributeName="value")

Atlas_i1_SpeciesByOcean <- function(inputFilePath, 
                                    yearAttributeName="ns0:year", 
                                    oceanAttributeName="ns0:ocean", 
                                    speciesAttributeName="ns0:species",
                                    valueAttributeName="ns0:value")
{
  if (! require(XML) | ! require(ggplot2) | ! require(RColorBrewer)) {
    stop("Missing library")
  }
  
  if (missing(inputFilePath)) {
    stop("Input file not specified")
  }
  
  checkInputFile(inputFilePath)
  
  #for gml input file
  if (tolower(substring(inputFilePath, nchar(inputFilePath)-3)) == ".gml") {
    #read the input file
    my.XML <- xmlInternalTreeParse(inputFilePath)
    
    df <- data.frame(year=as.numeric(xpathSApply(my.XML, paste("//", yearAttributeName, sep=""), xmlValue)), 
                     ocean=as.factor(xpathSApply(my.XML, paste("//", oceanAttributeName, sep=""), xmlValue)), 
                     species=as.factor(xpathSApply(my.XML, paste("//", speciesAttributeName, sep=""), xmlValue)), 
                     value=as.numeric(xpathSApply(my.XML, paste("//", valueAttributeName, sep=""), xmlValue)))
    rm(my.XML)
  }
  
  #for csv input file
  if (tolower(substring(inputFilePath, nchar(inputFilePath)-3)) == ".csv") {
    #read the input file
    csv_df <- read.csv(inputFilePath, stringsAsFactors=FALSE)
    
    df <- data.frame(year=as.numeric(csv_df[,yearAttributeName]),
                     ocean=as.factor(csv_df[,oceanAttributeName]),
                     species=as.factor(csv_df[,speciesAttributeName]),
                     value=as.numeric(csv_df[,valueAttributeName]))
    rm(csv_df)
  }
  
  if (! exists("df")) {
    stop("Error with input file")
  }
  
  #define the resulr df  
  result.df <- c()
  
  #test if IRD usual ocean names are used
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
    
    #keep only common time line
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
    base_temp_file <- tempfile(pattern=paste("I1_", gsub(" ", "_", species.current), "_", sep=""))
    plot_file_path <- paste(base_temp_file, ".png", sep="")
    ggsave(filename=plot_file_path, plot=resultPlot, dpi=100)
    
    #create the RDF metadata
    rdf_file_path <- paste(base_temp_file, ".rdf", sep="")
    buildRdf(rdf_file_path=paste(base_temp_file, ".rdf", sep=""),
              rdf_subject="http://ecoscope.org/indicatorI1", 
              titles=c("IRD Tuna Atlas: indicator #1 - catches by species and by ocean", 
                       "IRD Atlas thonier : indicateur #1 - captures par espèces et par océan"),
              descriptions=c(paste(species.label, "catches by ocean"), 
                       paste("Captures de", species.label, "par océan")),
              subjects=c(as.character(species.current)),
              processes="&localfile;/processI1",
              start=as.character(min(aggData$year)),
              end=as.character(max(aggData$year)),
              spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))")
        
    result.df <- rbind(result.df, c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
  }
  return(result.df)
}
