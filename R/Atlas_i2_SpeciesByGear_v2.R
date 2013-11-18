#inputFilePath <- "/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i1i2.csv"
#yearAttributeName="year"
#oceanAttributeName="ocean"
#speciesAttributeName="species"
#gearTypeAttributeName="gear_type"
#valueAttributeName="value"

#Atlas_i2_SpeciesByGear_v2("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i1i2.csv", yearAttributeName="year", oceanAttributeName="ocean", speciesAttributeName="species", gearTypeAttributeName="gear_type", valueAttributeName="value")

Atlas_i2_SpeciesByGear_v2 <- function(inputFilePath, 
                                   yearAttributeName="ns0:year", 
                                   oceanAttributeName="ns0:ocean", 
                                   speciesAttributeName="ns0:species",
                                   gearTypeAttributeName="ns0:gear_type",
                                   valueAttributeName="ns0:value")
{
  if (! require(XML) | ! require(ggplot2)) {
    stop("Missing library")
  }
  
  if (missing(inputFilePath)) {
    stop("Input file not specified")
  }
  
  checkInputFile(inputFilePath)
  
  #for gml input file
  if (tolower(substring(inputFilePath, nchar(inputFilePath)-3)) == ".gml") {
    #read the input file
    my_XML <- xmlInternalTreeParse(inputFilePath)
    
    df <- data.frame(year=as.numeric(xpathSApply(my_XML, paste("//", yearAttributeName, sep=""), xmlValue)), 
                     ocean=as.factor(xpathSApply(my_XML, paste("//", oceanAttributeName, sep=""), xmlValue)), 
                     species=as.factor(xpathSApply(my_XML, paste("//", speciesAttributeName, sep=""), xmlValue)), 
                     gear_type=as.factor(xpathSApply(my_XML, paste("//", gearTypeAttributeName, sep=""), xmlValue)), 
                     value=as.numeric(xpathSApply(my_XML, paste("//", valueAttributeName, sep=""), xmlValue)))
    rm(my_XML)
  }
  
  #for csv input file
  if (tolower(substring(inputFilePath, nchar(inputFilePath)-3)) == ".csv") {
    #read the input file
    csv_df <- read.csv(inputFilePath, stringsAsFactors=FALSE)
    
    df <- data.frame(year=as.numeric(csv_df[,yearAttributeName]),
                     ocean=as.factor(csv_df[,oceanAttributeName]),
                     species=as.factor(csv_df[,speciesAttributeName]),
                     gear_type=as.factor(csv_df[,gearTypeAttributeName]),
                     value=as.numeric(csv_df[,valueAttributeName]))
    rm(csv_df)
  }
  
  if (! exists("df")) {
    stop("Error with input file")
  }
  
  #test if IRD usual gears names are used
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
    base_temp_file <- tempfile(pattern=paste("I2_", gsub(" ", "_", species.current), "_", sep=""))
    plot_file_path <- paste(base_temp_file, ".png", sep="")
    ggsave(filename=plot_file_path, plot=resultPlot, dpi=100)
    
    #create the RDF metadata
    rdf_file_path <- paste(base_temp_file, ".rdf", sep="")
    buildRdf(rdf_file_path=paste(base_temp_file, ".rdf", sep=""),
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
    
    result.df <- rbind(result.df, c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
  }

  return(result.df)
}