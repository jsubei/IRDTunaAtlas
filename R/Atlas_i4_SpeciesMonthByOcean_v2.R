#Norbert Billet - IRD
#2013/11/04: V2 version: add RDF export and allow production of multiple graph (i.e. species and years)
#2013/09/03: Norbert - Add attributes names as parameters
#2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
#2013/04/22: Norbert - First version
#Atlas_i4_SpeciesMonthByOcean : build a graph of monthly (seasonal) catches by ocean for a unique species

#inputFilePath <- "/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i4.csv"
#oceanAttributeName="ocean"
#yearAttributeName="year"
#monthAttributeName="month"
#speciesAttributeName="species"
#valueAttributeName="value"

# Atlas_i4_SpeciesMonthByOcean_v2("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i4.csv", 
#                                    oceanAttributeName="ocean",
#                                    yearAttributeName="year",
#                                    monthAttributeName="month",
#                                    speciesAttributeName="species",
#                                    valueAttributeName="value")

Atlas_i4_SpeciesMonthByOcean_v2 <- function(inputFilePath, 
                                         oceanAttributeName="ns0:ocean",
                                         yearAttributeName="ns0:year", 
                                         monthAttributeName="ns0:month", 
                                         speciesAttributeName="ns0:species",                                         
                                         valueAttributeName="ns0:value")
{
  if(! require(XML) | ! require(ggplot2))
  {
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
    
    df <- data.frame(year=as.factor(xpathSApply(my.XML, paste("//", yearAttributeName, sep=""), xmlValue)), 
                     month=as.numeric(xpathSApply(my.XML, paste("//", monthAttributeName, sep=""), xmlValue)), 
                     species=as.factor(xpathSApply(my.XML, paste("//", speciesAttributeName, sep=""), xmlValue)), 
                     ocean=as.factor(xpathSApply(my.XML, paste("//", oceanAttributeName, sep=""), xmlValue)), 
                     value=as.numeric(xpathSApply(my.XML, paste("//", valueAttributeName, sep=""), xmlValue)))
    rm(my.XML)
  }
  
  #for csv input file
  if (tolower(substring(inputFilePath, nchar(inputFilePath)-3)) == ".csv") {
    #read the input file
    my.csv <- read.csv(inputFilePath, stringsAsFactors=FALSE)
    
    df <- data.frame(year=as.numeric(my.csv[,yearAttributeName]),     
                     month=as.numeric(my.csv[,monthAttributeName]),
                     species=as.factor(my.csv[,speciesAttributeName]),
                     ocean=as.factor(my.csv[,oceanAttributeName]),
                     value=as.numeric(my.csv[,valueAttributeName]))
    rm(my.csv)
  }
  
  if (! exists("df")) {
    stop("Error with input file")
  }
  
  #setup factors
  df$month <- factor(df$month, labels=month.name)
  #test if IRD usual ocean names are used
  if (length(intersect(levels(df$ocean), c("ATL", "IND", "PAC_E", "PAC_W"))) == length(levels(df$ocean))) {
    df$ocean <- factor(df$ocean, levels=c("ATL", "IND", "PAC_E", "PAC_W"), labels=c("Atlantic O.", "Indian O.", "East Pacific O.", "West Pacific O."))
  }
  
  #setup the palette
  my.colors <- brewer.pal(length(levels(df$ocean)), "Set1")
  names(my.colors) <- levels(df$ocean)
  
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
    
    #compute the decade
    species.df$decade <- species.df$year - (species.df$year %% 10)
    decades.factor <- sort(unique(species.df$decade))
    
    species.df$decade <- factor(species.df$decade, 
                                levels=decades.factor,
                                labels=unlist(lapply(X=decades.factor, FUN=function(dec) paste(min(species.df[species.df$decade == dec,]$year), "-", max(species.df[species.df$decade == dec,]$year), sep=""))))
    for (decade.current in levels(species.df$decade)) {
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
      base_temp_file <- tempfile(pattern=paste("I4_", gsub(" ", "_", species.current), "_", as.character(decade.current), "_", sep=""))
      plot_file_path <- paste(base_temp_file, ".png", sep="")
      ggsave(filename=plot_file_path, plot=resultPlot, dpi=100)
      
      #create the RDF metadata
      rdf_file_path <- paste(base_temp_file, ".rdf", sep="")
      buildRdf(rdf_file_path=paste(base_temp_file, ".rdf", sep=""),
               rdf_subject="http://ecoscope.org/indicatorI4", 
               titles=c("IRD Tuna Atlas: indicator #4 - monthly catches by ocean", 
                        "IRD Atlas thonier : indicateur #4 - captures mensuelle par océan"),
               descriptions=c(paste(species.label, "monthly catches by ocean"), 
                              paste("Captures mensuelles de", species.label, "par océan")),
               subjects=c(as.character(species.current), as.character(unique(current.df$ocean))),
               processes="&localfile;/processI4",
               start=as.character(min(current.df$year)),
               end=as.character(max(current.df$year)),
               spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))")
      
      result.df <- rbind(result.df, c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
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
      base_temp_file <- tempfile(pattern=paste("I4_", gsub(" ", "_", species.current), "_byDecade_", sep=""))
      plot_file_path <- paste(base_temp_file, ".png", sep="")
      ggsave(filename=plot_file_path, plot=resultPlot, dpi=100)
      
      #create the RDF metadata
      rdf_file_path <- paste(base_temp_file, ".rdf", sep="")
      buildRdf(rdf_file_path=paste(base_temp_file, ".rdf", sep=""),
               rdf_subject="http://ecoscope.org/indicatorI4", 
               titles=c("IRD Tuna Atlas: indicator #4 - monthly catches by ocean and by decade", 
                        "IRD Atlas thonier : indicateur #4 - captures mensuelle par océan et par décénie"),
               descriptions=c(paste(species.label, "monthly catches by ocean and by decade"), 
                              paste("Captures mensuelles de", species.label, "par océan et par décénie")),
               subjects=c(species.current, levels(current.df$ocean)),
               processes="&localfile;/processI4",
               start=as.character(min(current.df$year)),
               end=as.character(max(current.df$year)),
               spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))")
      
      result.df <- rbind(result.df, c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
    }
  }
  
  return(result.df)
}





