#Norbert Billet - IRD
#2013/11/04: V2 version: add RDF export and allow production of multiple graph (i.e. species and years)
#2013/09/03: Norbert - Add attributes names as parameters
#2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
#2013/03/15: Norbert - Initial version
#Atlas_i3_SpeciesYearByGearMonth : build a graph of catches of a given species for a given year by gear type and by month. Provide comparaison with monthly mean of the 5 previous years


#inputFilePath <- "/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i3.csv"
#yearAttributeName="year"
#monthAttributeName="month"
#speciesAttributeName="species"
#gearTypeAttributeName="gear_type"
#valueAttributeName="value"
#meanPrev5YearsAttributeName="mean_prev_5_years"
#stddevPrev5YearsAttributeName="stddev_prev_5_years"

# Atlas_i3_SpeciesYearByGearMonth_v2("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i3.csv", 
#                                    yearAttributeName="year",
#                                    monthAttributeName="month",
#                                    speciesAttributeName="species",
#                                    gearTypeAttributeName="gear_type",
#                                    valueAttributeName="value",
#                                    meanPrev5YearsAttributeName="mean_prev_5_years",
#                                    stddevPrev5YearsAttributeName="stddev_prev_5_years")

Atlas_i3_SpeciesYearByGearMonth_v2 <- function(inputFilePath, 
                                            yearAttributeName="ns0:year", 
                                            monthAttributeName="ns0:month", 
                                            speciesAttributeName="ns0:species",
                                            gearTypeAttributeName="ns0:gear_type",
                                            valueAttributeName="ns0:value",
                                            meanPrev5YearsAttributeName="ns0:mean_prev_5_years",
                                            stddevPrev5YearsAttributeName="ns0:stddev_prev_5_years")
{
  if(! require(XML) | ! require(ggplot2))
  {
    stop("Missing library")
  }
  
  if (missing(inputFilePath)) {
    stop("Input file not specified")
  }
  
  checkGMLFile(inputFilePath)
  
  #for gml input file
  if (tolower(substring(inputFilePath, nchar(inputFilePath)-3)) == ".gml") {
    #read the input file
    my.XML <- xmlInternalTreeParse(inputFilePath)
    
    df <- data.frame(year=as.factor(xpathSApply(my.XML, paste("//", yearAttributeName, sep=""), xmlValue)), 
                     month=as.numeric(xpathSApply(my.XML, paste("//", monthAttributeName, sep=""), xmlValue)), 
                     species=as.factor(xpathSApply(my.XML, paste("//", speciesAttributeName, sep=""), xmlValue)), 
                     gear_type=as.factor(xpathSApply(my.XML, paste("//", gearTypeAttributeName, sep=""), xmlValue)), 
                     value=as.numeric(xpathSApply(my.XML, paste("//", valueAttributeName, sep=""), xmlValue)), 
                     mean_prev_5_years=as.numeric(xpathSApply(my.XML, paste("//", meanPrev5YearsAttributeName, sep=""), xmlValue)), 
                     stddev_prev_5_years=as.numeric(xpathSApply(myXML, paste("//", stddevPrev5YearsAttributeName, sep=""), xmlValue)))
    rm(my.XML)
  }
  
  #for csv input file
  if (tolower(substring(inputFilePath, nchar(inputFilePath)-3)) == ".csv") {
    #read the input file
    my.csv <- read.csv(inputFilePath, stringsAsFactors=FALSE)
    
    df <- data.frame(year=as.numeric(my.csv[,yearAttributeName]),     
                     month=as.numeric(my.csv[,monthAttributeName]),
                     species=as.factor(my.csv[,speciesAttributeName]),
                     gear_type=as.factor(my.csv[,gearTypeAttributeName]),
                     value=as.numeric(my.csv[,valueAttributeName]),
                     mean_prev_5_years=as.numeric(my.csv[,meanPrev5YearsAttributeName]), 
                     stddev_prev_5_years=as.numeric(my.csv[,stddevPrev5YearsAttributeName]))
    rm(my.csv)
  }
  
  if (! exists("df")) {
    stop("Error with input file")
  }
  
  
  #from std deviation to variance, and root square the sum of variances
  fct <- function(vec)
  {
    var <- vec * vec
    var <- sum(var)
    return(sqrt(var))
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
    
    for (year.current in unique(df[df$species == species.current,]$year)) {
      current.df <- df[df$species == species.current & df$year == year.current,]
      
      
      if (! all(table(current.df$month) == 1)) {
        if (all(is.na(current.df$stddev_prev_5_years))) {
          stddev.agg <- cbind(month=unique(current.df$month), stddev_prev_5_years=NA)
        } else {
          stddev.agg <- aggregate(stddev_prev_5_years ~ month, data=current.df, fct)
        }
        
        if (all(is.na(current.df$mean_prev_5_years))) {
          mean.agg <- cbind(month=unique(current.df$month), stddev_prev_5_years=NA)
        } else {
          mean.agg <- aggregate(mean_prev_5_years ~ month, data=current.df, sum)
        }
        
        dfPrev5Years <- merge(mean.agg, stddev.agg)    
        #order gear factor levels by value
        current.df$gear_type <- factor(current.df$gear_type, levels=rev(levels(reorder(current.df$gear_type, current.df$value))))
        
      } else {
        dfPrev5Years <- current.df
      }
      current.df$month <- factor(month.abb[current.df$month], levels=levels(reorder(month.abb[current.df$month], current.df$month)))
      #build the plot
      resultPlot <- ggplot() +
        geom_bar(data=current.df, mapping=aes(x=month, y=value, fill=gear_type, order=gear_type), stat="identity", width=0.8) +
        scale_fill_manual(name="Gear type", values=my.colors) +
        geom_errorbar(data=dfPrev5Years, mapping=aes(x=month, ymax = mean_prev_5_years + stddev_prev_5_years, ymin=mean_prev_5_years - stddev_prev_5_years), width=0.25, color="dimgray") +
        geom_line(data=dfPrev5Years, mapping=aes(x=month, y=mean_prev_5_years)) +
        xlab("Month") + ylab("Catches in tons") + 
        ggtitle(paste(species.current, "monthly catches by gear type on", year.current))
        
      #draw the plot
      base_temp_file <- tempfile(pattern=paste("I3_", gsub(" ", "_", species.current), "_", as.character(year.current), "_", sep=""))
      plot_file_path <- paste(base_temp_file, ".png", sep="")
      ggsave(filename=plot_file_path, plot=resultPlot, dpi=100)
      
      #create the RDF metadata
      rdf_file_path <- paste(base_temp_file, ".rdf", sep="")
      buildRdf(rdf_file_path=paste(base_temp_file, ".rdf", sep=""),
               rdf_subject="http://ecoscope.org/indicatorI3", 
               titles=c("IRD Tuna Atlas: indicator #3 - catches by species for a given year by gear type and by month", 
                        "IRD Atlas thonier : indicateur #3 - captures par espèces pour une année donnée par mois et par type d'engin"),
               descriptions=c(paste(species.label, "catches by gear type and by month on", as.character(year.current)), 
                              paste("Captures de", species.label, "par mois et par type d'engin pour l'année", as.character(year.current))),
               subjects=c(as.character(species.current), as.character(unique(current.df$gear_type))),
               processes="&localfile;/processI3",
               start=as.character(year.current),
               end=as.character(year.current),
               spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))")
      
      result.df <- rbind(result.df, c(plot.file.path=plot_file_path, rdf.file.path=rdf_file_path))
      
    }
  }

  return(result.df)
}