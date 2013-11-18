#Atlas_i3_SpeciesYearByGearMonth.R
#Tuna Atlas - IRD / MR EME
#
#Build a graph of catches of a given species for a given year by gear type and by month. Provide comparaison with monthly mean of the 5 previous years
##################################################################
#Norbert Billet - IRD
#2013/11/04: V2 version: add RDF export and allow production of multiple graph (i.e. species)
#2013/09/03: Norbert - Add attributes names as parameters
#2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
#2013/03/15: Norbert - Initial version
##################################################################
#Use example:
# library(IRDTunaAtlas)
# csv.df <- read.csv("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i3.csv", stringsAsFactors=FALSE)
# Atlas_i3_SpeciesYearByGearMonth(csv.df, 
#                                    yearAttributeName="year",
#                                    monthAttributeName="month",
#                                    speciesAttributeName="species",
#                                    gearTypeAttributeName="gear_type",
#                                    valueAttributeName="value",
#                                    meanPrev5YearsAttributeName="mean_prev_5_years",
#                                    stddevPrev5YearsAttributeName="stddev_prev_5_years")
##################################################################

Atlas_i3_SpeciesYearByGearMonth <- function(df, 
                                            yearAttributeName="ns0:year", 
                                            monthAttributeName="ns0:month", 
                                            speciesAttributeName="ns0:species",
                                            gearTypeAttributeName="ns0:gear_type",
                                            valueAttributeName="ns0:value",
                                            meanPrev5YearsAttributeName="ns0:mean_prev_5_years",
                                            stddevPrev5YearsAttributeName="ns0:stddev_prev_5_years")
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
  
  if(sum(names(df) == monthAttributeName) == 0) {
    stop("Cannot found month attribute")
  } else {
    df[, monthAttributeName] <- as.numeric(df[, monthAttributeName])
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
  
  if(sum(names(df) == meanPrev5YearsAttributeName) == 0) {
    stop("Cannot found mean for previous years attribute")
  } else {
    df[, meanPrev5YearsAttributeName] <- as.numeric(df[, meanPrev5YearsAttributeName])
  }
  
  if(sum(names(df) == stddevPrev5YearsAttributeName) == 0) {
    stop("Cannot found std_dev for previous years attribute")
  } else {
    df[, stddevPrev5YearsAttributeName] <- as.numeric(df[, stddevPrev5YearsAttributeName])
  }
  
  #from std deviation to variance, and root square the sum of variances
  fct <- function(vec)
  {
    var <- vec * vec
    var <- sum(var)
    return(sqrt(var))
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
  
  #for each species
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
    
    #for each year
    for (year.current in unique(df[df$species == species.current,]$year)) {
      current.df <- df[df$species == species.current & df$year == year.current,]
      
      if (! all(table(current.df$month) == 1)) {
        if (all(is.na(current.df$stddev_prev_5_years))) {
          stddev.agg <- cbind(month=unique(current.df$month), stddev_prev_5_years=NA)
        } else {
          stddev.agg <- aggregate(stddev_prev_5_years ~ month, data=current.df, fct)
        }
        
        if (all(is.na(current.df$mean_prev_5_years))) {
          mean.agg <- cbind(month=unique(current.df$month), mean_prev_5_years=NA)
        } else {
          mean.agg <- aggregate(mean_prev_5_years ~ month, data=current.df, sum)
        }
        
        dfPrev5Years <- merge(mean.agg, stddev.agg)            
        
      } else {
        dfPrev5Years <- current.df
      }
      #order gear factor levels by value
      current.df$gear_type <- factor(current.df$gear_type, levels=rev(levels(reorder(current.df$gear_type, current.df$value))))
      #set proper month label
      current.df$month <- factor(month.abb[current.df$month], levels=levels(reorder(month.abb[current.df$month], current.df$month)))
      dfPrev5Years$month <- factor(month.abb[dfPrev5Years$month], levels=levels(reorder(month.abb[dfPrev5Years$month], dfPrev5Years$month)))
      #build the plot
      resultPlot <- ggplot() +
        layer(data=current.df,
              mapping=aes(x=month, y=value, fill=gear_type, order=gear_type),
              stat="identity",
              geom="bar") +
        layer(data=dfPrev5Years,
              mapping=aes(x=month, y=mean_prev_5_years, group=1),
              stat="identity",
              geom="line") + 
        layer(data=dfPrev5Years,
              mapping=aes(x=month, ymax=mean_prev_5_years + stddev_prev_5_years, ymin=mean_prev_5_years - stddev_prev_5_years), 
              width=0.25, 
              color="dimgray",
              stat="identity",
              geom="errorbar") +
        scale_fill_manual(name="Gear type", values=my.colors) +
        xlab("Month") + ylab("Catches in tons") + 
        ggtitle(paste(species.label, "monthly catches by gear type on", year.current))
        
      #draw the plot
      tempfile.base <- tempfile(pattern=paste("I3_", gsub(" ", "_", species.current), "_", as.character(year.current), "_", sep=""))
      plot.filepath <- paste(tempfile.base, ".png", sep="")
      ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)
      
      #create the RDF metadata
      rdf.filepath <- paste(tempfile.base, ".rdf", sep="")
      buildRdf(rdf_file_path=rdf.filepath,
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
      
      result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf.filepath))
      
    }
  }

  return(result.df)
}