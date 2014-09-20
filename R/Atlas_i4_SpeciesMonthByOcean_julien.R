# Atlas_i4_SpeciesMonthByOcean.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a graph of monthly (seasonal) catches by ocean for a species. An associated RDF file is also produced.
##################################################################
# Norbert Billet - IRD
# 2013/11/04: V2 version: add RDF export and allow production of multiple graph (i.e. species)
# 2013/09/03: Norbert - Add attributes names as parameters
# 2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
# 2013/04/22: Norbert - First version
##################################################################
# Use example:
#  library(IRDTunaAtlas)
#  csv.df <- read.csv("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i4.csv", stringsAsFactors=FALSE)
#  csv.df <- csv.df[csv.df$species == "ALB",]
#  Atlas_i4_SpeciesMonthByOcean(csv.df, 
#                                    oceanAttributeName="ocean",
#                                    yearAttributeName="year",
#                                    monthAttributeName="month",
#                                    speciesAttributeName="espece",
#                                    valueAttributeName="value")
##################################################################
library(rCharts)
source("/home/julien/SVNs/GIT/IRDTunaAtlas/R/IRDTunaAtlas_julien.R")
Atlas_i4_SpeciesMonthByOcean_julien <- function(df, 
                                         oceanAttributeName="ocean",
                                         yearAttributeName="year", 
                                         monthAttributeName="month", 
                                         speciesAttributeName="species",                                         
                                         valueAttributeName="value",
                                         withSparql=TRUE)
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
  
  
  
  #List to store URLs of the set of files generated for each species
  liste <- list()
  store = new.rdf(ontology=FALSE)
  add.prefix(store,
             prefix="resources_def",
             namespace="http://www.ecoscope.org/ontologies/resources_def/")
  add.prefix(store,
             prefix="ical",
             namespace="http://www.w3.org/2002/12/cal/ical/")
  add.prefix(store,
             prefix="dct",
             namespace="http://purl.org/dc/terms/")
  
  
    
  for (species.current in unique(df$species)) {
    
    if (withSparql) {      
      #get species scientific name from ecoscope sparql
      sparqlResult <- getSpeciesFromEcoscope(as.character(species.current))
      if (length(sparqlResult) > 0) {
        species.label <- sparqlResult[1,"scientific_name"]
        species.URI <- sparqlResult[1,"uri"]
      } else {
        species.label <- species.current
        species.URI <- species.current
      } 
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
#       tempfile.base <- tempfile(pattern=paste("I4_", gsub(" ", "_", species.label), "_", as.character(decade.current), "_", sep=""))
      filename <- tempfile(pattern=paste("I4", gsub(" ", "_", species.label), "_", sep=""),tmpdir="")
      tempfile.base <- paste("/data/www/html/tmp/SpeciesByMonthByOcean",filename, sep="")
      plot.filepath <- paste(tempfile.base, ".png", sep="")
      ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)
      plot.URLpng <- paste("http://mdst-macroes.ird.fr/tmp/SpeciesByMonthByOcean",filename, ".png", sep="")
      
      
      ## Example 4
      plotRchartsHighcharts <- data.frame(key = c("a", "b", "c"), value = c(1, 2, 3))
      hPlot(x = "ocean", y = "value", data = mergedDf, type = "pie")
      plotRchartsHighcharts
      
      ## {title: Pie Chart}
      plotRchartsNVD3 <- nPlot(~ ocean, data = mergedDf, type = 'pieChart')
      plotRchartsNVD3
      
      
      
      ## Dataset in HTML
      Datatable <- dTable(
        mergedDf,
        sPaginationType= "full_numbers"
      )
      Datatable
      
      
      ## Storage of files in a given repository (temporary or permanent)
      plot.filepathtml <- paste(tempfile.base, ".html", sep="")
      plot.URLhtml <- paste("http://mdst-macroes.ird.fr/tmp/SpeciesByMonthByOcean",filename, ".html", sep="")
      plotRchartsHighcharts$save(plot.filepathtml,standalone=TRUE) 
      plotRchartsNVD3$save(plot.filepathtmlNVD3,standalone=TRUE) 
      plot.filepathtmlNVD3 <- paste(tempfile.base, "_NVD3.html", sep="")
#       Datatable
      plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
      Datatable$save(plot.filepathtmltable,standalone=TRUE)     
      plot.URLhtmlTable <- paste("http://mdst-macroes.ird.fr/tmp/SpeciesByMonthByOcean",filename, "_table.html", sep="")    
      
      
      #create the RDF metadata
      rdf.filepath <- paste("/data/www/html/tmp/La_totale/SpeciesByMonthByOcean", ".rdf", sep="")
      rdf.URL <- paste("http://mdst-macroes.ird.fr/tmp",filename, ".rdf", sep="")
      buildRdf(store=store, rdf_file_path=rdf.filepath,
               #rdf_subject="http://ecoscope.org/indicatorI4", 
               rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""),               
               titles=c("IRD Tuna Atlas: indicator #4 - monthly catches by ocean", 
                        "IRD Atlas thonier : indicateur #4 - captures mensuelle par océan"),
               descriptions=c(paste(species.label, "monthly catches by ocean"), 
                              paste("Captures mensuelles de", species.label, "par océan")),
               subjects=c(as.character(species.current), as.character(unique(current.df$ocean))),
               processes="http://www.ecoscope.org/ontologies/resources/processI4",
               data_output_identifier=plot.filepath,  
               start=as.character(min(current.df$year)),
               end=as.character(max(current.df$year)),
               spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))",
               withSparql)
      
      result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf_file_path))
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
      
      #create the RDF metadata
      rdf.filepath <- paste("/data/www/html/tmp/La_totale/SpeciesByMonthByOcean", ".rdf", sep="")
      rdf.URL <- paste("http://mdst-macroes.ird.fr/tmp",filename, ".rdf", sep="")
      buildRdf(store=store, rdf_file_path=rdf.filepath,               #rdf_subject="http://ecoscope.org/indicatorI4", 
               rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""), 
               titles=c("IRD Tuna Atlas: indicator #4 - monthly catches by ocean and by decade", 
                        "IRD Atlas thonier : indicateur #4 - captures mensuelle par océan et par décénie"),
               descriptions=c(paste(species.label, "monthly catches by ocean and by decade"), 
                              paste("Captures mensuelles de", species.label, "par océan et par décénie")),
               subjects=c(species.current, levels(current.df$ocean)),
               processes="http://www.ecoscope.org/ontologies/resources/processI4",
               data_output_identifier=plot.filepath,             
               start=as.character(min(current.df$year)),
               end=as.character(max(current.df$year)),
               spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))",
               withSparql)
      
      result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf_file_path))
    }
  }
  
  return(result.df)
}





