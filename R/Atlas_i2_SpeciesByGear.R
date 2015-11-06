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
# library(rCharts)
# source("/home/julien/SVNs/GIT/IRDTunaAtlas/R/IRDTunaAtlas_julien.R")

Atlas_i2_SpeciesByGear <- function(df, 
                                   yearAttributeName="year", 
                                   speciesAttributeName="species",
                                   gearTypeAttributeName="gear_type",
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
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot found year attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
  
  if(sum(names(df) == gearTypeAttributeName) == 0) {
    stop("Cannot found gear attribute")
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot found value attribute")
  }  
  
  #format columns  
  df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  df[, gearTypeAttributeName] <- as.factor(df[, gearTypeAttributeName])
  df[, valueAttributeName] <- as.numeric(df[, valueAttributeName])    
  
  #aggregate to cut other columns
  df <- aggregate(x=df[, valueAttributeName], 
                  by=list(df[, yearAttributeName], df[, speciesAttributeName], df[, gearTypeAttributeName]), 
                  FUN=sum)
  #rename columns
  names(df) <- c("year", "species", "gear_type", "value")
    
  #test if FAO usual gear codes are used
  #if (length(intersect(levels(df$gear_type), c("BB", "GILL", "LL", "PS", "OTHER_I", "OTHER_A", "TROL", "TRAP"))) == length(levels(df$gear_type))) {
  #  df$gear_type <- factor(df$gear_type, levels=c("BB", "GILL", "LL", "PS", "OTHER_I", "OTHER_A", "TROL", "TRAP"), labels=c("Baitboat", "Gillnet", "Longline", "Purse seine", "Unclass. art. Indian O.", "Unclass. art. Atl. O.", "Trol.", "Trap"))
  #}
  
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
    
    #TODO : mcforeach ?
#     for (gear_type.current in unique(df$gear_type)) {
#       current.df <- df[df$gear_type == gear_type.current,]
#       
#       
      #get getFishingGearId  current name and URI from ecoscope sparql
#       sparqlResult <- getFishingGearIdFromEcoscope(as.character(gear_type.current))
#       if (length(sparqlResult) > 0) {
#         gear_type.label <- sparqlResult[1,"label"]
#         gear_type.URI <- sparqlResult[1,"uri"]
#       } else {
#         gear_type.label <- gear_type.current
#         gear_type.URI <- gear_type.current
#       }
#       }    

    
    
    
    
    
    
    #build the plot
    resultPlot <- ggplot(aggData, aes(x=year, y=value, fill=gear_type, order=gear_type)) + 
      geom_bar(stat="identity", width=0.8) + 
      geom_bar(stat="identity", width=0.8, colour="grey20", show_guide=FALSE) + 
      scale_fill_manual(name="Gear type", values=my.colors) +
      xlab("Year") + ylab("Catches in thousand tons") + 
      ggtitle(paste(species.label, "catches by gear type")) +
      theme(legend.position="bottom")

    #draw the plot
    
#     tempfile.base <- tempfile(pattern=paste("I2", gsub(" ", "_", species.label), as.character(min(aggData$year)), as.character(max(aggData$year)), "_", sep="_"), tmpdir="")
    tempfile.base <- paste("/data/www/html/tmp",filename, sep="")
    filename <- tempfile(pattern=paste("I2", gsub(" ", "_", species.label), "_", sep=""),tmpdir="")


    #plot.filepath <- paste(tempdir(), tempfile.base, ".png", sep="")
    plot.filepath <- paste(tempdir(), tempfile.base, ".png", sep="")
    ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)


## AJOUT Julien RChart

#p8 <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarHorizontalChart')
#p8$chart(showControls = F)
#p8  <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarChart')
plotRchartsHighcharts  <- hPlot(value ~ year, group = 'gear_type', data = aggData, type = 'column', radius = 6)
plotRchartsHighcharts$plotOptions(column = list(dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))
plotRchartsHighcharts$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif')), replace = F)
plotRchartsHighcharts 


## {title: MultiBar Chart}
plotRchartsNVD3 <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'column')
plotRchartsNVD3$chart(color = c('brown', 'blue', '#594c26', 'green'))
plotRchartsNVD3

plotRchartsHighcharts

Datatable <- dTable(
  aggData,
  sPaginationType= "full_numbers"
)

Datatable

plot.filepathtml <- paste(tempfile.base, ".html", sep="")
plot.URLhtml <- paste("http://mdst-macroes.ird.fr/tmp",filename, ".html", sep="")
plot.filepathtmlNVD3 <- paste(tempfile.base, "_NVD3.html", sep="")
plotRchartsHighcharts$save(plot.filepathtml,standalone=TRUE) 
plotRchartsNVD3$save(plot.filepathtmlNVD3,standalone=TRUE) 
plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
Datatable$save(plot.filepathtmltable,standalone=TRUE)     
plot.URLhtmlTable <- paste("http://mdst-macroes.ird.fr/tmp",filename, "_table.html", sep="")    



    #create the RDF metadata
    rdf.filepath <- paste(tempdir(), tempfile.base, ".rdf", sep="")
    buildRdf(rdf_file_path=rdf.filepath,
             rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""), 
             titles=c(c("en", "IRD Tuna Atlas: indicator #2 - catches by species and by gear type"), 
                      "IRD Atlas thonier : indicateur #2 - captures par espèces et par type d'engin"),
             descriptions=c(paste(species.label, "catches by gear type"), 
                            paste("Captures de", species.label, "par type d'engin")),
             # EVENTUELLEMENT AJOUTER D'AUTRES SUJETS COMME LA ZONE
             #subjects=c(as.character(species.current), as.character(gear_type.current), as.character(unique(current.df$gear_type))),
             subjects=c(as.character(species.current), as.character(unique(current.df$gear_type))),
             #subjects=c(as.character(species.current)),
             #subjects=c(as.character(species.current), as.character(gear_type.current)),
             processes="http://www.ecoscope.org/ontologies/resources/processI2",
             #data_input=url,
             data_output_identifier=plot.filepath,
             start=as.character(min(aggData$year)),
             end=as.character(max(aggData$year)),
             #TODO julien => A ADAPTER AVEC LA CONVEX HULL / ou la collection DE TOUTES LES GEOMETRIES CONCERNEES
             spatial="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))",
             withSparql)
    
    result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf.filepath))
  }

  return(result.df)
}