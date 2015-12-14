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
# library(IndicatorsForFisheries)
# csv.df <- read.csv("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i1i2.csv", stringsAsFactors=FALSE)
# Atlas_i2_SpeciesByGear(csv.df, 
#                         yearAttributeName="year", 
#                         gearTypeAttributeName="gear_type", 
#                         speciesAttributeName="species", 
#                         valueAttributeName="value")
##################################################################
# library(rCharts)
# library(jsonlite)

#  source("/home/tomcat7/temp/IRDTunaAtlas.R")
# source("/home/julien/SVNs/GIT/IRDTunaAtlas/R/IRDTunaAtlas_julien.R")
Atlas_i2_SpeciesByGear_julien <- function(df, 
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
  
  
  #List to store URLs of the set of files generated for each species
  liste <- list()
  #Julien comment rrdf
#   store = new.rdf(ontology=FALSE)
#   add.prefix(store,
#              prefix="resources_def",
#              namespace="http://www.ecoscope.org/ontologies/resources_def/")
#   add.prefix(store,
#              prefix="ical",
#              namespace="http://www.w3.org/2002/12/cal/ical/")
#   add.prefix(store,
#              prefix="dct",
#              namespace="http://purl.org/dc/terms/")
  
  
  # tableauResult$results <- data.frame(titre=character(),
  tableauResult <- data.frame(stringsAsFactors=FALSE)   
  
  URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesByGear/default/"
  repository<-paste(mywd,"outputs/www/html/tmp/SpeciesByGear/default/",sep="")
                
# URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesByGear/cdn/"
# repository<-"/data/www/html/tmp/SpeciesByGear/cdn/"

  #TODO : mcforeach ?
  for (species.current in unique(df$species)) {
    current.df <- df[df$species == species.current,]
    
    #aggregate values by years and gear type
    aggData <- aggregate(value ~ gear_type + year, data=current.df, sum)
    
    #convert values from tons to thousand tons
    aggData$value <- aggData$value / 1000
    
    #order factors levels by value
    aggData$gear_type <- factor(aggData$gear_type, levels=rev(levels(reorder(aggData$gear_type, aggData$value))))
    #Julien comment rrdf
#     
#     if (withSparql) {      
#       #get species scientific name from ecoscope sparql
#       sparqlResult <- getSpeciesFromEcoscope(as.character(species.current))
#       if (length(sparqlResult) > 0) {
#         species.label <- sparqlResult[1,"scientific_name"]
#         species.URI <- sparqlResult[1,"uri"]
#       } else {
#         species.label <- species.current
#         species.URI <- species.current
#       } 
#     } else {
#       species.label <- species.current
#       species.URI <- species.current
#     }
    species.label <- species.current
    species.URI <- species.current
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
    
    #tempfile.base <- tempfile(pattern=paste("I2", gsub(" ", "_", species.label), as.character(min(aggData$year)), as.character(max(aggData$year)), "_", sep="_"), tmpdir="")
#filename <- tempfile(pattern=paste("I2", gsub(" ", "_", species.label), "_", sep=""),tmpdir="")
    filename <- paste("I2", gsub(" ", "_", species.label), sep="_")
    tempfile.base <- paste(repository,filename, sep="")
    plot.filepath <- paste(tempfile.base, ".png", sep="")
    plot.URLpng <- paste(URL,filename, ".png", sep="")
    ggsave(filename=plot.filepath, plot=resultPlot, dpi=300)

    ## AJOUT Julien RChart
    #p8 <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarHorizontalChart')
    #p8$chart(showControls = F)
    #p8  <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarChart')
#     plotRchartsHighcharts  <- hPlot(value ~ year, group = 'gear_type', data = aggData, type = c("column","line","scatter", "bubble"), radius = 6, size='value')
#     plotRchartsHighcharts$plotOptions(column = list(dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))

plotRchartsHighcharts  <- hPlot(value ~ year, data = aggData, type = 'column', group = 'gear_type', radius = 6, title = "Catches per month per fishing gear",width = "100%")
plotRchartsHighcharts$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif')), replace = F)
plotRchartsHighcharts$plotOptions(column = list(stacking = "normal", dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))
plotRchartsHighcharts$legend(align = 'center', verticalAlign = 'top', y = 30, margin = 20)
# plotRchartsHighcharts$chart(width = 800,height = 400, zoomType = "xy")
plotRchartsHighcharts$chart(zoomType = "xy")
plotRchartsHighcharts$exporting(enabled = T)
plotRchartsHighcharts 

    
    ## {title: MultiBar Chart}
plotRchartsNVD3 <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarChart', width = 800, height = 400)
# plotRchartsNVD3 <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarChart', width = "100%")
#     plotRchartsNVD3$chart(width = 800, height = 400, color = c('brown', 'blue', '#594c26', 'green'), useInteractiveGuideline=TRUE)
    plotRchartsNVD3$xAxis(axisLabel = 'Year')
    plotRchartsNVD3$yAxis(axisLabel = 'Catches')
# plotRchartsNVD3$chart(width = 800, height = 400, useInteractiveGuideline=TRUE)

    #plotRchartsNVD3$chart(useInteractiveGuideline=TRUE)
    plotRchartsNVD3

# plotRchartsNVD3 <- nPlot(value ~ month, group='gear_type', data = current.df, type = 'multiChart')
  
    ## Dataset in HTML
    Datatable <- dTable(
      aggData,
      sPaginationType= "full_numbers"
    )
    Datatable


  ## Storage of files in a given repository (temporary or permanent)
  plot.filepathtml <- paste(tempfile.base, ".html", sep="")
  plotRchartsHighcharts$save(plot.filepathtml,standalone=TRUE) 
#   plotRchartsHighcharts$save(plot.filepathtml,cdn=TRUE)
  plot.URLRchartsHighcharts <- paste(URL, filename, ".html", sep="")

  plot.filepathtmlNVD3 <- paste(tempfile.base, "NVD3.html", sep="")
  plotRchartsNVD3$save(plot.filepathtmlNVD3,standalone=TRUE) 
#   plotRchartsNVD3$save(plot.filepathtmlNVD3,cdn=TRUE)
  plot.URLRchartsNVD3 <- paste(URL, filename, "_NVD3.html", sep="")

  plot.filepathtmltable <- paste(tempfile.base, "table.html", sep="_")
  Datatable$save(plot.filepathtmltable,standalone=TRUE)     
#   Datatable$save(plot.filepathtmltable,cdn=TRUE)     
  plot.URLhtmlTable <- paste(URL,filename, "_table.html", sep="")    


################################################################################################


# ligne <- data.frame(TYPE="URI", URL=URI,  stringsAsFactors=FALSE)


################################################################################################



#Metadata elements (in addition to OGC WPS metadata) to describe the current indicator which will be used by other applications (Ecoscope and Tuna Atlas Websites)


titles=c(paste(species.label, "catches by gear type"), 
               paste("Captures de", species.label, "par type d'engin"))


descriptions=c(c("en",paste("IRD Tuna Atlas: indicator #2 - catches for",species.label, "species: catches by species and by gear type", sep=" ")),
         c("fr",paste("IRD Atlas Thonier: indicator #2 - Captures par type d'engins de pêche pour l'espèce",species.label, sep=" ")))
         

subjects=c(as.character(species.current), as.character(unique(current.df$gear_type)))

#Collect the URIs of related Topics from Ecoscope SPARQL endpoint
# URI <- FAO2URIFromEcoscope(as.character(species.current))
# tabURIs<- data.frame(type="species",URI=URI,stringsAsFactors=FALSE)

# for (gear_type.current in unique(df$gear_type)) {

# URIGear <- FAO2URIFromEcoscope(as.character(unique(current.df$gear_type)))
# ligne<- c(x="gear",y=URIGear)
# tabURIs<- rbind(tabURIs,ligne)

# }
# EVENTUELLEMENT AJOUTER D'AUTRES SUJETS COMME LA ZONE
#subjects=c(as.character(species.current), as.character(gear_type.current), as.character(unique(current.df$gear_type))),
#subjects=c(as.character(species.current)),
#subjects=c(as.character(species.current), as.character(gear_type.current)),
#data_input=url,

#TODO julien => A ADAPTER AVEC LA CONVEX HULL / ou la collection DE TOUTES LES GEOMETRIES CONCERNEES
spatial_extent="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))"
temporal_extent_begin=as.character(min(aggData$year))
temporal_extent_end=as.character(max(aggData$year))


rdf.filepath <- paste(repository, ".rdf", sep="")
#rdf.filepath <- paste(tempfile.base, ".rdf", sep="")
rdf.URL <- paste(URL,filename, ".rdf", sep="")
# il faudrait ajouter un attribut qui précise le type de visualisation: carte, chart...
data_output_identifiers=data.frame(titre="1 en fait y a pas besoin de cet attribut",type="image",year=temporal_extent_begin, fileURL=plot.URLpng, stringsAsFactors=FALSE)
ligne <- c(titre="2 en fait y a pas besoin de cet attribut",type="stackedArea", year=temporal_extent_begin, fileURL=plot.URLRchartsHighcharts)
data_output_identifiers <- rbind(data_output_identifiers, ligne)
ligne <- c(titre="3 en fait y a pas besoin de cet attribut",type="map|lines|pies|radarPlots",year=temporal_extent_begin, fileURL=plot.URLRchartsNVD3)
data_output_identifiers <- rbind(data_output_identifiers, ligne)
ligne <- c(titre="6 en fait y a pas besoin de cet attribut",type="dataTable",year=temporal_extent_begin, fileURL=plot.URLhtmlTable)
data_output_identifiers <- rbind(data_output_identifiers, ligne)

# ligneTableauResult$uri=list(data.frame(typeURI="Species",URI=URI))

download=data.frame(format="csv",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.csv", stringsAsFactors=FALSE)
ligne <- c(format="shp",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.shp")
download <- rbind(download, ligne)
ligne <- c(format="GML|WKT|shp|netCDF",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.nc....")
download <- rbind(download, ligne)


#Write the RDF metadata describing the current indicator in the RDF model of the whole execution: used by Ecoscope and Tuna Atlas
#Write the Json metadata used by the SIP
# tableauResult$results <- data.frame(titre=character(),


# tableauResult <- buildRdf(store=store,
#                           tableauResult = tableauResult,
#                           RDFMetadata=rdf.URL,
#                           rdf_file_path=rdf.filepath,
#                           rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""), 
#                           #rdf_subject="http://ecoscope.org/indicatorI1", 
#                           titles=titles,
#                           descriptions=descriptions,
#                           subjects=subjects,
#                           tabURIs=tabURIs,
#                           processes="http://www.ecoscope.org/ontologies/resources/processI2",
#                           image=plot.URLpng,
#                           data_output_identifiers=data_output_identifiers,
#                           download=download,
#                           start=temporal_extent_begin,
#                           end=temporal_extent_end,
#                           spatial=spatial_extent,
#                           withSparql)

# result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf.filepath))

#################################################################################################

  }
# julien<-buildJson(type="bar Chart", description="Rapport d'exécution du traitement i2",processSourceCode="http://mdst-macroes.ird.fr:8084/wps//R/scripts/Atlas_i2_SpeciesByOcean_HighCharts.R",results=tableauResult)

listeResult<-list("data"=aggData,"species"=species.label, "colors"=my.colors)
return(listeResult)

    

}
