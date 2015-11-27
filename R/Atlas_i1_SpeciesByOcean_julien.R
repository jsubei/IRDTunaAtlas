# Atlas_i1_SpeciesByOcean_julien.R
#Tuna Atlas - IRD / MR EME
#
#This indicator produce a graph of annual catches by ocean for each species present in the input data. An associated RDF file is also produced.
##################################################################
#Norbert Billet - IRD
#2014/03/28: add the possibility to desactivate ecoscope sparql query
#2013/11/04: V2 version: add RDF export and allow production of multiple graph (i.e. species)
#2013/09/03: Norbert - Add attributes names as parameters
#2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
#2013/03/15: Norbert - Initial version
##################################################################
#Use example:
# library(IRDTunaAtlas)
# csv.df <- read.csv("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i1i2.csv", stringsAsFactors=FALSE)
# Atlas_i1_SpeciesByOcean_julien(csv.df, 
#                         yearAttributeName="year", 
#                         oceanAttributeName="ocean", 
#                         speciesAttributeName="species", 
#                         valueAttributeName="value")
# 
# OTHER EXAMPLE with WFS
# 
# require(IndicatorsForFisheries)
# df <- readData(connectionType="remote", dataType="WFS",
#                url="http://mdst-macroes.ird.fr:8080/constellation/WS/wfs/tuna_atlas",
#                layer="ns11:i1i2_mv", 
#                ogcFilter='')
# result <- Atlas_i1_SpeciesByOcean_julien(df=df, yearAttributeName="year",
#                                   oceanAttributeName="ocean",
#                                   speciesAttributeName="species",
#                                   valueAttributeName="value")
##################################################################
# a decommenter (rCharts)
# library(rCharts)
# library(jsonlite)

# library(rjson)
# library(dplyr)

# source("/home/tomcat7/temp/IRDTunaAtlas_OLD.R")
# source("/home/julien/SVNs/GIT/IRDTunaAtlas/R/IRDTunaAtlas_julien.R")

Atlas_i1_SpeciesByOcean_julien <- function(df, 
                                           yearAttributeName="year", 
                                           oceanAttributeName="ocean", 
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
  
  # 
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
  #   
  
  
  #   tableauResult <- data.frame(result=character())
  tableauResults <- data.frame(type="plot||download||map||..",
                               description="Rapport d'exécution du traitement i1",
                               stringsAsFactors=FALSE)   
  
  # tableauResult$results <- data.frame(titre=character(),
  tableauResult <- data.frame(titre=character(),
                              Description=character(),
                              uri=character(),
                              start=character(),
                              Metadata=character(),
                              image=character(),                  
                              radarPlots=character(),
                              dataTable=character(),
                              download=character(),
                              stringsAsFactors=FALSE)   
  
  
  listeResult <- list()
  # URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesByOcean/"
  
  URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesByOcean/default/"
  
  repository<-paste(mywd,"outputs/www/html/tmp/SpeciesByOcean/default/",sep="")
                    
  # repository<-"/data/www/html/tmp/SpeciesByOcean/"
  
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
    
    #build the plot
    resultPlot <- ggplot(aggData, aes(x=year, y=value, group=ocean)) + 
      geom_area(aes(fill=ocean), position="stack") + 
      geom_line(position="stack", color="grey20") + 
      scale_fill_manual(name="Ocean", values=my.colors) +
      xlab("Year") + ylab("Catches in thousand tons") + 
      ggtitle(paste(species.label, "catches by Ocean")) +
      theme(legend.position="bottom")
    
    #draw the plot
    #tempfile.base <- tempfile(pattern=paste("I1_", gsub(" ", "_", species.label), "_", sep=""),tmpdir="/data/www/html/tmp/SpeciesByOcean")
    filename <- paste("I1", gsub(" ", "_", species.label), sep="_")
    tempfile.base <- paste(repository,filename, sep="")
    plot.filepath <- paste(tempfile.base, ".png", sep="")
    plot.URLpng <- paste(URL,filename, ".png", sep="")
    ggsave(filename=plot.filepath, plot=resultPlot, dpi=300)
    
    ## AJOUT Julien RChart NVD3
    #plotRchartsNVD3 <- nPlot(value ~ year, group = 'ocean', data = aggData, type = 'line')
    plotRchartsNVD3 <- nPlot(value ~ year, group = 'ocean', data = aggData, type = 'stackedAreaChart', id = 'chart')
    #plotRchartsNVD3$addFilters("East Pacific O.", "Atlantic O.")
    #plotRchartsNVD3$addControls("group", value = "ocean", values = names(aggData$ocean[1:3]))
    plotRchartsNVD3$xAxis(axisLabel = 'Year')
    plotRchartsNVD3$yAxis(axisLabel = 'Catches')
    plotRchartsNVD3$chart(width = 800, height = 400, useInteractiveGuideline=TRUE)
    
    plotRchartsNVD3bis <- nPlot(value ~ year, group = 'ocean', data = aggData, type = 'lineChart')
    #     plotRchartsNVD3bis$xAxis(axisLabel = 'Year')
    #     plotRchartsNVD3bis$yAxis(axisLabel = 'Catches')
    #     plotRchartsNVD3bis$chart(width = 800, height = 400)
    
    
    #plotRchartsNVD3$addFilters("East Pacific O.", "Atlantic O.")
    #plotRchartsNVD3$addControls("group", value = "ocean", values = names(aggData$ocean[1:3]))
    
    
    ## AJOUT Julien RChart Highcharts
    plotRchartsHighcharts <- hPlot(value ~ year, data = aggData, type = c("line","scatter", "bubble"), group = "ocean", title = "Captures par espÃ¨ce et par ocÃ©an", subtitle = "species.label", size = "value", width = "100%")
    #     plotRchartsHighcharts$chart(width = 800, height = 400, zoomType = "xy")
    plotRchartsHighcharts$chart(zoomType = "xy")
    plotRchartsHighcharts$yAxis(title = list(text = "Captures"))
    plotRchartsHighcharts$exporting(enabled = T)
    plotRchartsHighcharts$legend(align = 'center', verticalAlign = 'top', y = 30, margin = 20)
    plotRchartsHighcharts$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif')), replace = F)
    plotRchartsHighcharts$plotOptions(column = list(stacking = "normal", dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))
    
    #plotRchartsHighcharts$addFilters("East Pacific O.", "Atlantic O.")
    #plotRchartsHighcharts$addControls("x", value = "value", values = names(aggData$ocean))
    #plotRchartsHighcharts$addControls("group", value = "FacVar1", values = names(simoriginal[, 1:3]))
    #plotRchartsHighcharts$addControls("y", value = "year", values = names(aggData$year[1:3]))
    #Display Plots
    #     plotRchartsHighcharts
    
    ## AJOUT Julien RChart Highcharts
    plotRchartsRickshaw <- Rickshaw$new()
    plotRchartsRickshaw$layer(value ~ year, data = aggData, group='ocean', type = 'area', title = "C'est de la balle")#, colors = 'steelblue', , subtitle = species.label
    #plotRchartsRickshaw$xAxis(type = 'Time')
    plotRchartsRickshaw$yAxis(orientation = 'right')
    #plotRchartsRickshaw$legend("De la bonne grosse lÃ©gende")
    plotRchartsRickshaw$set(width = 800, height = 400, legend = TRUE, slider = TRUE)
    #Rickshaw.Graph.JSONP
    #plotRchartsRickshaw$chart(zoomType = "xy") 
    
    #plotRchartsHighcharts$show('iframe', cdn = TRUE)
    
    #Datatable in HTML to be browsed online
    Datatable <- dTable(
      aggData,
      sPaginationType= "full_numbers"
    )    
    
    plot.filepathtml <- paste(tempfile.base, ".html", sep="")
    #{plotRchartsHighcharts results = "asis", comment = NA}
    #plotRchartsHighcharts$save(plot.filepathtml,include_assets = TRUE,cdn=TRUE)     
    plotRchartsHighcharts$save(plot.filepathtml,standalone=TRUE) 
    #     plotRchartsHighcharts$save(plot.filepathtml,chartId="julien", cdn=TRUE) 
    plot.URLhtml <- paste(URL,filename, ".html", sep="")
    
    plot.filepathtmlNVD3 <- paste(tempfile.base, "NVD3.html", sep="_")
    plotRchartsNVD3$save(plot.filepathtmlNVD3,standalone=TRUE)
    #     plotRchartsNVD3$save(plot.filepathtmlNVD3,chartId="julien", cdn=TRUE) 
    plot.URLhtmlNVD3 <- paste(URL,filename, "_NVD3.html", sep="")
    
    plot.filepathtmlNVD3bis <- paste(tempfile.base, "NVD3bis.html", sep="_")
    plotRchartsNVD3bis$save(plot.filepathtmlNVD3bis,standalone=TRUE)
    #     plotRchartsNVD3bis$save(plot.filepathtmlNVD3bis,chartId="julien", cdn=TRUE)
    plot.URLhtmlNVD3bis <- paste(URL,filename, "_NVD3bis.html", sep="")
    
    plot.filepathtmlRickshaw <- paste(tempfile.base, "Rickshaw.html", sep="_")
    plotRchartsRickshaw$save(plot.filepathtmlRickshaw,standalone=TRUE)
    #     plotRchartsRickshaw$save(plot.filepathtmlRickshaw,chartId="julien", cdn=TRUE)
    plot.URLhtmlRickshaw <- paste(URL,filename, "_Rickshaw.html", sep="")
    
    
    plot.filepathtmltable <- paste(tempfile.base, "table.html", sep="_")
    Datatable$save(plot.filepathtmltable,standalone=TRUE)     
    #     Datatable$save(plot.filepathtmltable,chartId="julien", cdn=TRUE)     
    plot.URLhtmlTable <- paste(URL,filename, "_table.html", sep="")    
    
    # #     Display Plots
    # Datatable
    
    # 
    #     #Metadata elements (in addition to OGC WPS metadata) to describe the current indicator which will be used by other applications (Ecoscope and Tuna Atlas Websites)
    #     titles=c(paste("IRD Tuna Atlas: indicator #1 - catches for",species.label, "species by ocean", sep=" "),
    #              paste("IRD Atlas thonier : indicateur #1 - captures par océan pour l'espèce ",species.label, sep=" "))
    #     titre=paste("Espèce:",species.label, sep=" ")
    #     descriptions=c(paste(species.label, "catches by ocean"),
    #                    paste("Captures de", species.label, "par océan"))
    #     Description=paste("IRD Tuna Atlas: indicator #1 - catches of species",species.label,"by ocean", sep=" ")
    #     subjects=c(as.character(species.current))
    #     #Collect the URIs of related Topics from Ecoscope SPARQL endpoint
    #     URI <- FAO2URIFromEcoscope(as.character(species.current))
    #     tabURIs<- data.frame(type="speciesJulien",URI=URI,stringsAsFactors=FALSE)
    # 
    #     #TODO julien => A ADAPTER AVEC LA CONVEX HULL / ou la collection DE TOUTES LES GEOMETRIES CONCERNEES
    #     spatial_extent="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))"
    #     temporal_extent_begin=as.character(min(aggData$year))
    #     temporal_extent_end=as.character(max(aggData$year))
    #     
    #     rdf.filepath <- paste(repository, "La_totale.rdf", sep="")
    #     rdf.URL <- paste(URL,filename, ".rdf", sep="")
    # # il faudrait ajouter un attribut qui précise le type de visualisation: carte, chart...
    # 
    # data_output_identifiers=data.frame(titre="2 en fait y a pas besoin de cet attribut",type="stackedArea",year=temporal_extent_begin, fileURL=plot.URLhtml,stringsAsFactors=FALSE)
    # ligne <- c(titre="3 en fait y a pas besoin de cet attribut",type="stackedArea",year=temporal_extent_begin,fileURL=plot.URLhtmlNVD3)
    # data_output_identifiers <- rbind(data_output_identifiers, ligne)
    # ligne <- c(titre="4 en fait y a pas besoin de cet attribut",type="stackedArea",year=temporal_extent_begin,fileURL=plot.URLhtmlNVD3bis)
    # data_output_identifiers <- rbind(data_output_identifiers, ligne)
    # ligne <- c(titre="5 en fait y a pas besoin de cet attribut",type="stackedArea",year=temporal_extent_begin,fileURL=plot.URLhtmlRickshaw)
    # data_output_identifiers <- rbind(data_output_identifiers, ligne)
    # ligne <- c(titre="6 en fait y a pas besoin de cet attribut",type="dataTable",year=temporal_extent_begin,fileURL=plot.URLhtmlTable)
    # data_output_identifiers <- rbind(data_output_identifiers, ligne)
    # # map|lines|pies|radarPlots
    # # ligneTableauResult$uri=list(data.frame(typeURI="Species",URI=URI))
    # 
    # download=data.frame(format="csv",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByOcean/XXX.csv", stringsAsFactors=FALSE)
    # ligne <- c(format="shp",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByOcean/XXX.shp")
    # download <- rbind(download, ligne)
    # ligne <- c(format="GML|WKT|shp|netCDF",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByOcean/XXX.nc....")
    # download <- rbind(download, ligne)
    # 
    # #Write the RDF metadata describing the current indicator in the RDF model of the whole execution: used by Ecoscope and Tuna Atlas
    # #Write the Json metadata used by the SIP
    # # tableauResult <- buildRdf(store=store, tableauResult = tableauResult,
    #              RDFMetadata=rdf.URL,
    #              rdf_file_path=rdf.filepath,
    #              rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""), 
    #              #rdf_subject="http://ecoscope.org/indicatorI1", 
    #              titles=titles,
    #              descriptions=descriptions,
    #              subjects=subjects,
    #              tabURIs=tabURIs,             
    #              processes="http://www.ecoscope.org/ontologies/resources/processI1",
    #              image=plot.URLpng,
    #              data_output_identifiers=data_output_identifiers,
    #              download=download,
    #              start=temporal_extent_begin,
    #              end=temporal_extent_end,
    #              spatial=spatial_extent,
    #              withSparql)
    # 
    # # result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf.filepath))
    # 
  }
  
  
  # julien<-buildJson(type="plot||download||map||..",
  #                   description="Rapport d'exécution du traitement i1",
  #                   processSourceCode="http://mdst-macroes.ird.fr:8084/wps//R/scripts/toto_wps.R",
  #                   results=tableauResult)
  
  listeResult<-aggData
#   listeResult<-list(listeResult,resultPlot)
  return(aggData)
    
}