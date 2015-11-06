
# Atlas_i9_RelativeSizeFrequenciesBySchoolType.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a graph relative contribution of size frequencies in catches for a species by school type. An associated RDF file is also produced.
##################################################################
# Norbert Billet - IRD
# 2013/11/04: Norbert - Add RDF export and allow production of multiple maps (i.e. species)
# 2013/09/03: Norbert - Add attributes names as parameters
# 2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
# 2013/06/12: Norbert - First version
##################################################################
# Use example:
#  library(IRDTunaAtlas)
#  csv.df <- read.csv("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i9i10.csv", stringsAsFactors=FALSE)
#  csv.df <- csv.df[csv.df$species == "ALB",]
#  Atlas_i9_RelativeSizeFrequenciesBySchoolType(csv.df,                       
#                        yearAttributeName="year",
#                        speciesAttributeName="species",
#                        schoolAttributeName="school",
#                        sizeClassLowerBoundAttributeName="class_low",
#                        sizeClassUpperBoundAttributeName="class_up",
#                        fishCountAttributeName="fish_count")
##################################################################
# library(rCharts)
# library(jsonlite)

# source("/home/tomcat7/temp/IRDTunaAtlas.R")
# source("/home/julien/SVNs/GIT/IRDTunaAtlas/R/IRDTunaAtlas_julien.R")
Atlas_i9_RelativeSizeFrequenciesBySchoolType_julien <- function(df,
                                                         yearAttributeName="year",
                                                         speciesAttributeName="species",
                                                         schoolAttributeName="school",
                                                         sizeClassLowerBoundAttributeName="class_low",
                                                         sizeClassUpperBoundAttributeName="class_up",
                                                         fishCountAttributeName="fish_count",
                                                         withSparql=TRUE)
{
  if (! require(ggplot2) | ! require(RColorBrewer)) {
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
  
  if(sum(names(df) == schoolAttributeName) == 0) {
    stop("Cannot found school type attribute")
  }  
  
  if(sum(names(df) == sizeClassLowerBoundAttributeName) == 0) {
    stop("Cannot found size class lower bound attribute")
  }  
  
  if(sum(names(df) == sizeClassUpperBoundAttributeName) == 0) {
    stop("Cannot found size class upper bound attribute")
  }
  
  if(sum(names(df) == fishCountAttributeName) == 0) {
    stop("Cannot found fish count attribute")
  }  
  
  
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
  
  #define the result df
  result.df <- c()

  
  
  # tableauResult$results <- data.frame(titre=character(),
  tableauResult <- data.frame(stringsAsFactors=FALSE)   
  
    URL<-"http://mdst-macroes.ird.fr/tmp/RelativeSizeFrequenciesBySchoolType/cdn/"
    repository<-"/data/www/html/tmp/RelativeSizeFrequenciesBySchoolType/cdn/"
#   URL<-"http://mdst-macroes.ird.fr/tmp/RelativeSizeFrequenciesBySchoolType/"
#   repository<-"/data/www/html/tmp/RelativeSizeFrequenciesBySchoolType/"
  
  
  
  #format columns
  df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  df[, schoolAttributeName] <- as.factor(df[, schoolAttributeName])
  df[, sizeClassLowerBoundAttributeName] <- as.numeric(df[, sizeClassLowerBoundAttributeName])    
  df[, sizeClassUpperBoundAttributeName] <- as.numeric(df[, sizeClassUpperBoundAttributeName])
  df[, fishCountAttributeName] <- as.numeric(df[, fishCountAttributeName])    
  
  #rename columns
  names(df)[which(names(df) == yearAttributeName)] <- "year"  
  names(df)[which(names(df) == speciesAttributeName)] <- "species"
  names(df)[which(names(df) == schoolAttributeName)] <- "school"
  names(df)[which(names(df) == sizeClassLowerBoundAttributeName)] <- "sizeClassLowerBound"
  names(df)[which(names(df) == sizeClassUpperBoundAttributeName)] <- "sizeClassUpperBound"
  names(df)[which(names(df) == fishCountAttributeName)] <- "fishCount"
  
  
  
  #test if usual school codes are used
  if (length(intersect(levels(df$school), c("IND", "BO", "BL"))) == length(levels(df$school))) {
    df$school <- factor(df$school, levels=c("IND", "BO", "BL"), labels=c("Undefined school", "Log school", "Free school"))
  }
  
  #setup the palette
  my.colors <- brewer.pal(length(levels(df$school)), "Set1")
  names(my.colors) <- levels(df$school)
  
  #plot fct
  plotFct <- function(subDf, species.label, species.current, tableauResult, store, lims=c()) {
    #aggregate values by size class and school type
    valuesSum <- aggregate(fishCount ~ sizeClassLowerBound + sizeClassUpperBound + school, data=subDf, FUN=sum)
    valuesSum$relative <- (valuesSum$fishCount / sum(valuesSum$fishCount)) * 100
#     mergedDf <- data.frame(sizeClass=valuesSum$sizeClass, school=valuesSum$school, relative=valuesSum$fishCount / sum(valuesSum$fishCount))
#     mergedDf$relative <- mergedDf$relative * 100
    
    #plot title
    if (min(subDf$year) == max(subDf$year)) {
      my.title <- paste(species.label , " size frequencies by school type for ",  min(subDf$year), sep="")
    } else {
      my.title <- paste(species.label , " size frequencies by school type for ",  min(subDf$year), "-",  max(subDf$year), sep="")
    }
    
    #build the plot
#     resultPlot <- ggplot(aggData, aes(x=year, y=value, fill=gear_type, order=gear_type)) + 
  
      resultPlot <- ggplot(mapping=aes(fill=school, order=school)) +
      geom_rect(data=valuesSum, mapping=aes(xmin = sizeClassLowerBound, xmax = sizeClassUpperBound, ymin = 0, ymax = relative), colour="grey25") +
      scale_fill_manual(name="School type", values=my.colors) +
      xlab("Size (in cm)") + ylab("Relative contribution (in %)") + 
      labs(colour="School type", title=my.title) +
      theme(legend.position="bottom")
    
    if (length(lims) == 4) {
      resultPlot <- resultPlot + scale_x_continuous(limits=c(lims[1], lims[2])) + scale_y_continuous(limits=c(lims[3], lims[4]))
    }
    

    #draw the plot
    filename <- paste("I9_", gsub(" ", "_", species.label),"_",as.character(min(subDf$year)), "-", as.character(max(subDf$year)), sep="")
    tempfile.base <- paste(repository,filename, sep="")
    plot.filepath <- paste(tempfile.base, ".png", sep="")
    plot.URLpng <- paste(URL,filename, ".png", sep="")
    ggsave(filename=plot.filepath, plot=resultPlot, width=20, unit="cm", dpi=300)




    
    ## AJOUT Julien RChart
    #p8 <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarHorizontalChart')
    #p8$chart(showControls = F)
    #p8  <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarChart')
    plotRchartsHighcharts  <- hPlot(relative ~ sizeClassLowerBound, group = 'school', data = valuesSum, type = 'column', radius = 6)
#     plotRchartsHighcharts$plotOptions(column = list(dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))
#     plotRchartsHighcharts$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif')), replace = F)
    plotRchartsHighcharts 
    
    

    plotRchartsHighchartsbis <- nPlot(relative ~ sizeClassLowerBound, group = 'school', data = valuesSum, type = 'multiBarChart')
    #plotRchartsHighchartsbis$chart(showControls = F)
    plotRchartsHighchartsbis



#     ## {title: MultiBar Chart}
#     plotRchartsNVD3 <- nPlot(value ~ year, group = 'gear_type', data = aggData, type = 'multiBarChart')
#     plotRchartsNVD3$chart(color = c('brown', 'blue', '#594c26', 'green'))
#     plotRchartsNVD3$xAxis(axisLabel = 'Year')
#     plotRchartsNVD3$yAxis(axisLabel = 'Catches')
#     #plotRchartsNVD3$chart(useInteractiveGuideline=TRUE)
#     plotRchartsNVD3
    
    ## Dataset in HTML
    Datatable <- dTable(
      valuesSum,
      sPaginationType= "full_numbers"
    )
    Datatable
    
    
    ## Storage of files in a given repository (temporary or permanent)
plot.filepathtml <- paste(tempfile.base, ".html", sep="")
plot.filepathtmlbis <- paste(tempfile.base, "bis.html", sep="")
plot.URLhtml <- paste("http://mdst-macroes.ird.fr/tmp",filename, ".html", sep="")
#     plot.filepathtmlNVD3 <- paste(tempfile.base, "_NVD3.html", sep="")
# plotRchartsHighcharts$save(plot.filepathtml,standalone=TRUE) 
plotRchartsHighcharts$save(plot.filepathtml,cdn=TRUE) 
# plotRchartsHighchartsbis$save(plot.filepathtmlbis,standalone=TRUE) 
plotRchartsHighchartsbis$save(plot.filepathtmlbis,cdn=TRUE) 
#     plotRchartsNVD3$save(plot.filepathtmlNVD3,standalone=TRUE) 
    plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
# Datatable$save(plot.filepathtmltable,standalone=TRUE)     
Datatable$save(plot.filepathtmltable,cdn=TRUE)     
plot.URLhtmlTable <- paste("http://mdst-macroes.ird.fr/tmp",filename, "_table.html", sep="")    


#Datatable
plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
plot.URLhtmlTable <- paste(URL,filename, "_table.html", sep="")    
# Datatable$save(plot.filepathtmltable,standalone=TRUE)     
      Datatable$save(plot.filepathtmltable,cdn=TRUE)       


################################################################################################


# ligne <- data.frame(TYPE="URI", URL=URI,  stringsAsFactors=FALSE)


################################################################################################


#Metadata elements (in addition to OGC WPS metadata) to describe the current indicator which will be used by other applications (Ecoscope and Tuna Atlas Websites)

titles=c(paste(species.label, ": relative contribution of size frequencies in catches for a species by school type"), 
         paste("Captures de", species.label, ": contributions des classes de tailles par type de banc"))


descriptions=c(c("en", paste("IRD Tuna Atlas: indicator #9 - Size frequencies contribution to catches for species ",species.label, "by school type", as.character(min(subDf$year)), as.character(max(subDf$year)), sep="-")),
               c("fr", paste("IRD Atlas Thonier: indicator #9 - Contributions des classes de tailles aux captures de l'espèce:",species.label, "par type de banc", as.character(min(subDf$year)), as.character(max(subDf$year)), sep="-")))


subjects=c(as.character(species.current))
#Collect the URIs of related Topics from Ecoscope SPARQL endpoint
URI <- FAO2URIFromEcoscope(as.character(species.current))
tabURIs<- data.frame(type="species",URI=URI,stringsAsFactors=FALSE)
rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep="")               


#TODO julien => A ADAPTER AVEC LA CONVEX HULL / ou la collection DE TOUTES LES GEOMETRIES CONCERNEES
spatial_extent="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))"
temporal_extent_begin=as.character(min(subDf$year))
temporal_extent_end=as.character(max(subDf$year))

#create the RDF metadata
rdf.filepath <- paste(repository, "La_totale.rdf", sep="")
rdf.URL <- paste(URL,filename, ".rdf", sep="")


download=data.frame(format="csv",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.csv", stringsAsFactors=FALSE)
ligne <- c(format="shp",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.shp")
download <- rbind(download, ligne)
ligne <- c(format="GML|WKT|shp|netCDF",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.nc....")
download <- rbind(download, ligne)

data_output_identifiers=data.frame(titre="1 en fait y a pas besoin de cet attribut",type="image",year=temporal_extent_begin, fileURL=plot.filepath, stringsAsFactors=FALSE)
ligne <- c(titre="2 en fait y a pas besoin de cet attribut",type="bar", year=temporal_extent_begin, fileURL=plot.filepathtml)
data_output_identifiers <- rbind(data_output_identifiers, ligne)
ligne <- c(titre="3 en fait y a pas besoin de cet attribut",type="bar",year=temporal_extent_begin, fileURL=plot.filepathtmlbis)
data_output_identifiers <- rbind(data_output_identifiers, ligne)
ligne <- c(titre="4 en fait y a pas besoin de cet attribut",type="dataTable",year=temporal_extent_begin, fileURL=plot.URLhtmlTable)
data_output_identifiers <- rbind(data_output_identifiers, ligne)




one <-list(tableauResult = tableauResult,
           RDFMetadata=rdf.URL,
           rdf_file_path=rdf.filepath,
           rdf_subject=rdf_subject, 
           titles=titles,
           descriptions=descriptions,
           subjects=subjects,
           tabURIs=tabURIs,
           processes="http://www.ecoscope.org/ontologies/resources/processI9",
           image=plot.URLpng,
           data_output_identifiers=data_output_identifiers,
           download=download,
           start=temporal_extent_begin,
           end=temporal_extent_end,
           spatial=spatial_extent,
           withSparql=withSparql)

return(one)  

  }
  



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
    
    #species.df <- df[df$species == species.current,]
    species.df <- aggregate(fishCount ~ sizeClassLowerBound + sizeClassUpperBound + school + year, data=df[df$species == species.current,], FUN=sum)
    
    #plot for all the period
    one <- plotFct(species.df, species.label, species.current, tableauResult, store)
    tableauResult <- buildRdf(store,
                               one$tableauResult,
                               one$RDFMetadata,
                               one$rdf_file_path,
                               one$rdf_subject, 
                               one$titles,
                               one$descriptions,
                               one$subjects,
                               one$tabURIs,
                               one$processes,
                               one$image,
                               one$data_output_identifiers,
                               one$download,
                               one$start,
                               one$end,
                               one$spatial,
                               one$withSparql)  
    
    years <- unique(species.df$year)
    if (length(years) > 1)
    {      
      contrib.max <- max(unlist(lapply(years, FUN=function(x) {max((species.df[species.df$year == x,]$fishCount / sum(species.df[species.df$year == x,]$fishCount)) * 100)})))
      sizeClass.range <- range(species.df$sizeClassLowerBound, species.df$sizeClassUpperBound)
      #for each year
      for(year.current in years) {
        one <- plotFct(species.df[species.df$year==year.current,], species.label, species.current, tableauResult, store,lims=c(sizeClass.range[1], sizeClass.range[2], 0, contrib.max))
        tableauResult <- buildRdf(store,
                                   one$tableauResult,
                                   one$RDFMetadata,
                                   one$rdf_file_path,
                                   one$rdf_subject, 
                                   one$titles,
                                   one$descriptions,
                                   one$subjects,
                                   one$tabURIs,
                                   one$processes,
                                   one$image,
                                   one$data_output_identifiers,
                                   one$download,
                                   one$start,
                                   one$end,
                                   one$spatial,
                                   one$withSparql)
      }
      
      #for each decade
      species.df$decade <- species.df$year - (species.df$year %% 10)
      decades <- unique(species.df$decade)
      if (length(decades) > 1)
      {
        species.decade.df <- aggregate(fishCount ~ sizeClassLowerBound + sizeClassUpperBound + school + decade, data=species.df, FUN=sum)
        contrib.max <- max(unlist(lapply(decades, FUN=function(x) {max((species.decade.df[species.decade.df$decade == x,]$fishCount / sum(species.decade.df[species.decade.df$decade == x,]$fishCount)) * 100)})))
        for(decade.current in decades) {
          one <- plotFct(species.df[species.df$decade==decade.current,], species.label, species.current, tableauResult, store,lims=c(sizeClass.range[1], sizeClass.range[2], 0, contrib.max))
          tableauResult <- buildRdf(store,
                                     one$tableauResult,
                                     one$RDFMetadata,
                                     one$rdf_file_path,
                                     one$rdf_subject, 
                                     one$titles,
                                     one$descriptions,
                                     one$subjects,
                                     one$tabURIs,
                                     one$processes,
                                     one$image,
                                     one$data_output_identifiers,
                                     one$download,
                                     one$start,
                                     one$end,
                                     one$spatial,
                                     one$withSparql)
        }
      }
    }
  }

julien<-buildJson(type="Bar Chart", description="Rapport d'exécution du traitement i9", processSourceCode="http://mdst-macroes.ird.fr:8084/wps/R/scripts/Atlas_i9XXX.R",results=tableauResult)

return(julien)



}




