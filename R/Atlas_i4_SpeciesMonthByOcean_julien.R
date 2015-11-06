# Atlas_i4_SpeciesMonthByOcean_julien.R
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
# library(rCharts)
# library(jsonlite)

# source("/home/tomcat7/temp/IRDTunaAtlas.R")
# source("/home/julien/SVNs/GIT/IRDTunaAtlas/R/IRDTunaAtlas_julien.R")

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
  
  
  # tableauResult$results <- data.frame(titre=character(),
  tableauResult <- data.frame(stringsAsFactors=FALSE)   
  
  URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesByMonthByOcean/cdn/"
  repository<-"/data/www/html/tmp/SpeciesByMonthByOcean/cdn/"
  # URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesByMonthByOcean/"
  # repository<-"/data/www/html/tmp/SpeciesByMonthByOcean/"
    
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
      filename <- paste("I4", gsub(" ", "_", species.label), decade.current,sep="_")
      tempfile.base <- paste(repository,filename, sep="")
      plot.filepath <- paste(tempfile.base, ".png", sep="")
      ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)
      plot.URLpng <- paste(URL,filename, ".png", sep="")


           
      ## plot Rcharts Highcharts
      plotRchartsHighcharts <- hPlot(x = "ocean", y = "value", data = mergedDf, type = "pie")
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
      plot.URLhtml <- paste(URL,filename, ".html", sep="")
plotRchartsHighcharts$save(plot.filepathtml,standalone=TRUE) 
# plotRchartsHighcharts$save(plot.filepathtml,cdn=TRUE) 
      plot.filepathtmlNVD3 <- paste(tempfile.base, "_NVD3.html", sep="")
plotRchartsNVD3$save(plot.filepathtmlNVD3,standalone=TRUE) 
# plotRchartsNVD3$save(plot.filepathtmlNVD3,cdn=TRUE) 

#       Datatable
      plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
Datatable$save(plot.filepathtmltable,standalone=TRUE)     
# Datatable$save(plot.filepathtmltable,cdn=TRUE)     
    plot.URLhtmlTable <- paste(URL,filename, "_table.html", sep="")    
        
    
    
################################################################################################


# ligne <- data.frame(TYPE="URI", URL=URI,  stringsAsFactors=FALSE)


################################################################################################



#Metadata elements (in addition to OGC WPS metadata) to describe the current indicator which will be used by other applications (Ecoscope and Tuna Atlas Websites)
titles=c(paste(species.label, "IRD Tuna Atlas: indicator #4 - monthly catches by ocean and by year"), 
         paste("IRD Atlas thonier : indicateur #4 - captures mensuelles de", species.label, " par océan et par année"))

# 
# descriptions=c(c("en",paste(species.label, "monthly catches by ocean and by year"),
#                  c("fr",paste("Captures mensuelles de", species.label, "par océan et par année"))
#                  

descriptions=c(c("en",paste(species.label, "monthly catches by ocean and by year")),
               c("fr",paste("Captures mensuelles de", species.label, "par océan et par année")))
                 
subjects=c(as.character(species.current),as.character(unique(current.df$ocean)))

#Collect the URIs of related Topics from Ecoscope SPARQL endpoint
URI <- FAO2URIFromEcoscope(as.character(species.current))
tabURIs<- data.frame(type="species",URI=URI,stringsAsFactors=FALSE)
                 
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
                 temporal_extent_begin=as.character(min(current.df$year))
                 temporal_extent_end=as.character(max(current.df$year))
                 
 #create the RDF metadata
 rdf.filepath <- paste(repository, "La_totale.rdf", sep="")
 rdf.URL <- paste(URL,filename, ".rdf", sep="")
 # il faudrait ajouter un attribut qui précise le type de visualisation: carte, chart...
 data_output_identifiers=data.frame(titre="1 en fait y a pas besoin de cet attribut",type="image",year=temporal_extent_begin, fileURL=plot.filepath, stringsAsFactors=FALSE)
 ligne <- c(titre="2 en fait y a pas besoin de cet attribut",type="pie", year=temporal_extent_begin, fileURL=plot.URLhtml)
 data_output_identifiers <- rbind(data_output_identifiers, ligne)
 ligne <- c(titre="3 en fait y a pas besoin de cet attribut",type="pie",year=temporal_extent_begin, fileURL=plot.filepathtmlNVD3)
 data_output_identifiers <- rbind(data_output_identifiers, ligne)
 ligne <- c(titre="6 en fait y a pas besoin de cet attribut",type="dataTable",year=temporal_extent_begin, fileURL=plot.filepathtmltable)
 data_output_identifiers <- rbind(data_output_identifiers, ligne)

                 
                 download=data.frame(format="csv",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.csv", stringsAsFactors=FALSE)
                 ligne <- c(format="shp",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.shp")
                 download <- rbind(download, ligne)
                 ligne <- c(format="GML|WKT|shp|netCDF",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.nc....")
                 download <- rbind(download, ligne)
                 
                 
                 tableauResult <- buildRdf(store=store,
                                           tableauResult = tableauResult,
                                           RDFMetadata=rdf.URL,
                                           rdf_file_path=rdf.filepath,
                                           rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""), 
                                           #rdf_subject="http://ecoscope.org/indicatorI1", 
                                           titles=titles,
                                           descriptions=descriptions,
                                           subjects=subjects,
                                           tabURIs=tabURIs,
                                           processes="http://www.ecoscope.org/ontologies/resources/processI4",
                                           image=plot.URLpng,
                                           data_output_identifiers=data_output_identifiers,
                                           download=download,
                                           start=temporal_extent_begin,
                                           end=temporal_extent_end,
                                           spatial=spatial_extent,
                                           withSparql)
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
      filename <- paste("I4", gsub(" ", "_", species.label), "_byDecade_", decade.current, sep="")
      tempfile.base <- paste(repository,filename, sep="")
      plot.filepath <- paste(tempfile.base, ".png", sep="")
      ggsave(filename=plot.filepath, plot=resultPlot, dpi=100)
      plot.URLpng <- paste(URL,filename, ".png", sep="")
      
      
      ## plot Rcharts Highcharts
	#      plotRchartsHighcharts <- hPlot(x = "ocean", y = "value", data = mergedDf, type = "pie")

      plotRchartsHighcharts <- hPlot(x = "ocean", y = "value", data = mergedDf, type = "pie",title = "Tableau de pies", subtitle = "species.label", size = "value", group = "ocean")
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
      plot.URLhtml <- paste(URL,filename, ".html", sep="")
	plotRchartsHighcharts$save(plot.filepathtml,standalone=TRUE) 
# 	plotRchartsHighcharts$save(plot.filepathtml,cdn=TRUE) 
	    plot.filepathtmlNVD3 <- paste(tempfile.base, "_NVD3.html", sep="")
	plotRchartsNVD3$save(plot.filepathtmlNVD3,standalone=TRUE) 
# 	plotRchartsNVD3$save(plot.filepathtmlNVD3,cdn=TRUE) 
	
      #       Datatable
      plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
	Datatable$save(plot.filepathtmltable,standalone=TRUE)     
# 	Datatable$save(plot.filepathtmltable,cdn=TRUE)     
    	plot.URLhtmlTable <- paste(URL,filename, "_table.html", sep="")    





################################################################################################


# ligne <- data.frame(TYPE="URI", URL=URI,  stringsAsFactors=FALSE)


################################################################################################



#Metadata elements (in addition to OGC WPS metadata) to describe the current indicator which will be used by other applications (Ecoscope and Tuna Atlas Websites)



titles=c(paste(species.label, "IRD Tuna Atlas: indicator #4 - monthly catches by ocean and by decade"), 
         paste("IRD Atlas thonier : indicateur #4 - captures mensuelles de", species.label, " par océan et par décénie"))


# descriptions=c(c("en",paste(species.label, "monthly catches by ocean and by decade"),
#                c("fr",paste("Captures mensuelles de", species.label, "par océan et par décénie"))
descriptions=c(paste(species.label, "monthly catches by ocean and by decade")
                 ,paste("Captures mensuelles de", species.label, "par océan et par décénie"))
                 

subjects=c(as.character(species.current),levels(current.df$ocean))

#Collect the URIs of related Topics from Ecoscope SPARQL endpoint
URI <- FAO2URIFromEcoscope(as.character(species.current))
tabURIs<- data.frame(type="species",URI=URI,stringsAsFactors=FALSE)

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
temporal_extent_begin=as.character(min(current.df$year))
temporal_extent_end=as.character(max(current.df$year))


#create the RDF metadata
rdf.filepath <- paste(repository, "La_totale.rdf", sep="")
rdf.URL <- paste(URL,filename, ".rdf", sep="")
# il faudrait ajouter un attribut qui précise le type de visualisation: carte, chart...
data_output_identifiers=data.frame(titre="1 en fait y a pas besoin de cet attribut",type="image",year=temporal_extent_begin, fileURL=plot.filepath, stringsAsFactors=FALSE)
ligne <- c(titre="2 en fait y a pas besoin de cet attribut",type="pie", year=temporal_extent_begin, fileURL=plot.URLhtml)
data_output_identifiers <- rbind(data_output_identifiers, ligne)
ligne <- c(titre="3 en fait y a pas besoin de cet attribut",type="pie",year=temporal_extent_begin, fileURL=plot.filepathtmlNVD3)
data_output_identifiers <- rbind(data_output_identifiers, ligne)
ligne <- c(titre="6 en fait y a pas besoin de cet attribut",type="dataTable",year=temporal_extent_begin, fileURL=plot.filepathtmltable)
data_output_identifiers <- rbind(data_output_identifiers, ligne)


download=data.frame(format="csv",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.csv", stringsAsFactors=FALSE)
ligne <- c(format="shp",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.shp")
download <- rbind(download, ligne)
ligne <- c(format="GML|WKT|shp|netCDF",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.nc....")
download <- rbind(download, ligne)



tableauResult <- buildRdf(store=store,
                          tableauResult = tableauResult,
                          RDFMetadata=rdf.URL,
                          rdf_file_path=rdf.filepath,
                          rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""), 
                          #rdf_subject="http://ecoscope.org/indicatorI1", 
                          titles=titles,
                          descriptions=descriptions,
                          subjects=subjects,
                          tabURIs=tabURIs,
                          processes="http://www.ecoscope.org/ontologies/resources/processI4",
                          image=plot.URLpng,
                          data_output_identifiers=data_output_identifiers,
                          download=download,
                          start=temporal_extent_begin,
                          end=temporal_extent_end,
                          spatial=spatial_extent,
                          withSparql)
          }
  }
  
julien<-buildJson(type="Pies Table", description="Rapport d'exécution du traitement i4",processSourceCode="http://mdst-macroes.ird.fr:8084/wps/R/scripts/Atlas_i4_XXXX.R",results=tableauResult)
return(julien)

}






