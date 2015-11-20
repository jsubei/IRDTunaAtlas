# Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a map of 5° degrees squares catches of selected species percent of total catches for the all species. An associated RDF file is also produced.
##################################################################
# Norbert Billet - IRD
# 2013/11/04: Norbert - Add RDF export and allow production of multiple maps (i.e. species)
# 2013/09/03: Norbert - Add attributes names as parameters
# 2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
# 2013/05/28: Norbert - First version
##################################################################
# Use example:
#  library(IRDTunaAtlas)
#  library(rgdal)
#  inputFilePath <- "/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i6i7i8.shp"
#  layerName <- ogrListLayers(inputFilePath)[1]
#  sp.df <- readOGR(dsn=inputFilePath, layer=layerName, disambiguateFIDs=TRUE)
#  sp.df <- sp.df[sp.df$year >= 1980 & sp.df$year <= 2000,]
#  Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies(sp.df, "MLS",
#                        geomIdAttributeName="geom_id",
#                        yearAttributeName="year",
#                        speciesAttributeName="species",
#                        valueAttributeName="value")
##################################################################
# 
# library(rCharts)
# library(jsonlite)
# library(rgdal)

# source("/home/tomcat7/temp/IRDTunaAtlas.R")
# source("/home/julien/SVNs/GIT/IRDTunaAtlas/R/IRDTunaAtlas_julien.R")


Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies <- function(df, targetedSpecies,
                                                           geomIdAttributeName="geom_id",
                                                           yearAttributeName="year", 
                                                           speciesAttributeName="species",                                         
                                                           valueAttributeName="value")#,                                                           withSparql=TRUE)
{
  require(maps)
  
  if (missing(targetedSpecies)) {
    stop("Missing target species")
  }
  
  #check inputs
  if (class(df) != "SpatialPolygonsDataFrame")
  {
    stop(paste("Bad geometrical feature type, must be a SpatialPolygonsDataFrame"))
  }
  
  if(sum(names(df) == geomIdAttributeName) == 0) {
    stop("Cannot found geom id attribute")
  }
  
  if(sum(names(df) == yearAttributeName) == 0) {
    stop("Cannot found year attribute")
  }
  
  if(sum(names(df) == speciesAttributeName) == 0) {
    stop("Cannot found species attribute")
  }
  
  if(sum(names(df) == valueAttributeName) == 0) {
    stop("Cannot found value attribute")
  }
  
  #format columns
  df@data[, geomIdAttributeName] <- as.character(df@data[, geomIdAttributeName])
  df@data[, yearAttributeName] <- as.numeric(df@data[, yearAttributeName])
  df@data[, speciesAttributeName] <- as.factor(df@data[, speciesAttributeName])
  df@data[, valueAttributeName] <- as.numeric(df@data[, valueAttributeName])
  
  #rename columns
  names(df)[which(names(df) == geomIdAttributeName)] <- "geom_id"
  names(df)[which(names(df) == yearAttributeName)] <- "year"  
  names(df)[which(names(df) == speciesAttributeName)] <- "species"
  names(df)[which(names(df) == valueAttributeName)] <- "value"
  
  #check if there is only one species
  if (length(unique(df@data[, speciesAttributeName])) < 2) {
    stop("Less than two species found in input data")
  }
  
  #check if targeted species is in the data
  if (all(is.na(match(unique(df@data[, speciesAttributeName]), targetedSpecies)))) {
    stop(paste("Targeted species not found in the dataset"))
  }
  
  
  # tableauResult$results <- data.frame(titre=character(),
  tableauResult <- data.frame(stringsAsFactors=FALSE)   
  
  #   URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesMapRelativeCatchesOtherSpecies/cdn/"
  #   repository<-"/data/www/html/tmp/SpeciesMapRelativeCatchesOtherSpecies/cdn/"
  URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesMapRelativeCatchesOtherSpecies/default/"
  repository<-"./outputs/www/html/tmp/SpeciesMapRelativeCatchesOtherSpecies/default/"
  
  
  #RDF schema to store the descriptions of results
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
  
  
  plotFct <- function(subDf, species.targeted, species.current, tableauResult, store, lims) {
    
    #aggregate values by 5° CWP square and species
    aggData <- aggregate(value ~ geom_id + species, data=subDf, sum)    
    #aggregate values by 5° CWP square only
    aggDataAllSpecies <- aggregate(value ~ geom_id, data=aggData, sum)          
    #merge data
    aggData <- merge(aggData, aggDataAllSpecies, by="geom_id")    
    #keep only data for the selected species
    aggData <- aggData[aggData$species == species.targeted, ]    
    #transform value of each square in percentage of total catches value
    aggData$value <- aggData$value.x / aggData$value.y * 100    
    #create a spatial df object from
    aggSp <- SpatialPolygons(subDf[match(aggData$geom_id, subDf$geom_id),]@polygons, proj4string=CRS("+init=epsg:4326"))
    aggSpdf <- SpatialPolygonsDataFrame(Sr=aggSp, data=aggData, match.ID=FALSE)    
    
    names(aggSpdf)[names(aggSpdf) == "geom_id"] <- "id"
    aggSpdf.fortify <- fortify(aggSpdf, region="id")
    aggSpdf.df <- join(aggSpdf.fortify, aggSpdf@data, by="id")
    
    world.df <-  fortify(maps::map("world", plot = FALSE, fill = TRUE))
    
    if (missing(lims)) {
      lims <- range(aggSpdf.df$value)
    }
    
    if (min(subDf$year) == max(subDf$year)) {
      my.title <- paste(species.label , " Catches 5x5° contribution / all species for ",  min(subDf$year), sep="")
    } else {
      my.title <- paste(species.label , " Catches 5x5° contribution / all species for ",  min(subDf$year), "-",  max(subDf$year), sep="")
    }
    
    resultPlot <- ggplot() +
      geom_polygon(data=aggSpdf.df, mapping=aes(x = long, y = lat, fill=value, group=group)) +
      scale_fill_continuous(low="yellow", high="blue", na.value="grey25", name="Contribution (%)", limits=lims, 
                            guide=guide_colourbar(direction="horizontal", 
                                                  title.position="top",
                                                  label.position="bottom",
                                                  barwidth=20)) +      
      geom_path(data=world.df, mapping=aes(x=long, y=lat, group=group), colour="grey25") +
      coord_equal() +
      theme(legend.position="bottom", axis.title.x=element_blank(), axis.title.y=element_blank()) +
      labs(title=my.title)
    
    #draw the plot
    filename <- paste("I8_", gsub(" ", "_", species.label), "_", as.character(min(subDf$year)), "-", as.character(max(subDf$year)), "_", sep="")
    tempfile.base <- paste(repository,filename, sep="")
    plot.filepath <- paste(tempfile.base, ".png", sep="")
    plot.URLpng <- paste(URL,filename, ".png", sep="")
    ggsave(filename=plot.filepath, plot=resultPlot, width=20, unit="cm", dpi=300)
    
    
    ## Dataset in HTML
    Datatable <- dTable(
      aggSpdf.df,
      sPaginationType= "full_numbers"
    )
    Datatable
    
    
    plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
    Datatable$save(plot.filepathtmltable,standalone=TRUE)     
    # Datatable$save(plot.filepathtmltable,cdn=TRUE)     
    plot.URLhtmlTable <- paste("http://mdst-macroes.ird.fr/tmp",filename, "_table.html", sep="")    
    
    
    #Datatable
    plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
    plot.URLhtmlTable <- paste(URL,filename, "_table.html", sep="")    
    #     Datatable$save(plot.filepathtmltable,standalone=TRUE)     
    Datatable$save(plot.filepathtmltable,cdn=TRUE)       
    
      
    
    #     julien#     julien#     julien#     julien#     julien#     julien#     julien#     julien#     julien#     julien#     julien#     julien#     julien#     julien#     julien#     julien
    ################################################################################################
    
    
    # ligne <- data.frame(TYPE="URI", URL=URI,  stringsAsFactors=FALSE)
    
    
    ################################################################################################
#     titles=c(paste(species.label, ":  Map of contribution of catches"), 
#              paste("Carte des contribution aux captures de", species.label))
#     
#     
#     descriptions=c(c("en", paste("IRD Tuna Atlas: indicator #8 - Map of contribution of catches for species:",species.label, "(in percent of catches for all species)", sep=" ")),
#                    c("fr", paste("IRD Atlas Thonier: indicator #8 - Carte des contributions aux captures pour l'espèce:",species.label, ", en pourcentage des captures pour toutes les espèces", sep=" ")))
#     
#     subjects=c(as.character(targetedSpecies))
#     rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep="")               
#     URI <- FAO2URIFromEcoscope(as.character(species.current))
#     tabURIs<- data.frame(type="species",URI=URI,stringsAsFactors=FALSE)    
#     
#     #TODO julien => A ADAPTER AVEC LA CONVEX HULL / ou la collection DE TOUTES LES GEOMETRIES CONCERNEES
#     spatial_extent="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))"
#     temporal_extent_begin=as.character(min(subDf$year))
#     temporal_extent_end=as.character(max(subDf$year))
#     
#     
#     #create the RDF metadata
#     rdf.filepath <- paste(repository, "La_totale.rdf", sep="")
#     rdf.URL <- paste(URL,filename, ".rdf", sep="")
#     
#     download=data.frame(format="csv",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.csv", stringsAsFactors=FALSE)
#     ligne <- c(format="shp",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.shp")
#     download <- rbind(download, ligne)
#     ligne <- c(format="GML|WKT|shp|netCDF",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.nc....")
#     download <- rbind(download, ligne)
#     
#     data_output_identifiers=data.frame(titre="1 en fait y a pas besoin de cet attribut",type="image",year=temporal_extent_begin, fileURL=plot.filepath, stringsAsFactors=FALSE)
#     ligne <- c(titre="4 en fait y a pas besoin de cet attribut",type="dataTable",year=temporal_extent_begin, fileURL=plot.URLhtmlTable)
#     data_output_identifiers <- rbind(data_output_identifiers, ligne)
#     
#     one <-list(tableauResult = tableauResult,
#                RDFMetadata=rdf.URL,
#                rdf_file_path=rdf.filepath,
#                rdf_subject=rdf_subject, 
#                titles=titles,
#                descriptions=descriptions,
#                subjects=subjects,
#                tabURIs=tabURIs,
#                processes="http://www.ecoscope.org/ontologies/resources/processI8",
#                image=plot.URLpng,
#                data_output_identifiers=data_output_identifiers,
#                download=download,
#                start=temporal_extent_begin,
#                end=temporal_extent_end,
#                spatial=spatial_extent,
#                withSparql=withSparql)
#     
one<-'toto'
    return(one)      
  }
  
#   if (withSparql) {      
#     #get species scientific name from ecoscope sparql
#     sparqlResult <- getSpeciesFromEcoscope(as.character(targetedSpecies))
#     if (length(sparqlResult) > 0) {
#       species.label <- sparqlResult[1,"scientific_name"]
#       species.URI <- sparqlResult[1,"uri"]
#     } else {
#       species.label <- species
#       species.URI <- species
#     } 
#   } else {
#     species.label <- species
#     species.URI <- species
#   }
species.label <- species
species.URI <- species
  #define the result df  
  result.df <- c()
  
  #plot for all the period
  one <- plotFct(df, targetedSpecies, species.label, tableauResult, store)
#   tableauResult <- buildRdf(store,
#                             one$tableauResult,
#                             one$RDFMetadata,
#                             one$rdf_file_path,
#                             one$rdf_subject, 
#                             one$titles,
#                             one$descriptions,
#                             one$subjects,
#                             one$tabURIs,
#                             one$processes,
#                             one$image,
#                             one$data_output_identifiers,
#                             one$download,
#                             one$start,
#                             one$end,
#                             one$spatial,
#                             one$withSparql)  
#   
  
  if (length(unique(df$year)) > 1)
  {
    #for each year
    for(year.current in unique(df$year)) {
      one <- plotFct(df[df$year==year.current,], targetedSpecies, species.label, species.current, tableauResult, store)
#       tableauResult <- buildRdf(store,
#                                 one$tableauResult,
#                                 one$RDFMetadata,
#                                 one$rdf_file_path,
#                                 one$rdf_subject, 
#                                 one$titles,
#                                 one$descriptions,
#                                 one$subjects,
#                                 one$tabURIs,
#                                 one$processes,
#                                 one$image,
#                                 one$data_output_identifiers,
#                                 one$download,
#                                 one$start,
#                                 one$end,
#                                 one$spatial,
#                                 one$withSparql)
    }
    
    #for each decade
    df$decade <- df$year - (df$year %% 10)
    if (length(unique(df$decade)) > 1)
    {
      for(decade.current in unique(df$decade)) {
        one <- plotFct(df[df$decade==decade.current,], targetedSpecies, species.label, species.current, tableauResult, store)
#         tableauResult <- buildRdf(store,
#                                   one$tableauResult,
#                                   one$RDFMetadata,
#                                   one$rdf_file_path,
#                                   one$rdf_subject, 
#                                   one$titles,
#                                   one$descriptions,
#                                   one$subjects,
#                                   one$tabURIs,
#                                   one$processes,
#                                   one$image,
#                                   one$data_output_identifiers,
#                                   one$download,
#                                   one$start,
#                                   one$end,
#                                   one$spatial,
#                                   one$withSparql)      
      }
    }
  }
  # Packing the description of results in Json file storing all metadata (same as RDF)  
# julien<-buildJson(type="map", description="Résultats de l'exécution du traitement i8 sur tout le jeu de données Sardara",processSourceCode="http://mdst-macroes.ird.fr:8084/wps/R/Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies.R",results=tableauResult)
julien<-resultPlot
return(julien)
  
}