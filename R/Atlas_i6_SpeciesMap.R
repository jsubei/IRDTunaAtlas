# Atlas_i6_SpeciesMap.R
# Tuna Atlas - IRD / MR EME
#
# This indicator build a map of 5x5 degrees squares aggregated catches. An associated RDF file is also produced.
##################################################################
# Norbert Billet - IRD
# 2013/11/04: Norbert - Add RDF export and allow production of multiple maps (i.e. species)
# 2013/09/03: Norbert - Add attributes names as parameters
# 2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
# 2013/04/22: Norbert - First version
##################################################################
# Use example:
#  library(IRDTunaAtlas)
#  library(rgdal)
#  inputFilePath <- "/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i6i7i8.shp"
#  layerName <- ogrListLayers(inputFilePath)[1]
#  sp.df <- readOGR(dsn=inputFilePath, layer=layerName, disambiguateFIDs=TRUE)
#  sp.df <- sp.df[sp.df$species == "MLS" & sp.df$year == 1990,]
#  Atlas_i6_SpeciesMap(sp.df, 
#                        geomIdAttributeName="geom_id",
#                        yearAttributeName="year",
#                        speciesAttributeName="species",
#                        valueAttributeName="value")
# ##################################################################
# library(rCharts)
# library(jsonlite)
# library(rgdal)


# source("/home/tomcat7/temp/IRDTunaAtlas.R")
# source("/home/julien/SVNs/GIT/IRDTunaAtlas/R/IRDTunaAtlas_julien.R")

Atlas_i6_SpeciesMap <- function(df,
                                geomIdAttributeName="geom_id",
                                yearAttributeName="year", 
                                speciesAttributeName="species",                                         
                                valueAttributeName="value")#,                                withSparql=TRUE)
{
   if (! require(maps)) {
     stop("Missing library")
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
  
    URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesMap/cdn/"
    repository<-"/data/www/html/tmp/SpeciesMap/cdn/"
#   URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesMap/"
#   repository<-"/data/www/html/tmp/SpeciesMap/"  
#   
  #List to store URLs of the set of files generated for each species
  liste <- list()
  #define the resulr df  
  result.df <- c()
  # tableauResult$results <- data.frame(titre=character(),
  tableauResult <- data.frame(stringsAsFactors=FALSE)   
  listeResult <- list()

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
  
  
  #convert values from tons to thousand tons
  df$value <- df$value / 1000
  
  
  ################################################################################################
  # FUNCTION plotFct
  ################################################################################################
  
  
  plotFct <- function(subDf, species.label, species.current, tableauResult, store, lims) {
    #aggregate values by 5° CWP square
    aggData <- aggregate(value ~ geom_id, data=subDf, sum)      
    
    #create a spatial df object from
    aggSp <- SpatialPolygons(species.df[match(aggData$geom_id, species.df$geom_id),]@polygons, proj4string=CRS("+init=epsg:4326"))
    aggSpdf <- SpatialPolygonsDataFrame(Sr=aggSp, data=aggData, match.ID=FALSE)
    
    names(aggSpdf)[names(aggSpdf) == "geom_id"] <- "id"
    aggSpdf.fortify <- fortify(aggSpdf, region="id")
    aggSpdf.df <- join(aggSpdf.fortify, aggSpdf@data, by="id")
    
    world.df <-  fortify(maps::map("world", plot = FALSE, fill = TRUE))
    
    if (missing(lims)) {
      lims <- range(aggSpdf.df$value, na.rm=TRUE)
    }
    
    if (min(subDf$year) == max(subDf$year)) {
      my.title <- paste(species.label , " catches 5x5° for ",  min(subDf$year), sep="")
    } else {
      my.title <- paste(species.label , " catches 5x5° for ",  min(subDf$year), "-",  max(subDf$year), sep="")
    }
    
    resultPlot <- ggplot() +
      geom_polygon(data=aggSpdf.df, mapping=aes(x = long, y = lat, fill=value, group=group)) +
      scale_fill_continuous(low="yellow", high="blue", na.value="grey25", name="Catches in k. tons", limits=lims, 
                            guide=guide_colourbar(direction="horizontal", 
                                                  title.position="top",
                                                  label.position="bottom",
                                                  barwidth=20)) +
      geom_path(data=world.df, mapping=aes(x=long, y=lat, group=group), colour="grey25") +
      coord_equal() +
      theme(legend.position="bottom", axis.title.x=element_blank(), axis.title.y=element_blank()) +
      labs(title=my.title)
    
    #draw the plot
    filename <- paste("I6_", gsub(" ", "_", species.label), "_", as.character(min(subDf$year)), "_to_", as.character(max(subDf$year)), "_", sep="")
    tempfile.base <- paste(repository,filename, sep="")
    plot.filepath <- paste(tempfile.base, ".png", sep="")
    plot.URLpng <- paste(URL,filename, ".png", sep="")    
    ggsave(filename=plot.filepath, plot=resultPlot, width=20, unit="cm", dpi=300)
    
    ## Dataset as a Table in HTML
    Datatable <- dTable(aggSpdf.df,sPaginationType= "full_numbers")
    #Datatable is stored in html file
    plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
    Datatable$save(plot.filepathtmltable,cdn=TRUE)       
    #     Datatable$save(plot.filepathtmltable,standalone=TRUE)     
    plot.URLhtmlTable <- paste(URL,filename, "_table.html", sep="")    
      
    
    ################################################################################################
    #Data displayed in a map 
    ################################################################################################
    
    #     json2 = toJSON(aggSpdf.df, pretty=TRUE)
    
    #     regions2=RJSONIO::fromJSON(json2)
    #    
    
#     
#     json = '{"type":"FeatureCollection","features":[
# {"type":"Feature",
#     "properties":{"region_id":1, "region_name":"Australian Alps"},
#     "geometry":{"type":"Polygon","coordinates":[[[141.13037109375,-38.788345355085625],[141.13037109375,-36.65079252503469],[144.38232421875,-36.65079252503469],[144.38232421875,-38.788345355085625],[141.13037109375,-38.788345355085625]]]}},
# {"type":"Feature",
#     "properties":{"region_id":4, "region_name":"Shark Bay"},
#     "geometry":{"type":"Polygon","coordinates":[[[143.10791015625,-37.75334401310656],[143.10791015625,-34.95799531086791],[146.25,-34.95799531086791],[146.25,-37.75334401310656],[143.10791015625,-37.75334401310656]]]}}
#     ]}'
#     regions=RJSONIO::fromJSON(json)
#     lmap <- Leaflet$new()
#     lmap$tileLayer(provide='Stamen.TonerLite')
#     lmap$setView(c(-37, 145), zoom = 1)
#     lmap$geoJson(
#       regions, 
#       style = "#! function(feature) {
#       var rgn2col = {1:'red',2:'blue',4:'green'};     
#       return {
#       color: rgn2col[feature.properties['region_id']],
#       strokeWidth: '1px',
#       strokeOpacity: 0.5,
#       fillOpacity: 0.2
#       }; } !#",
#       onEachFeature = "#! function (feature, layer) {
#       
#       // info rollover
#       if (document.getElementsByClassName('info leaflet-control').length == 0 ){
#       info = L.control({position: 'topright'});  // NOTE: made global b/c not ideal place to put this function
#       info.onAdd = function (map) {
#       this._div = L.DomUtil.create('div', 'info');
#       this.update();
#       return this._div;
#       };
#       info.update = function (props) {
#       this._div.innerHTML = '<h4>Field Name</h4>' +  (props ?
#       props['region_id'] + ': <b> + props[fld] + </b>'
#       : 'Hover over a region');
#       };
#       info.addTo(map);
#       };
#       
#       // mouse events
#       layer.on({
#       
#       // mouseover to highlightFeature
#       mouseover: function (e) {
#       var layer = e.target;
#       layer.setStyle({
#       strokeWidth: '3px',
#       strokeOpacity: 0.7,
#       fillOpacity: 0.5
#       });
#       if (!L.Browser.ie && !L.Browser.opera) {
#       layer.bringToFront();
#       }
#       info.update(layer.feature.properties);
#       },
#       
#       // mouseout to resetHighlight
#       mouseout: function (e) {
#       geojsonLayer.resetStyle(e.target);
#       info.update();
#       },
#       
#       // click to zoom
#       click: function (e) {
#       var layer = e.target;        
#       if ( feature.geometry.type === 'MultiPolygon' ) {        
#       // for multipolygons get true extent
#       var bounds = layer.getBounds(); // get the bounds for the first polygon that makes up the multipolygon
#       // loop through coordinates array, skip first element as the bounds var represents the bounds for that element
#       for ( var i = 1, il = feature.geometry.coordinates[0].length; i < il; i++ ) {
#       var ring = feature.geometry.coordinates[0][i];
#       var latLngs = ring.map(function(pair) {
#       return new L.LatLng(pair[1], pair[0]);
#       });
#       var nextBounds = new L.LatLngBounds(latLngs);
#       bounds.extend(nextBounds);
#       }
#       map.fitBounds(bounds);
#       } else {
#       // otherwise use native target bounds
#       map.fitBounds(e.target.getBounds());
#       }
#       }
#       });
#       } !#")
#     legend_vec = c('Red'='Blood', 'Green'='Nature', 'Yellow'='Sun')
#     lmap$legend(position = 'bottomright', 
#                 colors   =  names(legend_vec), 
#                 labels   =  as.vector(legend_vec))
#     
#     #Data displayed in a map are stored in html file
#     plot.filepathtmlMap <- paste(tempfile.base, "_maptoto.html", sep="")
#     lmap$save(plot.filepathtmlMap,standalone=TRUE)       
#     #     Datatable$save(plot.filepathtmltable,standalone=TRUE)     
#     plot.URLhtmlMap <- paste(URL,filename, "_map_toto.html", sep="")    
#     
#     
    
    
    #alternative json
    # jojo<- "jijo"
    # return(jojo)  
    # return(json2)  
    
    # #Write geojson
    # #dataMap is a dataframe with coordinates on cols 11 (LATITUDE) and 12 (LONGITUDE)
    # #Transfor coordinates to numeric
    # dataMap$LATITUDE <- as.numeric(dataMap$LATITUDE)
    # dataMap$LONGITUDE <- as.numeric(dataMap$LONGITUDE)
    # dataMap.SP <- SpatialPointsDataFrame(dataMap[,c(12,11)],dataMap[,-c(12,11)])
    # str(dataMap.SP) # Now is class SpatialPointsDataFrame
    # 
    # #Write as geojson
    # writeOGR(dataMap.SP, 'dataMap.geojson','dataMap', driver='GeoJSON') 
    # 
    # 
    # # http://recology.info/2013/06/geojson/
    # togeojson(file, "~/github/sac/rgeojson/acer_spicatum.geojson")
    
    
    ################################################################################################

#     
#     titles=c(paste(species.label, ":  Map of catches"), 
#              paste("Carte des captures de", species.label))
#     
#     
#     descriptions=c(c("en", paste("IRD Tuna Atlas: indicator #6 -  Map of catches for species ",species.label, sep=" ")),
#                    c("fr", paste("IRD Atlas Thonier: indicator #6 - Carte des captures de pour l'espèce:",species.label, sep=" ")))
#     
#     subjects=c(as.character(species.current))
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
#     
#     
#     download=data.frame(format="csv",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.csv", stringsAsFactors=FALSE)
#     ligne <- c(format="shp",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.shp")
#     download <- rbind(download, ligne)
#     ligne <- c(format="GML|WKT|shp|netCDF",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.nc....")
#     download <- rbind(download, ligne)
#     
#     data_output_identifiers=data.frame(titre="1 en fait y a pas besoin de cet attribut",type="stackedArea",year=temporal_extent_begin, fileURL=plot.filepath, stringsAsFactors=FALSE)
#     ligne <- c(titre="4 en fait y a pas besoin de cet attribut",type="map",year=temporal_extent_begin, fileURL=plot.URLhtmlMap)
#     data_output_identifiers <- rbind(data_output_identifiers, ligne)
#     ligne <- c(titre="4 en fait y a pas besoin de cet attribut",type="dataTable",year=temporal_extent_begin, fileURL=plot.URLhtmlTable)
#     data_output_identifiers <- rbind(data_output_identifiers, ligne)
# 
# one <-list(tableauResult = tableauResult,
#                   RDFMetadata=rdf.URL,
#                   rdf_file_path=rdf.filepath,
#                   rdf_subject=rdf_subject, 
#                   titles=titles,
#                   descriptions=descriptions,
#                   subjects=subjects,
#                   tabURIs=tabURIs,
#                    processes="http://www.ecoscope.org/ontologies/resources/processI6",
#                    image=plot.URLpng,
#                   data_output_identifiers=data_output_identifiers,
#                   download=download,
#                   start=temporal_extent_begin,
#                   end=temporal_extent_end,
#                   spatial=spatial_extent,
#                   withSparql=withSparql)
one <- 'toto'

    return(one)  
    
    }
  
  ################################################################################################
  # FUNCTION plotFct
  ################################################################################################
  
  
  
  
  
  #fisrt subset by species
  for (species.current in unique(df$species)) {
    
#         if (withSparql) {      
#           #get species scientific name from ecoscope sparql
#           sparqlResult <- getSpeciesFromEcoscope(as.character(species.current))
#           
#           if (length(sparqlResult) > 0) {
#             species.label <- sparqlResult[1,"scientific_name"]
#             species.URI <- sparqlResult[1,"uri"]
#           } else {
#             species.label <- species.current
#             species.URI <- species.current
#           } 
#         } else {
#     species.label <- species.current
#     species.URI <- species.current
#         }
    
    species.label <- species.current
    species.URI <- species.current
    
    species.df <- df[df$species == species.current,]
    
    #plot for all the period
    #     result.plot.df <- plotFct(species.df, species.label)
    #     result.df <- rbind(result.df, result.plot.df)
    one <- plotFct(species.df, species.label, species.current, tableauResult, store)
  #     buildRdf <- function(store, tableauResult, RDFMetadata, rdf_file_path, rdf_subject, titles=c(), descriptions=c(), subjects=c(), tabURIs, processes=c(), image, data_output_identifiers, download, start=NA, end=NA, spatial=NA, withSparql=TRUE) {
      
#     tableauResult <- buildRdf(store,
#                       one$tableauResult,
#                       one$RDFMetadata,
#                       one$rdf_file_path,
#                       one$rdf_subject, 
#                       one$titles,
#                       one$descriptions,
#                       one$subjects,
#                       one$tabURIs,
#                       one$processes,
#                       one$image,
#                       one$data_output_identifiers,
#                       one$download,
#                       one$start,
#                       one$end,
#                       one$spatial,
#                       one$withSparql)
#   
        #for each year
        if (length(unique(species.df$year)) > 1)
        {
          for(year.current in unique(species.df$year)) {
            one <- plotFct(species.df[species.df$year==year.current,], species.label, species.current, tableauResult, store)
#             tableauResult <- buildRdf(store,
#                                        one$tableauResult,
#                                        one$RDFMetadata,
#                                        one$rdf_file_path,
#                                        one$rdf_subject, 
#                                        one$titles,
#                                        one$descriptions,
#                                        one$subjects,
#                                        one$tabURIs,
#                                        one$processes,
#                                        one$image,
#                                        one$data_output_identifiers,
#                                        one$download,
#                                        one$start,
#                                        one$end,
#                                        one$spatial,
#                                        one$withSparql)
            
          }
            
          #for each decade
          species.df$decade <- species.df$year - (species.df$year %% 10)
          if (length(unique(species.df$decade)) > 1)
          {
            for(decade.current in unique(species.df$decade)) {

              one <- plotFct(species.df[species.df$decade==decade.current,], species.label, species.current, tableauResult, store)
#               tableauResult <- buildRdf(store,
#                                          one$tableauResult,
#                                          one$RDFMetadata,
#                                          one$rdf_file_path,
#                                          one$rdf_subject, 
#                                          one$titles,
#                                          one$descriptions,
#                                          one$subjects,
#                                          one$tabURIs,
#                                          one$processes,
#                                          one$image,
#                                          one$data_output_identifiers,
#                                          one$download,
#                                          one$start,
#                                          one$end,
#                                          one$spatial,
#                                          one$withSparql)
              
            }
            }
          }
    
    
  }


# Packing the description of results in Json file storing all metadata (same as RDF)  
# julien<-buildJson(type="map", description="Rapport d'exécution du traitement i6",processSourceCode="http://mdst-macroes.ird.fr:8084/wps/R/Atlas_i6_SpeciesMap.R",results=tableauResult)
# fileJulien=paste(repository,"WebProcessingService.json", sep="")
# cat(julien, file=fileJulien)

julien<-'julien'
return(julien)


}


