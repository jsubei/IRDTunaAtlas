checkInputFile <- function(inputFilePath)
{
  if(missing(inputFilePath))
  {
    stop("Missing inputFilePath")
  }      
  
  #test if file existing
  if(! file.exists(inputFilePath))
  {
    stop(paste("Cannot found", inputFilePath))
  }
  
  #test if file can be read
  if(file.access(inputFilePath, 4) != 0)
  {
    stop(paste("Cannnot read", inputFilePath))
  }
  
  return(TRUE)
}

# Read WFS & returns a sp object
# Emmanuel Blondel - source here : https://github.com/openfigis/RFigisGeo/blob/master/R/Utils.R
#
# Arguments:
# - url: the baseURL of the WFS GetFeature request
# - outputFormat: the output format for the WFS GetFeature request, by default "GML"
# - p4s: an optional proj4string, by default NULL (an attempt will be performed to get the projection from the data)
# - gmlIdAttributeName: specific to GML, the name of the ID attribute, by default "gml_id"
#
#
readWFS <- function(url, outputFormat = "GML", p4s = NULL, gmlIdAttributeName="gml_id"){
  
  #request
  wfsRequest <- url
  if(outputFormat != "GML") {
    wfsRequest <- paste(url, "&outputFormat=", outputFormat, sep="")
  }
  
  if(outputFormat == "GML") {
    # download the data
    content <- getURL(wfsRequest)
    xmlfile <- xmlTreeParse(content, useInternalNodes = TRUE)
    #write the file to disk
    
    tempf = tempfile()
    destfile = paste(tempf,".gml",sep='')
    saveXML(xmlfile, destfile)
    #download.file(wfsRequest, destfile, mode="wb")
    layername <- ogrListLayers(destfile)
    if (length(layername) != 1) {
      stop("Mistake with layers in the input dataset")
    }
    
    hasGeometry = ((length(getNodeSet(xmlfile, "//gml:featureMember//gml:coordinates")) > 0)
                   || (length(getNodeSet(xmlfile, "//gml:featureMember//gml:pos")) > 0)
                   || (length(getNodeSet(xmlfile, "//gml:featureMember//gml:posList")) > 0))
    if(hasGeometry){
      
      # get the Spatial Reference System (SRS)
      srs <- NA
      #xmlfile<-xmlTreeParse(destfile, useInternalNodes = TRUE)
      srsName <- getNodeSet(xmlfile, "(//gml:featureMember//@srsName)[1]")
      if (length(srsName) == 1) {
        srsName <- as.character(srsName[[1]])
        
        #srsName patterns matching
        srsPattern = "http://www.opengis.net/gml/srs/epsg.xml#" #match case 1
        if(attr(regexpr(srsPattern, srsName, ignore.case = T),"match.length") > 0){
          epsg <- unlist(strsplit(srsName, srsPattern))[2]
          srs <- paste("+init=epsg:", epsg, sep="")
        }else{
          srsPattern = "urn:(x-)?ogc:def:crs:EPSG" #match case 2
          if(attr(regexpr(srsPattern, srsName, ignore.case = T),"match.length") > 0){
            srsStr <- unlist(strsplit(srsName, ":"))
            epsg <- srsStr[length(srsStr)]
            srs <- paste("+init=epsg:", epsg, sep="")
          }else{
            #search if srsName is a WKT PROJ name (PROJCS or GEOGCS)
            #if yes set srs with the corresponding proj4string
            #first search without any consideration of the ESRI representation
            srs <- findP4s(srsName, morphToESRI=FALSE)
            if (is.na(srs)) {
              #if not found search with consideration of the ESRI representation
              srs <- findP4s(srsName, morphToESRI=TRUE)
            }
            if (! is.na(srs) && ! length(srs) == 1) {
              srs <- NA
            }
          }
        }
      }  
      
      if(is.na(srs)){
        warning("Unable to convert GML srsName to a CRS object. CRS will be set to NA", call. = T)
      }
      
      if (missing(p4s)){
        features = readOGR(destfile, layername, p4s = srs, disambiguateFIDs=TRUE, stringsAsFactors=FALSE)
      }else{
        features = readOGR(destfile, layername, p4s = p4s, disambiguateFIDs=TRUE, stringsAsFactors=FALSE)
      }
      features <- spChFIDs(features, as.character(features@data[,gmlIdAttributeName]))
      
    }else{
      membersContent <- sapply(getNodeSet(xmlfile, "//gml:featureMember"), function(x) xmlChildren(x))
      fid <- sapply(membersContent, function(x) xmlAttrs(x))
      membersAttributes <- xmlToDataFrame(nodes = getNodeSet(xmlfile, "//gml:featureMember/*[@*]"), stringsAsFactors=FALSE)
      features <- cbind(fid, membersAttributes, stringsAsFactors=FALSE)      
    }
    file.remove(destfile)
  }else{
    stop("Unsupported WFS format")
  }
  return(features)
}



WFS2GMLFile <- function(URL, layerName, filter)
{
  #check the URL input
  http_prefix <- "http://"
  if(substr(URL, 1, 7) != http_prefix)
  {
    URL <- paste(http_prefix, URL, sep="")
  }
  
  if(! require(rgdal))
  {
    stop("Missing rgdal library")
  }
  
  #check the layer name
  avalaibleLayers <- ogrListLayers(paste("WFS:", URL, sep=""))
  res <- avalaibleLayers == layerName
  if(length(res[res == TRUE]) != 1)
  {
    stop(paste("Cannot find requested layer[", layerName, "] on WFS server[", URL, "] or multiple layer. avalaibles layers: [", paste(avalaibleLayers, collapse=", ") ,"]", sep=""))
  }
  
  #ok, we build the full URL request
  desc <- paste(URL, "?REQUEST=GetFeature&SERVICE=WFS&VERSION=1.1.0&TYPENAME=", layerName, sep="")
  
  if(! require(RCurl))
  {
    stop("Missing RCurl library")
  }
  
  #we add the filter
  if(! missing(filter) && nchar(filter) > 0)
  {
    ogc_filter_prefix <- "<ogc:Filter"
    if(substr(filter, 1, 7) == ogc_filter_prefix)
    {
      filter <- curlEscape(filter)
    }
    desc <- paste(desc, "&filter=", filter, sep="")
  }    
  
  if (! require(XML)) {
    stop("Missing XML library")
  }
  
#  message(desc)
  res <- getURLContent(desc)
  
  con <- xmlTreeParse(res, useInternalNodes = TRUE)
  file_path <- paste(tempfile(), ".gml", sep="")
  
  saveXML(con, file_path)
  
  return(file_path)
}

convertToDate <- function(input) {
  if (is.na(input) || length(input) == 0) {
    stop("Empty input")
  }
  if (! inherits(input, what="POSIXlt")) {
    input <- as.character(input)
    formats <- c("%Y-%m-%dT%H:%M:%OSZ", "%Y/%m/%dT%H:%M:%OSZ", 
                 "%Y-%m-%dT%H:%M:%SZ", "%Y/%m/%dT%H:%M:%SZ",
                 "%Y-%m-%dT%H:%M:%S", "%Y/%m/%dT%H:%M:%S",
                 "%Y-%m-%d %H:%M:%SZ", "%Y/%m/%d %H:%M:%SZ",
                 "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
                 "%Y-%m-%d", "%Y/%m/%d",
                 "%d-%m-%Y", "%d/%m/%Y")
    found <- FALSE
    for (format.current in formats) {
      date.test <- strptime(input[1], format=format.current)
      if (! is.na(date.test)) {
        found <- TRUE
        break
      }
    }
    
    if (found) {
      return(strptime(input, format=format.current))
    } else {
      return(NA)
    }
  } else {
    return(input)
  }
}

getSpeciesFromEcoscope <- function(speciesFAOName) {
  if (! require(rrdf)) {
    stop("Missing rrdf library")
  }
  
  if (missing(speciesFAOName) || is.na(speciesFAOName) || nchar(speciesFAOName) == 0) {
    stop("Missing speciesFAOName parameter")
  }
  
  sparqlResult <- sparql.remote("http://ecoscopebc.mpl.ird.fr/joseki/ecoscope", 
                                paste("PREFIX ecosystems_def: <http://www.ecoscope.org/ontologies/ecosystems_def/> ", 
                                      "PREFIX skos:<http://www.w3.org/2004/02/skos/core#> ", 
                                      "SELECT * WHERE { ?uri ecosystems_def:faoId '", 
                                      speciesFAOName, 
                                      "' .?uri skos:prefLabel ?scientific_name}", 
                                      sep="")
                                )
  return(sparqlResult)
}

FAO2URIFromEcoscope <- function(FAOId) {
  if (! require(rrdf)) {
    stop("Missing rrdf library")
  }
  
  if (missing(FAOId) || is.na(FAOId) || nchar(FAOId) == 0) {
    stop("Missing FAOId parameter")
  }
  
  sparqlResult <- sparql.remote("http://ecoscopebc.mpl.ird.fr/joseki/ecoscope", 
                                paste("PREFIX ecosystems_def: <http://www.ecoscope.org/ontologies/ecosystems_def/> ", 
                                      "SELECT * WHERE { ?uri ecosystems_def:faoId '", FAOId, "'}", sep="")
  )
  if (length(sparqlResult) > 0) {
    return(as.character(sparqlResult[1, "uri"]))
  } 
  
  return(NA)
}


buildRdf <- function(rdf_file_path, rdf_subject, titles=c(), descriptions=c(), subjects=c(), processes=c(), start=NA, end=NA, spatial=NA) {
  if (! require(rrdf)) {
    stop("Missing rrdf library")
  }
  
  store = new.rdf(ontology=FALSE)
  
  add.prefix(store,
             prefix="ecoscope",
             namespace="http://www.ecoscope.org/ontologies/resources_def/")
  
  add.prefix(store,
             prefix="ical",
             namespace="http://www.w3.org/2002/12/cal/ical/")
  
  add.prefix(store,
             prefix="dct",
             namespace="http://purl.org/dc/terms/")
  
  #type
  add.triple(store,
             subject=rdf_subject,
             predicate="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             object="http://www.ecoscope.org/ontologies/resources_def/indicator")
  #title
  for (title.current in titles) {
    add.data.triple(store,
                  subject=rdf_subject,
                  predicate="http://purl.org/dc/elements/1.1/title",
                  data=title.current)
  }
  #description
  for (description.current in descriptions) {
    add.data.triple(store,
                    subject=rdf_subject,
                    predicate="http://purl.org/dc/elements/1.1/description",
                    data=description.current)
  }
  
  if (! is.na(start)) {
    add.data.triple(store,
                    subject=rdf_subject,
                    predicate="http://www.w3.org/2002/12/cal/ical/dtstart",
                    data=start)
  }
  
  if (! is.na(end)) {
    add.data.triple(store,
                    subject=rdf_subject,
                    predicate="http://www.w3.org/2002/12/cal/ical/dtend",
                    data=end)
  }
  
  for (subject.current in subjects) {
    URI <- FAO2URIFromEcoscope(subject.current)
    if (! is.na(URI)) {
      add.triple(store,
                      subject=rdf_subject,
                      predicate="http://purl.org/dc/elements/1.1/subject",
                      object=URI)
    } else {
      add.data.triple(store,
                 subject=rdf_subject,
                 predicate="http://purl.org/dc/elements/1.1/subject",
                 data=subject.current)
    }
  }
  
  if (! is.na(spatial)) {
    add.data.triple(store,
                    subject=rdf_subject,
                    predicate="http://purl.org/dc/terms/spatial",
                    data=spatial)
  }
  
  save.rdf(store=store, filename=rdf_file_path)
}

readData <- function(connectionType="local", dataType="csv", url, MDSTQuery, layer, ogcFilter, wfsVersion="1.1.0", ...) {
  
  if (missing(url)) {
    stop("Missing url parameter")
  }
  
  #check for connection type
  if (! connectionType %in% c("local", "remote")) {
    stop(paste("Invalid connectionType, must be \"local\" or \"remote\""))
  }
  
  #check for data type
  if (! dataType %in% c("csv", "MDSTServer", "WFS", "OGR")) {
    stop("Invalid connectionType, must be \"csv\", \"MDSTServer\", \"OGR\" or \"WFS\"")
  }
  
  if (dataType %in% c("MDSTServer", "WFS")) {
    connectionType <- "remote"
  }
  
  if (connectionType == "local") {
    #check if file exist  
    if (! file.exists(url)) {
      stop(paste("Cannot found requested file", url))
    }
    #test if file can be read
    if (file.access(url, 4) != 0) {
      stop(paste("Cannnot read", url, "please check access rights"))
    }
  }
  
  #CSV
  if (dataType == "csv") {
    if (connectionType == "remote") {
      #download the file
      localFilePath <- downloadRemoteFile(url)
      #read the downloaded file
      df <- readData(connectionType="local", dataType="csv", url=localFilePath, ...)
      #delete the downloaded file
      file.remove(localFilePath)
      
      return(df)
    }
    if (connectionType == "local") {
      return(read.csv(url, stringsAsFactors=FALSE, ...))
    }
  }
  
  #OGR
  if (dataType == "OGR") {    
    if (connectionType == "remote") {
      #download the file
      localFilePath <- downloadRemoteFile(url)
      #read the downloaded file
      df <- readData(connectionType="local", dataType="OGR", url=localFilePath, ...)
      #delete the downloaded file
      file.remove(localFilePath)
      
      return(df)
    }
    if (connectionType == "local") {
      require(rgdal)
      #test for layer
      availableLayers <- ogrListLayers(url)
      if (missing(layer)) {
        if (length(availableLayers) == 1) {
          layer <- availableLayers[1]
        } else {
          stop(paste("No layer parameter specified. Available layers: [", paste(availableLayers, collapse=", ") ,"]", sep=""))
        }
      }
      if (! layer %in% availableLayers) {
        stop(paste("Specified layer [", layer, "] not found. Available layers: [", paste(availableLayers, collapse=", ") ,"]", sep=""))
      }
      return(readOGR(url, layer, disambiguateFIDs=TRUE, stringsAsFactors=FALSE))
    }
  }
  
  #MDSTServer
  if (dataType == "MDSTServer") {
    if (missing(MDSTQuery)) {
      stop("Missing MDSTQuery parameter")
    }
    #download the file from a MDST data server
    localFilePath <- downloadMDSTServerData(url, MDSTQuery)    
    #read the file
    df <- readData(connectionType="local", dataType="csv", url=localFilePath)
    #delete the downloaded file
    file.remove(localFilePath)
    
    return(df)
  }
  
  #WFS
  if (dataType == "WFS") {
    if (missing(layer)) {
      stop("Missing layer parameter")
    }
    #build base wfs getFeature request url
    wfsUrl <- paste(url, "?REQUEST=GetFeature&SERVICE=WFS&VERSION=", wfsVersion, "&TYPENAME=", layer, sep="")
    #test for OGC Filter
    if(! missing(ogcFilter) && nchar(ogcFilter) > 0) {
      if(substr(ogcFilter, 1, 7) == "<ogc:Filter") {
        ogcFilter <- curlEscape(ogcFilter)
      }
      wfsUrl <- paste(wfsUrl, "&filter=", ogcFilter, sep="")
    }
    #read the GML file
#old version    
    #localFilePath <- downloadRemoteFile(wfsUrl)
    #read the file
    #df <- readData(connectionType="local", dataType="OGR", url=localFilePath)
    #delete the downloaded file
#new version
    df <- readWFS(wfsUrl)

    return(df)
  }
}

downloadRemoteFile <- function(url) {
  require(RCurl)
  if (! url.exists(url)) {
    stop(paste("Specified url not exist", url))
  }
  
  file.dest <- tempfile()
  download.file(url=url, destfile=file.dest, method="curl", quiet=TRUE)
  if (file.exists(file.dest)) {
    return(file.dest)
  } else {
    stop(paste("Cannot found downloaded file", file.dest))
  }
  return(NA)
}

downloadMDSTServerData <- function(MDSTServerUrl, xmlQuery) {
  require(RCurl)
  require(XML)
  
  #create a CURL reader
  r <- basicTextGatherer()
  r$reset()
  #post the query
  curlPerform(url=MDSTServerUrl, 
              postfields=xmlQuery,
              httpheader=c(Accept="application/xml", "Content-Type"="application/xml"),
              verbose=TRUE, 
              writefunction = r$update)
  #read the result
  xml.doc <- xmlTreeParse(file=r$value(), asText=TRUE, useInternal=TRUE)
  file.web.path <- xpathApply(xml.doc, "//fileWebPath", xmlValue)
  if (length(file.web.path) == 1) {
    #download the zipped file
    zip.file.dest <- downloadRemoteFile(url=unlist(file.web.path))
    #list the includded files
    zip.files.list <- unzip(zip.file.dest, list=TRUE)
    #find th csv one
    file.csv <- grep("*.csv", tolower(zip.files.list$Name))    
    if (length(file.csv) == 1) {
      #extract the csv file
      unzip(zipfile=zip.file.dest, files=zip.files.list$Name[file.csv], exdir=tempdir())
      #delete zip file
      file.remove(zip.file.dest)
      file.dest <- paste(tempdir(), zip.files.list$Name[file.csv], sep="/")
      if (file.exists(file.dest)) {
        return(file.dest)
      } else {
        stop(paste("Cannot found downloaded file", file.dest))
      }
    }
  }
  return(NA)
}