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
  
  message(desc)
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
