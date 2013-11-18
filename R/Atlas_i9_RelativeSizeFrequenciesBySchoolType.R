#Norbert Billet - IRD
#2013/09/03: Norbert - Add attributes names as parameters
#2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
#2013/06/12: Norbert - First version
#Atlas_i9_RelativeSizeFrequenciesBySchoolType : build a graph relative contribution of size frequencies in catches for a species by school type


Atlas_i9_RelativeSizeFrequenciesBySchoolType <- function(gmlFile,
                                                         yearAttributeName="ns0:year",
                                                         speciesAttributeName="ns0:species",
                                                         schoolAttributeName="ns0:school",
                                                         sizeClassAttributeName="ns0:class",
                                                         fishCountAttributeName="ns0:fish_count")
{
  if(! require(XML) | ! require(ggplot2) | ! require(scales))
  {
    stop("Missing library")
  }
  
  if(missing(gmlFile))
  {
    stop("Missing gml_file")
  }      
  
  if(! checkGMLFile(gmlFile))
  {
    stop("Error with GML file")
  }
  
  myXML <- xmlInternalTreeParse(gmlFile)
  species <- xpathSApply(myXML, paste("//", speciesAttributeName, sep=""), xmlValue)
  year <- xpathSApply(myXML, paste("//", yearAttributeName, sep=""), xmlValue)
  school <- xpathSApply(myXML, paste("//", schoolAttributeName, sep=""), xmlValue)
  sizeClass <- xpathSApply(myXML, paste("//", sizeClassAttributeName, sep=""), xmlValue)
  fishCount <- xpathSApply(myXML, paste("//", fishCountAttributeName, sep=""), xmlValue)
  
  df <- data.frame(species=as.factor(species), year=as.numeric(year), school=as.factor(school), sizeClass=as.numeric(sizeClass), fishCount=as.numeric(fishCount))
  
  #check if there is only one species
  if(length(levels(df$species)) > 1)
  {
    stop("More than one species found in input data")
  }
  
  #aggregate values by size class and school type
  valuesSum <- aggregate(fishCount ~ sizeClass + school, data=df, FUN=sum)
  
  mergedDf <- data.frame(sizeClass=valuesSum$sizeClass, school=valuesSum$school, relative=valuesSum$fishCount / sum(valuesSum$fishCount))
  mergedDf$school <- factor(mergedDf$school, levels=c("BO", "BL", "IND"), labels=c("Log school", "Free school", "Undefined school"))
  mergedDf$relative <- mergedDf$relative * 100
  
  #build the plot
  resultPlot <- ggplot(data=mergedDf, mapping=aes(x=sizeClass, y=relative, group=school)) + 
    geom_line(aes(color=school)) + 
    geom_point() +
    ggtitle(paste(levels(df$species)[1], " (", min(df$year), "-", max(df$year), ") Size frequencies by school type", sep=""))  + 
    scale_x_continuous() + scale_y_continuous() +
    xlab("Size (in cm)") + ylab("Relative contribution (in %)") + 
    labs(colour="School type")
    
  #draw the plot
  file_path <- paste(tempfile(), ".png", sep="")
  ggsave(filename=file_path, plot=resultPlot, dpi=100)
  
  return(file_path)
}




