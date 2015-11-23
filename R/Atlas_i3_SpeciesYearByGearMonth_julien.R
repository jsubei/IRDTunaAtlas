#Atlas_i3_SpeciesYearByGearMonth.R
#Tuna Atlas - IRD / MR EME
#
#This indicator build a graph of catches of a given species for a given year by gear type and by month. Provide comparaison with monthly mean of the 5 previous years. An associated RDF file is also produced.
##################################################################
#Norbert Billet - IRD
#2013/11/04: V2 version: add RDF export and allow production of multiple graph (i.e. species)
#2013/09/03: Norbert - Add attributes names as parameters
#2013/08/30: Norbert - Modifications to use with IRDTunaAtlas package
#2013/03/15: Norbert - Initial version
##################################################################
#Use example:
# library(IRDTunaAtlas)
# csv.df <- read.csv("/home/norbert/Boulot/iMarine/WPS/Atlas/CSV/i3.csv", stringsAsFactors=FALSE)
# csv.df <- csv.df[csv.df$species == "MLS",]
# Atlas_i3_SpeciesYearByGearMonth(csv.df, 
#                                    yearAttributeName="year",
#                                    monthAttributeName="month",
#                                    speciesAttributeName="species",
#                                    gearTypeAttributeName="gear_type",
#                                    valueAttributeName="value",
#                                    meanPrev5YearsAttributeName="mean_prev_5_years",
#                                    stddevPrev5YearsAttributeName="stddev_prev_5_years")
##################################################################
# library(rCharts)
# library(jsonlite)

#  source("/home/tomcat7/temp/IRDTunaAtlas.R")
# source("/home/julien/SVNs/GIT/IRDTunaAtlas/R/IRDTunaAtlas_julien.R")
Atlas_i3_SpeciesYearByGearMonth_julien<- function(df, 
                                            yearAttributeName="year", 
                                            monthAttributeName="month", 
                                            speciesAttributeName="species",
                                            gearTypeAttributeName="gear_type",
                                            valueAttributeName="value",
                                            meanPrev5YearsAttributeName="mean_prev_5_years",
                                            stddevPrev5YearsAttributeName="stddev_prev_5_years",
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
  
  if(sum(names(df) == monthAttributeName) == 0) {
    stop("Cannot found month attribute")
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
  
  if(sum(names(df) == meanPrev5YearsAttributeName) == 0) {
    stop("Cannot found mean for previous years attribute")
  }
  
  if(sum(names(df) == stddevPrev5YearsAttributeName) == 0) {
    stop("Cannot found std_dev for previous years attribute")
  }
  
  #format columns  
  df[, yearAttributeName] <- as.numeric(df[, yearAttributeName])
  df[, monthAttributeName] <- as.numeric(df[, monthAttributeName])
  df[, speciesAttributeName] <- as.factor(df[, speciesAttributeName])
  df[, gearTypeAttributeName] <- as.factor(df[, gearTypeAttributeName])
  df[, valueAttributeName] <- as.numeric(df[, valueAttributeName])
  df[, meanPrev5YearsAttributeName] <- as.numeric(df[, meanPrev5YearsAttributeName])
  df[, stddevPrev5YearsAttributeName] <- as.numeric(df[, stddevPrev5YearsAttributeName])
  
  #rename columns
  names(df)[which(names(df) == yearAttributeName)] <- "year"
  names(df)[which(names(df) == monthAttributeName)] <- "month"  
  names(df)[which(names(df) == speciesAttributeName)] <- "species"
  names(df)[which(names(df) == gearTypeAttributeName)] <- "gear_type"
  names(df)[which(names(df) == valueAttributeName)] <- "value"
  names(df)[which(names(df) == meanPrev5YearsAttributeName)] <- "mean_prev_5_years"
  names(df)[which(names(df) == stddevPrev5YearsAttributeName)] <- "stddev_prev_5_years"
  
  #from std deviation to variance, and root square the sum of variances
  fct <- function(vec)
  {
    var <- vec * vec
    var <- sum(var)
    return(sqrt(var))
  }
  
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
  
  URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesByYearByMonthByGear/default/"
  repository<-paste(mywd,"outputs/www/html/tmp/SpeciesByYearByMonthByGear/default/",sep="")
#   URL<-"http://mdst-macroes.ird.fr/tmp/SpeciesByYearByMonthByGear/cdn/"
#   repository<-"/data/www/html/tmp/SpeciesByYearByMonthByGear/cdn/"
  
  
  
  #for each species
  for (species.current in unique(df$species)) {
    
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
    #for each year
    for (year.current in unique(df[df$species == species.current,]$year)) {
      current.df <- df[df$species == species.current & df$year == year.current,]
      
      if (! all(table(current.df$month) == 1)) {
        if (all(is.na(current.df$stddev_prev_5_years))) {
          stddev.agg <- cbind(month=unique(current.df$month), stddev_prev_5_years=NA)
        } else {
          stddev.agg <- aggregate(stddev_prev_5_years ~ month, data=current.df, fct)
        }
        
        if (all(is.na(current.df$mean_prev_5_years))) {
          mean.agg <- cbind(month=unique(current.df$month), mean_prev_5_years=NA)
        } else {
          mean.agg <- aggregate(mean_prev_5_years ~ month, data=current.df, sum)
        }
        
        dfPrev5Years <- merge(mean.agg, stddev.agg)            
        
      } else {
        dfPrev5Years <- current.df
      }
      


      
      #order gear factor levels by value
      current.df$gear_type <- factor(current.df$gear_type, levels=rev(levels(reorder(current.df$gear_type, current.df$value))))
      #set proper month label
      current.df$month <- factor(month.abb[current.df$month], levels=levels(reorder(month.abb[current.df$month], current.df$month)))
      dfPrev5Years$month <- factor(month.abb[dfPrev5Years$month], levels=levels(reorder(month.abb[dfPrev5Years$month], dfPrev5Years$month)))
      dfPrev5Years$intervalWidthsMax = dfPrev5Years$mean_prev_5_years + dfPrev5Years$stddev_prev_5_years
      dfPrev5Years$intervalWidthsMin = dfPrev5Years$mean_prev_5_years - dfPrev5Years$stddev_prev_5_years
#       intervalWidths <- dfPrev5Years[,c("month","intervalWidthsMax","intervalWidthsMin")]
intervalWidths <- list()
for (i in 1:12) {
  intervalWidths[[i]] <- c(dfPrev5Years$intervalWidthsMin[i], dfPrev5Years$intervalWidthsMax[i])
}
#       intervalWidths <- c(dfPrev5Years$intervalWidthsMax, dfPrev5Years$intervalWidthsMin)
#       dfPrev5Years$intervalWidth = intervalWidths
# for (month in 1:12) {
  #         intervalWidths[[month]] <- c(mean_prev_5_years + stddev_prev_5_years, mean_prev_5_years - stddev_prev_5_years)
# intervalWidths$month <- factor(month.abb[dfPrev5Years$month], levels=levels(reorder(month.abb[dfPrev5Years$month], dfPrev5Years$month)))
# intervalWidths$errobar <- merge(dfPrev5Years$intervalWidthsMax, dfPrev5Years$intervalWidthsMin)
# }
      
      n = c("gear_type","mean_prev_5_years")
      #build the plot
      resultPlot <- ggplot() +
        layer(data=current.df,
              mapping=aes(x=month, y=value, fill=gear_type, order=gear_type),
              stat="identity",
              geom="bar") +
        layer(data=dfPrev5Years,
              mapping=aes(x=month, y=mean_prev_5_years, group=1),
              stat="identity",
              geom="line") + 
        layer(data=dfPrev5Years,
              mapping=aes(x=month, ymax=mean_prev_5_years + stddev_prev_5_years, ymin=mean_prev_5_years - stddev_prev_5_years), 
              width=0.25, 
              color="dimgray",
              stat="identity",
              geom="errorbar") +
        scale_fill_manual(name="Gear type", values=my.colors) +
        xlab("Month") + ylab("Catches in tons") + 
        ggtitle(paste(species.label, "monthly catches by gear type on", year.current))



      #draw the plot
#     tempfile.base <- tempfile(pattern=paste("I3_", gsub(" ", "_", species.label), "_", as.character(year.current), "_", sep=""))
      filename <- paste("I3", gsub(" ", "_", species.label),as.character(year.current), sep="_")
      tempfile.base <- paste(repository,filename, sep="")
      plot.filepath <- paste(tempfile.base, ".png", sep="")
      ggsave(filename=plot.filepath, plot=resultPlot,  width=20, unit="cm", dpi=300)
      plot.URLpng <- paste(URL,filename, ".png", sep="")

      
      ##debut Julien
#       plotRchartsHighcharts  <- hPlot(value ~ year, data = current.df, type = c('column', 'line'), group = 'gear_type', radius = 6)
#spline
#plotRchartsHighcharts  <- hPlot(value ~ month, data = current.df, type = 'line', group = 'gear_type', radius = 6, title = "Catches per month per fishing gear")
plotRchartsHighcharts  <- hPlot(value ~ month, data = current.df, type = 'column', group = 'gear_type', radius = 6, title = "Catches per month per fishing gear")
# plotRchartsHighcharts  <- hPlot(value ~ month, data = current.df, type = c('column', 'line'), group = 'gear_type, mean_prev_5_years', radius = 6)
      plotRchartsHighcharts$plotOptions(column = list(stacking = "normal",dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))
#       plotRchartsHighcharts$xAxis(title = list(text = "Months"),labels = list(rotation = -45, align = 'right', style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif')), replace = F)
#       plotRchartsHighcharts$yAxis(title = list(text = "Catches"), tickInterval = 2)
      plotRchartsHighcharts$chart(width = 800,height = 400, zoomType = "xy")
      plotRchartsHighcharts$legend(align = 'center', verticalAlign = 'top', y = 30, margin = 20)
      plotRchartsHighcharts$exporting(enabled = T)
#       plotRchartsHighcharts$chart("#339900", "#FF9900")

# test de superposition
h <- Highcharts$new()
h  <- hPlot(value ~ month, data = current.df, type = 'column', group = 'gear_type', radius = 6, title = "Catches per month per fishing gear")
#h$xAxis(categories = current.df$month)
h$yAxis(list(title = list(text = 'Catches')), list(title = list(text = 'mean_prev_5_years'), opposite = TRUE), list(title = list(text = 'stddev_prev_5_years'), opposite = TRUE))
#good
# h$yAxis(list(title = list(text = 'Catches')), list(title = list(text = 'stddev_prev_5_years'), opposite = TRUE))
#h$yAxis(list(title = list(text = 'Catches')), list(title = list(text = 'stddev_prev_5_years'), opposite = TRUE))
# h$series(name = 'Catches', type = 'column', color = '#4572A7', data = current.df$value, group = 'gear_type')
h$series(name = 'mean_prev_5_years', type = 'spline', color = '#89A54E', data = dfPrev5Years$mean_prev_5_years)
#pour rCharts une liste de tableaux alors que Tableaux de Tableaux pour HighCharts
h$series(name = 'stddev_prev_5_years', type = 'errorbar', color = '#0d0c0c',  whiskerLength = "40%", lineWidth = "3", data = intervalWidths)

# h$series(name = 'stddev_prev_5_years', type = 'errorbar', color = '#AA4643', data = toJSONArray2(intervalWidths$errobar))
h$chart(width = 800,height = 400, zoomType = "xy")
h$plotOptions(column = list(stacking = "normal",dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))
h$exporting(enabled = T)

# ymax=mean_prev_5_years + stddev_prev_5_years, ymin=mean_prev_5_years - stddev_prev_5_years
         


#       samplePlot$xAxis(title = list(text="Name of Child"), type = "category", labels = list(rotation = 60, align = "left"))
#       samplePlot$plotOptions(column = list(stacking = "normal", dataLabels = list(enabled = T,align = 'center', verticalAlign = "top",  color = '#FFFFFF', y = 10)))
#       #samplePlot

      ## OK{title: MultiBar Chart}  n = c("gear_type","mean_prev_5_years")multiChart
#       plotRchartsNVD3 <- nPlot(value ~ month, group='gear_type', data = current.df, type = 'multiBarChart')
# #       plotRchartsNVD3$layer(value ~ month, data = current.df, type = 'line', group = 'mean_prev_5_years', radius = 6)
# # plotRchartsNVD3$chart(color = c('brown', 'blue', '#594c26', 'green'))
# # plotRchartsNVD3$set(multi = list(mean_prev_5_years = list(type="line", yAxis=1), gear_type = list(type="multiBarChart", yAxis=2)))
# # plotRchartsNVD3$setTemplate(script = system.file("/home/julien/R/x86_64-pc-linux-gnu-library/3.1/rCharts/libraries/nvd3/layouts/multiChart.html", package = "rCharts"))
# # plotRchartsNVD3$params$multi <-
# #   list(x = list(type="bar", yAxis = 1),
# #        y = list(type="bar", yAxis = 1),
# #        z = list(type="line", yAxis = 2))
#       plotRchartsNVD3$xAxis(axisLabel = 'Months and Years')
#       plotRchartsNVD3$yAxis(axisLabel = 'Catches')
#       #plotRchartsNVD3$chart(useInteractiveGuideline=TRUE)
#       plotRchartsNVD3

# p12$set(multi = list(
#   uempmed = list(type="area", yAxis=1),
#   psavert = list(type="line", yAxis=2)
# ))

# plotRchartsNVD3 <- nPlot(value | mean_prev_5_years ~ month, group='gear_type', data = current.df, type = 'line')
plotRchartsNVD3 <- nPlot(value ~ month, group='gear_type', data = current.df, type = 'multiBarChart',width = 800,height = 400)
# plotRchartsNVD3$chart(color = c('brown', 'blue', '#594c26', 'green'))
# plotRchartsNVD3$chart( useInteractiveGuideline=TRUE)
plotRchartsNVD3$xAxis(axisLabel = 'Months and Years')
plotRchartsNVD3$yAxis(axisLabel = 'Catches')
plotRchartsNVD3


# plot <- nPlot(value ~ Date,
#               data = test,
#               group = "Item",
#               type = 'multiChart')
# 
# #Set which axes the item should follow
# plot$params$multi <-
#   list(x = list(type="bar", yAxis = 1),
#        y = list(type="bar", yAxis = 1),
#        z = list(type="line", yAxis = 2))

# p12 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'multiChart')
# p12$set(multi = list(
#   uempmed = list(type="area", yAxis=1),
#   psavert = list(type="line", yAxis=2)
# ))
# p12$setTemplate(script = system.file(
#   "/libraries/nvd3/layouts/multiChart.html",
#   package = "rCharts"
# ))
# p12


      ### {title: Bar Chart, working: FALSE}
      p1 <- xCharts$new()
      p1$set(xScale = 'month', yScale = 'value')
      p1$layer(value ~ month, data = current.df, type = 'bar', group='gear_type')
#p1$layer(value ~ mean_prev_5_years, data = current.df, type = 'bar')#line-dotted
      p1




# p1 <- Rickshaw$new()
# p1$layer(~ cyl, group = 'am', data = mtcars, type = 'bar')
# p1
      ## Dataset in HTML
      Datatable <- dTable(
#         current.df,
        dfPrev5Years,
#          dfPrev5Years,intervalWidths,intervalWidths$month
        sPaginationType= "full_numbers"
      )
      Datatable





      ## Storage of files in a given repository (temporary or permanent)
      plot.filepathtml <- paste(tempfile.base, ".html", sep="")
      plot.filepathtmlbis <- paste(tempfile.base, "_bis.html", sep="")
      plot.filepathtmlter <- paste(tempfile.base, "_bingo.html", sep="")
      plot.URLhtml <- paste(URL,filename, ".html", sep="")
      plot.URLhtmlbis <- paste(URL,filename, "_bis.html", sep="")
      plot.URLhtmlter <- paste(URL,filename, "_bingo.html", sep="")
      plotRchartsHighcharts$save(plot.filepathtml,standalone=TRUE) 
#      plotRchartsHighcharts$save(plot.filepathtml,cdn=TRUE)     
      p1$save(plot.filepathtmlbis,standalone=TRUE) 
#       p1$save(plot.filepathtmlbis,cdn=TRUE)   
# celui qui marche pas avec cdn
      h$save(plot.filepathtmlter,standalone=TRUE) 
#       h$save(plot.filepathtmlter,cdn=TRUE)   

      ## Storage of files in a given repository (temporary or permanent)
      plot.filepathtmlNVD3 <- paste(tempfile.base, "_NVD3.html", sep="")
      plot.URLRchartsNVD3 <- paste(URL,filename, "_NVD3.html", sep="")
      plotRchartsNVD3$save(plot.filepathtmlNVD3,standalone=TRUE) 
#       plotRchartsNVD3$save(plot.filepathtmlNVD3,cdn=TRUE)   

      #Datatable
      plot.filepathtmltable <- paste(tempfile.base, "_table.html", sep="")
      plot.URLhtmlTable <- paste(URL,filename, "_table.html", sep="")    
      Datatable$save(plot.filepathtmltable,standalone=TRUE)     
#       Datatable$save(plot.filepathtmltable,cdn=TRUE)       


################################################################################################


# ligne <- data.frame(TYPE="URI", URL=URI,  stringsAsFactors=FALSE)


################################################################################################



#Metadata elements (in addition to OGC WPS metadata) to describe the current indicator which will be used by other applications (Ecoscope and Tuna Atlas Websites)
# 
# titles=c(paste(species.label, "catches by gear type for each month of a given year"), 
#          paste("Captures de", species.label, "par type d'engin pour chaque mois d'une année donnée"))
# 
# 
# descriptions=c(c("en",paste("IRD Tuna Atlas: indicator #3 - Catches for ",species.label, "species: catches by gear type for each month of the year", as.character(year.current), sep=" ")),
#                c("fr",paste("IRD Atlas Thonier: indicator #3 - Captures par type d'engins de pêche pour l'espèce:",species.label, "pour chaque mois de l'année", as.character(year.current), sep=" ")))
# 
# 
# subjects=c(as.character(species.current), as.character(unique(current.df$gear_type)))
# #Collect the URIs of related Topics from Ecoscope SPARQL endpoint
# URI <- FAO2URIFromEcoscope(as.character(species.current))
# tabURIs<- data.frame(type="species",URI=URI,stringsAsFactors=FALSE)
# 
# # for (gear_type.current in unique(df$gear_type)) {
# 
# # URIGear <- FAO2URIFromEcoscope(as.character(unique(current.df$gear_type)))
# # ligne<- c(x="gear",y=URIGear)
# # tabURIs<- rbind(tabURIs,ligne)
# 
# # }
# # EVENTUELLEMENT AJOUTER D'AUTRES SUJETS COMME LA ZONE
# #subjects=c(as.character(species.current), as.character(gear_type.current), as.character(unique(current.df$gear_type))),
# #subjects=c(as.character(species.current)),
# #subjects=c(as.character(species.current), as.character(gear_type.current)),
# #data_input=url,
# 
# #TODO julien => A ADAPTER AVEC LA CONVEX HULL / ou la collection DE TOUTES LES GEOMETRIES CONCERNEES
# spatial_extent="POLYGON((-180 -90,-180 90,180 90,180 -90,-180 -90))"
# temporal_extent_begin=as.character(year.current)
# temporal_extent_end=as.character(year.current)
# 
# rdf.filepath <- paste(repository, ".rdf", sep="")
# #rdf.filepath <- paste(tempfile.base, ".rdf", sep="")
# rdf.URL <- paste(URL,filename, ".rdf", sep="")
# # il faudrait ajouter un attribut qui précise le type de visualisation: carte, chart...
# data_output_identifiers=data.frame(titre="1 en fait y a pas besoin de cet attribut",type="image", year=temporal_extent_begin, fileURL=plot.URLpng, stringsAsFactors=FALSE)
# ligne <- c(titre="2 en fait y a pas besoin de cet attribut",type="stacked bar", year=temporal_extent_begin, year=temporal_extent_begin, fileURL=plot.URLhtml)
# data_output_identifiers <- rbind(data_output_identifiers, ligne)
# ligne <- c(titre="3 en fait y a pas besoin de cet attribut",type="bar", year=temporal_extent_begin, fileURL=plot.URLhtmlbis)
# data_output_identifiers <- rbind(data_output_identifiers, ligne)
# ligne <- c(titre="3 en fait y a pas besoin de cet attribut",type="bar", year=temporal_extent_begin, fileURL=plot.URLhtmlter)
# data_output_identifiers <- rbind(data_output_identifiers, ligne)
# ligne <- c(titre="3 en fait y a pas besoin de cet attribut",type="bar", year=temporal_extent_begin, fileURL=plot.URLRchartsNVD3)
# data_output_identifiers <- rbind(data_output_identifiers, ligne)
# ligne <- c(titre="6 en fait y a pas besoin de cet attribut",type="dataTable", year=temporal_extent_begin, fileURL=plot.URLhtmlTable)
# data_output_identifiers <- rbind(data_output_identifiers, ligne)
# 
# # ligneTableauResult$uri=list(data.frame(typeURI="Species",URI=URI))
# 
# download=data.frame(format="csv",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.csv", stringsAsFactors=FALSE)
# ligne <- c(format="shp",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.shp")
# download <- rbind(download, ligne)
# ligne <- c(format="GML|WKT|shp|netCDF",URL="http://mdst-macroes.ird.fr/tmp/SpeciesByGear/XXX.nc....")
# download <- rbind(download, ligne)
# 
# 
# rdf.filepath <- paste(repository, "La_totale.rdf", sep="")
# rdf.URL <- paste(URL,filename, ".rdf", sep="")
# 
# 
# #Write the RDF metadata describing the current indicator in the RDF model of the whole execution: used by Ecoscope and Tuna Atlas
# #Write the Json metadata used by the SIP
# # tableauResult$results <- data.frame(titre=character(),
# 
# tableauResult <- buildRdf(store=store,
#                           tableauResult = tableauResult,
#                           RDFMetadata=rdf.URL,
#                           rdf_file_path=rdf.filepath,
#                           rdf_subject=paste("http://www.ecoscope.org/ontologies/resources", tempfile.base, sep=""), 
#                           titles=titles,
#                           descriptions=descriptions,
#                           subjects=subjects,
#                           tabURIs=tabURIs,
#                           processes="http://www.ecoscope.org/ontologies/resources/processI3",
#                           image=plot.URLpng,
#                           data_output_identifiers=data_output_identifiers,
#                           download=download,
#                           start=temporal_extent_begin,
#                           end=temporal_extent_end,
#                           spatial=spatial_extent,
#                           withSparql)
# 
# result.df <- rbind(result.df, c(plot.file.path=plot.filepath, rdf.file.path=rdf.filepath))
      
    }
  }

# julien<-buildJson(type="multi Bar Chart", description="Rapport d'exécution du traitement i3", processSourceCode="http://mdst-macroes.ird.fr:8084/wps/R/scripts/Atlas_i3XXX.R",results=tableauResult)
julien<-'toto'

return(resultPlot)

}