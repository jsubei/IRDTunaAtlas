######################################################################
##### CLEAR WORKSPACE AND SET UP WORKING DIRECTORY ##########
######################################################################
rm(list=ls())
######################################################################
##### SET UP PACKAGES ##########
######################################################################
if(!require(maps)){
  install.packages("maps")
  library(maps)
}
if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}
if(!require(IndicatorsForFisheries)){
  install_github("IRDTunaAtlas", "juldebar")
}
library(IndicatorsForFisheries)
if(!require(plyr)){
  library(plyr)
}
if(!require(maptools)){
  install.packages("maptools")
  library(maptools)
}
if(!require(sp)){
  install.packages("sp")
  library(sp)
}
if(!require(rgeos)){
  install.packages("rgeos")
  library(rgeos)
}
if(!require(ggmap)){
  install.packages("ggmap")
  library(ggmap)
}
if(!require(shapefiles)){
  install.packages("shapefiles")
  library(shapefiles)
}
if(!require(rworldmap)){
  install.packages("rworldmap")
  library(rworldmap)
}
if(!require(geosphere)){
  install.packages("geosphere")
  library(geosphere)
}
if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}
if(!require(RCurl)){
  install.packages("RCurl")
  library(RCurl)
}
if(!require(rgdal)){
  install.packages("rgdal")
  library(rgdal)
}
if(!require(mapplots)){
  install.packages("mapplots")
  library(mapplots)
}

if(!require(XML)){
  install.packages("XML")
  library(XML)
}

# http://slidify.github.io/dcmeetup
# http://slidify.github.io/playground/
if(!require(knitr)){
  install.packages("knitr")
  library(knitr)
}
if(!require(rmarkdown)){
  install.packages("rmarkdown")
  library(rmarkdown)
}
if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}
if(!require(slidify)){
  install_github('ramnathv/slidify')
  install_github('ramnathv/slidifyLibraries')
  library(slidify)

}

zip_package_url = "https://github.com/juldebar/IRDTunaAtlas/archive/master.zip"
working_directory_init=getwd()
setwd(working_directory_init)
zip_file=paste(working_directory_init,"/master.zip",sep="")
download.file(zip_package_url, destfile = zip_file, method='auto', quiet = FALSE, mode = "w",cacheOK = TRUE,extra = getOption("download.file.extra"))
unzip(zip_file, overwrite = T, exdir = working_directory_init)
system("chmod -R 777 ./*")



mywd <- paste(getwd(),"/IRDTunaAtlas-master/",sep="")
setwd(mywd)
# source(paste(mywd,"/R/IRDTunaAtlas_julien.R",sep=""))
# source(paste(mywd,"/R/wkt2spdf.R",sep=""))
# source(paste(mywd,"/R/Atlas_i1_SpeciesByOcean_julien.R",sep=""))
# source(paste(mywd,"/R/Atlas_i2_SpeciesByGear_julien.R",sep=""))
# source(paste(mywd,"/R/Atlas_i3_SpeciesYearByGearMonth_julien.R",sep=""))
# source(paste(mywd,"/R/Atlas_i4_SpeciesMonthByOcean_julien.R",sep=""))
# source(paste(mywd,"/R/Atlas_i6_SpeciesMap_julien.R",sep=""))
# source(paste(mywd,"/R/Atlas_i7_SpeciesMapRelativeCatches.R",sep=""))
# source(paste(mywd,"/R/Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies.R",sep=""))
# source(paste(mywd,"/R/Atlas_i9_RelativeSizeFrequenciesBySchoolType_julien.R",sep=""))
# source(paste(mywd,"/R/Atlas_i10_RelativeSizeFrequenciesByDecade.R",sep=""))
# source(paste(mywd,"/R/Atlas_i11_CatchesByCountry.R",sep=""))

# require(IndicatorsForFisheries)
report_subdirectory <- paste(mywd,'/report/markdown/',sep="")
setwd(report_subdirectory)
# slidify("examples_codes.Rmd")
slidify("markdown_report.Rmd")
# knit('markdown_report.Rmd')
# http://mdst-macroes.ird.fr/tmp/SpeciesMapRelativeCatches/cdn/I7_Thunnus_albacares_2000_to_2000_.png
# http://mdst-macroes.ird.fr/tmp/SpeciesMapRelativeCatchesOtherSpecies/I8_Thunnus_albacares_2000-2000_.png
# rm(list=ls())
# https://github.com/yihui/knitr-examples/blob/master/001-minimal.md

