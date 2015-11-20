#Julien Barde - IRD

#52North WPS annotations
# wps.des: id = knitrCompile, title = Generic R code to compile knitr documents, abstract =  This script is meant to enable the compilation of any knitr Code using open data and source code avilable through URLS or made available by complying with a template of sub-repositories coming along the knitr main code within a zip file;
# wps.in: id = default_URL_executeResponse_XML, type = string, title = Default URL ExecuteResponse, value = "http://mdst-macroes.ird.fr/tmp/SpeciesMapRelativeCatchesOtherSpecies/default/cdn/WebProcessingService.xml";
# wps.in: id = default_URL_executeResponse_Json, type = string, title = Default URL ExecuteResponse, value = "http://mdst-macroes.ird.fr/tmp/SpeciesMapRelativeCatchesOtherSpecies/default/cdn/WebProcessingService.json";



# wps.in: id = upload_report, type = string, title = , value = "/main.Rnd";
# wps.in: id = report_parametrization, type = string, title = Data URL, value = "http://mdst-macroes.ird.fr:8080/constellation/WS/wfs/tuna_atlas";

# wps.out: id = result, type = string, title = result files path list;

#load knitr Package
require(knitr)
#load specifi R Packages and related functions parametrizations when needed to customize the report
#Specify the working directory
mywd <- '/home/julien/SVNs/GIT/IRDTunaAtlas/report/knitr'
setwd(mywd)
source('KnitrTest2_parametrization.R')
#Specify the names of knitr file to be compiled and resulting latex file name (knitr output)
file.in <-'KnitrTest2.Rnw'
file.out <- 'KnitrTest2.tex'
# system(paste("unzip ",zipfile, " ./", sep=""))
#knitr Compilation
knit(file.in,file.out) 
#Latex Compilation
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))

