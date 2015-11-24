#Taha Imzilen & Julien Barde - IRD

#52North WPS annotations
# wps.des: id = knitrCompile, title = Generic R code to compile knitr documents, abstract =  This script is meant to enable the compilation of any knitr Code using open data and source code avilable through URLS or made available by complying with a template of sub-repositories coming along the knitr main code within a zip file;
# wps.in: id = upload_report, type = string, title = , value = "/main.Rnd";
# wps.in: id = report_parametrization, type = string, title = Data URL, value = "http://mdst-macroes.ird.fr:8080/constellation/WS/wfs/tuna_atlas";

# wps.out: id = Pdfresult, type = string, title = result file path list;

#load knitr Package
require(knitr)
require(rworldmap)
#load specifi R Packages and related functions parametrizations when needed to customize the report
#Specify the working directory
mywd <- '/home/julien/SVNs/GIT/IRDTunaAtlas/'
report_subdirectory <- paste(mywd,'report/knitr/',sep="")
setwd(mywd)
report_parametrization_file <-paste(report_subdirectory,"KnitrTest2_parametrization.R",sep="")
source(report_parametrization_file)
#Specify the names of knitr file to be compiled and resulting latex file name (knitr output)
setwd(report_subdirectory)
# file.in <-'KnitrTest2.Rnw'
# file.out <- 'KnitrTest2.tex'
file.in <-'ICCAT_Report_BFTE.Rnw'
file.out <- 'ICCAT_Report_BFTE.tex'
# file.in <-'Report_Sardara.Rnw'
# file.out <- 'Report_Sardara.tex'
# system(paste("unzip ",zipfile, " ./", sep=""))
#knitr Compilation
knit(file.in,file.out) 
#Latex Compilation
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))

