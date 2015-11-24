#Taha Imzilen & Julien Barde - IRD
#52North WPS annotations
# wps.des: id = knitrCompile, title = Generic R code to compile knitr documents, abstract =  This script is meant to enable the compilation of any knitr Code using open data and source code avilable through URLS or made available by complying with a template of sub-repositories coming along the knitr main code within a zip file;
# wps.in: id = upload_report, type = string, title = , value = "/main.Rnd";
# wps.in: id = report_parametrization, type = string, title = Data URL, value = "http://mdst-macroes.ird.fr:8080/constellation/WS/wfs/tuna_atlas";
# wps.out: id = Pdfresult, type = string, title = result file path list;

#load knitr Package
require(knitr)
#load specifi R Packages and related functions parametrizations when needed to customize the report
#Specify the working directory
mywd <- '/home/julien/SVNs/GIT/IRDTunaAtlas/'
# mywd <- '/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/reports/'

# file.inout <-'KnitrTest2'
# file.inout <-'KnitrTest2'
file.inout <-'ICCAT_Report_BFTE'

Run_Report(path.inout = mywd,file.inout =file.inout,show.pdf=F)

Run_Report <- function(path.inout,file.inout,show.pdf=F){

report_subdirectory <- paste(mywd,'report/knitr/',sep="")
setwd(report_subdirectory)

#Creating variables for names of files requires to be compiled
file.in <- paste(file.inout,".Rnw",sep="")
file.out <- paste(file.inout,".tex",sep="")
# report_parametrization_file
file.input.par <- paste(file.inout,".R",sep="")

#### Check existence of .R and .Rnw fields
if(!file.exists(paste(report_subdirectory,file.input.par,sep="/"))){ stop("You have to create the .R field.")}
if(!file.exists(paste(report_subdirectory,file.in,sep="/"))){ stop("You have to create the .Rnw field.")}
##
report_parametrization_file<-paste(report_subdirectory,file.input.par,sep="")
# source(report_parametrization_file)
source(report_parametrization_file)

#Specify the names of knitr file to be compiled and resulting latex file name (knitr output)
setwd(report_subdirectory)

# system(paste("unzip ",zipfile, " ./", sep=""))
#knitr Compilation
knit(file.in,file.out) 
#Latex Compilation
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
if(show.pdf){
  system(paste("acroread ",file.inout,".pdf",sep=""))
}

}
