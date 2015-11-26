#Taha Imzilen & Julien Barde - IRD - BlueBridge project http://www.bluebridge-vres.eu/ 

#R annotations to get OGC WPS metadata from 52North WPS Server Java API
# wps.des: id = knitrCompile, title = Generic R code to compile knitr documents, abstract =  This script is meant to enable the compilation of any knitr Code using open data and source code avilable through URLS or made available by complying with a template of sub-repositories coming along the knitr main code within a zip file;
# wps.in: id = upload_zip_knitr_report, type = string, title = Zip file containing all files required for knitR compilation, value = "";
# wps.in: id = file_prefix, type = string, title = Prefix of files to be used for compilation, value = "main_report.Rnw";
# wps.out: id = Pdfresult, type = string, title = result file path list, value = "main_report.pdf";

#STEP: load knitr Package
require(markdown)
#STEP 1: Specify the working directory of the machine running the process
mywd <- '/home/julien/SVNs/GIT/IRDTunaAtlas/'
# mywd <- '/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/reports/'

#STEP 2: file.inout variable will store the prefix used for the names of the 2 default files expected to run the compilation:
#   * "name.Rmd" for the markdown code
file.inout <-'examples_codes'


#STEP 3: file.inout variable will store the prefix used for the names of the 2 default files expected to run the compilation:
# zip='/tmp/knitr.zip'
zip='/tmp/ICCAT_report.zip'

#STEP 3: Execution of the function which compiles the knitR code
# Run_Report(path.inout = mywd,file.inout =file.inout,show.pdf=F)
Run_markdown_Report(path.inout = mywd,file.inout =file.inout, zip_file = zip, show.pdf=F)
