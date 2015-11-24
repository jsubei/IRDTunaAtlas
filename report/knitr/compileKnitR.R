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
# file.inout <-'ICCAT_Report_BFTE'
# file.inout <-'Report_Sardara'

Run_Report(path.inout = mywd,file.inout =file.inout,show.pdf=F)
