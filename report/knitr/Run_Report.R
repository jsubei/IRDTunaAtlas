#Taha IMZILEN - IRD / MARBEC

#2015/11/20: Taha - Initial version
##################################################################
#Run_Report : Function to run report
####

################
##EXAMPLE OF USE
# Run_Report(file.inout ="ICCAT_Report_BFTE",
#           path.inout = "/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/reports/",show.pdf=F)
# 


##########
##Notes: file.inout is the knitr and R file without extension(.Rdw et .R)
##         path.inout is the address where the knitr file is located





################
### RUN FONCTION
################

Run_Report <- function(file.inout,path.inout,show.pdf=F){

library(knitr)
setwd(path.inout)

file.in <- paste(file.inout,".Rnw",sep="")
file.out <- paste(file.inout,".tex",sep="")
file.input.par <- paste(file.inout,".R",sep="")

#### Check existence of .R and .Rnw fields
if(!file.exists(paste(path.inout,file.input.par,sep="/"))){ stop("You have to create the .R field.")}
if(!file.exists(paste(path.inout,file.in,sep="/"))){ stop("You have to create the .Rnw field.")}
##
source(file.input.par)
knit(file.in,file.out)  ### knitr function to transform the .Rnw into a tex file

system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
if(show.pdf){
system(paste("evince ",file.inout,".pdf",sep=""))
}
}


