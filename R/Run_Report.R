Run_Report <- function(path.inout,file.inout,zip_file,show.pdf=F){
  
  report_subdirectory <- paste(path.inout,'report/knitr/',sep="") 
  setwd(report_subdirectory)
  system(paste("unzip ",zip_file, sep=""))
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
  system(paste("cp ",file.inout,".pdf /data/www/html/tmp/report/",file.inout,".pdf",sep=""))
  
  if(show.pdf){
    system(paste("acroread ",file.inout,".pdf",sep=""))
  }
  
  
}
