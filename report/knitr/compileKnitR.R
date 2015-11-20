#load knitr Package
require(knitr)
#Specify the working directory
mywd <- '/home/julien/SVNs/Processes/R/data_inputs/Databases_SQL/Sardara/knitR'
setwd(mywd)
#Specify the names of knitr file to be compiled and resulting latex file name (knitr output)
file.in <-'KnitrTest2.Rnw'
file.out <- 'KnitrTest2.tex'
#knitr Compilation
knit(file.in,file.out) 
#Latex Compilation
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
