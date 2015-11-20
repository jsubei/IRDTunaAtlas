#load knitr Package
require(knitr)
#load specifi R Packages and related functions parametrizations when needed to customize the report
#Specify the working directory
mywd <- '/home/julien/SVNs/GIT/IRDTunaAtlas'
setwd(mywd)
#Specify the names of knitr file to be compiled and resulting latex file name (knitr output)
file.in <-'./report/knitr/KnitrTest2.Rnw'
file.out <- './report/knitr/KnitrTest2.tex'
#knitr Compilation
knit(file.in,file.out) 
#Latex Compilation
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
system(paste("pdflatex ",file.out, sep=""))
