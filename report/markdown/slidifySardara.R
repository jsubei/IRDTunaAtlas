require(slidify)
mywd <- '/home/julien/SVNs/GIT/IRDTunaAtlas/'
report_subdirectory <- paste(mywd,'report/markdown/',sep="")
setwd(report_subdirectory)
slidify("examples_codes.Rmd")
