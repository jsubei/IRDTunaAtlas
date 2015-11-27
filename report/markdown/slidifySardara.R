# http://slidify.github.io/dcmeetup
# http://slidify.github.io/playground/
require(knitr)
require(slidify)
require(IndicatorsForFisheries)
mywd <- '/home/julien/SVNs/GIT/IRDTunaAtlas/'
report_subdirectory <- paste(mywd,'report/markdown/',sep="")
setwd(report_subdirectory)
# slidify("examples_codes.Rmd")
slidify("markdown_report.Rmd")
# http://mdst-macroes.ird.fr/tmp/SpeciesMapRelativeCatches/cdn/I7_Thunnus_albacares_2000_to_2000_.png
# http://mdst-macroes.ird.fr/tmp/SpeciesMapRelativeCatchesOtherSpecies/I8_Thunnus_albacares_2000-2000_.png
# rm(list=ls())