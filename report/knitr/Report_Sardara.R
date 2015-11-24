###############################################################
#####         BUILD THE UPDATE REPORT                     #####
###############################################################

#######################
### LOAD libraries  ###
#######################
library(ggplot2)
library(RColorBrewer)
library(knitr)
require(IRDTunaAtlas)

#####################################
### SOURCE PLOT FUNCTION for INDICATORS  ###
#####################################
#remove it if update IRDTunaAtlas package
source("/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/R_scripts/multiplot.R")
source("/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/R_scripts/Atlas_i1_SpeciesByOcean.R")
source("/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/R_scripts/Atlas_i2_SpeciesByGear.R")
source("/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/R_scripts/Atlas_i3_SpeciesYearByGearMonth.R")
source("/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/R_scripts/Atlas_i4_SpeciesMonthByOcean.R")
source("/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/R_scripts/Atlas_i9_RelativeSizeFrequenciesBySchoolType.R")
source("/home/taha/R/monR-2013/esp_travail/IRDTunaAtlas/R_scripts/Atlas_i10_RelativeSizeFrequenciesByDecade.R")


### SET INPUT PARAMETERS  ###
############################
i1_specie_name=c("YFT","BET","BFT","SKJ")
i1_scientific_specie_name=c("Thunnus albacares","Thunnus obesus","Thunnus Thynnus","Katsuwonus peianis")
i2_specie_name=c("YFT","BET","BFT","SKJ")
i2_scientific_specie_name=c("Thunnus albacares","Thunnus obesus","Thunnus Thynnus","Katsuwonus peianis")
i3_specie_name=c("YFT","BET","BFT","SKJ")
i3_scientific_specie_name=c("Thunnus albacares","Thunnus obesus","Thunnus Thynnus","Katsuwonus peianis")
i3_year = c("2000")
i4_specie_name=c("YFT","BET","BFT","SKJ")
i4_scientific_specie_name=c("Thunnus albacares","Thunnus obesus","Thunnus Thynnus","Katsuwonus peianis")
i4_year = "2000"
i9_specie_name=c("YFT","BET","BFT","SKJ")
i9_scientific_specie_name=c("Thunnus albacares","Thunnus obesus","Thunnus Thynnus","Katsuwonus peianis")
i9_year = "2000"
i10_specie_name=c("YFT","BET","BFT","SKJ")
i10_scientific_specie_name=c("Thunnus albacares","Thunnus obesus","Thunnus Thynnus","Katsuwonus peianis")
i10_period = c("1980","2005")
#####

