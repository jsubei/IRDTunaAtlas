
## Functions for CWP Grid harmonization

# Harmo_CWPQuadrant_1: for ICCAT . CAUTION: this function will not work for an ocean other than the Atlantic.
# The columns Lat and Lon in input give the coordinates of the point closest to (0,0). The column ColQuadrant gives the number of the CWP globe quadrant (1 to 4). ColSquareSize gives the CWP code of the square size.
# TO DO: EXTEND THIS FUNCTION SO THAT IT WORKS FOR ANY LAT, LON, COLQUADRANT, COLSQUARESIZE)

Harmo_CWPgrid_1<-function(Table, ColLongi, ColLati, ColQuadran, ColSquareSize,SampAreaCode_ColName){
  
  
  Table$SquareSizeCode<-NULL
  
  indice.5<- which(Table[,ColSquareSize]=="1x1")
  indice.6<- which(Table[,ColSquareSize]=="5x5")
  indice.7<- which(Table[,ColSquareSize]=="10x10")
  indice.8<- which(Table[,ColSquareSize]=="20x20")
  indice.9<- which(Table[,ColSquareSize]=="30x30")
  #We add indices that do not exist in the CWP handbook. It is because in the ICCAT CE files there are square sizes that do not exist in the CWP handbook
  indice.2<- which(Table[,ColSquareSize]=="10x20")
  indice.1<- which(Table[,ColSquareSize]=="5x10")
  indice.none<- which(Table[,ColSquareSize]=="none")
  indice.ICCAT <-which(Table[,ColSquareSize]=="ICCAT")
  indice.LatLon <-which(Table[,ColSquareSize]=="LatLon")
  
  
  Table[indice.5,ColSquareSize]<-5
  Table[indice.6,ColSquareSize]<-6
  Table[indice.7,ColSquareSize]<-7
  Table[indice.8,ColSquareSize]<-8
  Table[indice.9,ColSquareSize]<-9
  Table[indice.1,ColSquareSize]<-1
  Table[indice.2,ColSquareSize]<-2
  Table[indice.none,ColSquareSize]<-99
  Table[indice.ICCAT,ColSquareSize]<-98
  Table[indice.LatLon,ColSquareSize]<-98
  
  indice.col <- which((colnames(Table) == ColLongi) |(colnames(Table) == ColLati)|(colnames(Table) == ColQuadran))
  
  indice.longi10 <- which(Table[,ColLongi]<10)
  indice.longi100 <- which((as.numeric(Table[,ColLongi])<100) & (as.numeric(Table[,ColLongi])>=10))
  
  
  if(length(indice.longi10)){
    Table[indice.longi10,ColLongi]<-paste("00", Table[indice.longi10,ColLongi], sep="")
  }
  if(length(indice.longi100)){
    Table[indice.longi100,ColLongi]<-paste("0", Table[indice.longi100,ColLongi], sep="")
  }
  
  indice.lati10 <- which(as.numeric(Table[,ColLati])<10)
  if(length(indice.lati10)){
    Table[indice.lati10,ColLati]<-paste("0", Table[indice.lati10,ColLati], sep="")
  }
  
  Table$AreaCWPgrid <- as.numeric(paste(Table[,ColSquareSize],Table[,ColQuadran],Table[,ColLati],Table[,ColLongi], sep=""))
  
  Table$AreaName<-Table[,"AreaCWPgrid"]
  
  Table$AreaType<-Table[,ColSquareSize]
  
  #Table<-cbind(Table,as.numeric(paste(Table[,ColSquareSize],Table[,ColQuadran],Table[,ColLati],Table[,ColLongi], sep="")))
  
  index.indet<-which(Table[,ColSquareSize]=='99')
  if(length(index.indet)){
    Table[index.indet,"AreaName"]<-"UNK"
    Table[index.indet,"AreaCWPgrid"]<-NA
  }
  
  index.Get_SampAreaCode_instead_of_SquareCodeList<-which(Table[,ColSquareSize]=='98')
  if(length(index.Get_SampAreaCode_instead_of_SquareCodeList)){
    Table[index.Get_SampAreaCode_instead_of_SquareCodeList,"AreaName"]<-Table[index.Get_SampAreaCode_instead_of_SquareCodeList,SampAreaCode_ColName]
    Table[index.Get_SampAreaCode_instead_of_SquareCodeList,"AreaCWPgrid"]<-NA
  }
  
  return(Table)
  
}


# Harmo_CWPQuadrant_2: for IOTC . Compute AreaType 

Harmo_CWPgrid_2<-function(Table, ColIrrArea){
  
  Table$AreaType<-as.numeric(substr(Table[,ColIrrArea],0,1))
  
  indice.na <- which(is.na(Table[,"AreaType"]))
  Table[indice.na,"AreaType"]<-99
  
  
  # In IOTC datasets, the code 3 is equivalent to the CWP code 7; and the code 4 is equivalent to the CWP code 8. Therefore we transform the codes to be compliant with CWP standard.
  
  indice.3_to_7 <- which(Table[,"AreaType"]==3)
  Table[indice.3_to_7,"AreaType"]<-7
  Table[indice.3_to_7,"AreaCWPgrid"]<-as.numeric(paste("7",substr(Table[indice.3_to_7,"AreaCWPgrid"],2,7),sep=""))
  Table[indice.3_to_7,"AreaName"]<-paste("7",substr(Table[indice.3_to_7,"AreaCWPgrid"],2,7),sep="")
  
  indice.4_to_8 <- which(Table[,"AreaType"]==4)
  Table[indice.4_to_8,"AreaType"]<-8
  Table[indice.4_to_8,"AreaCWPgrid"]<-as.numeric(paste("8",substr(Table[indice.4_to_8,"AreaCWPgrid"],2,7),sep=""))
  Table[indice.4_to_8,"AreaName"]<-paste("8",substr(Table[indice.4_to_8,"AreaCWPgrid"],2,7),sep="")
  
  return(Table)
}


# Harmo_CWPQuadrant_3: for WCPFC. Lat and Lon represent the latitude/longitude of the south-west corner of the square. CodeSquareSize = CWP code square size. SquareSize = length (in degrees) of a square (in lat or long)

Harmo_CWPgrid_3<-function(Table, ColLat,ColLon,SquareSize,CodeSquareSize){
  
  # Calculate CWP quadrant in column "quadrant"
  Table$Lon_W_or_E<-substr(Table[,ColLon], nchar(Table[,ColLon]), nchar(Table[,ColLon]))
  Table$Lat_N_or_S<-substr(Table[,ColLat], nchar(Table[,ColLat]), nchar(Table[,ColLat]))
  
  Table[,ColLon]<-sub('W', '', Table[,ColLon])
  Table[,ColLon]<-sub('E', '', Table[,ColLon])
  Table[,ColLat]<-sub('N', '', Table[,ColLat])
  Table[,ColLat]<-sub('S', '', Table[,ColLat])
  
  indice.quad.1 <- which(Table$Lon_W_or_E=="E" & Table$Lat_N_or_S=="N")
  indice.quad.2 <- which(Table$Lon_W_or_E=="E" & Table$Lat_N_or_S=="S")
  indice.quad.3 <- which(Table$Lon_W_or_E=="W" & Table$Lat_N_or_S=="S")
  indice.quad.4 <- which(Table$Lon_W_or_E=="W" & Table$Lat_N_or_S=="N")
  
  Table$quadrant<-9
  
  Table[indice.quad.1,"quadrant"]<-1
  Table[indice.quad.2,"quadrant"]<-2
  Table[indice.quad.3,"quadrant"]<-3
  Table[indice.quad.4,"quadrant"]<-4
  
  Table[,ColLat]<-as.numeric(Table[,ColLat])
  Table[,ColLon]<-as.numeric(Table[,ColLon])
  
  if(length(indice.quad.4)){
    Table[indice.quad.4,ColLon]<-Table[indice.quad.4,ColLon]-SquareSize
  }
  if(length(indice.quad.2)){
    Table[indice.quad.2,ColLat]<-Table[indice.quad.2,ColLat]-SquareSize
  }
  if(length(indice.quad.3)){
    Table[indice.quad.3,ColLat]<-Table[indice.quad.3,ColLat]-SquareSize
    Table[indice.quad.3,ColLon]<-Table[indice.quad.3,ColLon]-SquareSize
  }
  
  Table$SquareSize<-CodeSquareSize
  
  Table<-Harmo_CWPgrid_1(Table,ColLon,ColLat,"quadrant","SquareSize")
  
  return(Table)
}



# Harmo_CWPQuadrant_4: for IATTC. Lat and Lon represent the latitude/longitude of the center of the square

Harmo_CWPgrid_4<-function(Table, ColLat,ColLon,ColSquareSize,ColCodeSquareSize){
  
  
  indice.quad.4 <- which(Table[,ColLon]>=-180 & Table[,ColLon]<=0 & Table[,ColLat]>=0 & Table[,ColLat]<=180)
  indice.quad.3 <- which(Table[,ColLon]>=-180 & Table[,ColLon]<=0 & Table[,ColLat]>=-180 & Table[,ColLat]<=0)
  indice.quad.2 <- which(Table[,ColLon]>=0 & Table[,ColLon]<=180 & Table[,ColLat]>=-180 & Table[,ColLat]<=0)
  indice.quad.1 <- which(Table[,ColLon]>=0 & Table[,ColLon]<=180 & Table[,ColLat]>=0 & Table[,ColLat]<=180)
  
  Table$quadrant<-9
  
  Table[indice.quad.1,"quadrant"]<-1
  Table[indice.quad.2,"quadrant"]<-2
  Table[indice.quad.3,"quadrant"]<-3
  Table[indice.quad.4,"quadrant"]<-4
  
  
  if(length(indice.quad.4)){
    Table[indice.quad.4,ColLon]<-Table[indice.quad.4,ColLon]+Table[indice.quad.4,ColSquareSize]/2
    Table[indice.quad.4,ColLat]<-Table[indice.quad.4,ColLat]-Table[indice.quad.4,ColSquareSize]/2
  }
  if(length(indice.quad.2)){
    Table[indice.quad.2,ColLon]<-Table[indice.quad.2,ColLon]-Table[indice.quad.2,ColSquareSize]/2
    Table[indice.quad.2,ColLat]<-Table[indice.quad.2,ColLat]+Table[indice.quad.2,ColSquareSize]/2
  }
  if(length(indice.quad.3)){
    Table[indice.quad.3,ColLon]<-Table[indice.quad.3,ColLon]+Table[indice.quad.3,ColSquareSize]/2
    Table[indice.quad.3,ColLat]<-Table[indice.quad.3,ColLat]+Table[indice.quad.3,ColSquareSize]/2
  }
  if(length(indice.quad.1)){
    Table[indice.quad.1,ColLon]<-Table[indice.quad.1,ColLon]-Table[indice.quad.1,ColSquareSize]/2
    Table[indice.quad.1,ColLat]<-Table[indice.quad.1,ColLat]-Table[indice.quad.1,ColSquareSize]/2
  }
  
  Table[,ColLon]<-abs(Table[,ColLon])
  Table[,ColLat]<-abs(Table[,ColLat])
  
  
  Table<-Harmo_CWPgrid_1(Table,ColLon,ColLat,"quadrant",ColCodeSquareSize)
  
  return(Table)
  
  
}







# function convert CWP to WKT (for vizualisation in Qgis)
cwp_tp_wkt<-function(tab,cwp_grid_column){
  #tab<-read.csv("/home/taconet/Bureau/IOTC-2015-WPB13-DATA-CELongline.csv",sep=",")
  tab$size_grid<-as.integer(substr(tab[,cwp_grid_column],1,1))
  tab$quadrant<-as.integer(substr(tab[,cwp_grid_column],2,2))
  
  
  conversions_aires<-data.frame(c(1,2,3,4,5,6),c(5,10,10,20,1,5),c(10,20,10,20,1,5))
  colnames(conversions_aires)<-c("code","latitude","longitude")
  
  tab$lon_min<-0
  tab$lon_max<-0
  tab$lat_min<-0
  tab$lat_max<-0
  
  
  
  # if (tab$quadrant==1 || tab$quadrant==2){
  
  
  index_quad1_2 <- which(tab$quadrant==1 | tab$quadrant==2)
  tab$lon_min[index_quad1_2]<-as.integer(substr(tab[,cwp_grid_column][index_quad1_2],5,7))
  
  index1 <- which(tab$quadrant==1 | tab$quadrant==4)
  tab$lat_min[index1]<- as.integer(substr(tab[,cwp_grid_column][index1],3,4))
  
  index2 <- which(tab$quadrant==2 | tab$quadrant==3)
  tab$lat_min[index2]<- - (as.integer(substr(tab[,cwp_grid_column][index2],3,4)))
  
  
  index3_1 <- which(tab$size_grid==1 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_1]<-tab$lon_min[index3_1]+10
  
  index3_2 <- which(tab$size_grid==2 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_2]<-tab$lon_min[index3_2]+20
  
  index3_3 <- which(tab$size_grid==3 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_3]<-tab$lon_min[index3_3]+10
  
  index3_4 <- which(tab$size_grid==4 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_4]<-tab$lon_min[index3_4]+20
  
  index3_5 <- which(tab$size_grid==5 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_5]<-tab$lon_min[index3_5]+1
  
  index3_6 <- which(tab$size_grid==6 & (tab$quadrant==1 | tab$quadrant==2))
  tab$lon_max[index3_6]<-tab$lon_min[index3_6]+5
  
  
  
  index4_1 <- which(tab$quadrant==1 & tab$size_grid==1)
  tab$lat_max[index4_1]<-tab$lat_min[index4_1]+5
  
  index4_2 <- which(tab$quadrant==1 & tab$size_grid==2)
  tab$lat_max[index4_2]<-tab$lat_min[index4_2]+10
  
  index4_3 <- which(tab$quadrant==1 & tab$size_grid==3)
  tab$lat_max[index4_3]<-tab$lat_min[index4_3]+10
  
  index4_4 <- which(tab$quadrant==1 & tab$size_grid==4)
  tab$lat_max[index4_4]<-tab$lat_min[index4_4]+20
  
  index4_5 <- which(tab$quadrant==1 & tab$size_grid==5)
  tab$lat_max[index4_5]<-tab$lat_min[index4_5]+1
  
  index4_6 <- which(tab$quadrant==1 & tab$size_grid==6)
  tab$lat_max[index4_6]<-tab$lat_min[index4_6]+5
  
  
  index5_1 <- which(tab$quadrant==2 & tab$size_grid==1)
  tab$lat_max[index5_1]<-tab$lat_min[index5_1]-5
  
  index5_2 <- which(tab$quadrant==2 & tab$size_grid==2)
  tab$lat_max[index5_2]<-tab$lat_min[index5_2]-10
  
  index5_3 <- which(tab$quadrant==2 & tab$size_grid==3)
  tab$lat_max[index5_3]<-tab$lat_min[index5_3]-10
  
  index5_4 <- which(tab$quadrant==2 & tab$size_grid==4)
  tab$lat_max[index5_4]<-tab$lat_min[index5_4]-20
  
  index5_5 <- which(tab$quadrant==2 & tab$size_grid==5)
  tab$lat_max[index5_5]<-tab$lat_min[index5_5]-1
  
  index5_6 <- which(tab$quadrant==2 & tab$size_grid==6)
  tab$lat_max[index5_6]<-tab$lat_min[index5_6]-5
  
  
  #  if (tab$quadrant==3 || tab$quadrant==4){
  index_quad3_4 <- which(tab$quadrant==3 | tab$quadrant==4)   
  tab$lon_max[index_quad3_4]<- - as.integer(substr(tab[,cwp_grid_column][index_quad3_4],5,7))
  
  
  
  index3_1 <- which(tab$size_grid==1 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_1]<-tab$lon_max[index3_1]-10
  
  index3_2 <- which(tab$size_grid==2 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_2]<-tab$lon_max[index3_2]-20
  
  index3_3 <- which(tab$size_grid==3 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_3]<-tab$lon_max[index3_3]-10
  
  index3_4 <- which(tab$size_grid==4 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_4]<-tab$lon_max[index3_4]-20
  
  index3_5 <- which(tab$size_grid==5 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_5]<-tab$lon_max[index3_5]-1
  
  index3_6 <- which(tab$size_grid==6 & (tab$quadrant==3 | tab$quadrant==4))
  tab$lon_min[index3_6]<-tab$lon_max[index3_6]-5
  
  
  
  index4_1 <- which(tab$quadrant==4 & tab$size_grid==1)
  tab$lat_max[index4_1]<-tab$lat_min[index4_1]+5
  
  index4_2 <- which(tab$quadrant==4 & tab$size_grid==2)
  tab$lat_max[index4_2]<-tab$lat_min[index4_2]+10
  
  index4_3 <- which(tab$quadrant==4 & tab$size_grid==3)
  tab$lat_max[index4_3]<-tab$lat_min[index4_3]+10
  
  index4_4 <- which(tab$quadrant==4 & tab$size_grid==4)
  tab$lat_max[index4_4]<-tab$lat_min[index4_4]+20
  
  index4_5 <- which(tab$quadrant==4 & tab$size_grid==5)
  tab$lat_max[index4_5]<-tab$lat_min[index4_5]+1
  
  index4_6 <- which(tab$quadrant==4 & tab$size_grid==6)
  tab$lat_max[index4_6]<-tab$lat_min[index4_6]+5
  
  
  index5_1 <- which(tab$quadrant==3 & tab$size_grid==1)
  tab$lat_max[index5_1]<-tab$lat_min[index5_1]-5
  
  index5_2 <- which(tab$quadrant==3 & tab$size_grid==2)
  tab$lat_max[index5_2]<-tab$lat_min[index5_2]-10
  
  index5_3 <- which(tab$quadrant==3 & tab$size_grid==3)
  tab$lat_max[index5_3]<-tab$lat_min[index5_3]-10
  
  index5_4 <- which(tab$quadrant==3 & tab$size_grid==4)
  tab$lat_max[index5_4]<-tab$lat_min[index5_4]-20
  
  index5_5 <- which(tab$quadrant==3 & tab$size_grid==5)
  tab$lat_max[index5_5]<-tab$lat_min[index5_5]-1
  
  index5_6 <- which(tab$quadrant==3 & tab$size_grid==6)
  tab$lat_max[index5_6]<-tab$lat_min[index5_6]-5
  
  
  tab$polygon_wkt<-paste("POLYGON ((",tab$lon_min," ",tab$lat_min," , ", tab$lon_min," ",tab$lat_max," , ", tab$lon_max," ",tab$lat_max," , " , tab$lon_max," ",tab$lat_min," , " , tab$lon_min," ",tab$lat_min," )) " , sep="")
  
  
  return(tab)
  
}
