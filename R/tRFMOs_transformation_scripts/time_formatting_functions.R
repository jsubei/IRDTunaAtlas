#functions for time formatting

## Functions for time harmonization

#Description of Sardara's Time format. 
# One column "Year" giving the year 
# One column "Period" giving the period. The period is defined as hereunder:

# 1 = one month
# 3 = one trimester
# 6 = one semester
# 12 = one year

# One coumn "MonthStart" giving the first month of the period



#--------------------------------------------------#
# Format 1. To transform data provided with a Year column and a Time period column that has the same coding system as Sardara's.
#Input :
#Table = data.frame
#Year_ColName = Name of the column of years. 
#TimePeriod_ColName = Name of the column of time periods. Periods are defined as hereunder:
# 1 to 12: 1 month time period. The number gives the month number in the year (e.g. 1=January, 2=February, etc.)
# 13 to 16: 1 quarter time period. 13=January to March, 14=April to June, 15=July to September, 16=October to December
# 17: One year time period
# 18 and 19: 1 semester time period. 18=January to June, 19=July to December

#Concerned TRFMO(s): ICCAT
#--------------------------------------------------#

Harmo_Time_1<-function(Table, Year_ColName, TimePeriod_ColName){
  
  Table[,"MonthStart"] <- NA
  
  index1to12 <- which( Table[,TimePeriod_ColName]>=1 & Table[,TimePeriod_ColName]<=12 )
  if (length(index1to12)>1){
    Table[index1to12,"MonthStart"]<- Table[index1to12,TimePeriod_ColName]
    Table[index1to12,TimePeriod_ColName]<- 1
  }
  
  index17 <- which( Table[,TimePeriod_ColName] == 17)
  if (length(index17)>1){
    Table[index17,"MonthStart"]<- 1
    Table[index17,TimePeriod_ColName]<- 12
  }
  index13 <- which( Table[,TimePeriod_ColName] == 13)
  if (length(index13)>1){
    Table[index13,"MonthStart"]<- 1
    Table[index13,TimePeriod_ColName]<- 3
  }
  index14 <- which( Table[,TimePeriod_ColName] == 14)
  if (length(index14)>1){
    Table[index14,"MonthStart"]<- 4
    Table[index14,TimePeriod_ColName]<- 3
  }
  index15 <- which( Table[,TimePeriod_ColName] == 15)
  if (length(index15)>1){
    Table[index15,"MonthStart"]<- 7
    Table[index15,TimePeriod_ColName]<- 3
  }
  index16 <- which( Table[,TimePeriod_ColName] == 16)
  if (length(index16)>1){
    Table[index16,"MonthStart"]<- 10
    Table[index16,TimePeriod_ColName]<- 3
  }
  index18 <- which( Table[,TimePeriod_ColName] == 18)
  if (length(index18)>1){
    Table[index18,"MonthStart"]<- 1
    Table[index18,TimePeriod_ColName]<- 6
  }
  index19 <- which( Table[,TimePeriod_ColName] == 19)
  if (length(index19)>1){
    Table[index19,"MonthStart"]<- 7
    Table[index19,TimePeriod_ColName]<- 6
  }
  
  colnames(Table)[which(colnames(Table) == Year_ColName)] <- "Year"
  colnames(Table)[which(colnames(Table) == TimePeriod_ColName)] <- "Period" 
  
  
  
  return(Table)
}

#--------------------------------------------------#
# Format 2. To transform data provided with a Year column and a Month column.
#Input :
#Table = data.frame
#Year_ColName = Name of the column of years. 
#Month_ColName = Name of the column of months. The month is numerical (e.g. 1=January, 2=February, etc.)

#Concerned TRFMO(s): WCPFC, CCSBT
#--------------------------------------------------#

Harmo_Time_2<-function(Table, Year_ColName, Month_ColName){
  
  colnames(Table)[which(colnames(Table) == Year_ColName)] <- "Year"
  colnames(Table)[which(colnames(Table) == Month_ColName)] <- "MonthStart"
  
  Table$Period<-1
  #for the month that are not defined, we set the data to a year time period
  index_na<-which(is.na(Table$MonthStart))
  if (length(index_na)>0){
    Table$Period[index_na]=12
    Table$MonthStart[index_na]=1
  }
  
  
  return(Table)
}


#--------------------------------------------------#
# Format 3. To transformed data provided with a Year column and two Month columns that define a time period.
#Input :
#Table = data.frame
#Year_ColName = Name of the column of years. 
#MonthStart_ColName = Name of the column giving the first Month of the time interval
#MonthStop_ColName_ColName = Name of the column giving the last Month of the time interval

#Concerned TRFMO(s): IOTC
#--------------------------------------------------#

Harmo_Time_3<-function(Table, Year_ColName, MonthStart_ColName, MonthStop_ColName){
  
  Table$Period<-NA
  
  index1 <- which( Table[,MonthStart_ColName] == Table[,MonthStop_ColName])
  index17 <- which( Table[,MonthStop_ColName] - Table[,MonthStart_ColName] == 11)
  index13 <- which( (Table[,MonthStop_ColName] - Table[,MonthStart_ColName] == 2) & ( Table[,MonthStart_ColName] == 1))
  index14 <- which( (Table[,MonthStop_ColName] - Table[,MonthStart_ColName] == 2) & ( Table[,MonthStart_ColName] == 4))
  index15 <- which( (Table[,MonthStop_ColName] - Table[,MonthStart_ColName] == 2) & ( Table[,MonthStart_ColName] == 7))
  index16 <- which( (Table[,MonthStop_ColName] - Table[,MonthStart_ColName] == 2) & ( Table[,MonthStart_ColName] == 10))
  index18 <- which( (Table[,MonthStop_ColName] - Table[,MonthStart_ColName] == 5) & ( Table[,MonthStart_ColName] == 1))
  index19 <- which( (Table[,MonthStop_ColName] - Table[,MonthStart_ColName] == 5) & ( Table[,MonthStart_ColName] == 7))
  
  
  if (length(index1)>1){
    Table[index1,"Period"]<- Table[index1,MonthStart_ColName]
  }
  
  if (length(index13)>1){
    Table[index13,"Period"]<- 13
  }
  
  if (length(index14)>1){  
    Table[index14,"Period"]<- 14
  }
  
  if (length(index15)>1){
    Table[index15,"Period"]<- 15
  }
  
  if (length(index16)>1){
    Table[index16,"Period"]<- 16
  }
  if (length(index17)>1){
    Table[index17,"Period"]<- 17
  }
  if (length(index18)>1){
    Table[index18,"Period"]<- 18
  }
  
  if (length(index19)>1){  
    Table[index19,"Period"]<- 19
  }
  
  
  Table<-Harmo_Time_1(Table,Year_ColName,"Period")
  
  return(Table)
  
}


#Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
FormatTime_DBFormat<-function(list_of_dfs_to_upload){
  
  list_of_dfs_to_upload$time_start<-as.Date(paste(list_of_dfs_to_upload[,"Year"],"-",list_of_dfs_to_upload[,"MonthStart"],"-01",sep=""))
  # "months" is a function from the lubridate library
  list_of_dfs_to_upload$time_end<-list_of_dfs_to_upload$time_start+months(list_of_dfs_to_upload[,"Period"])

  list_of_dfs_to_upload$time_start<-as.character(list_of_dfs_to_upload$time_start)  
  list_of_dfs_to_upload$time_end<-as.character(list_of_dfs_to_upload$time_end)
  
  #list_of_dfs_to_upload$time_start<-as.POSIXlt(cut(list_of_dfs_to_upload$time_start,breaks="day"),format="%Y-%m-%d",tz="UTC")
  #list_of_dfs_to_upload$time_end<-as.POSIXlt(cut(list_of_dfs_to_upload$time_end,breaks="day"),format="%Y-%m-%d",tz="UTC")
  
  #We transform to numeric type, because the package data.table that is used in the script "integration_sardara" does not accept the PosixCT type. We will, after, transform it back to PosixCT
  
  #list_of_dfs_to_upload$time_start<-as.numeric(list_of_dfs_to_upload$time_start)
  #list_of_dfs_to_upload$time_end<-as.numeric(list_of_dfs_to_upload$time_end)
  
  return(list_of_dfs_to_upload)
}


