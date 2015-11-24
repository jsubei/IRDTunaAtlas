                      #WKT to sp data frame
#
#v0.2 - N. Billet - add CRS option in args, default to "+proj=longlat"
#v0.1 - N. Billet - initial version
wkt2spdf <- function(df, wkt_col_name, id_col_name, crs="+proj=longlat")
{
  if(require("rgeos")==FALSE)
  {
    stop("You have to install the \"rgeos\" library.")
  }

  if(length(which(names(df)==wkt_col_name))==0)
  {
    stop(paste("Cannot found the specified WKT column name (\"", wkt_col_name, "\") in the specified data frame.", sep=""))
  }

  if(length(which(names(df)==id_col_name))==0)
  {
    stop(paste("Cannot found the specified ID column name (\"", id_col_name, "\") in the specified data frame.", sep=""))
  }

  if(length(which(duplicated(df[,which(names(df)==id_col_name)])==TRUE)) > 0)
  {
    stop(paste("The ID is not unique in the specified data frame.", sep=""))
  }

  wkt2sp <- function(wkt, id)
  {
    readWKT(wkt, p4s=CRS(crs), id=as.character(id))
  }
  sp_object_list <- mapply(wkt2sp, df[,which(names(df)==wkt_col_name)], df[,which(names(df)==id_col_name)])

  if(length(sp_object_list) == 0)
  {
    stop("Empty list.")
  }

  names(sp_object_list) <- df[,which(names(df)==id_col_name)]
  sp_object_collection <- do.call(rbind, sp_object_list)
  row.names(df) <- df[,which(names(df)==id_col_name)]

  if(class(sp_object_list[[1]])=="SpatialPolygons")
  {
    return(SpatialPolygonsDataFrame(sp_object_collection, data=df))
  } else {
    if(class(sp_object_list[[1]])=="SpatialPoints")
    {
      return(SpatialPointsDataFrame(sp_object_collection, data=df))
    } else {
      stop(paste("Type ", class(sp_object_list[[1]]), " not yet implmented."), sep="")
    }
  }
}
