ICCAT_fig_1 <- function(first.year.of.decade=2000){
  plot.new()

  if(require("repmis")==FALSE)
  {
    stop("You have to install the \"repmis\" library.")
  }
  if(require("rworldmap")==FALSE)
  {
    stop("You have to install the \"rworldmap\" library.")
  }
  if(require("pryr")==FALSE)
  {
    stop("You have to install the \"pryr\" library.")
  }
  if(require("RPostgreSQL")==FALSE)
  {
    stop("You have to install the \"RPostgreSQL\" library.")
  }
  
  ##Connection to db
  sql=paste("SELECT sum(capture_rf1.v_capt_rf1) AS value_v_capt_rf1, espece.lc_esp AS species ,g_engin.lc_g_engin AS gear_type,temps.an AS year, ocean.luk_ocean AS tuna_commission,ST_asText(carre.g_carre5) AS wkt FROM capture_rf1 JOIN temps USING(id_date)  JOIN ocean USING(c_ocean)  JOIN espece USING(c_esp)  JOIN g_engin USING(c_g_engin) JOIN carre USING(id_carre) WHERE temps.an >",first.year.of.decade-1,"AND temps.an <",first.year.of.decade+10 ,"AND espece.lc_esp = 'BFT' AND ocean.luk_ocean = 'Atlantic O.' AND (carre.c_t_carre IN (5, 6) AND capture_rf1.c_periode = 1) GROUP BY ocean.luk_ocean,temps.an,espece.lc_esp, g_engin.lc_g_engin, carre.g_carre5 ORDER BY espece.lc_esp, temps.an,g_engin.lc_g_engin, carre.g_carre5;",sep="")  
  drv <- dbDriver("PostgreSQL")
  id <- repmis::source_DropboxData("conn.csv",
                                               "ay4uoisahecht56",
                                               sep = ",",
                                               header = TRUE)
  con <- dbConnect(drv, dbname=id$dbname, user=id$user, password=id$password, host=id$host)
  df <- dbGetQuery(con, sql)
  for(con in dbListConnections(drv)){
         dbDisconnect(con)
     }
  ####
  
  df$fake_id <- 1:nrow(df)
  spDF <- wkt2spdf(df, "wkt", "fake_id")

xlim <- c(-100,40)
ylim <- c(-60,70)

##
##
lon <- coordinates(spDF)[,1]
lat <-  coordinates(spDF)[,2]
value <- df$value_v_capt_rf1
by=df$gear_type

##creat and plot world grid
# grid(nx=(xlim[2]-xlim[1])/5,ny=(ylim[2]-ylim[1])/5,col="black",lwd=1,equilogs=F)
bb <- matrix(data=c(-180,180,-90,90),nrow=2,byrow=T)
cs <- c(5, 5)
cc <- bb[, 1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(spDF)))
a %<a-% plot(sp_grd,xlim=xlim,ylim=ylim,axes=T,lwd=0.4,cex.axis=0.8,col='azure2')
# ,main=paste("BFT","(",first.year.of.decade,"-",max(unique(df$year)),")",sep=""),cex.main=0.5)
a
a1 %<a-% lines(x=c(-25,-25,-30,-30,-35,-35,-45,-45),y=c(-80,0,0,5,5,10,10,100),lwd=4)
a1
a2 %<a-% mtext(paste("BFT","(",first.year.of.decade,"-",max(unique(df$year)),")",sep=""))
a2
###plot map
map <- getMap()
b %<a-% plot(map,axes=F,bg="blue4",col="darkgoldenrod2",border="darkgoldenrod2",add=T)
b
b1 %<a-% box()
b1
###plot pie
###to have the same legend
# lon<-c(rep(0,6),lon)
# lat<-c(rep(0,6),lat)
# value<-c(rep(0,6),value)
# by<-c(c("LL","PS","TRAP","BB","OTHER_A","TROL"),by)



##
xyz <- make.xyz(lon,lat,value,by)
col <- rainbow(length(unique(by)))
# col <- rainbow(6)

c %<a-% draw.pie(xyz$x, xyz$y, xyz$z, radius = 5, col=col)
c
d %<a-% legend.pie(-90,-20,labels=unique(by), radius=6, bty="n", col=col, cex=0.7, label.dist=1.3)
d
# d %<a-% legend.pie(-90,-20,labels=c("LL","PS","TRAP","BB","OTHER_A","TROL"), radius=5, bty="n", col=col, cex=0.4, label.dist=1.3)

legend.z <- round(max(rowSums(xyz$z,na.rm=TRUE))/10^3,0)
e %<a-% legend.bubble(-90,0,z=legend.z,round=1,maxradius=5,bty="n",txt.cex=0.5)
e

f %<a-% text(-90,10,"catches(10³tons)",cex=0.5)
f


}
