## Make the map
# install node & topojson https://easyengine.io/tutorials/nodejs/node-js-npm-install-ubuntu/ et sudo npm install -g topojson (https://milkator.wordpress.com/2013/02/23/installing-topojson-on-ubuntu-12-04/)
library(rMaps)
library(rCharts)
require(RPostgreSQL)
library(dplyr)
library(foreign)
library(stringr)
library(plyr)
library(Quandl)
library(reshape2)
library(knitr)

drv <- dbDriver("PostgreSQL")
con_sardara <- dbConnect(drv, user="invsardara", password="fle087", dbname="sardara_world_2015", host="mdst-macroes.ird.fr")
query <- "select distinct geom_id,year,species,country,value,count from tuna_atlas.i6i7i8 where country='FRA' AND species='YFT' AND (year=2000 or year=2001 or year=2002 or year=2003 or year=2004) AND ST_Within(ST_Transform(geom, 4326), ST_GeomFromText('POLYGON((36.73828125 37.996162679728116,118.65234375 37.996162679728116,118.65234375 -55.578344672182055,36.73828125 -55.578344672182055,36.73828125 37.996162679728116))',4326)) ORDER BY geom_id, country, year"
data_frame_spatial_extent <- dbGetQuery(con_sardara,query)
dbDisconnect(con_sardara)
head(data_frame_spatial_extent)
sapply(data_frame_spatial_extent,typeof)

value <- as.numeric(data_frame_spatial_extent$value)
crime <- as.numeric(data_frame_spatial_extent$crime)

# geom_id <- toString(data_frame_spatial_extent$geom_id)

datm2 <- transform(data_frame_spatial_extent,
                   geom_id=as.integer(data_frame_spatial_extent$geom_id),
                   value=value,
                   count=data_frame_spatial_extent$count,
                   year=as.numeric(substr(data_frame_spatial_extent$year, 1, 4)),
                   fillKey = cut(value, quantile(value, seq(0, 1, 1/5)), labels = LETTERS[1:5])
)
toto<-toString(datm2$fillKey)
toto

kable(head(datm2), format = 'html', table.attr = "class=nofluid")
head(datm2)
sapply(datm2,typeof)

datm22 <- datm2[-174,] # On supprime la 20eme ligne


fills = setNames(
  c(RColorBrewer::brewer.pal(5), 'white'),
  c(LETTERS[1:5], 'defaultFill')
)

dat2 <- dlply(na.omit(datm22), "year", function(x){
  y = toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'geom_id')
  return(y)
})
head(dat2)
sapply(dat2,typeof)
dat2[2]




map <- ichoropleth(value ~ geom_id, data = datm2, animate = "year",  map = 'states')
map$set(
  geographyConfig = list(
    dataUrl = "https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/data/mx_states.json"
#     dataUrl = "https://dl.dropboxusercontent.com/u/10794332/mx_states.json"
  ),
  dom = 'chart_1',
  scope = 'states',
# setProjection='mercator',
setProjection = '#!  function(element) {
  var projection = d3.geo.equirectangular()
  .center([90, 0])
  .rotate([4.4, 0])
  .scale(200)
  .translate([element.offsetWidth / 2, element.offsetHeight / 2]);
  var path = d3.geo.path()
  .projection(projection);
  
  return {path: path, projection: projection};
 } !#',
#   setProjection = '#! function( element, options ) {
#    var projection, path;
#    projection = d3.geo.mercator()
#     .center([-89, 21]).scale(element.offsetWidth)
#     .translate([element.offsetWidth / 2, element.offsetHeight / 2]);
# 
#    path = d3.geo.path().projection( projection );
#    return {path: path, projection: projection};
#   } !#'
#   fills = fills,
#   data = dat2[1],
  legend = TRUE,
  labels = TRUE
)
map$save('/tmp/rMaps.html', cdn = TRUE)


#   setProjection = '#! function( element, options ) {
#    var projection, path;
#    projection = d3.geo.mercator()
#     .center([0, 0]).scale(element.offsetWidth)
#     .translate([element.offsetWidth / 2, element.offsetHeight / 2]);
# 
#    path = d3.geo.path().projection( projection );
#    return {path: path, projection: projection};
#   } !#',

setProjection = '#!function( element, options ) {
  var projection, path;
  
  projection = d3.geo.albersUsa()
  .scale(element.offsetWidth)
  .center([0, 0]).scale(element.offsetWidth)

  .translate([element.offsetWidth / 2, element.offsetHeight / 2]);
  
  path = d3.geo.path().projection( projection );
  
  return {path: path, projection: projection};
}!#',




map <- ichoropleth(count ~ geom_id, data = data_frame_spatial_extent, ncuts = 5, pal = 'YlOrRd', animate ="year",  map = 'world')

d1 <- ichoropleth(count ~ geom_id, data = data_frame_spatial_extent, animate = 'year')
d1$save('/tmp/rMaps.html', cdn = TRUE)

d1 <- crosslet(
  x = "geom_id", 
  y = c("value", "count"), 
  data = data_frame_spatial_extent, 
  map = map_world(geo = list(url = 'https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/data/mx_states.json'))
)
d1
map$save('/tmp/mapsp1.html', cdn = TRUE)


d1 <- Datamaps$new()
d1$set(
  geographyConfig = list(
    dataUrl = "https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/data/mx_states.json"  
  ),
  scope = "features",
  setProjection = '#! function( element, options ) {

  var projection, path;

  projection = d3.geo.mercator()
  .center([-157.86, 21.30])
  .scale(3000)
  .translate([element.offsetWidth / 2, element.offsetHeight / 2]);

  path = d3.geo.path()
  .projection( projection );

  return {path: path, projection: projection};
  } !#',
  fills = fills,
  data = dat2[[1]],
  legend = TRUE,
  labels = TRUE
)
map$save('/tmp/mapsp1.html', cdn = TRUE)




library(rMaps)
d1 <- Datamaps$new()
d1$set(
  geographyConfig = list(
    dataUrl = "https://rawgithub.com/tjhladish/g3_dengue_map/5570dc18d675d63b02929fa4855ba73922fdfcd9/mx_tj.json"  
  ),
  scope = 'estados2',
  setProjection = '#! function( element, options ) {

    var projection, path;

      projection = d3.geo.mercator()
      .center([-89, 21])
      .scale(element.offsetWidth)
      .translate([element.offsetWidth / 2, element.offsetHeight / 2]);

    path = d3.geo.path()
      .projection( projection );

    return {path: path, projection: projection};
  } !#'
)
d1