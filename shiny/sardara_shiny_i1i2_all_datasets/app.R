library(shiny)
library(geojsonio)
library(plyr)
library(dplyr)
library(DBI)
library(leaflet)
library(leaflet.extras)
library(leafpm)
library(mapedit)
library(sf)
library(sp)
library(plotly)
library(DT)
library(ncdf4)
library(raster)
library(maps)
library(ggplot2)
library(XML)
library(streamgraph)
library(viridis)
library(xts)
library(dygraphs)
library(tidyr)
####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i1_SpeciesByOcean.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i2_SpeciesByGear.R")
####################################################################################################################################################################################################################################
DRV=RPostgres::Postgres()
# source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")
####################################################################################################################################################################################################################################

new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt) 
switch_unit <- reactiveVal(TRUE) 

# target_species<- c("YFT","BFT")
# target_year <- c(seq(1:10)+1994)
target_dataset <- dbGetQuery(con, "SELECT DISTINCT(dataset) FROM public.i1i2_spatial_all_datasets ORDER BY dataset;")
target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM public.i1i2_spatial_all_datasets ORDER BY species;")
target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM public.i1i2_spatial_all_datasets ORDER BY year;")
target_gear <- dbGetQuery(con, "SELECT DISTINCT(gear_group) as gear FROM public.i1i2_spatial_all_datasets ORDER BY gear_group;")
target_ocean <- dbGetQuery(con, "SELECT DISTINCT(ocean) as ocean FROM public.i1i2_spatial_all_datasets ORDER BY ocean;")
target_unit <- dbGetQuery(con, "SELECT DISTINCT(unit) AS unit FROM public.i1i2_spatial_all_datasets ORDER BY unit;")
target_area <- dbGetQuery(con, "SELECT DISTINCT(area)::varchar AS area FROM public.i1i2_spatial_all_datasets ORDER BY area DESC;")

default_species <- 'YFT'
default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)
# default_gear <- c('BB','PS')
default_gear <- unique(target_gear)
filters_combinations <- dbGetQuery(con, "SELECT species, year, gear_group as gear FROM public.i1i2_spatial_all_datasets GROUP BY species, year,gear_group;")
# default_dataset <- 'global_catch_5deg_1m_firms_level0'
default_dataset <- unique(target_dataset$dataset)
default_unit <-  c('MT','t','no')
# default_unit <- unique(target_unit$unit)
# default_area <- '25'
default_area <- unique(target_area$area)

ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques"),
  navbarPage(title="TunaAtlas", 
             tabPanel("Interactive",
                      
                      # If not using custom CSS, set height of leafletOutput to a number instead of percent
                      # leafletOutput("mymap", width="1000", height="1000"),
                      absolutePanel(id = "themap", class = "panel panel-default", fixed = TRUE,
                                    draggable = FALSE, top = 200, right = "auto", left = 20, bottom = "auto",
                                    width = "65%", height = "30%",
                                    leafletOutput('mymap')
                      ),
                      absolutePanel(id = "theplot", class = "panel panel-default", fixed = TRUE,
                                    draggable = FALSE, top = "auto", right = "auto", left = 20, bottom = "100",
                                    width = "65%", height = "65%",
                                    dygraphOutput("dygraph_all_datasets", height=300, width='90%'),
                                    textOutput("legendDivID"),
                                    tags$br(),
                                    actionButton(
                                      inputId = "switched",
                                      label = "Switch unit for pie chart"
                                    ),
                                    plotlyOutput("pie_area_catch"),
                                    tags$br(),
                                    plotlyOutput("barplot_datasets"),
                                    tags$br()
                                    
                      ),
                      
                      # Shiny versions prior to 0.11 should use class = "modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = FALSE, top = 200, left = "auto", right = 20, bottom = "auto",
                                    width = "30%", height = "auto",
      selectInput(
        inputId = "dataset",
        label = "Dataset",
        choices = target_dataset$dataset,
        multiple = TRUE,
        selected= default_dataset
      ),
      selectInput(
        inputId = "unit",
        label = "Unit",
        choices = target_unit$unit,
        multiple = TRUE,
        selected= default_unit
      ),
      selectInput(
        inputId = "area",
        label = "Area",
        choices = target_area$area,
        multiple = TRUE,
        selected= default_area
      ),
      selectInput(
        inputId = "species",
        label = "Species",
        choices = target_species$species,
        multiple = TRUE,
        selected= default_species
      ),
      selectInput(
        inputId = "year",
        label = "Year",
        choices = target_year$year,
        multiple = TRUE,
        selected= default_year
      ),
      selectInput(
        inputId = "gear",
        label = "Gear",
        choices = target_gear$gear,
        multiple = TRUE,
        selected= target_gear$gear
      ),
      actionButton(
        inputId = "submit",
        label = "Submit"
      ),
      actionButton("resetWkt", "Reset WKT to global"),
      tags$br()
      
                      )
      
             ),
      tabPanel(
        title = "Pie ratio catch",
        # plotlyOutput("plot1")
        plotlyOutput("pie_ratio_catch")
      ),
      tabPanel(
        title = "dygraph indicator 1",
        actionButton(
          inputId = "stacked",
          label = "Stack/unstack time series"
        ),
        dygraphOutput("plot1_dygraph")
      ),
      tabPanel(
        title = "Streamgraph indicator 1",
        # plotlyOutput("plot1")
        streamgraphOutput("plot1_streamgraph")
      ),
      tabPanel(
        title = "Plot indicator 2",
        plotlyOutput("plot2")
      ),
      tabPanel(
        title = "Streamgraph indicator 2",
        streamgraphOutput("plot2_streamgraph")
      ),
      tabPanel(
        title = "Browse i1 data",
        DT::dataTableOutput("DTi1")
      ),
      tabPanel(
        title = "Browse i2 table",
        DT::dataTableOutput("DTi2")
      ),
      tabPanel(
        title = "Your SQL query",
        textOutput("sql_query")
      ),
      tabPanel(
        title = "Your filters",
        textOutput("selected_var")
      )
  )
)


server <- function(input, output, session) {
  
  
  
  sql_query <- eventReactive(input$submit, {
    if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
    # if(is.null(input$dataset)){dataset_name=target_dataset$dataset}else{year_name=input$dataset}
    query <- glue::glue_sql(
      "SELECT dataset, unit, ocean, gear_group, year, species, value, area, geom  FROM public.i1i2_spatial_all_datasets 
      WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND  dataset IN ({dataset_name*}) 
      AND  species IN ({species_name*}) 
    AND gear_group IN ({gear_group_name*})
    AND year IN ({year_name*})
    AND area::varchar IN ({area_name*})
    AND unit IN ({unit_name*}) ",
      wkt = wkt(),
      dataset_name = input$dataset,
      species_name = input$species,
      gear_group_name = input$gear,
      year_name = year_name,
      unit_name = input$unit,
      area_name = input$area,
      .con = con)
  },
  ignoreNULL = FALSE)
  
  # 
  # sql_query <- eventReactive(input$submit, {
  #   # query = paste0("SELECT sum(value) as value, unit, ocean, gear_group, year, species, st_collect(geom) FROM public.i1i2_spatial_all_datasets WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))  AND species IN ('",paste0(input$species,collapse="','"),"')  AND year IN ('",paste0(input$year,collapse="','"),"') GROUP BY  unit, ocean, gear_group, year, species ;")
  #   query = paste0("SELECT unit, ocean, gear_group, year, species, sum(value) as value, ST_ConvexHull(st_collect(geom)) as convexhull FROM public.i1i2_spatial_all_datasets WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))  AND species IN ('",paste0(input$species,collapse="','"),"')  GROUP BY  unit, ocean, gear_group, year, species ;")
  # },
  # ignoreNULL = FALSE)
  
  
  data_all_datasets <- eventReactive(input$submit, {
    # st_read(con, query =sql_query()) # %>% mutate(time_start = ISOdate(year, 1, 1), time_end = ISOdate(year, 12,31)) %>% dplyr::select (-c(ogc_fid, geom_id,geom,count,year)) %>%  dplyr::rename(v_catch=value,flag=country)
    # st_read(con, query = paste0("SELECT dataset, to_date(year::varchar(4),'YYYY') AS year, species, sum(value) as value FROM (",sql_query(),") AS foo GROUP BY  dataset, year, species"))
    st_read(con, query = paste0("SELECT dataset, to_date(year::varchar(4),'YYYY') AS year, species, sum(value) as value, unit FROM (",sql_query(),") AS foo GROUP BY  dataset, year, species, unit"))
    
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  data_pie_all_datasets <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT dataset, sum(value) as value FROM (",sql_query(),") AS foo GROUP BY  dataset"))
    
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  data_pie_area_catch <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT dataset, area, unit, count(*), SUM(value)  as value FROM (",sql_query(),") AS foo GROUP BY dataset, area, unit ORDER BY dataset"))
    
  },
  ignoreNULL = FALSE)
  
  
  
  data_barplot_all_datasets <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT  dataset, unit, count(*) AS count, SUM(value) AS value  FROM (",sql_query(),") AS foo GROUP BY dataset, unit ORDER BY dataset"))
    # st_read(con, query = paste0("SELECT  dataset, unit, count(*) AS count FROM (",sql_query(),") AS foo  WHERE unit in ('t','MT','no') GROUP BY dataset, unit ORDER BY dataset"))
    
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  data_i1 <- eventReactive(input$submit, {
    # st_read(con, query =sql_query()) # %>% mutate(time_start = ISOdate(year, 1, 1), time_end = ISOdate(year, 12,31)) %>% dplyr::select (-c(ogc_fid, geom_id,geom,count,year)) %>%  dplyr::rename(v_catch=value,flag=country)
    st_read(con, query = paste0("SELECT dataset, unit, ocean, to_date(year::varchar(4),'YYYY') AS year, species, sum(value) as value FROM (",sql_query(),") AS foo GROUP BY  dataset,unit, ocean, year, species"))

  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  data_i2 <- eventReactive(input$submit, {
    
    st_read(con, query = paste0("SELECT unit, gear_group AS gear_type, year, species, sum(value) as value FROM (",sql_query(),") AS foo GROUP BY unit, gear_group, year, species")) 
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  metadata <- reactive({
    st_read(con, query = paste0("SELECT geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY geom")) 
  })  
  
  change <- reactive({
    unlist(strsplit(paste(c(input$species,input$year,input$gear),collapse="|"),"|",fixed=TRUE))
  })
  
  observeEvent(input$resetWkt, {
    wkt(new_wkt)
  })
  
  observeEvent(input$species,{
    temp <- filters_combinations %>% filter(species %in% change()[1])
    updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
      updateSelectInput(session,"gear",choices = unique(temp$gear),selected=unique(temp$gear))
    
  }
  )
  

  
  observeEvent(input$switched, {
    if(switch_unit()){switch_unit(FALSE)}else{switch_unit(TRUE)}
  })
  
  
  # observeEvent(input$year,{
  #   temp <- filters_combinations %>% filter(species %in% change()[1], year %in% change()[2])
  #   updateSelectInput(session,"gear",choices = unique(temp$gear),selected=unique(temp$gear))
  # }
  # )

  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$flag, "and \n", wkt())
  })
  
  
  output$sql_query <- renderText({ 
    paste("Your SQL Query is : \n", sql_query())
  })
  
  
  output$DTi1 <- renderDT({
    this <- data_all_datasets()
    # this <-data_i1()   %>% filter(unit %in% c('t','MT'))  %>% spread(dataset, value, fill=0)
  })
  
  
  output$DTi2 <- renderDT({
    this <- data_barplot_all_datasets()   %>% mutate(unit=replace(unit,unit=='MT', 't'))  %>% pivot_wider(names_from = unit, values_from = c("value", "count"), names_sep="_",values_fill = 0)
    # this <- data() %>% filter(year %in% input$year) %>% filter(gear_group %in% input$gear) %>% group_by(year,gear_group,species)   %>% summarise(value = sum(value))  %>% dplyr::rename(gear_type=gear_group)
    
  })
  
  
  
  
  output$mymap <- renderLeaflet({
    
    df <- metadata()
    centroid <-  st_convex_hull(df) %>% st_centroid()
    lat_centroid <- st_coordinates(centroid)[2]
    lon_centroid <- st_coordinates(centroid)[1]
    
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = df$value
    )
    # brewer.pal(7, "OrRd")
    pal_fun <- colorQuantile(   "YlOrRd", NULL, n = 10)
    
    qpal <- colorQuantile(rev(viridis::viridis(10)),
                          df$value, n=10)
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    map_leaflet <- leaflet()  %>%  
      addProviderTiles("Esri.OceanBasemap")  %>% 
      clearBounds() %>%
      addPolygons(data = df,
                                                 label = ~value,
                                                 popup = ~paste0("Captures pour cette espece: ", round(value), " tonnes(t) et des brouettes"),
                                                 # fillColor = ~pal_fun(value),
                                                 fillColor = ~qpal(value),
                                                 fill = TRUE,
                                                 fillOpacity = 0.8,
                                                 smoothFactor = 0.5
                                                 # color = ~pal(value)
    ) %>%
      addDrawToolbar(
        targetGroup = "draw",
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      ) %>%
      # setView(lng =48, lat =-8, zoom = 5
      setView(lng = lon_centroid, lat =lat_centroid, zoom = 3
      ) %>%
      addLayersControl(
        overlayGroups = c("draw"),
        options = layersControlOptions(collapsed = FALSE)
      )
    # %>%  addLegend("bottomright", 
    #            # colors = colorQuantile("YlOrRd", NULL, n = 10), 
    #            pal = pal, values = ~value,
    #            # labFormat = labelFormat(prefix = "$"),
    #            # labels = paste0("up to ", format(df$values[-1], digits = 2)),
    #            title = "Captures de thons")
  })
  
  
  observe({
    #use the draw_stop event to detect when users finished drawing
    feature <- input$mymap_draw_new_feature
    req(input$mymap_draw_stop)
    print(feature)
    polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
    # see  https://rstudio.github.io/leaflet/shiny.html
    bb <- input$mymap_bounds
    geom_polygon <- input$mymap_draw_new_feature$geometry
    # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    geoJson <- geojsonio::as.json(feature)
    # spdf <- geojsonio::geojson_sp(feature)
    geom <- st_read(geoJson)
    wkt(st_as_text(st_geometry(geom[1,])))
    coord <- st_as_text(st_geometry(geom[1,]))

    north <- polygon_coordinates[[1]][[1]]
    south <- polygon_coordinates[[2]][[1]]
    east <- polygon_coordinates[[1]][[2]]
    west <- polygon_coordinates[[2]][[2]]


    if(is.null(polygon_coordinates))
      return()
    text<-paste("North ", north, "South ", east)

    mymap_proxy = leafletProxy("mymap") %>% clearPopups() %>% addPopups(south,west,coord)
    textOutput("wkt")
    
  })
  
  
  output$plot1 <- renderPlotly({
    
    # df_i1 <- data() %>% filter(year %in% input$year) %>% group_by(ocean,year,species) %>% summarise(value = sum(value))  
    df_i1 <- data_i1()
    
    # df_i1 <- data() 
    
    # i1 <- Atlas_i1_SpeciesByOcean(as.data.frame(df_i1), 
    #                               yearAttributeName="year", 
    #                               oceanAttributeName="ocean", 
    #                               speciesAttributeName="species", 
    #                               valueAttributeName="value")
    

    i1 <- df_i1 %>%
        ggplot(aes(x=year, y=value, fill=ocean, text=ocean)) +
        geom_area( ) +
        theme(legend.position="none") +
        ggtitle("Popularity of American names in the previous 30 years") +
        theme(legend.position="none")

      # Turn it interactive
      i1 <- ggplotly(i1, tooltip="text")

    
  })
  
  output$plot1_streamgraph <- renderStreamgraph({
    
    # df_i1 <- data() %>% filter(year %in% input$year) %>% group_by(ocean,year,species) %>% summarise(value = sum(value))
    df_i1 <-data_i1()
    
        streamgraph(df_i1, key="ocean", value="value", date="year", height="300px", width="1000px", offset="zero", interpolate="linear") %>% 
      # streamgraph("name", "n", "year", offset="zero", interpolate="linear") %>%
      sg_legend(show=TRUE, label="I=RFMO - names: ")
  })
  
  output$plot2 <- renderPlotly({ 
    
 
    # df_i2 = data() %>% filter(year %in% input$year) %>% filter(gear_group %in% input$gear) %>% group_by(year,gear_group,species)   %>% summarise(value = sum(value))  %>% dplyr::rename(gear_type=gear_group)
    df_i2 <- data_i2()
    
    
    i2 <- Atlas_i2_SpeciesByGear(as.data.frame(df_i2),
                                 yearAttributeName="year",
                                 speciesAttributeName="species",
                                 valueAttributeName="value",
                                 gearTypeAttributeName="gear_type",
                                 withSparql=FALSE)
  i2 
  })
  
  
  
  output$plot1_dygraph <- renderDygraph({
    
    df_i1 = data_i1()   %>% spread(ocean, value, fill=0)   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    df_i1 <- as_tibble(df_i1)  # %>% top_n(3)
    
    wcpfc <- xts(x = df_i1$WCPFC, order.by = df_i1$year)
    iotc <-  xts(x = df_i1$IOTC, order.by = df_i1$year)
    iccat <-  xts(x = df_i1$ICCAT, order.by = df_i1$year)
    iattc <-  xts(x = df_i1$ICCAT, order.by = df_i1$year)
    tuna_catches_timeSeries <- cbind(wcpfc, iotc, iccat,iattc)
    
    # create the area chart
    # g1 <- dygraph(tuna_catches_timeSeries)  %>% dyOptions( fillGraph=TRUE )
    g1 <- dygraph(tuna_catches_timeSeries, main = "Catches by ocean") %>%
      dyRangeSelector() %>%
      dyStackedBarGroup(c('iotc', 'iattc','iccat','wcpfc'))
    # >%  dyOptions( fillGraph=TRUE) %>%        # create bar chart with the passed dygraph
    #   dyOptions(stackedGraph = stack())
  #     dySeries(iotc, label = "iotc") %>%
  #   dySeries(iattc, label = "iattc") %>%
  # dySeries(iccat, label = "iccat") 
  #   
  })
  
  
  
  
  
  output$plot2_streamgraph<- renderStreamgraph({ 
    df_i2 =  data_i2() %>% 
      streamgraph("gear_type", "value", "year", offset="zero", interpolate="step") %>%
      sg_axis_x(1, "year", "%Y") %>%
      sg_fill_brewer("PuOr") %>%
      sg_legend(show=TRUE, label="I=RFMO - names: ")
  })
  
  
  
  output$dygraph_all_datasets <- renderDygraph({
    
    # df_i1 = data_all_datasets()  %>% filter(unit %in% c('t','MT'))  %>% spread(dataset, value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    # df_i1 = data_all_datasets()  %>% spread(dataset, value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    df_i1 = data_all_datasets()  %>% mutate(unit=replace(unit,unit=='MT', 't')) %>% spread(dataset, value, fill=0) #   %>%  mutate(total = rowSums(across(any_of(as.vector(target_ocean$ocean)))))
    df_i1 <- as_tibble(df_i1)  # %>% top_n(3)
    
    tuna_catches_timeSeries <-NULL
    
    if(length(unique(df_i1$unit))>1){
      df_i1_t <- df_i1 %>% filter(unit  == 't') 
      df_i1_no <- df_i1 %>% filter(unit == 'no')  
      # if(switch_unit()){
      #   df_i1 <- df_i1_no
      # }else{
      #   df_i1 <- df_i1_t
      # }
      
      # if(length(colnames(dplyr::select(df_i1_t,-c(species,year,unit))))>1){
        for(d in 1:length(colnames(dplyr::select(df_i1_t,-c(species,year,unit))))){
          this_dataset <-colnames(dplyr::select(df_i1_t,-c(species,year,unit)))[d]
          if(sum(dplyr::select(df_i1_t, this_dataset))>0){
            df_i1_t  <- df_i1_t  %>% rename(!!paste0(this_dataset,'_t') := !!this_dataset)
            this_time_serie <- xts(x = dplyr::select(df_i1_t, c(paste0(this_dataset,'_t'))), order.by = df_i1_t$year)
            if(d==1){tuna_catches_timeSeries <- this_time_serie}else{
              tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
            }
          }
        }
      # if(length(colnames(dplyr::select(df_i1_no,-c(species,year,unit))))>1){
        for(d in 1:length(colnames(dplyr::select(df_i1_no,-c(species,year,unit))))){
          this_dataset <- colnames(dplyr::select(df_i1_no,-c(species,year,unit)))[d]
          if(sum(dplyr::select(df_i1_no,this_dataset))>0){
          df_i1_no  <- df_i1_no  %>% rename(!!paste0(this_dataset,'_no') := !!this_dataset)
          this_time_serie <- xts(x = dplyr::select(df_i1_no, c(paste0(this_dataset,'_no'))), order.by = df_i1_no$year)
            tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
          }
        }
      
    }else{
      for(d in 1:length(colnames(dplyr::select(df_i1,-c(species,year,unit))))){
        this_dataset <- colnames(dplyr::select(df_i1,-c(species,year,unit)))[d]
        this_time_serie <- xts(x = dplyr::select(df_i1, c(this_dataset)), order.by = df_i1$year)
        if(d==1){tuna_catches_timeSeries <- this_time_serie}else{
          tuna_catches_timeSeries <- cbind(tuna_catches_timeSeries, this_time_serie)
        }
      }

      
      # create the area chart
      # g1 <- dygraph(tuna_catches_timeSeries) # %>% dyOptions( fillGraph=TRUE )
      # g1 <- dygraph(tuna_catches_timeSeries, main = "Catches by ocean") %>% dyRangeSelector() %>%       dyLegend(labelsDiv = "legendDivID")
      # %>%
      #   dyStackedBarGroup(c('global_catch_5deg_1m_firms_level0', 'global_catch_1deg_1m_ps_bb_firms_level0','spatial','nominal'))
      # >%  dyOptions( fillGraph=TRUE) %>%        # create bar chart with the passed dygraph
      #   dyOptions(stackedGraph = stack())
      #     dySeries(iotc, label = "iotc") %>%
      #   dySeries(iattc, label = "iattc") %>%
      # dySeries(iccat, label = "iccat") 
      #   
    }
      
      g1 <- dygraph(tuna_catches_timeSeries, main = "Catches by ocean") %>% dyRangeSelector() %>%       dyLegend(labelsDiv = "legendDivID")
      
    
    
  })
  
  
  
  
  
  output$pie_ratio_catch<- renderPlotly({ 
    # output$plot_species<- renderPlot({ 
    df_i2 = data_pie_all_datasets() # %>% spread(dataset, value, fill=0)  
   if('global_nominal_catch_firms_level0' %in% unique(df_i2$dataset)){
    total <- filter(df_i2, dataset=='global_nominal_catch_firms_level0')  
    total <- total$value
}else{total=1}
    df_i2 =  df_i2 %>% mutate(value = value/total)  %>% subset(dataset!='global_nominal_catch_firms_level0')
    
    fig <- plot_ly(df_i2, labels = ~dataset, values = ~value, type = 'pie',
                   # marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   showlegend = TRUE)
    fig <- fig %>% layout(title = 'Ratio of all datasets for selected units',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
    
  })
  
  
  
  output$pie_area_catch<- renderPlotly({ 

    df_i2 = data_pie_area_catch()   %>% mutate(unit=replace(unit,unit=='MT', 't')) # %>% filter(unit == 't') # %>% filter(dataset=='global_nominal_catch_firms_level0')
    # df_i2 = st_read(con, query = paste0("SELECT dataset, area, unit, count(*), SUM(value)  as value FROM (SELECT dataset, unit, ocean, gear_group, year, species, value, area, geom FROM public.i1i2_spatial_all_datasets                                         WHERE ST_Within(geom,ST_GeomFromText(('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'),4326)) AND  species IN ('YFT') AND gear_group IN ('LL', 'PS', 'BB', 'OTHER', 'UNK') AND year IN ('1919', '1920', '1921', '1922', '1923', '1924', '1925', '1926', '1927', '1928', '1929', '1930', '1931', '1932', '1933', '1934', '1935', '1936', '1937', '1938', '1939', '1940', '1941', '1942', '1943', '1944', '1945', '1946', '1947', '1948', '1949', '1950', '1951', '1952', '1953', '1954', '1955', '1956', '1957', '1958', '1959', '1960', '1961', '1962', '1963', '1964', '1965', '1966', '1967', '1968', '1969', '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019') AND area::varchar IN ('NA', '9101.3790835111', '825', '775', '750', '6995.49529425369', '6014.62349518009', '600', '525', '50', '450', '400', '375', '325', '3125', '3021.33388249682', '2750', '2625', '2612.4315513254', '2550', '250', '25', '2375', '2325', '2275', '2100', '200', '1425', '1350', '1300', '1250', '1125', '10040.35433829', '100', '1') ) AS foo GROUP BY dataset, area, unit ORDER BY dataset"))
    # df_i2 <- df_i2 %>% mutate(unit=replace(unit,unit=='MT', 't')) # %>% filter(unit == 't')
    if(length(unique(df_i2$unit))>1){
      df_i2_t <- df_i2 %>% filter(unit == 't')
      df_i2_no <- df_i2 %>% filter(unit == 'no')
      if(switch_unit()){
        df_i2 <- df_i2_no
      }else{
        df_i2 <- df_i2_t
      }
    }
    
    row=c(0,0,1,1)
    column=c(0,1,0,1)
    
    if(length(unique(df_i2$dataset))>1){
      fig <- plot_ly()
      for(d in 1:length(unique(df_i2$dataset))){
        cat(df_i2$dataset[d])
        fig <- fig %>% add_pie(data = df_i2 %>% filter(dataset == unique(df_i2$dataset)[d]), labels = ~area, values = ~value,
                               name = paste0("Dataset : ",df_i2$dataset[d]), domain = list(row =row[d], column =column[d]))
      }
      fig <- fig %>% layout(title = "Pie Charts for each dataset using selected unit(s)", showlegend = T,
                            grid=list(rows=2, columns=2),
                            xaxis = list(showgrid = TRUE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = TRUE, zeroline = FALSE, showticklabels = FALSE)) %>% add_annotations(x=row+0.5,
                                                                                                                         y=column+05,
                                                                                                                         text = unique(df_i2$dataset),
                                                                                                                         xref = "paper",
                                                                                                                         yref = "paper",
                                                                                                                         xanchor = "left",
                                                                                                                         showarrow = FALSE
                            )
      }else{
      fig <- plot_ly(df_i2, labels = ~area, values = ~value, type = 'pie',
                     # marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     showlegend = TRUE)
      df_i2 = d <- fig %>% layout(title = 'Total catch according to area type',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
      
    
    fig
    # barplot_datasets <- ggplotly(fig, tooltip="text")
    
    
  })
  
  # st_read(con, query = paste0("SELECT dataset, area, unit, count(*), SUM(value)  as value FROM (",sql_query(),") AS foo GROUP BY dataset, area, unit ORDER BY dataset"))
  # st_read(con, query = paste0("SELECT  dataset, unit, count(*) AS count, SUM(value) AS value  FROM (",sql_query(),") AS foo  WHERE unit in ('t','MT','no') GROUP BY dataset, unit ORDER BY dataset"))
  
  # output$barplot_datasets <- renderPlotly({
  #   
  #   df_i1 <- data_barplot_all_datasets()
  #   # df_i1 <- data_pie_area_catch() 
  #   df_i1 <- df_i1 %>% filter(unit  == 't') %>% dplyr::select(c(dataset, unit,value)) # %>% spread(dataset, value, fill=0)
  #   
  #   # df_i1$dataset <- factor(df_i1$dataset) 
  #   # df_i1$unit <- factor(df_i1$unit) 
  #   
  #   p <- ggplot(df_i1) + geom_bar(aes(x = dataset, stat=value, fill = unit))
  #   # geom_bar(aes(x = dataset, fill = factor(unit)), position = position_dodge(preserve = 'single'))
  # 
  #   # p <- ggplot(data=df_i1, aes(x = dataset, stat = value, fill = unit)) + geom_bar(stat = "identity", width = 1)
  #   p <- p + ggtitle("Catch by dataset") + xlab("") + ylab("Datasets") # Adds titles
  #   # p <- p + facet_grid(facets=. ~ dataset) # Side by side bar chart
  #   # p <- p + coord_polar(theta="y") # side by side pie chart
  #   # 
  #   
  #   # Turn it interactive
  #   barplot_datasets <- ggplotly(p, tooltip="text")
  #   
  #   
  # })
  
  output$barplot_datasets <- renderPlotly({
    # https://stackoverflow.com/questions/55002248/plotly-stacked-bar-chart-add-trace-loop-issue
    df_i1 <- data_barplot_all_datasets()  %>% mutate(unit=replace(unit,unit=='MT', 't'))  %>% pivot_wider(names_from = unit, values_from = c("value", "count"), names_sep="_",values_fill = 0)
    # df_i1 <- data_barplot_all_datasets()  %>% mutate(unit=replace(unit,unit=='MT', 't'))   %>% df_i1(id = rownames(.))   %>% pivot_wider(names_from = unit, values_from = c("value", "count"), names_sep="_",values_fill = 0, -id)  %>%  plot_ly(x = ~id, y=~value, type="bar", color=~variable) %>% layout(barmode = "stack")
    
    # mtcars %>%    df_i1(id = rownames(.)) %>% gather(key = "variable",value = "value",-id) %>%  plot_ly(x = ~id, y=~value, type="bar", color=~variable) %>%       layout(barmode = "stack")
    
    # fig <- plot_ly(data=df_i1)
    # for(c in 2:length(colnames(df_i1))){
      for(c in 1:length(colnames(dplyr::select(df_i1,-c(dataset))))){
        this_column_name <- colnames(dplyr::select(df_i1,-c(dataset)))[c]
      
      # df_i1$tmp <-  dplyr::select(df_i1, c(dataset,!!this_column_name)) 
      
      if(c==1){
        fig <- plot_ly(df_i1, x = ~dataset, y =~value_t, type = 'bar', name = this_column_name)
      }else{
        fig <- fig %>% add_trace(y =~value_no, name = this_column_name)
        # fig <- fig %>% add_trace(y = ~dplyr::select(df_i1_t, c(paste0(this_column,'_t'))), name = this_column)
      }
    }
    fig <- fig %>% layout(yaxis = list(title = 'Count lines or catches in tons'), barmode = 'group')
    
    # fig <- plot_ly(df_i1, x = ~dataset, y = ~count, type = 'bar', name = 'Number of lines')
    # fig <- fig %>% add_trace(y = ~value, name = 'Total catch (in tons)')
    # fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    fig
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)




