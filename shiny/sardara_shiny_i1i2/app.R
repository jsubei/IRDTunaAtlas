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
source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
# source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")
####################################################################################################################################################################################################################################

new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt) 
stack <- reactiveVal(TRUE) 
# target_species<- c("YFT","BFT")
# target_year <- c(seq(1:10)+1994)
target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM public.i1i2_spatial ORDER BY species;")
target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM public.i1i2_spatial ORDER BY year;")
target_gear <- dbGetQuery(con, "SELECT DISTINCT(gear_group) as gear FROM public.i1i2_spatial ORDER BY gear_group;")
target_ocean <- dbGetQuery(con, "SELECT DISTINCT(ocean) as ocean FROM public.i1i2_spatial ORDER BY ocean;")

default_species <- 'YFT'
default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)
# default_gear <- c('BB','PS')
default_gear <- unique(target_gear)
filters_combinations <- dbGetQuery(con, "SELECT species, year, gear_group as gear FROM public.i1i2_spatial GROUP BY species, year,gear_group;")

ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques"),
  navbarPage(title="TunaAtlas", 
             tabPanel("Interactive",
                      
                      # If not using custom CSS, set height of leafletOutput to a number instead of percent
                      # leafletOutput("mymap", width="1000", height="1000"),
                      leafletOutput('mymap', width = "60%", height = 1500),
                      
                      
                      # Shiny versions prior to 0.11 should use class = "modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 200, left = "auto", right = 20, bottom = "auto",
                                    width = "30%", height = "auto",
                                    
                                    selectInput(
                                      inputId = "species",
                                      label = "Species",
                                      choices = target_species$species,
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
                                    # conditionalPanel("input.gear",
                                    #                  inputId = "gear",
                                    #                  label = "Gear",
                                    #                  choices = target_gear$gear,
                                    #                  multiple = TRUE,
                                    #                  selected= target_gear$gear
                                    # ),
                                    
                                    plotlyOutput("plot1", height = "20%"),
                                    plotlyOutput("plot2", height = "20%")
                      ),
                      
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
  
  
  
  # sql_query <- eventReactive(input$submit, {
  #   if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
  #   query <- glue::glue_sql(
  #     "SELECT unit, ocean, gear_group, year, species, sum(value) as value, ST_ConvexHull(st_collect(geom)) as convexhull FROM public.i1i2_spatial 
  #     WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
  #     AND  species IN ({species_name*}) 
  #   AND gear_group IN ({gear_group_name*})
  #   AND year IN ({year_name*})
  #     GROUP BY  unit, ocean, gear_group, year, species",
  #     wkt = wkt(),
  #     species_name = input$species,
  #     gear_group_name = input$gear,
  #     year_name = year_name,
  #     .con = con)
  # },
  # ignoreNULL = FALSE)
  
  sql_query <- eventReactive(input$submit, {
    if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
    query <- glue::glue_sql(
      "SELECT unit, ocean, gear_group, year, species, value, geom  FROM public.i1i2_spatial 
      WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND  species IN ({species_name*}) 
    AND gear_group IN ({gear_group_name*})
    AND year IN ({year_name*}) ",
      wkt = wkt(),
      species_name = input$species,
      gear_group_name = input$gear,
      year_name = year_name,
      .con = con)
  },
  ignoreNULL = FALSE)
  
  # 
  # sql_query <- eventReactive(input$submit, {
  #   # query = paste0("SELECT sum(value) as value, unit, ocean, gear_group, year, species, st_collect(geom) FROM public.i1i2_spatial WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))  AND species IN ('",paste0(input$species,collapse="','"),"')  AND year IN ('",paste0(input$year,collapse="','"),"') GROUP BY  unit, ocean, gear_group, year, species ;")
  #   query = paste0("SELECT unit, ocean, gear_group, year, species, sum(value) as value, ST_ConvexHull(st_collect(geom)) as convexhull FROM public.i1i2_spatial WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))  AND species IN ('",paste0(input$species,collapse="','"),"')  GROUP BY  unit, ocean, gear_group, year, species ;")
  # },
  # ignoreNULL = FALSE)
  
  
  data_i1 <- eventReactive(input$submit, {
    # st_read(con, query =sql_query()) # %>% mutate(time_start = ISOdate(year, 1, 1), time_end = ISOdate(year, 12,31)) %>% dplyr::select (-c(ogc_fid, geom_id,geom,count,year)) %>%  dplyr::rename(v_catch=value,flag=country)
    st_read(con, query = paste0("SELECT unit, ocean, to_date(year::varchar(4),'YYYY') AS  year, species, sum(value) as value FROM (",sql_query(),") AS foo GROUP BY  unit, ocean, year, species"))

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
  

  
  observeEvent(input$stacked, {
    if(stack()){stack(FALSE)}else{stack(TRUE)}
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
    # this <- data() %>% filter(year %in% input$year)
    this <-data_i1()
  })
  
  
  output$DTi2 <- renderDT({
    this <- data_i2()
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
      addProviderTiles("Esri.OceanBasemap")  %>%
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
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)




