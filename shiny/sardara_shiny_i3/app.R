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
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i3_SpeciesYearByGearMonth.R")
# source("/home/julien/Desktop/CODES/IRDTunaAtlas/R/TunaAtlas_i3_SpeciesYearByGearMonth.R")
####################################################################################################################################################################################################################################
DRV=RPostgres::Postgres()
source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
# source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")
####################################################################################################################################################################################################################################

new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt) 
# target_species<- c("YFT","BFT")

default_dataset <- 'i3_spatial_tunaatlas'
default_species <- 'YFT'
# default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)
default_year <- "2000"
default_year <- c(seq(1:10)+1994)
filters_combinations <- dbGetQuery(con,  paste0("SELECT c_esp, year, c_g_engin as gear FROM public.",default_dataset," GROUP BY c_esp, year,c_g_engin;"))



# target_dataset <- dbGetQuery(con, "select concat(schemaname,'.',matviewname)  as dataset from pg_matviews WHERE matviewname LIKE 'i3_%'  ORDER BY matviewname;")
target_dataset <- dbGetQuery(con, "select matviewname as dataset from pg_matviews WHERE matviewname LIKE 'i3_%'  ORDER BY matviewname;")
target_species <- dbGetQuery(con, paste0("SELECT DISTINCT(c_esp) AS species FROM public.",default_dataset," ORDER BY c_esp;"))
target_year <- dbGetQuery(con, paste0("SELECT DISTINCT(year) FROM public.",default_dataset," ORDER BY year;"))
target_gear <- dbGetQuery(con, paste0("SELECT DISTINCT(c_g_engin) as gear FROM public.",default_dataset," ORDER BY c_g_engin;"))

default_gear <- unique(target_gear)

ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques"),
  navbarPage(title="TunaAtlas", 
             tabPanel("Interactive",
                      
                      # If not using custom CSS, set height of leafletOutput to a number instead of percent
                      # leafletOutput("mymap", width="1000", height="1000"),
                      absolutePanel(id = "themap", class = "panel panel-default", fixed = TRUE,
                                    draggable = FALSE, top = 200, right = "auto", left = 20, bottom = "auto",
                                    width = "60%", height = "auto",
                                    
                                    leafletOutput('mymap'),
                      ),
                      absolutePanel(id = "theplot", class = "panel panel-default", fixed = TRUE,
                                    draggable = FALSE, top = "auto", right = "auto", left = 20, bottom = "200",
                                    width = "90%", height = "auto",
                                    
                                    dygraphOutput("plot3_dygraph"),
                      ),
                      
                      
                      # Shiny versions prior to 0.11 should use class = "modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = FALSE, top = 200, left = "auto", right = 20, bottom = "auto",
                                    width = "30%", height = "auto",
                                    
      selectInput(
        inputId = "dataset",
        label = "Dataset",
        choices = target_dataset$dataset,
        selected= default_dataset
      ),
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
      tags$br(),
      tags$br(),
      # conditionalPanel("input.gear",
      #                  inputId = "gear",
      #                  label = "Gear",
      #                  choices = target_gear$gear,
      #                  multiple = TRUE,
      #                  selected= target_gear$gear
      # ),
      
      # dygraphOutput("plot3_dygraph", height = "20%")
      # dygraphOutput("plot3_dygraph", height = "20%")
      plotlyOutput("plot3", height = "20%"),
      tags$br(),
      tags$br(),
      # dygraphOutput("plot3_dygraph"),
      tags$br(),
      tags$br()
      
      
                      ),
      
             ),

      tabPanel(
        title = "Browse i3 data",
        DT::dataTableOutput("DTi3")
      ),
      tabPanel(
        title = "Browse i3 data 1deg",
        DT::dataTableOutput("DTi3_1deg")
      ),
      tabPanel(
        title = "Your SQL query",
        textOutput("sql_query_text")
      ),
      tabPanel(
        title = "Your filters",
        textOutput("selected_var")
      )
    )
)


server <- function(input, output, session) {
  
  
  # sql_query <- eventReactive(input$submit, {
  #   query <- ("select year, month,  c_esp AS species, c_g_engin AS gear_type, value, mean_prev_5_years, stddev_prev_5_years from public.i3 ;")
  #   query = paste0("select year, month,  c_esp AS species, c_g_engin AS gear_type, value, mean_prev_5_years, stddev_prev_5_years, ST_ConvexHull(st_collect(geom)) as convexhull FROM public.i3 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))  AND species IN ('",paste0(input$species,collapse="','"),"')  GROUP BY  year, month,  c_esp, c_g_engin ;")
  # },
  # 
  # ignoreNULL = FALSE)
  
  
  sql_query <- eventReactive(input$submit, {
    if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
    query <- glue::glue_sql("SELECT year, month,  c_esp AS species, c_g_engin AS gear_type, SUM(value) as value, mean_prev_5_years, stddev_prev_5_years, geom FROM public.{`dataset_name`}
      WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326)) 
      AND  c_esp IN ({species_name*}) 
          AND c_g_engin IN ({gear_name*}) 
    AND year IN ({year_name*}) 

                             GROUP BY  year, month,  c_esp, c_g_engin,  mean_prev_5_years, stddev_prev_5_years, geom",
                            wkt = wkt(),
                            dataset_name = input$dataset,
                            species_name = input$species,
                            gear_name = input$gear,
                            year_name = year_name,
                            .con = con)
  },
  ignoreNULL = FALSE)
  
  
  metadata <- reactive({
    st_read(con, query = paste0("SELECT geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY geom")) 
  })  
  
  query <- eventReactive(input$submit, {
    paste0("SELECT year, month,  species, gear_type, SUM(value) as value, mean_prev_5_years, stddev_prev_5_years FROM (",sql_query(),") AS foo  GROUP BY  year, month,  species, gear_type, mean_prev_5_years, stddev_prev_5_years")
  },
  ignoreNULL = FALSE)
  
  
  data_i3 <- eventReactive(input$submit, {
    # st_read(con, query = paste0("SELECT to_date(concat(year::varchar(4),month::varchar(4)),''YYYY Mon') AS  year, month,  species, gear_type, SUM(value) as value, SUM(mean_prev_5_years) AS mean_prev_5_years, SUM(stddev_prev_5_years) AS stddev_prev_5_years FROM (",sql_query(),") AS foo  GROUP BY  year, month,  species, gear_type"))
    st_read(con, query = paste0("SELECT year, to_date(concat(year::varchar(4),'/',month::varchar(4)),'YYYY/MM') AS  date, month,  species, gear_type, SUM(value) as value, SUM(mean_prev_5_years) AS mean_prev_5_years, SUM(stddev_prev_5_years) AS stddev_prev_5_years FROM (",sql_query(),") AS foo  GROUP BY  year, month,  species, gear_type"))
    
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)

  
  change <- reactive({
    unlist(strsplit(paste(c(input$species,input$year,input$gear),collapse="|"),"|",fixed=TRUE))
  })
  
  observeEvent(input$resetWkt, {
    wkt(new_wkt)
  })
  
  # observeEvent(input$species,{
  #   temp <- filters_combinations %>% filter(species %in% change()[1])
  #   updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
  #     updateSelectInput(session,"gear",choices = unique(temp$gear),selected=unique(temp$gear))
  #   
  # }
  # )
  
  # observeEvent(input$year,{
  #   temp <- filters_combinations %>% filter(species %in% change()[1], year %in% change()[2])
  #   updateSelectInput(session,"gear",choices = unique(temp$gear),selected=unique(temp$gear))
  # }
  # )

  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$flag, "and \n", wkt())
  })
  
  
  output$sql_query_text <- renderText({ 
    paste("Your SQL Query is : \n", sql_query())
  })
  
  
  output$DTi3 <- renderDT({
    this <- data_i3()   #%>%  dplyr::select(-c(mean_prev_5_years,stddev_prev_5_years))  %>% spread(gear_type, value, fill=0)  
    # this <- data() %>% filter(year %in% input$year) %>% filter(gear_group %in% input$gear) %>% group_by(year,gear_group,species)   %>% summarise(value = sum(value))  %>% dplyr::rename(gear_type=gear_group)
    # df_i3_filtered <- df_i3 %>% filter(year %in% input$year, gear_type %in% input$gear, species %in% input$species) 
    
  })
  
  
  
  
  output$mymap <- renderLeaflet({
    
    df <- metadata()
    centroid <-  st_convex_hull(df)   %>% st_centroid()
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
                                                 popup = ~paste0("Captures de : ", round(value), " tonnes(t) et des brouettes"),
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
  
  
  
  # renderPlot({
  output$plot3 <- renderPlotly({ 
    
    df_i3_filtered <- data_i3()
    
    i3 <- Atlas_i3_SpeciesYearByGearMonth(df=df_i3_filtered,
                                          yearAttributeName="year",
                                          monthAttributeName="month",
                                          speciesAttributeName="species",
                                          gearTypeAttributeName="gear_type",
                                          valueAttributeName="value",
                                          meanPrev5YearsAttributeName="mean_prev_5_years",
                                          stddevPrev5YearsAttributeName="stddev_prev_5_years",
                                          withSparql=FALSE
    )
    i3
  })
  
  
  



output$plot3_dygraph <- renderDygraph({
  
  df_i3 = data_i3()  %>%  dplyr::select(-c(mean_prev_5_years,stddev_prev_5_years))  %>% spread(gear_type, value, fill=0)   #%>%  mutate(total = rowSums(across(any_of(as.vector(target_gear$gear_type)))))
  # df_i3 <- as_tibble(df_i3)  # %>% top_n(3)
  
  ll <- xts(x = df_i3$LL, order.by = df_i3$date)
  other <-  xts(x = df_i3$OTHER, order.by = df_i3$date)
  ps <-  xts(x = df_i3$PS, order.by = df_i3$date)
  bb <-  xts(x = df_i3$BB, order.by = df_i3$date)
  tuna_catches_timeSeries <- cbind(ll, other, ps,bb)
  
  # create the area chart
  g1 <- dygraph(tuna_catches_timeSeries)  %>% dyOptions( fillGraph=TRUE ) %>% dyOptions(stackedGraph = stack()) %>% dyRoller(rollPeriod = 1)
  # g1 <- dygraph(tuna_catches_timeSeries, main = "Catches by gear") %>%
  #   dyRangeSelector()     %>%     dyStackedBarGroup(c('bb', 'other','ll','ps'))    #%>% dyOptions( fillGraph=TRUE) create bar chart with the passed dygraph
  # dySeries(bb, label = "bb") %>%
  #   dySeries(other, label = "other") %>%
  #   dySeries(ll, label = "ll") %>%
  #   dySeries(ps, label = "ps")

  
  })


}


# Run the application 
shinyApp(ui = ui, server = server)




