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

####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i3_SpeciesYearByGearMonth.R")
####################################################################################################################################################################################################################################
DRV=RPostgres::Postgres()
# source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")
####################################################################################################################################################################################################################################

new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt) 
# target_species<- c("YFT","BFT")
# target_year <- c(seq(1:10)+1994)
target_species <- dbGetQuery(con, "SELECT DISTINCT(c_esp) AS species FROM public.i3 ORDER BY c_esp;")
target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM public.i3 ORDER BY year;")
target_gear <- dbGetQuery(con, "SELECT DISTINCT(c_g_engin) as gear FROM public.i3 ORDER BY c_g_engin;")

default_species <- 'YFT'
default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)
default_gear <- unique(target_gear)
filters_combinations <- dbGetQuery(con, "SELECT c_esp, year, c_g_engin as gear FROM public.i3 GROUP BY c_esp, year,c_g_engin;")

ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques"),
  sidebarLayout(
    sidebarPanel(
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
      actionButton("resetWkt", "Reset WKT to global")
    ),
    # mainPanel(
    #     # tableOutput("tbl"),
    #     leafletOutput('mymap')
    # ),
    tabsetPanel(
      tabPanel(
        title = "Map Postgis Vector / SF data",
        leafletOutput('mymap', width = "50%", height = 600)
      ),
      tabPanel(
        title = "Plot indicator 3",
        # plotlyOutput("plot1")
        plotlyOutput("plot1")
      ),
      tabPanel(
        title = "Browse i3 data",
        DT::dataTableOutput("DTi3")
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
)


server <- function(input, output, session) {
  
  
  sql_query <- eventReactive(input$submit, {
    query <- ("select year, month,  c_esp AS species, c_g_engin AS gear_type, value, mean_prev_5_years, stddev_prev_5_years from public.i3 ;")
    query = paste0("select year, month,  c_esp AS species, c_g_engin AS gear_type, value, mean_prev_5_years, stddev_prev_5_years, ST_ConvexHull(st_collect(geom)) as convexhull FROM public.i3 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))  AND species IN ('",paste0(input$species,collapse="','"),"')  GROUP BY  year, month,  c_esp, c_g_engin ;")
  },
  
  ignoreNULL = FALSE)
  
  data <- eventReactive(input$submit, {
    query = paste0("SELECT unit, ocean, gear_group, year, species, sum(value) as value, ST_ConvexHull(st_collect(geom)) as convexhull FROM public.i1i2_spatial WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))  AND species IN ('",paste0(input$species,collapse="','"),"')  GROUP BY  unit, ocean, gear_group, year, species ;")
    st_read(con, query =query)
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
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
    this <- data() %>% filter(year %in% input$year) %>% group_by(ocean,year,species) %>% summarise(value = sum(value))
  })
  
  
  output$DTi2 <- renderDT({
    this <- data()
    this <- data() %>% filter(year %in% input$year) %>% filter(gear_group %in% input$gear) %>% group_by(year,gear_group,species)   %>% summarise(value = sum(value))  %>% dplyr::rename(gear_type=gear_group)
    
  })
  
  
  
  
  output$mymap <- renderLeaflet({
    
    df <- data()
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
                                                 popup = ~paste0("Captures de",species,": ", round(value), " tonnes(t) et des brouettes"),
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
    
    # df_i3_filtered <- df_i3 %>% filter(gear_group %in% input$gear_choice,species %in% input$species_choice) %>% rename(gear_type=gear_group)
    # df_i3_filtered <- df_i3 %>% filter(year %in% target_year, gear_type %in% target_gear, species %in% target_species) 
    df_i3_filtered <- df_i3 %>% filter(year %in% input$year, gear_type %in% input$gear, species %in% input$species) 
    
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
  
  plotlyOutput("plot3")
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)




