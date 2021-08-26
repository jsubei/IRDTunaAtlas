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
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i4_SpeciesMonthByOcean.R")
####################################################################################################################################################################################################################################
source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
# source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")
####################################################################################################################################################################################################################################

new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt) 

query_i4 <- "select species, year, month, ocean, value from public.i4_spatial ;"

target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM public.i4_spatial ORDER BY species;")
target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM public.i4_spatial ORDER BY year;")
target_ocean <- dbGetQuery(con, "SELECT DISTINCT(ocean) FROM public.i4_spatial ORDER BY ocean;")

selectInput("species_choice", label = h3("Select species for i4 "), choices = target_species, selected = c("SKJ","YFT","BET"),multiple = TRUE)
selectInput("year_choice", label = h3("Select year for i4"), choices = target_year, selected = c("2000","2001","2002"),multiple = TRUE)

default_species <-  c("SKJ")
# default_species <-  c("SKJ","YFT")
default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)
# default_year <- "c(seq(2000:2010))"
default_ocean <- unique(target_ocean)
filters_combinations <- dbGetQuery(con, "SELECT species, year, ocean FROM public.i4_spatial GROUP BY  species, year, ocean  ;")

ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques"),
  navbarPage(title="TunaAtlas", 
             tabPanel("Interactive",
                      
                      # If not using custom CSS, set height of leafletOutput to a number instead of percent
                      leafletOutput('mymap', width = "60%", height = 1500),
                      
                      
                      # Shiny versions prior to 0.11 should use class = "modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 200, left = "auto", right = 20, bottom = "auto",
                                    width = "30%", height = "auto",
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
                        inputId = "ocean",
                        label = "Ocean",
                        choices = target_ocean$ocean,
                        multiple = TRUE,
                        selected= target_ocean$ocean
                      ),
                      actionButton(
                        inputId = "submit",
                        label = "Submit"
                      ),
                      actionButton("resetWkt", "Reset WKT to global"),
                      tags$br(),
                      imageOutput("plot4_image", width = "400", height = "300",inline = TRUE)
                      
                      )
    ),

      # tabPanel(
      #   title = "Plot indicator 4",
      #   # plotlyOutput("plot1")
      #   plotlyOutput("plot4")
      # ),
      tabPanel(
        title = "Browse i4 data",
        DT::dataTableOutput("DTi4")
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
  
  # 
  # sql_query <- eventReactive(input$submit, {
  #   
  #   query = paste0("select species, year, month, ocean, sum(value) as value, geom FROM public.i4_spatial WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))  AND species IN ('",paste0(input$species,collapse="','"),"')   AND year IN ('",paste0(input$year,collapse="','"),"')  GROUP BY  species, year, month, ocean ;")
  # },
  # 
  # ignoreNULL = FALSE)
  
  
  sql_query <- eventReactive(input$submit, {
    if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
    query <- glue::glue_sql("SELECT species, year, month, ocean, sum(value) as value, geom FROM public.i4_spatial 
      WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326)) 
      AND  species IN ({species_name*}) 
    AND year IN ({year_name*}) 
                             GROUP BY  species, year, month, ocean, geom",
      wkt = wkt(),
      species_name = input$species,
      year_name = year_name,
      .con = con)
  },
  ignoreNULL = FALSE)
  
  
  data_i4 <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT species, year, month, ocean, sum(value) as value FROM(",sql_query(),") AS foo GROUP BY  species, year, month, ocean"))
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

  observeEvent(input$year,{
    temp <- filters_combinations %>% filter(species %in% change()[1], year %in% change()[2])
    updateSelectInput(session,"gear",choices = unique(temp$gear),selected=unique(temp$gear))
  }
  )

  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$flag, "and \n", wkt())
  })
  
  
  output$sql_query <- renderText({ 
    paste("Your SQL Query is : \n", sql_query())
  })
  
  
  output$DTi4 <- renderDT({
    # this <- data() %>% filter(year %in% input$year)
    data_i4()
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
                  popup = ~paste0("Catches for this species: ", round(value), " tonnes(t) et des brouettes"),
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
  
  
  # 
  # output$plot4 <- renderPlotly({ 
  #   
  #   df_i4_filtered <- data_i4()
  #   
  #   i4 <-  Atlas_i4_SpeciesMonthByOcean(df=df_i4_filtered, 
  #                                       oceanAttributeName="ocean", 
  #                                       yearAttributeName="year", 
  #                                       monthAttributeName="month",
  #                                       speciesAttributeName="species", 
  #                                       valueAttributeName="value")
  # i4
  # })
  
  
  output$plot4_image <- renderImage({
    df_i4_filtered <- data_i4()
    
    i4 <-  Atlas_i4_SpeciesMonthByOcean(df=df_i4_filtered, 
                                        oceanAttributeName="ocean", 
                                        yearAttributeName="year", 
                                        monthAttributeName="month",
                                        speciesAttributeName="species", 
                                        valueAttributeName="value")
    i4 
    png(i4, width = 1600, height = 1200)
    dev.off()
    
    # Return a list containing the filename
    list(src = i4,
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)




