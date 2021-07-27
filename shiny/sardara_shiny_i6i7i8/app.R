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
library(viridis)

####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i7_SpeciesMapRelativeCatches.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i8_SpeciesMapRelativeCatchesOtherSpecies.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_database_to_NetCDF.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/wkt2spdf.R")
####################################################################################################################################################################################################################################
DRV=RPostgres::Postgres()
# source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")
####################################################################################################################################################################################################################################

new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt) 
# target_species<- c("YFT","BFT")
# target_year <- c(seq(1:10)+1994)
# target_flag <- c("331","747")
# target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM public.i6i7i8;")
# target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM public.i6i7i8;")
# target_flag <- dbGetQuery(con, "SELECT DISTINCT(country) FROM public.i6i7i8;")
target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM fact_tables.global_catch_1deg_1m_ps_bb_ird_level0 ORDER BY species;")
target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM fact_tables.global_catch_1deg_1m_ps_bb_ird_level0 ORDER BY year;")
target_flag <- dbGetQuery(con, "SELECT DISTINCT(flag) AS country FROM fact_tables.global_catch_1deg_1m_ps_bb_ird_level0 ORDER BY flag;;")

default_species <- 'YFT'
default_year <- '2015'
default_flag <- '322'
# filters_combinations <- dbGetQuery(con, "SELECT species, year, country FROM  public.i6i7i8 GROUP BY species, year, country;")
filters_combinations <- dbGetQuery(con, "SELECT species, year, flag AS country FROM  fact_tables.global_catch_1deg_1m_ps_bb_ird_level0 GROUP BY species, year, country;")

ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "species_i6i7i8",
        label = "Species",
        choices = target_species$species,
        selected= default_species
      ),
      selectInput(
        inputId = "year_i8",
        label = "Year",
        choices = target_year$year,
        selected= default_year
      ),
      selectInput(
        inputId = "flag_i8",
        label = "Flag",
        choices = target_flag$country,
        selected= default_flag
      ),
      selectInput(
        inputId = "species_i8",
        label = "Additionnal species (only required for i8)",
        choices = target_species$species[-which(target_species$species %in% default_species)],
        selected=c("BET","SKJ"),
        multiple=TRUE
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
        leafletOutput('overviewmap', width = "50%", height = 600)
      ),
      tabPanel(
        title = "Map NetCDF data",
        leafletOutput('mymap', width = "50%", height = 600)
      ),
      tabPanel(
        title = "Your Map Plotted",
        plotOutput("plotmap")
      ),
      tabPanel(
        title = "Plot indicator 6",
        plotlyOutput("plot6")
      ),
      tabPanel(
        title = "Plot indicator 7",
        plotlyOutput("plot7")
      ),
      tabPanel(
        title = "Plot indicator 8",
        plotlyOutput("plot8")
      ),
      tabPanel(
        title = "Browse i6i7 data",
        DT::dataTableOutput("DTi6i7")
      ),
      tabPanel(
        title = "Browse i8 table",
        DT::dataTableOutput("DTi8")
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
    #query =  paste0("SELECT ogc_fid, geom_id, geom, year, species, country, value, count FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) AND species IN ('",paste0(input$species_i6i7i8,collapse="','"),"') AND country IN ('",paste0(input$flag_i8,collapse="','"),"') AND year IN ('",paste0(input$year_i8,collapse="','"),"');")
    # paste0("SELECT  ogc_fid, geom_id, geom, year, species, country, value, count,ST_asText(geom) AS geom_wkt FROM public.i6i7i8 
    #                   WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) 
    #                   AND species IN ('",paste0(append(input$species_i8,input$species_i6i7i8),collapse="','"),"') 
    #                   AND country IN ('",paste0(input$flag_i8,collapse="','"),"') 
    #                   AND year IN ('",paste0(input$year_i8,collapse="','"),"');")
    paste0("SELECT  geographic_identifier AS ogc_fid, id_area AS geom_id, ST_GeomFromText(geom_wkt) AS  geom, year, species,flag AS country, value, '1' AS count, geom_wkt FROM fact_tables.global_catch_1deg_1m_ps_bb_ird_level0 
           WHERE ST_Within(ST_GeomFromText(geom_wkt,4326),ST_GeomFromText('",wkt(),"',4326)) 
           AND species IN ('",paste0(append(input$species_i8,input$species_i6i7i8),collapse="','"),"') 
           AND flag IN ('",paste0(input$flag_i8,collapse="','"),"') 
           AND year IN ('",paste0(input$year_i8,collapse="','"),"');")
  },
  ignoreNULL = FALSE)
  
  data <- eventReactive(input$submit, {
    df <- as.data.frame(st_read(con, query = sql_query())) %>% mutate(time_start = ISOdate(year, 1, 1), time_end = ISOdate(year, 12,31)) %>% dplyr::select (-c(ogc_fid, geom_id,geom,count,year)) %>%  dplyr::rename(v_catch=value,flag=country)
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  
  change <- reactive({
    unlist(strsplit(paste(c(input$species_i6i7i8,input$year_i8,input$flag_i8),collapse="|"),"|",fixed=TRUE))
  })
  
  observeEvent(input$resetWkt, {
    wkt(new_wkt)
  })
  
  observeEvent(input$species_i6i7i8,{
    # temp <- filters_combinations %>% filter(grepl(pattern ="BFT",comb))
    temp <- filters_combinations %>% filter(species %in% change()[1]) %>% arrange(desc(year))
    # updateSelectInput(session,"year_i8",choices = unique(temp$year),selected = temp$year[1])
    # updateSelectInput(session,"flag_i8",choices = unique(temp$country),selected = temp$country[1])
    updateSelectInput(session,"year_i8",choices = unique(temp$year))
    # updateSelectInput(session,"flag_i8",choices = unique(temp$country))
  }
  )
  
  observeEvent(input$year_i8,{
    temp <- filters_combinations %>% filter(species %in% change()[1], year %in% change()[2])  %>% arrange(desc(country))
    # updateSelectInput(session,"species_i6i7i8",choices = unique(temp$species),selected = temp$species[1])
    # updateSelectInput(session,"flag_i8",choices = unique(temp$country),selected = temp$country[1])
    # updateSelectInput(session,"species_i6i7i8",choices = unique(temp$species))
    updateSelectInput(session,"flag_i8",choices =  unique(temp$country))
  }
  )
  
  # observeEvent(input$flag_i8,{
  #   temp <- filters_combinations%>% filter(country %in% change()[3] & year %in% change()[2])
  #   updateSelectInput(session,"species_i6i7i8",choices = unique(temp$species),selected = temp$species[1])
  #   updateSelectInput(session,"year_i8",choices = unique(temp$year),selected = temp$year[1])
  # }
  # )
  # 
  # observeEvent(input$flag_i8,{  
  #   temp <- dat %>% filter(id1 %in% change()[1] & id2 %in% change()[2])
  #   updateSelectInput(session,"id3",choices = unique(temp$id3))
  # }
  # )
  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species_i6i7i8, "and \n", input$year_i8, "and \n", input$flag_i8, "and \n", wkt())
  })
  
  
  output$sql_query <- renderText({ 
    paste("Your SQL Query is : \n", sql_query())
  })
  
  
  output$DTi6i7 <- renderDT({
    this <- data() %>% filter(species %in% input$species_i6i7i8)
  })
  
  
  output$DTi8 <- renderDT({
    this <- data()
  })
  
  # output$dataTable = DT::renderDataTable({
  #   this <- data()
  # })
  
  # output$dataTable <-DT::renderDT({
  #   DT::datatable(
  #     data(),
  #     selection = "none",
  #     extensions = 'Buttons',
  #     option = list(
  #       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
  #       dom = 'Brti'
  #     )
  #   ) %>%
  #     DT::formatCurrency(
  #       columns = 2:5,
  #       currency = '',
  #       mark = " ",
  #       digits = 0
  #     )
  # })
  
  output$plotmap <- renderPlot({
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    df <- st_read(con, query = sql_query())
    
    plot(st_geometry(df),col = sf.colors(12, categorical = TRUE), border = 'grey', 
         axes = TRUE)
    # plot(df)
  })
  
  
  output$overviewmap <- renderLeaflet({
    
    df <- st_read(con, query = sql_query())
    
    # pal <- colorBin(
    #   palette = "viridis",
    #   domain = df$value,
    #   reverse = TRUE,
    #   # Echelle avec des quantiles sinon Paris prend toute la place...
    #   bins = quantile(
    #     domain = df$value,
    #     probs = seq(0, 1, by = 0.2))
    # )
    # df <- st_read(con, query = "SELECT *  FROM public.i6i7i8 WHERE species='YFT' and year ='2000';")
    # 
    # color_pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(df$value),
    #                           na.color = "transparent")
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = df$value
    )
    # brewer.pal(7, "OrRd")
    pal_fun <- colorQuantile("YlOrRd", NULL, n = 10)
    
    # qpal <- colorQuantile(rev(viridis::viridis(10)),
    #                       df$value, n=10)
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    map_leaflet <- leaflet()  %>% addPolygons(data = df,
                                                 # label = ~species,
                                                 label = ~value,
                                                 popup = ~paste0("Captures de",species,": ", round(value), " tonnes(t) et des brouettes"),
                                                 fillColor = ~pal_fun(value),
                                                 # fillColor = ~qpal(value),
                                                 fill = TRUE, 
                                                 fillOpacity = 0.8,
                                                 smoothFactor = 0.5
                                                 # color = ~pal(value)      
    ) %>% 
      addProviderTiles("Esri.OceanBasemap"
      )
    # ) %>% 
    #   addLegend(
    #     "topright", pal = qpal, values = ~value,
    #     title = htmltools::HTML("Population density <br> (2005)"),
    #     opacity = 1 )
    # %>%  addLegend("bottomright", 
    #            # colors = colorQuantile("YlOrRd", NULL, n = 10), 
    #            pal = pal, values = ~value,
    #            # labFormat = labelFormat(prefix = "$"),
    #            # labels = paste0("up to ", format(df$values[-1], digits = 2)),
    #            title = "Captures de thons")
  })
  
  
  
  
  output$mymap <- renderLeaflet({
    df <- as.data.frame(st_read(con, query = sql_query())) %>% mutate(time_start = ISOdate(year, 1, 1), time_end = ISOdate(year, 12,31)) %>% dplyr::select (-c(ogc_fid, geom_id,geom,count,year)) %>%  dplyr::rename(v_catch=value,flag=country)  %>% filter(species %in% input$species_i6i7i8)
    # df <- data() %>% filter(species %in% input$species_i6i7i8)
    netCDF_file <- TunaAtlas_database_to_NetCDF(df)
    centroid <- st_union(st_as_sfc(df$geom_wkt)) %>% st_convex_hull()  %>% st_centroid()
    lat_centroid <- st_coordinates(centroid)[2]
    lon_centroid <- st_coordinates(centroid)[1]
    
    # Load raster and set projection and lat/lon extent
    pr <- raster(netCDF_file, varname="v_catch")
    
    # Project to the leaflet lat/long grid and visualize
    r <- projectRasterForLeaflet(pr, method = "bilinear")
    
    # set color palette
    color_pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                              na.color = "transparent")
    
    leaflet() %>% addTiles() %>%
      addRasterImage(r, colors = color_pal, opacity = .7) %>%
      addLegend(pal = color_pal, values = values(r),
                title = paste0("Captures de thons",lon_centroid,lat_centroid))  %>%
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
  
  
  output$plot7 <- renderPlotly({
    
    
    
    
    df_i6i7 = st_read(con,query = sql_query())  %>% filter(species %in% input$species_i6i7i8)
    df_i7_filtered <- as(df_i6i7, "Spatial")
    
    i7 <-  Atlas_i7_SpeciesMapRelativeCatches(df=df_i7_filtered, 
                                              geomIdAttributeName="geom_id",
                                              yearAttributeName="year", 
                                              speciesAttributeName="species",                                         
                                              valueAttributeName="value",
                                              withSparql=FALSE
    )
    
    
  })
  
  output$plot6 <- renderPlotly({ 
    
    
    df_i6i7 = st_read(con,query = sql_query())  %>% filter(species %in% input$species_i6i7i8)
    df_i6_filtered <- as(df_i6i7, "Spatial")
    
    # withProgress(message = 'Making plot i6', value = 0, {
    i6 <- Atlas_i6_SpeciesMap(df=df_i6_filtered,
                              geomIdAttributeName="geom_id",
                              yearAttributeName="year",
                              speciesAttributeName="species",
                              valueAttributeName="value",
                              withSparql=FALSE
    )
    # })
    
  })
  
  
  output$plot8 <- renderPlotly({
    
    # df_i8=df_i6i7i8 %>% filter(st_within(geom,st_as_sfc(wkt(), crs = 4326)) %>% lengths > 0, year %in% input$year_i8, species %in% append(input$species_i8,input$species_i6i7i8), country %in% input$flag_i8 ) 
    df_i8 = st_read(con,query = sql_query()) 
    df_i8_filtered <- as(df_i8, "Spatial")
    
    i8 <- Atlas_i8_SpeciesMapRelativeCatchesOtherSpecies(df=df_i8_filtered,
                                                         targetedSpecies=input$species_i6i7i8,
                                                         geomIdAttributeName="geom_id",
                                                         yearAttributeName="year",
                                                         speciesAttributeName="species",
                                                         valueAttributeName="value",
                                                         withSparql=FALSE
    )
    
    
  })
  
  
  }


# Run the application 
shinyApp(ui = ui, server = server)




