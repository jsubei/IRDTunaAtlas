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
library(mapplots)

####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i7_SpeciesMapRelativeCatches.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i8_SpeciesMapRelativeCatchesOtherSpecies.R")
source("/home/julien/Desktop/CODES/IRDTunaAtlas/R/Atlas_i11_CatchesByCountry.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/wkt2spdf.R")
####################################################################################################################################################################################################################################
source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
# source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")
####################################################################################################################################################################################################################################

new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt) 

target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM public.i6i7i8 ORDER BY species;")
target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM public.i6i7i8 ORDER BY year;")
target_flag <- dbGetQuery(con, "SELECT DISTINCT(country) FROM public.i6i7i8 ORDER BY country;")

default_species <- 'YFT'
default_year <- '2015'
default_flag <- c('322','747','736')
# default_flag <- unique(target_flag)
# default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)

filters_combinations <- dbGetQuery(con, "SELECT species, year, country FROM  public.i6i7i8 GROUP BY species, year, country;")


ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques i11"),
  navbarPage(title="TunaAtlas", 
             tabPanel("Interactive",
                      
                      leafletOutput('mymap', width = "60%", height = 1500),
                      # imageOutput("plot11", height = 200),
                      
                      
                      # Shiny versions prior to 0.11 should use class = "modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 1000, height = "auto",
                                    
                                    h2("Select filters to customize indicators"),
                                    # imageOutput("plot11", height = 200),
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
                                      inputId = "country",
                                      label = "Country",
                                      choices = target_flag$country,
                                      multiple = TRUE,
                                      selected= default_flag
                                    ),
                                    actionButton(
                                      inputId = "submit",
                                      label = "Submit"
                                    ),
                                    actionButton("resetWkt", "Reset WKT to global"),
                                    plotlyOutput("plot6", height = 200),
                                    imageOutput("plot11", height = 200)
                                    
                                    # conditionalPanel("input.gear",
                                    #                  inputId = "gear",
                                    #                  label = "Gear",
                                    #                  choices = target_gear$gear,
                                    #                  multiple = TRUE,
                                    #                  selected= target_gear$gear
                                    # ),
                                    
                      ),
                      
             ),
             tabPanel("Data explorer",
                      # hr(),
                      # textOutput("sql_query"),
                      hr(),
                      DT::dataTableOutput("DTi11")
             ),
             navbarMenu("More",
                        tabPanel("Table",
                                 DT::dataTableOutput("DTi11")
                        ),
                        tabPanel("SQL query",
                                 textOutput("sql_query"),
                        ),
                        tabPanel(
                          title = "Your Map Plotted",
                          plotOutput("plotmap")
                        ),
                        tabPanel(
                          title = "Your indictaor 11",
                          imageOutput("plot11",)
                        ),
                        tabPanel(
                          title = "Your SQL query",
                          textOutput("sql_query")
                        ),
                        tabPanel("About",
                                 fluidRow(
                                   column(6,
                                          includeMarkdown("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/README.md")
                                   ),
                                   column(3,
                                          img(class="logo_IRD",
                                              src=paste0("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg")),
                                          tags$small(
                                            "Source: IRD",
                                            "Julien Barde ",
                                            "Funding : BlueCloud ",
                                            a(href="https://www.documentation.ird.fr/hor/fdi:010012425",
                                              "IRD Tuna Atlas (Alain Fontenau)"),
                                            a(href="https://www.documentation.ird.fr/hor/fdi:010012425",
                                              "IRD Tuna Atlas (Alain Fontenau)"),
                                            a(href="https://horizon.documentation.ird.fr/exl-doc/pleins_textes/divers11-03/010012425.pdf",
                                              "PDF")
                                            )
                                          )
                                   )
                                 )
                        )
             )
  )


server <- function(input, output, session) {
  
  
  sql_query <- eventReactive(input$submit, {
    # query="SELECT ogc_fid, geom_id, geom, year, species, country, value, count, ST_asText(geom) AS geom_wkt FROM public.i6i7i8                    WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326))                    AND species IN ('ALB','BET','BFT','BIL','BLF','BLM','BLT','BON','BOP','BRS','BSH','BUM','CCL','CER','FAL','FRI','HAM','KGM','LTA','MAK','MAW','MLS','OCS','oSks','OTH','oTun','PBF','POR','RSK','SAI','SBF','SFA','SKH','SKJ','SMA','SPF','SPN','SSM','SSP','SWO','THR','TUN','UNK','WAH','WHM','YFT')               AND country IN ('0','248','249','250','251','252','253','259','264','273','283','284','286','292','296','302','305','308','311','312','313','314','315','316','317','318','320','321','322','323','324','325','326','327','328','329','330','331','333','334','335','336','337','338','339','341','342','343','347','348','349','351','352','354','357','360','362','364','365','367','369','376','378','383','385','387','388','389','392','393','395','403','408','409','411','412','414','418','424','428','490','735','736','738','739','743','744','745','747','748','749','750','753','754','755','756','757','759','761','766','767','768','769','770','773','776')                  AND year IN ('1950','1951','1952','1953','1954','1955','1956','1957','1958','1959','1960','1961','1962','1963','1964','1965','1966','1967','1968','1969','1970','1971','1972','1973','1974','1975','1976','1977','1978','1979','1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990','1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019');"
    # query <- paste0("SELECT ogc_fid, geom_id, geom, year, species, country, value, count, ST_asText(geom) AS geom_wkt FROM public.i6i7i8
    #                 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))
    #                 AND species IN ('",paste0(input$species,collapse="','"),"')
    #                 AND country IN ('",paste0(input$country,collapse="','"),"')
    #                 AND year IN ('",paste0(input$year,collapse="','"),"') LIMIT 500;")
    paste0("SELECT  ogc_fid, geom_id, geom, year, species, country, value, count,ST_asText(geom) AS geom_wkt FROM public.i6i7i8
                      WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))
                      AND species IN ('",paste0(append(input$species,input$species_i6i7i8),collapse="','"),"')
                      AND country IN ('",paste0(input$country,collapse="','"),"')
                      AND year IN ('",paste0(input$year,collapse="','"),"');")
    
    # query <- paste0("SELECT ogc_fid, geom_id, geom, year, species, country, value, count, ST_asText(geom) AS geom_wkt FROM public.i6i7i8
    #                 WHERE ST_Within(geom,ST_GeomFromText('",new_wkt,"',4326))
    #                 AND species IN ('",default_species,"')
    #                 AND country IN ('",paste0(default_flag,collapse="','"),"')
    #                 AND year IN ('",paste0(default_year,collapse="','"),"') ;")
    # #                   AND species IN ('",paste0(append(input$species,input$species_i6i7i8),collapse="','"),"')
  },
  ignoreNULL = FALSE)
  
  data <- eventReactive(input$submit, {
    df <- as.data.frame(st_read(con, query = sql_query())) #%>% mutate(time_start = ISOdate(year, 1, 1), time_end = ISOdate(year, 12,31)) %>% dplyr::select (-c(ogc_fid, geom_id,geom,count,year)) %>%  dplyr::rename(v_catch=value,flag=country)
  },
  # on.exit(dbDisconnect(conn), add = TRUE)
  ignoreNULL = FALSE)
  
  observeEvent(input$resetWkt, {
    wkt(new_wkt)
  })
  
  
  # change <- reactive({
  #   unlist(strsplit(paste(c(input$species,input$year,input$country),collapse="|"),"|",fixed=TRUE))
  # })
  # 
  # 
  # observeEvent(input$species,{
  #   temp <- filters_combinations %>% filter(species %in% change()[1])
  #   updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
  #   updateSelectInput(session,"country",choices = unique(temp$country),selected=unique(temp$country))
  #   
  # }
  # )
  
  
  output$selected_var <- renderText({ 
    paste("You have selected:\n", input$species, "and \n", input$year, "and \n", input$country, "and \n", wkt())
  })
  
  output$sql_query <- renderText({ 
    paste("Your SQL Query is : \n", sql_query())
  })
  
  output$DTi11 <- renderDT({
    this <- data() 
  })
  
  
  
  output$mymap <- renderLeaflet({
    
    df <- st_read(con, query = sql_query())
    df <- st_read(con, query = query)
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
    map_leaflet <- leaflet()      %>%       addProviderTiles("Esri.OceanBasemap")   %>% addPolygons(data = df,
                                                 # label = ~species,
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
    
     # map_leaflet
    
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
  
  
  output$plotmap <- renderPlot({
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    df <- st_read(con, query = sql_query())
    
    plot(st_geometry(df),col = sf.colors(12, categorical = TRUE), border = 'grey', 
         axes = TRUE)
    # plot(df)
  })
  
  output$plot11 <- renderImage({
    
    df_i11_filtered <- as(df, "Spatial")
    
    df_i11_filtered <- as(st_read(con,query = sql_query()), "Spatial")
    
    i11 <- Atlas_i11_CatchesByCountry(df=df_i11_filtered,
                                             geomIdAttributeName="geom_id",
                                             countryAttributeName="country",
                                             speciesAttributeName="species",
                                             valueAttributeName="value",
                                             withSparql=FALSE)
    
      i11
      png(i11, width = 400, height = 300)
      hist(rnorm(input$obs), main = "Generated in renderImage()")
      dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           width = 400,
           height = 300,
           alt = "This is alternate text")
  # }, deleteFile = TRUE)
  })
  
    # Generate the PNG



  output$plot6 <- renderPlotly({


    df_i6i7 = st_read(con,query = sql_query())  %>% filter(species %in% input$species)  %>% filter(year %in% input$year) 
    # df_i6i7 = st_read(con,query = query)  #%>% filter(species %in% input$species)
    df_i6_filtered <- as(df_i6i7, "Spatial")

    i6 <- Atlas_i6_SpeciesMap(df=df_i6_filtered,
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




