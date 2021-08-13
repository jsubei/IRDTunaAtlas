library(shiny)
library(shinyWidgets)
library(geojsonio)
library(plyr)
library(dplyr)
library(DBI)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(leafpm)
library(mapedit)
library(sf)
library(sp)
library(plotly)
library(DT)
library(ncdf4)
# library(raster)
library(maps)
library(ggplot2)
library(XML)
library(viridis)
library(mapplots)
library(reshape)
library(tidyr)
library(streamgraph)
library(RColorBrewer)
library(glue)

# pending issue with st_union
# https://keen-swartz-3146c4.netlify.app/sf.html
####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/wkt2spdf.R")
####################################################################################################################################################################################################################################
# source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
source(file = "~/Bureau/CODES/IRDTunaAtlas/credentials.R")
####################################################################################################################################################################################################################################

global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt) 
metadata <- reactiveVal() 
zoom <- reactiveVal(1) 
# data <- reactiveVal() 
# data_i11 <- reactiveVal() 
# data_pie_map<- reactiveVal() 
# centroid <- reactiveVal() 



target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM fact_tables.i6i7i8 ORDER BY species;")
target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM fact_tables.i6i7i8 ORDER BY year;")
target_flag <- dbGetQuery(con, "SELECT DISTINCT(country) FROM fact_tables.i6i7i8 ORDER BY country;")

default_species <- 'YFT'
default_year <- '2010'
default_flag <- c('EUESP','EUFRA','TWN','JPN')
# default_flag <- unique(target_flag)
# default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)

# sql_query <- reactiveVal(paste0("SELECT   geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt FROM fact_tables.i6i7i8
#            WHERE  species IN ('",paste0(default_species,collapse="','"),"')
#                       AND country IN ('",paste0(default_flag,collapse="','"),"')
#                       AND year IN ('",paste0(default_year,collapse="','"),"')
#            GROUP BY species, country,geom_wkt, geom
#            ORDER BY species,country DESC
#            ;"))


filters_combinations <- dbGetQuery(con, "SELECT species, year, country FROM  fact_tables.i6i7i8 GROUP BY species, year, country;")


ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques i11"),
  navbarPage(title="TunaAtlas", 
             tabPanel("Interactive",
                      div(class="outer",
                          tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 200, left = "auto", right = 20, bottom = "auto",
                                        width = 400, height = "auto",
                                        
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
                                        plotOutput(outputId = "plot_species",width="300")
                                        
                                        # actionButton("resetWkt", "Reset WKT to global"),
                                        # plotOutput(outputId = "plot_species")
                                        # plotOutput("cumulative_plot", height="130px", width="100%")
                                        
                          ),
                          absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='89',width='108')))
                      )
             ),
             tabPanel("Interactive Indicator 11",
                      div(class="outer",
                          tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
                          # leafletOutput('map_i11', width = "60%", height = 1500),
                          leafletOutput("map_i11", width="100%", height="100%"),
                          
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 200, left = "auto", width = 400, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        # h3(textOutput("sql_query"), align = "right"),
                                        plotOutput("plot1_streamgraph", height=200, width="100%"),
                                        plotlyOutput("pie_map_i11", height = 200),
                                        # h6(textOutput("sars_clean_date_reactive"), align = "right"),
                                        # h6(textOutput("sars_reactive_country_count"), align = "right"),
                                        # plotOutput("sars_epi_curve", height="130px", width="100%"),
                                        # plotOutput("sars_cumulative_plot", height="130px", width="100%"),
                                        sliderInput(inputId="yearInterval", "Range:",
                                                    min = min(target_year),
                                                    max = max(target_year),
                                                    value = c(min(target_year),max(target_year)),
                                                    round = TRUE, step=1
                                        ),
                                        span(("Rate of catch according to the flag of the fishing fleet"),align = "left", style = "font-size:80%"),
                                        tags$br(),
                                        span(("Circles in the grid shows the detail of this rate for a spefic square of the grid"),align = "left", style = "font-size:80%")
                                        
                                        # sliderTextInput("sars_plot_date",
                                        #                 label = h5("Select mapping date"),
                                        #                 choices = format(unique(sars_cases$date), "%d %b %y"),
                                        #                 # selected = format(sars_max_date, "%d %b %y"),
                                        #                 grid = FALSE,
                                        #                 animate=animationOptions(interval = 3000, loop = FALSE))
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216'))),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 15, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                      )
             ),
             
             tabPanel("ggplot Indicator 11",
                      imageOutput("plot11", height = 1200)
             ),
             tabPanel("Zoom level",
                      hr(),
                      textOutput("zoom")
             ),
             tabPanel("Data explorer overview",
                      # hr(),
                      # textOutput("sql_query"),
                      hr(),
                      DT::dataTableOutput("DT")
             ),
             tabPanel("Data explorer i11",
                      # hr(),
                      # textOutput("sql_query"),
                      hr(),
                      DT::dataTableOutput("DTi11")
             ),
             navbarMenu("More",
                        tabPanel(
                          title = "Your SQL query for overview",
                          textOutput("sql_query_metadata")
                        ),
                        tabPanel(
                          title = "Your SQL query",
                          textOutput("sql_query")
                        ),
                        tabPanel(
                          title = "Your SQL query plot1",
                          textOutput("sql_query_metadata_plot1")
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
                                            a(href="https://github.com/juldebar/IRDTunaAtlas/wiki/Indicator-I11-:-Catches-by-country",
                                              "IRD Indicator 11"),
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
  
  
  
  sql_query_metadata_plot1 <- eventReactive(input$submit, {
    paste0("Your zom is Zoom",zoom(),"   ;")
  },
  ignoreNULL = FALSE)
  
  
  sql_query <- eventReactive(input$submit, {
    query <- glue::glue_sql(
      "SELECT   geom_id, geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt, year FROM fact_tables.i6i7i8
      WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND species IN ({species_name*})
      AND country IN ({country_name*})
      AND year IN ({year_name*})
      GROUP BY species, country,geom_id, geom_wkt, geom , year
      ORDER BY species,country DESC",
      wkt = wkt(),
      species_name = input$species,
      country_name = input$country,
      year_name = input$year,
      .con = con)
  },
  ignoreNULL = FALSE)
  
  sql_query_metadata <- eventReactive(input$submit, {
    paste0("SELECT species, country, geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY species, country, geom") 
  },
  ignoreNULL = FALSE)
  
  
  data <- eventReactive(input$submit, {
    # req(input$species)
    # req(input$country)
    # req(input$year)
    outp <- st_read(con, query = sql_query())
    outp
  },
  ignoreNULL = FALSE)
  
  
  metadata <- reactive({
    st_read(con, query = paste0("SELECT species, geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY species, geom")) 
  })  
  
  data_pie_map <- reactive({
    # st_read(con, query = paste0("SELECT species, country, geom, sum(value) AS value FROM(SELECT geom_id, geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt, year FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText(('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'),4326)) AND species IN ('YFT') AND country IN ('EUESP', 'EUFRA', 'JPN', 'TWN') AND year IN ('2010') GROUP BY species, country,geom_id, geom_wkt, geom , year ORDER BY species,country DESC) AS foo GROUP BY species, country, geom"))
    st_read(con, query = paste0("SELECT species, country, geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY species, country, geom"))  %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country)))))
  })
  
  data_time_serie <- reactive({
    st_read(con, query = paste0("SELECT species, year, geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY species, year, geom")) 
  })
  
  data_pie_chart_country <- reactive({
    st_read(con, query = paste0("SELECT country, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY country"))
  })
  
  
  centroid <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT st_centroid(St_convexhull(st_collect(geom))) FROM  (",sql_query(),") AS foo;"))
  },
  ignoreNULL = FALSE)
  
  # observeEvent(sql_query(), {
  #   centroid(st_read(con, query = paste0("SELECT st_centroid(St_convexhull(st_collect(geom))) FROM  (",sql_query(),") AS foo;")))
  # },
  # ignoreInit = FALSE)
  
  # metadata <- eventReactive(input$submit, {
  #   st_read(con, query = sql_query_metadata())
  # },
  # ignoreNULL = FALSE)
  
  
  
  # data <- eventReactive(input$submit, {
  #   st_read(con, query = sql_query())
  # },
  # ignoreNULL = FALSE)
  
  # observeEvent(sql_query(), {
  #   data(st_read(con, query = sql_query()))
  # },
  # ignoreInit = FALSE)
  
  
  # observeEvent(data(), {
  #   # metadata(st_read(con, query = sql_query_metadata()))
  #   metadata(data()  %>% group_by(species,geom_wkt) %>% summarise(value = sum(value)))
  # },
  # ignoreInit = FALSE)
  # 
  # observeEvent(data(), {
  #   data_i11(data(data() %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country)))))))
  # },
  # ignoreInit = FALSE)
  
  
  # data_i11 <- eventReactive(input$submit, {
  #   # data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>% 
  #   #   mutate(total = rowSums(across(any_of(as.vector(input$country)))))
  #   data()  %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country))))) %>% filter (total>mean(total))
  #   # data() %>% spread(country, value, fill=0)  %>% mutate(total = rowSums(across(any_of(as.vector(input$country)))))
  #   # st_read(con, query = "SELECT ogc_fid, geom_id, geom, year, species, country, value, count,ST_asText(geom) AS geom_wkt FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326)) AND species IN ('SKJ') AND country IN ('EU.ESP','JPN','TWN') AND year IN ('2014')") %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value)  %>%
  #   #   replace(is.na(.), 0) %>% mutate(total = rowSums(across(all_of(c("JPN","TWN"))))) %>% class()
  #     # mutate(total = rowSums(across(all_of(c("JPN","TWN")))))
  #      # rowwise()  %>% mutate(sumrow = as_data_frame(.)[,-c(1:3)])     replace(is.na(.), 0) %>%    all_of(input$country)))    mutate(sum = rowSums(across(where(is.numeric)))))
  # },
  # ignoreNULL = FALSE)
  
  
  
  
  
  # metadata_i11 <- eventReactive(input$submit, {
  #   # data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value)) # %>% top_n(3)
  #   data() %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value)) # %>% top_n(3)
  #   
  # },
  # ignoreNULL = FALSE)
  
  
  
  observeEvent(input$resetWkt, {
    wkt(global_wkt)
  },
  ignoreInit = TRUE)
  
  
  change <- reactive({
    unlist(strsplit(paste(c(input$species,input$year,input$country),collapse="|"),"|",fixed=TRUE))
  })
  
  
  observeEvent(input$species,{
    temp <- filters_combinations %>% filter(species %in% change()[1])
    updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
    updateSelectInput(session,"country",choices = unique(temp$country),selected=unique(temp$country))
    
  },
  ignoreInit = TRUE)
  
  
  ############################################################# OUTPUTS   ############################################################# 
  
  output$sql_query <- renderText({ 
    paste("Your SQL Query for indicator 11 is : \n", sql_query())
  })
  
  output$sql_query_metadata <- renderText({ 
    paste("Your SQL Query is : \n", sql_query_metadata())
  })
  
  output$zoom <- renderText({ 
    paste0("Your zom is Zoom",zoom(),"   ;")
  })
  
  
  output$DT <- renderDT({
    data()  %>% st_drop_geometry()
    # dplyr::select(species,country,value,geom_wkt)
    # dplyr::select(-c(geom))
    # as_data_frame(toto)[-c(1:3,ncol(as_data_frame(toto)))]
  }) 
  
  
  output$DTi11 <- renderDT({
    # this <- data() %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value))
    # toto <- st_read(con, query = "SELECT geom_id, geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt, year FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326)) AND species IN ('YFT') AND country IN ('EUESP','EUFRA','JPN','TWN') AND year IN ('2010','2011') GROUP BY species, country,geom_id, geom_wkt, geom , year ORDER BY species,country DESC") %>% group_by(species,country,geom_id) %>% summarise(value = sum(value))  %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country)))))
    
    data_pie_map()  %>% st_drop_geometry()
    
  }) 
  
  
  
  output$mymap <- renderLeaflet({
    
    
    # df <-st_read(con, query = "SELECT geom, year, species, country, value, ST_asText(geom) AS geom_wkt, ST_area(geom) AS area FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326)) AND species IN ('SKJ') AND country IN ('EUESP','JPN','TWN') AND year IN ('2014') ORDER BY area DESC LIMIT 500;") %>% group_by(species,geom_wkt) %>% summarise(value = sum(value))
    # df <- metadata() %>% group_by(species,geom_wkt,area) %>% summarise(value = sum(value)) # %>% mutate(area=sf::st_area(st_as_sfc(geom_wkt)))  %>% filter(area>25)
    # df <- metadata() 
    # df <- data()  %>% group_by(species,geom_id) %>% summarise(value = sum(value))
    df <- metadata()
    # df <- st_read(con, query = query) %>% group_by(country,year,species,geom_wkt) %>% summarise(value = sum(value))  # %>% filter(species %in% input$species_i6i7i8)
    
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    
    # brewer.pal(7, "OrRd")
    # pal <- colorNumeric(palette = "YlGnBu",domain = df$value)
    # pal_fun <- colorQuantile("YlOrRd", NULL, n = 10)
    # qpal <- colorQuantile("RdYlBu",df$value, n = 10)
    qpal <- colorQuantile(rev(viridis::viridis(10)),df$value, n=10)
    # qpal <- brewer.pal(n = 20, name = "RdBu")
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    mymap <- leaflet() %>% 
      addProviderTiles("Esri.OceanBasemap") %>% 
      # setView(lng = lon_centroid, lat =lat_centroid, zoom = 3
      # ) %>%
      clearBounds() %>%
      addPolygons(data = df,
                  label = ~value,
                  popup = ~paste0("Captures de",species,": ", round(value), " tonnes(t) et des brouettes"),
                  # popup = ~paste0("Captures de",species,": ", area, " tonnes(t) et des brouettes"),
                  # fillColor = ~pal_fun(value),
                  # fillColor = brewer.pal(n = 20, name = "RdBu"),
                  fillColor = ~qpal(value),
                  # color = ~pal(value)
                  fill = TRUE,
                  fillOpacity = 0.8,
                  smoothFactor = 0.5) %>% 
      addDrawToolbar(
        targetGroup = "draw",
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      ) %>%
      addLayersControl(
        overlayGroups = c("draw"),
        options = layersControlOptions(collapsed = FALSE)
      )  %>% 
    addLegend("bottomright", pal = qpal, values = df$value,
              title = "Total catch per cell for selected criteria",
              labFormat = labelFormat(prefix = "MT "),
              opacity = 1
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
  
  
  output$plot_species<- renderPlot({ 
    df_i2 = st_read(con, query = paste0("SELECT species, count(species), sum(value) FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) GROUP BY species ORDER BY count;")) %>% filter (count>mean(count))
    
    # https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
    barplot(as.vector(as.integer(df_i2$count)),names.arg=df_i2$species, xlab="species",ylab="count",las = 2, cex.names = 1)
  })
  
  
  
  
  output$map_i11 <- renderLeaflet({
    # toto <- data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%
    # toto <- data() %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country))))) #  %>% filter (total>mean(total))
    
    
    # test_data$grp = sapply(st_equals(test_data), max)
    # toto <- data() %>% group_by(species,country,geom_id) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country)))))
    toto <- data_pie_map()
    
    # toto <- st_read(con, query = "SELECT geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt, ST_area(geom) AS area FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326)) AND species IN ('SKJ') AND country IN ('OMN','NAM','BRA','AGO','CPV','USA','JPN','MEX','BRB','EUPRT','UNK','ECU','SHN','MYS','MAR','COL','MDV') AND year IN ('2013','2014','2015','2016','2017','2018','2019') GROUP BY area,species, country,geom_wkt, geom ORDER BY area,species,country DESC ;")  %>% 
    #   spread(country, value, fill=0)  %>% mutate(total = sum(across(any_of(c('OMN','NAM','BRA','AGO','CPV','USA','JPN','MEX','BRB','EUPRT','UNK','ECU','SHN','MYS','MAR','COL','MDV')))))  %>% filter (total>mean(total))
    # %>% spread(country, value, fill=0)  %>% mutate(total = rowSums(across(any_of(as.vector(input$country)))))   %>% filter (total>mean(total))
    # toto <- df %>%  group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>% mutate(total = rowSums(across(any_of(default_flag))))
    # toto <- data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value) 
    # toto <- df %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value) 
    
    # centroid <-  st_convex_hull(st_union(toto)) %>%  st_centroid()
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    
    # colors2 <- c("#3093e5","#3000e5", "#fcba50"," #dd0e34", "#4e9c1e")
    # qpal <- colorQuantile(rev(viridis::viridis(length(unique(toto$country)))),unique(toto$country), n=length(unique(toto$country)))
    # pal_fun <- brewer.pal(n = 30, name = "Dark2")
    pal_fun <- colorQuantile("YlOrRd", NULL, n = 50)
    # pal_fun <- colorQuantile("YlOrRd", NULL, n = length(unique(input$country)))
    # cocolor<-factor(toto$Species, levels=as.vector(input$country), labels=rainbow_hcl(length(as.vector(input$country))))
    
    
    # new_zoom <- input$map_i11_zoom
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    map_i11 <-  leaflet() %>%  
      # map_i11 <-  leaflet(options = leafletOptions(zoomSnap=0.25)) %>%  
      setView(lng = lon_centroid, lat = lat_centroid, zoom = 3) %>% addProviderTiles("Esri.OceanBasemap", group = "background") %>%
      clearBounds() %>%
      addLayersControl(baseGroups = c("minicharts"), overlayGroups = c("background")) %>%
      addMinicharts(lng = st_coordinates(st_centroid(toto, crs = 4326))[, "X"],
                    lat = st_coordinates(st_centroid(toto, crs = 4326))[, "Y"],
                    # chartdata = as_data_frame(subset(toto, select = -c(species,geom_wkt))), type = "pie",
                    # chartdata = as_data_frame(toto)[-c(1:3,ncol(as_data_frame(toto)))], type = "pie",
                    chartdata = dplyr::select(toto,-c(species,total)) %>% st_drop_geometry(),type = "pie",
                    # showLabels = TRUE,
                    # layerId = "tartothon",
                    # colorPalette = pal_fun,
                    colorPalette = d3.schemeCategory10,
                    width = (60*toto$total/max(toto$total))+20,
                    legend = TRUE, legendPosition = "bottomright")
  })
  
  
  
  observe({
    new_zoom <- input$map_i11_zoom
    req(input$map_i11_zoom)
    if(zoom()!=new_zoom){
      zoom(new_zoom)
      #%>% setView(lng = lon_centroid, lat = lat_centroid, zoom = zoom()) %>%  addProviderTiles("Esri.OceanBasemap", group = "background") %>%  clearBounds() %>%
      map_i11_proxy = leafletProxy("map_i11") %>% clearMinicharts()  %>% 
        addMinicharts(lng = st_coordinates(st_centroid(data_pie_map(), crs = 4326))[, "X"],
                      lat = st_coordinates(st_centroid( data_pie_map(), crs = 4326))[, "Y"],
                      chartdata = dplyr::select(data_pie_map(),-c(species,total)) %>% st_drop_geometry(),type = "pie",
                      colorPalette = d3.schemeCategory10,
                      width = 10+(zoom()^2+200*(data_pie_map()$total/max(data_pie_map()$total))),
                      legend = TRUE, legendPosition = "bottomright")
      
      
    }
    
  })
  
  
  output$pie_map_i11 <- renderPlotly({
    # output$pie_map_i11 <- renderPlot({
    
    # df_i11_map <- data_i11() %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value)) # %>% top_n(3)
    # metadata_i11 <- data() %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value)) # %>% top_n(3)
    metadata_i11 <- data_pie_chart_country() %>% arrange(desc(value)) # %>% top_n(3)
    # df_i11_map <- as_data_frame(metadata_i11())  # %>% top_n(3)
    df_i11_map <- as_tibble(metadata_i11)  # %>% top_n(3)
    
    # # Basic piechart
    # i11_map <-   ggplot(df_i11_map, aes(x="", y=value, fill=country)) +
    #   geom_bar(stat="identity", width=1) +
    #   coord_polar("y", start=0) 
    # # +
    # #   theme(axis.text.x = element_text(angle = 90))
    # 
    # i11_map
    
    
    
    
    
    fig <- plot_ly(df_i11_map, labels = ~country, values = ~value, type = 'pie')
    fig <- fig %>% layout(title = 'Tuna catches by country for selected species, area and period of time',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  
  
  output$plot1_streamgraph <- renderPlot({
    
    # df_i1 = st_read(con, query = sql_query_metadata_plot1()) %>% group_by(species,year) %>% summarise(value = sum(value))  %>% arrange(desc(value)) %>% filter (value>mean(value)) # %>% top_n(3)
    # df_i1 = data() %>% group_by(species,year) %>% summarise(value = sum(value))  %>% arrange(desc(value))  %>% filter (value>mean(value)) # %>% top_n(3)
    df_i1 = data_time_serie() %>%  arrange(desc(value))  %>% filter (value>mean(value)) # %>% top_n(3)
    # df_i1 = st_read(con, query = paste0("SELECT species, year, count(species), sum(value) AS value FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) GROUP BY species,year ORDER BY count;")) %>% filter (count>mean(count))
    
    value=as.vector(as.integer(df_i1$value))
    g1 = ggplot(df_i1, aes(x = year, y = value, colour = species)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
      # geom_bar(position="stack", stat="identity") +
      ylab("Catches in Tons") + xlab("Date") + theme_bw() +
      scale_colour_manual(values=c(value)) +
      # scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "kT")}) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
            plot.margin = margin(5, 12, 5, 5))
    g1
    # 
  })
  
  
  output$plot11 <- renderImage({
    # https://semba-blog.netlify.app/06/13/2020/plots-in-interactive-maps-with-r/
    df_i11_filtered <- data()
    df_i11_filtered <- as(df_i11_filtered, "Spatial")
    
    i11 <- Atlas_i11_CatchesByCountry(df=df_i11_filtered,
                                      geomIdAttributeName="geom_id",
                                      countryAttributeName="country",
                                      speciesAttributeName="species",
                                      valueAttributeName="value",
                                      withSparql=FALSE)
    
    i11
    png(i11, width = 400, height = 300)
    dev.off()
    
    # Return a list containing the filename
    list(src = i11,
         contentType = 'image/png',
         width = 1600,
         height = 1200,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)




