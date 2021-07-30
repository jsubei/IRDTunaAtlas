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
library(raster)
library(maps)
library(ggplot2)
library(XML)
library(viridis)
library(mapplots)
library(reshape)
library(tidyr)
library(streamgraph)

####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")
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

default_species <- 'SKJ'
default_year <- '2014'
default_flag <- c('EU.ESP','TWN','JPN')
# default_flag <- unique(target_flag)
# default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)

filters_combinations <- dbGetQuery(con, "SELECT species, year, country FROM  public.i6i7i8 GROUP BY species, year, country;")


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
                                    plotOutput(outputId = "plot_species")
                                    
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
                                        plotOutput("plot1_streamgraph", height=300, width="100%"),
                                        plotOutput("pie_map_i11", height = 200),
                                        # h6(textOutput("sars_clean_date_reactive"), align = "right"),
                                        # h6(textOutput("sars_reactive_country_count"), align = "right"),
                                        # plotOutput("sars_epi_curve", height="130px", width="100%"),
                                        # plotOutput("sars_cumulative_plot", height="130px", width="100%"),
                                        sliderInput("range", "Range:",
                                                    min = min(target_year), max = max(target_year),
                                                    value = c(min(target_year),max(target_year))
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
             # tabPanel("Interactive Indicator 11",
             #          # hr(),
             #          # textOutput("sql_query"),
             #          hr(),
             #          leafletOutput('map_i11', width = "60%", height = 1500)
             # ),
             tabPanel("Data explorer",
                      # hr(),
                      # textOutput("sql_query"),
                      hr(),
                      DT::dataTableOutput("DTi11")
             ),
             navbarMenu("More",
                        # tabPanel(
                        #   title = "Your SQL query",
                        #   textOutput("sql_query")
                        # ),
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
  
  
  
  sql_query <- eventReactive(input$submit, {
    paste0("SELECT  ogc_fid, geom_id, geom, year, species, country, value, count,ST_asText(geom) AS geom_wkt FROM public.i6i7i8 
                      WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326))
                      AND species IN ('",paste0(input$species,collapse="','"),"')
                      AND country IN ('",paste0(input$country,collapse="','"),"')
                      AND year IN ('",paste0(input$year,collapse="','"),"');")
  },
  ignoreNULL = FALSE)
  
  
  
  data <- eventReactive(input$submit, {
    # df <- as.data.frame(st_read(con, query = sql_query())) 
    st_read(con, query = sql_query())
  },
  ignoreNULL = FALSE)
  
  
  
  observeEvent(input$resetWkt, {900
    wkt(new_wkt)
  })
  
  
  change <- reactive({
    unlist(strsplit(paste(c(input$species,input$year,input$country),collapse="|"),"|",fixed=TRUE))
  })
  
  
  observeEvent(input$species,{
    temp <- filters_combinations %>% filter(species %in% change()[1])
    updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
    updateSelectInput(session,"country",choices = unique(temp$country),selected=unique(temp$country))
    
  }
  )
  
  
  output$sql_query <- renderText({ 
    paste("Your SQL Query is : \n", sql_query())
  })
  
  
  output$DTi11 <- renderDT({
    this <- as.data.frame(data())
  }) 
  
  
  
  
  
  
  output$mymap <- renderLeaflet({
    
    # df <- st_read(con, query = sql_query())  
    df <- data() %>% group_by(species,geom_wkt) %>% summarise(value = sum(value))  # %>% filter(species %in% input$species_i6i7i8)
    # df <- st_read(con, query = query)   %>% group_by(country,year,species,geom_wkt) %>% summarise(value = sum(value))  # %>% filter(species %in% input$species_i6i7i8)
    # df <- st_read(con, query = query)
    
    centroid <-  st_convex_hull(st_union(df))   %>%  st_centroid()
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
  
  

  
  output$plot1_streamgraph <- renderPlot({
    
    df_i1 = st_read(con, query = paste0("SELECT species, year, count(species), sum(value) AS value FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) GROUP BY species,year ORDER BY count;")) %>% filter (count>mean(count))
    # df_i1 = st_read(con, query = paste0("SELECT species, year, count(species), sum(value) FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",new_wkt,"',4326)) GROUP BY species,year ORDER BY count;")) %>% filter (count>mean(count))
    # df_i1=as_data_frame(df_i1)
    # streamgraph(df_i1, key="species",value="value", date="year", height="300px", width="1000px", offset="zero", interpolate="linear")
    value=as.vector(as.integer(df_i1$value))
    g1 = ggplot(df_i1, aes(x = df_i1$year, y = value, colour = df_i1$species)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
      # geom_bar(position="stack", stat="identity") +
      ylab("Catches in Tons") + xlab("Date") + theme_bw() +
      scale_colour_manual(values=c(value)) +
      # scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "kT")}) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
            plot.margin = margin(5, 12, 5, 5))
    g1
    # 
  })
  
  
  output$plot_species<- renderPlot({ 
    df_i2 = st_read(con, query = paste0("SELECT species, count(species), sum(value) FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) GROUP BY species ORDER BY count;")) %>% filter (count>mean(count))
    i1 <-  barplot(as.vector(as.integer(df_i2$count)),names.arg=df_i2$species, xlab="species",ylab="count")
    i1
  })
  
  
  
  
  output$map_i11 <- renderLeaflet({
    
    toto <- data() %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value) 
    # toto <- df %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value) 
    
    centroid <-  st_convex_hull(st_union(toto))   %>%  st_centroid()
    lat_centroid <- st_coordinates(centroid)[2]
    lon_centroid <- st_coordinates(centroid)[1]
    
    colors2 <- c("#3093e5","#3000e5", "#fcba50"," #dd0e34", "#4e9c1e")
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    map_leaflet <- leaflet()      %>%      
      setView(lng = lon_centroid, lat =lat_centroid, zoom = 3
      ) %>% addProviderTiles("Esri.OceanBasemap")   %>%  
      addMinicharts(lng = st_coordinates(st_centroid(toto))[, "X"],
                    lat = st_coordinates(st_centroid(toto))[, "Y"],
                    # chartdata = as_data_frame(subset(toto, select = -c(species,geom_wkt))), type = "pie",
                    chartdata =as_data_frame(toto)[,-c(1:3)], type = "pie",
                    # showLabels = TRUE,
                    # layerId = as.character(1:nrow(franconia)),
                    colorPalette = colors2,
                    legend = TRUE,legendPosition = "bottomright")
    # map_leaflet
  })
  
  output$pie_map_i11 <- renderPlot({
    
    df_i11_map <- data() %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value)) # %>% top_n(3)

        colors2 <- c("#3093e5","#3000e5", "#fcba50"," #dd0e34", "#4e9c1e")
        
        # Basic piechart
        i11_map <-   ggplot(df_i11_map, aes(x="", y=value, fill=country)) +
          geom_bar(stat="identity", width=1) +
          coord_polar("y", start=0)
        
        i11_map
        

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
  
  
  
  
  output$plot6 <- renderPlotly({
    
    
    df_i6i7 = data()  %>% filter(species %in% input$species)  %>% filter(year %in% input$year) 
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




