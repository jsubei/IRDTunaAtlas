library(shiny)
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

####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i7_SpeciesMapRelativeCatches.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i8_SpeciesMapRelativeCatchesOtherSpecies.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_database_to_NetCDF.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/wkt2spdf.R")
####################################################################################################################################################################################################################################
DRV=RPostgres::Postgres()
source(file = "~/Desktop/CODES/IRDTunaAtlas/credentials.R")
####################################################################################################################################################################################################################################
new_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(new_wkt) 
# target_species<- c("YFT","BFT")
# target_year <- c(seq(1:10)+1994)
# target_flag <- c("331","747")

# query_i6i7i8 =  paste0("SELECT ogc_fid, geom_id, geom, year, species, country, value, count FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",new_wkt,"',4326)) AND species IN ('",paste0(target_species,collapse="','"),"') AND country IN ('",paste0(target_flag,collapse="','"),"') AND year IN ('",paste0(target_year,collapse="','"),"');")
query_i6i7i8 =  paste0("SELECT ogc_fid, geom_id, geom, year, species, country, value, count FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",new_wkt,"',4326));")
df_i6i7i8 = st_read(con, query = query_i6i7i8)

# species_i8 = unique(df_i6i7i8$year)
# year_i8 = unique(df_i6i7i8$species)


ui <- fluidPage(
    titlePanel("test Tuna"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "species_i6i7i8",
                label = "Species",
                choices = unique(df_i6i7i8$species),
                selected="YFT"
            ),
            selectInput(
                inputId = "year_i8",
                label = "Year",
                choices = unique(df_i6i7i8$year),
                selected="2015"
            ),
            selectInput(
                inputId = "flag_i8",
                label = "Flag",
                choices = unique(df_i6i7i8$country),
                selected="322"
            ),
            selectInput(
              inputId = "species_i8",
              label = "Additionnal species (only required for i8)",
              choices = unique(df_i6i7i8$species),
              selected=c("YFT","BET","SKJ"),
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
                title = "Map data",
                leafletOutput('mymap', width = "50%", height = 600)
                # leafletOutput('mymap')
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
                title = "Browse data",
                tableOutput("tbl")
            ),
            tabPanel(
                title = "Your filters",
                textOutput("selected_var")
            )
        )
    )
)


server <- function(input, output, session) {
    
    output$selected_var <- renderText({ 
        paste("You have selected", input$species_i6i7i8, "and", wkt())
    })
    
    data <- eventReactive(input$submit, {
        conn <-con
        query <- paste0("SELECT year, species, country, value, ST_asText(geom) AS geom_wkt FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) AND species IN ('",paste0(input$species_i6i7i8,collapse="','"),"') AND country IN ('",paste0(input$flag_i8,collapse="','"),"') AND year IN ('",paste0(input$year_i8,collapse="','"),"');")
        # query <- paste0("SELECT species, country AS flag, value AS v_catch, ST_asText(geom) AS geom_wkt, (year || '-01-01') AS time_start, (year || '-12-31') AS time_end FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) AND species IN ('",paste0(input$species_i8,collapse="','"),"') AND country IN ('",paste0(input$flag_i8,collapse="','"),"') AND year IN ('",paste0(input$year_i8,collapse="','"),"');")
        # df <- st_read(con, query = query) %>% mutate(time_start = ISOdate(year, 1, 1), time_end = ISOdate(year, 12,31)) %>% dplyr::select (-c(ogc_fid, geom_id,count,year)) %>%  dplyr::rename(v_catch=value,flag=country,geom_wkt=geom)
        df <- st_read(con, query = query) %>% mutate(time_start = ISOdate(year, 1, 1), time_end = ISOdate(year, 12,31)) %>% dplyr::select (-c(year)) %>%  dplyr::rename(v_catch=value,flag=country)
    }, ignoreNULL = FALSE)
        
     
    output$tbl <- renderTable({

        this <- data()

        # on.exit(dbDisconnect(conn), add = TRUE)
        
    })
    

    output$mymap <- renderLeaflet({
        df <- data()
        netCDF_file <- TunaAtlas_database_to_NetCDF(df)
        centroid <- st_union(st_as_sfc(df$geom_wkt)) %>% st_convex_hull()   %>% st_centroid()
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
        
        
      #Filtering input dataset with selected parameters
      if(wkt()!=new_wkt){
            # df_i6i7 = st_read(con,query =  paste0("SELECT ogc_fid, geom_id, geom, year, species, country, value, count FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) AND species IN ('",paste0(input$species_i8,collapse="','"),"') AND country IN ('",paste0(input$flag_i8,collapse="','"),"') AND year IN ('",paste0(input$year_i8,collapse="','"),"');"))
            df_i6i7=df_i6i7i8 %>% filter(st_within(geom,st_as_sfc(wkt(), crs = 4326)) %>% lengths > 0, year %in% input$year_i8, species %in% input$species_i6i7i8, country %in% input$flag_i8 ) 
        }else{
          df_i6i7=df_i6i7i8 %>% filter(year %in% input$year_i8, species %in% input$species_i6i7i8,country %in% input$flag_i8) 
        }
        
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
      
      if(wkt()!=new_wkt){
        
        df_i6i7 = st_read(con, 
                          query =  paste0("SELECT ogc_fid, geom_id, geom, year, species, country, value, count FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) AND species IN ('",paste0(input$species_i6i7i8,collapse="','"),"') AND country IN ('",paste0(input$flag_i8,collapse="','"),"') AND year IN ('",paste0(input$year_i8,collapse="','"),"');")
        )
      }else{
        df_i6i7=df_i6i7i8 %>% filter(year %in% input$year_i8, species %in% input$species_i6i7i8,country %in% input$flag_i8) 
      }
      
      df_i6_filtered <- as(df_i6i7, "Spatial")
      
      withProgress(message = 'Making plot i6', value = 0, {
      i6 <- Atlas_i6_SpeciesMap(df=df_i6_filtered,
                                geomIdAttributeName="geom_id",
                                yearAttributeName="year",
                                speciesAttributeName="species",
                                valueAttributeName="value",
                                withSparql=FALSE
      )
      })
      
    })
    

    output$plot8 <- renderPlotly({

        if(wkt()!=new_wkt){
            df_i8=df_i6i7i8 %>% filter(st_within(geom,st_as_sfc(wkt(), crs = 4326)) %>% lengths > 0, year %in% input$year_i8, species %in% input$species_i8, country %in% input$flag_i8 ) 
        }else{
            df_i8=df_i6i7i8 %>% filter(year %in% input$year_i8, species %in% input$species_i8,country %in% input$flag_i8) 
        }

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

    
    
    
    
    observeEvent(input$resetWkt, {
        wkt(new_wkt)
        
    })

}


# Run the application 
shinyApp(ui = ui, server = server)




