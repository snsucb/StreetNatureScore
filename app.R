# Script to create a shiny leaflet app that queries a raster at the point of mouse-click
# To run in RStudio uncomment the  comments trailed by #UNCOMMENT FOR RSTUDIO
# To run in http://www.shinyapps.io/ - recomment those sections

# clean start - remove any objects in the enviroment
# rm(list = ls()) #UNCOMMENT FOR RSTUDIO

library(shiny)
library(ggmap)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras)

# spatial
library(raster)
library(sp)
library(rgdal)


#The image nlcd_compressed.tif is a compressed version of the NLCD 2011 file you can download from http://mlrc.com: nlcd_2011_impervious_2011_edition_2014_10_10.img
# It was compressed in QGIS using the RASTER Transform tool with the added option: -co COMPRESS=DEFLATE -co PREDICTOR=2
# The above was necessary to create a US-Wide raster of impervious surface that was small enough for a free RStudio shinystudio app (1GB limit)

isurf_file <- "nlcd_compressed.tif"
isurf<-raster(isurf_file)

isurf_crs <- as.character(isurf@crs)
leaflet_crs <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"
click_crs <-  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


ui <- fluidPage(
  fluidRow(
    column(4, br(), textInput("addressField", "Find your Nature Score:", placeholder = "Address or Location")), 
    column(3, br(), br(), actionButton("goButton", "Go", style='height:43px')),
    leafletOutput("map"),
    tags$script('
        $(document).on("keydown", function (e) {
                Shiny.onInputChange("lastkeypresscode", e.keyCode);
                });
                ')
  ))

server <- function(input, output, session) {
  dat <- reactiveValues(circs = data.frame(lng=numeric(), lat=numeric()))
  
  ## Make initial map
  output$map <- renderLeaflet({
      leaflet() %>%
      #setMaxBounds(-124.39, 25.82, -66.94, 49.38) %>%
      setView(lng = -122.2727, lat = 37.87159, zoom = 11) %>% # Berkeley, CA
      addTiles()
  })
  
  
  ## Observe mouse clicks on map
  observeEvent(input$map_click, {
    ## Get the click info like had been doing
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    address <- revgeocode(c(clng,clat))
    
    #--------------------------------------------------------------
    #project input coords to isurf CRS
    xy <- SpatialPoints(data.frame(clng,clat))
    proj4string(xy) <- click_crs
    xy <- as.data.frame(spTransform(xy, isurf_crs))
    
    imperviousSurfacePercent1 <- extract(isurf, matrix(c(xy$clng, xy$clat), ncol=2), buffer=100, fun=mean)
    imperviousSurfacePercent2 <- extract(isurf, matrix(c(xy$clng, xy$clat), ncol=2), buffer=1500, fun=mean)
    imperviousSurfacePercent3 <- extract(isurf, matrix(c(xy$clng, xy$clat), ncol=2), buffer=8000, fun=mean)
 
    imperviousSurfacePercent1 <- 100 - imperviousSurfacePercent1
    imperviousSurfacePercent2 <- 100 - imperviousSurfacePercent2
    imperviousSurfacePercent3 <- 100 - imperviousSurfacePercent3
    #--------------------------------------------------------------
    
    # don't show scores outside of the US
    if(is.na(imperviousSurfacePercent1))
      return()
    if(imperviousSurfacePercent1 == -27)
      return()
    # define new popup_text and delete old popups
    popup_text = paste0(address, "<br>Street Score: ", round(imperviousSurfacePercent1, 0), "%<br>Neighborhood Score: ", round(imperviousSurfacePercent2,0), "%<br>City Score: ", round(imperviousSurfacePercent3,0),'%' )
    leafletProxy('map') %>% clearPopups()
    
    leafletProxy('map') %>% # use the proxy to save computation
      addPopups(lng=clng, lat=clat, popup_text,
                options = popupOptions(closeButton = TRUE))
  })
  
  ## click on goButton
  observeEvent(input$goButton, {
    if(input$addressField != "") {
    
    typedAddress <- geocode(input$addressField)
    clat <- (typedAddress$lat)
    clng <- (typedAddress$lon)
    print(clng)
    print(clat)
    leafletProxy('map') %>% setView(clng, clat, zoom = 15)
    
    #--------------------------------------------------------------
    #project input coords to isurf CRS
    address <- revgeocode(c(clng,clat))
    xy <- SpatialPoints(data.frame(clng,clat))
    proj4string(xy) <- click_crs
    xy <- as.data.frame(spTransform(xy, isurf_crs))
    
    imperviousSurfacePercent1 <- extract(isurf, matrix(c(xy$clng, xy$clat), ncol=2), buffer=100, fun=mean)
    imperviousSurfacePercent2 <- extract(isurf, matrix(c(xy$clng, xy$clat), ncol=2), buffer=1500, fun=mean)
    imperviousSurfacePercent3 <- extract(isurf, matrix(c(xy$clng, xy$clat), ncol=2), buffer=8000, fun=mean)
    
    imperviousSurfacePercent1 <- 100 - imperviousSurfacePercent1
    imperviousSurfacePercent2 <- 100 - imperviousSurfacePercent2
    imperviousSurfacePercent3 <- 100 - imperviousSurfacePercent3
    #--------------------------------------------------------------
    
    # don't show scores outside of the US
    if(is.na(imperviousSurfacePercent1))
      return()
    if(imperviousSurfacePercent1 == -27)
      return()
    # define new popup_text and delete old popups
    popup_text = paste0(address, "<br>Street Score: ", round(imperviousSurfacePercent1, 0), "%<br>Neighborhood Score: ", round(imperviousSurfacePercent2,0), "%<br>City Score: ", round(imperviousSurfacePercent3,0),'%' )
    leafletProxy('map') %>% clearPopups()
    
    leafletProxy('map') %>% # use the proxy to save computation
      addPopups(lng=clng, lat=clat, popup_text,
                options = popupOptions(closeButton = TRUE))
    } 
  })
  
  observeEvent(input$lastkeypresscode, {
    
    if((input$lastkeypresscode == 13) && (input$addressField != "")){
    
    typedAddress <- geocode(input$addressField)
    clat <- (typedAddress$lat)
    clng <- (typedAddress$lon)
    print(clng)
    print(clat)
    leafletProxy('map') %>% setView(clng, clat, zoom = 15)
    
    #--------------------------------------------------------------
    #project input coords to isurf CRS
    address <- revgeocode(c(clng,clat))
    xy <- SpatialPoints(data.frame(clng,clat))
    proj4string(xy) <- click_crs
    xy <- as.data.frame(spTransform(xy, isurf_crs))
    
    imperviousSurfacePercent1 <- extract(isurf, matrix(c(xy$clng, xy$clat), ncol=2), buffer=100, fun=mean)
    imperviousSurfacePercent2 <- extract(isurf, matrix(c(xy$clng, xy$clat), ncol=2), buffer=1500, fun=mean)
    imperviousSurfacePercent3 <- extract(isurf, matrix(c(xy$clng, xy$clat), ncol=2), buffer=8000, fun=mean)
    
    imperviousSurfacePercent1 <- 100 - imperviousSurfacePercent1
    imperviousSurfacePercent2 <- 100 - imperviousSurfacePercent2
    imperviousSurfacePercent3 <- 100 - imperviousSurfacePercent3
    #--------------------------------------------------------------
    
    # don't show scores outside of the US
    if(is.na(imperviousSurfacePercent1))
      return()
    if(imperviousSurfacePercent1 == -27)
      return()
    # define new popup_text and delete old popups
    popup_text = paste0(address, "<br>Street Score: ", round(imperviousSurfacePercent1, 0), "%<br>Neighborhood Score: ", round(imperviousSurfacePercent2,0), "%<br>City Score: ", round(imperviousSurfacePercent3,0),'%' )
    leafletProxy('map') %>% clearPopups()
    
    leafletProxy('map') %>% # use the proxy to save computation
      addPopups(lng=clng, lat=clat, popup_text,
                options = popupOptions(closeButton = TRUE))
    } 
  })
  
}

shinyApp(ui=ui, server=server)