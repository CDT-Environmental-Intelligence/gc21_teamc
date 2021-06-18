library('shiny')
library('shinydashboard')
library('magrittr')
library('rvest')
library('leaflet')
library('rgdal')
library('geojsonio')
library('rmapshaper')
library('maps')
library('sf')
library('sp')
library('dplyr')
library('readr')
library('raster')

#read in spatial data
la <- geojsonio::geojson_read("C:/Users/nedwe/Documents/GC files/Local_Authority_Districts__April_2019__UK_BUC_v2.geojson", what = "sp")


#read in unemployment data
unemployment <- read.csv("C:/Users/nedwe/Documents/GC files/nomis_2021_06_15_174156.csv")


#join the unemployment data to the spatial data via the LA codes
merged_unemployment <- merge(la, unemployment, by.x="LAD19CD", by.y="la.code")


#Create colour palette to represent unemployment rates
pal <- colorNumeric(
  palette = "Blues",
  domain = merged_unemployment$number)


#read in renewables data
renewables <- read.csv("C:/Users/nedwe/Documents/GC files/renewable-energy-planning-database-q1-march-2021.csv")

#read in lat long data (I manually converted the XY coordinates using a free batch converter tool)
latlong_convert <- read.csv("C:/Users/nedwe/Documents/GC files/lat_long_convert.csv")

#join thes above together
renewables_latlong <- left_join(renewables, latlong_convert,
                                by = c("X.coordinate" = "first"))

#filter new dataframe by development status
renewables_ppguc <- renewables_latlong %>% filter(
  Development.Status %in% c("Planning Permission Granted", "Under Construction")
)

#save map
mp <- leaflet() %>%
  setView(lng = -2.5, lat = 54.5, zoom = 6) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(data = merged_unemployment,
              color = ~pal(number),
              fillOpacity = 1,
              weight  = 1,
              popup = ~LAD19NM)


#add markers to map (Something is wrong with this code, it is only showing default red markers)
getColor <- function(renewables_ppguc) {
  sapply(renewables_ppguc$Development.Status, function(Development.Status) {
    if(Development.Status == "Under Construction") {
      "green"
    } else if(Development.Status == "Planning Permission Granted") {
      "orange"
    }
  })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(renewables_ppguc)
)

#create final map
final_map <- mp %>% addAwesomeMarkers(data = renewables_ppguc, lng = ~Long, lat = ~Lat, icon=icons, popup = ~Site.Name)





#attempt an r shiny app

#UI
ui <- fluidPage(
  leafletOutput("final_map", height = 1000),
)

#server
server <- function(input, output, session) {
  
  output$final_map <- renderLeaflet({
    mp <- leaflet() %>%
      setView(lng = -2.5, lat = 54.5, zoom = 6) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = merged_unemployment,
                  color = ~pal(number),
                  fillOpacity = 1,
                  weight  = 1,
                  popup = ~LAD19NM)
    
  
    
    getColor <- function(renewables_ppguc) {
      sapply(renewables_ppguc$Development.Status, function(Development.Status) {
        if(Development.Status == "Under Construction") {
          "green"
        } else if(Development.Status == "Planning Permission Granted") {
          "orange"
        }
      })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(renewables_ppguc)
    )
    
    
    
    final_map <- mp %>% addAwesomeMarkers(data = renewables_ppguc, lng = ~Long, lat = ~Lat, icon=icons)
    final_map
  })
}

#run the app
shinyApp(ui, server)


