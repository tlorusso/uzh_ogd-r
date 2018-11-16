# shiny app
# voting results canton zurich

library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(shiny)
library(shinythemes)
library(DT)
library(tmap)
library(curl)

urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag")

datevote <- substr(urls$result$resources$download_url,38,47)

# urls$result$resources$download_url[1]

gemeinden<- sf::read_sf("GEN_A4_GEMEINDEN_SEEN_2018_F", stringsAsFactors = FALSE) %>% select(BFS)

   
   # Sidebar with a slider input for number of bins 
ui <-   fluidPage(
  tags$header(tags$style(".navbar-header { font-family: Arial Black;}"),
              tags$style(" .h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 { font-family: Arial Black;}")),
  navbarPage(theme = shinytheme("cerulean"),
                   title= "ZH Vote", 
                   
                   tabPanel("Vote date", 
                            fluidRow(
                              column(4, selectizeInput('urlselect', 
                                                       label= 'Vote date',
                                                       choices = datevote, 
                                                       selected = "2016_09_25"))),
                            
                            DT::dataTableOutput("table")
                            
                            ),
    
     tabPanel("Map", 
              
     # Application titl
     "This shiny-App is a Demo nurtred by the real-time data service on popular votes of the canton Zurich.",
     br(),
     # Show a plot of the generated distribution
     fluidRow(
       column(4, selectInput('topicselect', 
                                label= 'select votation',""))),
    
      # Show a plot of the generated distribution
      mainPanel(
        # plotOutput("plot", width = "100%")
        leaflet::leafletOutput("mapview")
        

      )
   )))


# Define server logic required to draw a histogram
server <- function(input, output,session) {

  
  data <- reactive({jsonlite::fromJSON(paste0( "http://www.wahlen.zh.ch/abstimmungen/",input$urlselect,"/viewer_download.php"))})

# transform nested list into dataframe
  datanew <- reactive({
    
              nd <- data() %>%
              purrr::map_dfr(bind_rows) %>%
              tidyr::unnest(VORLAGEN) 
              
              nd <- nd %>% 
                dplyr::mutate_at(vars(JA_STIMMEN_ABSOLUT,NEIN_STIMMEN_ABSOLUT,JA_PROZENT,STIMMBETEILIGUNG),as.numeric) %>% 
                dplyr::group_by(BFS,VORLAGE_NAME) %>% 
                dplyr::summarize(ja_anteil=round(sum(JA_STIMMEN_ABSOLUT,na.rm=T)/sum(JA_STIMMEN_ABSOLUT+NEIN_STIMMEN_ABSOLUT,na.rm=T)*100,1))
              
    
              dplyr::inner_join(gemeinden,nd, by=c("BFS"))
    
            })
  
  # data for the leaflet map, filtered by the selected votation
mapdata <- reactive({
  
    datanew() %>% dplyr::filter(VORLAGE_NAME==input$topicselect)
    
  })
  
  
# votations on the selected vote
 vorlagen <- reactive({ unique(datanew()$VORLAGE_NAME) })
 

observe({
   updateSelectInput(session, "topicselect",
                     choices = vorlagen())})

 
# 
 output$mapview <-  renderLeaflet({


   # m1 <-mapview::mapview(mapdata(), zcol = "ja_anteil", at = seq(0, 100, 5), legend = TRUE)
   # 
   # m1@map
   
   tm <- tm_basemap(leaflet::providers$Stamen.TerrainBackground) +
     tm_shape(mapdata()) +
     tm_polygons(col = "ja_anteil", palette = "-Blues") +
     tm_tiles(leaflet::providers$Stamen.TonerLabels, group = "Labels")
 
 tmap_leaflet(tm)


  })

  output$table <- DT::renderDataTable(DT::datatable({
                         
                        datanew() %>% 
                        dplyr::mutate(Auszaehlstand=ifelse(ja_anteil>0, "ausgezaehlt","nicht ausgezaehlt")) %>% 
                        dplyr::group_by(VORLAGE_NAME,Auszaehlstand) %>% 
                        dplyr::summarize(Gebiete=n()) %>% 
                        sf::st_set_geometry(NULL) %>% 
                        dplyr::rename(Vorlage=VORLAGE_NAME)

      
                         }, options = list(paging = FALSE))
                )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

