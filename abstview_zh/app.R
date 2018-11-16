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
library(ggplot2)
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
                   
                   tabPanel("Abstimmungstermin", 
                            fluidRow(
                              column(4, selectizeInput('urlselect', 
                                                       label= 'Abstimmungstermin auswählen',
                                                       choices = datevote, 
                                                       selected = "2016_09_25"))),
                            
                            plotOutput("histogram", width = "100%"),
                            DT::dataTableOutput("table")
                            
                            ),
    
     tabPanel("Karte", 
              
     br(),
     # Show a plot of the generated distribution
     fluidRow(
       column(4, selectInput('topicselect', 
                                label= 'Vorlage auswählen',""))),
    
      # Show a plot of the generated distribution
      mainPanel(
        leaflet::leafletOutput("mapview")
        

      )
   ),
   tabPanel("Über", 
            
            mainPanel(
            h3("Open Data : Abstimmungsresultate via Echtzeit-Webservice"),
            "Diese Shiny-App ist auf dem Echtzeit Abstimmungsdaten-Webservice des Kantons Zürich aufgebaut. Sie wird im Vorfeld von Abstimmungsterminen automatisch via opendata.swiss DCAT Action API aktualisiert.",
            br(),
            a("https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag")
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
                dplyr::group_by(BFS,VORLAGE_NAME,VORLAGE_ID) %>% 
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
 
 
 
 
  output$histogram <- renderPlot(ggplot(datanew()) + 
                              geom_histogram(aes(x=ja_anteil),fill="steelblue")+
                              facet_wrap(~VORLAGE_NAME)+
                              theme_minimal()+
                              geom_vline(xintercept=50)+
                              scale_x_continuous(limits = c(0, 100),breaks=seq(0,100,10))+
                              annotate("rect", xmin=50, xmax=100, ymin=0, ymax=Inf,fill="steelblue",alpha=0.3)+
                              annotate("rect", xmin=0, xmax=50, ymin=0, ymax=Inf,fill="coral",alpha=0.3)+
                              annotate("text", x=90,y=70,label = "bold(JA)", colour = "white",parse = TRUE,size = 10)+
                              annotate("text", x=10,y=70,label = "bold(NEIN)", colour = "white",parse = TRUE,size = 10)+
                              labs(x="Ja-Anteil (%)",y="Anzahl Gemeinden"), 
                              width = "auto", height = "auto", res = 72)
  
 

  output$table <- DT::renderDataTable(DT::datatable({
                         
                        datanew() %>% 
                        dplyr::mutate(Status=ifelse(ja_anteil>0, "ausgezählt","nicht ausgezählt")) %>% 
                        dplyr::group_by(VORLAGE_NAME,Status) %>% 
                        dplyr::summarize(Gebiete=n()) %>% 
                        sf::st_set_geometry(NULL) %>% 
                        dplyr::rename(Vorlage=VORLAGE_NAME)

      
                         }, options = list(paging = FALSE))
                )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

