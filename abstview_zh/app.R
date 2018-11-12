library(shiny)
library(jsonlite)
library(tidyverse)
library(sf)
library(Cairo)

options(shiny.usecairo=T)

urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag")

datevote <- substr(urls$result$resources$download_url,38,47)

# urls$result$resources$download_url[1]

gemeinden<- sf::read_sf("GEN_A4_GEMEINDEN_SEEN_2018_F", stringsAsFactors = FALSE)

   
   # Sidebar with a slider input for number of bins 
   ui <- fluidPage(
     theme = "bootstrap.css",
     tags$header(list(tags$style("img {display:inline-block;background-repeat:no-repeat;position:relative;left:10px;z-index:3;}"),
                      tags$a(href="http://www.zh.ch", tags$img(src="lionwhite.png", height="90%"), target="_blank")),
                 tags$style("header {background-color: #009ee0 ;padding-top:10px;height:60px}")),
     # Application title
     titlePanel("OGD Demo"),
     "This shiny-App is a Demo nurtred by the real-time data service on popular votes of the canton Zurich.",
     br(),
     # Show a plot of the generated distribution
     fluidRow(
       column(4, selectizeInput('urlselect', 
                                label= 'Vote date',
                                choices = datevote, 
                                selected = "2016_09_25"))),
    
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot", width = "100%")
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output) {


data <- reactive({jsonlite::fromJSON(paste0( "http://www.wahlen.zh.ch/abstimmungen/",input$urlselect,"/viewer_download.php"))})

# transform nested list into dataframe
  datanew <- reactive({
    
              nd <- data() %>%
              map_dfr(bind_rows) %>%
              unnest(VORLAGEN) 
              
              nd <- nd %>% 
                mutate_at(vars(JA_STIMMEN_ABSOLUT,NEIN_STIMMEN_ABSOLUT,JA_PROZENT,STIMMBETEILIGUNG),as.numeric) %>% 
                group_by(BFS,VORLAGE_NAME) %>% 
                summarize(ja_anteil=round(sum(JA_STIMMEN_ABSOLUT,na.rm=T)/sum(JA_STIMMEN_ABSOLUT+NEIN_STIMMEN_ABSOLUT,na.rm=T)*100,1))
              
    
         inner_join(gemeinden,nd, by=c("BFS"))
    
            })

  
 output$plot <- renderPlot({

   p<-ggplot(datanew())+
       geom_sf(aes(fill=ja_anteil),color="white")+
       facet_wrap(~VORLAGE_NAME)+
       coord_sf(datum = NA)+
       labs(fill="Ja (in %)")+
       theme_void()+
       scale_fill_gradient2(midpoint=50)+
       guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))
   
   print(p)
    
  }, height = 800)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

