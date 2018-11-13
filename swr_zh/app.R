# shiny app
# Compare Social Welfare Rates

library(shiny)
library(ggplot2)
library(devtools)
library(tidyr)
library(RColorBrewer)
library(DT)
library(plotly)
library(statR)
library(shinythemes)


# dataset on opendata.swiss : https://opendata.swiss/de/dataset/sozialhilfequote
swr_data <- read.csv("http://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_541.csv", 
                     header = TRUE, sep = ";", quote="\"", dec=".", encoding="UTF-8") %>% 
              dplyr::mutate_at(vars(INDIKATOR_VALUE,INDIKATOR_JAHR), as.character) %>% 
              dplyr::mutate_at(vars(INDIKATOR_VALUE,INDIKATOR_JAHR), as.numeric)

names(swr_data)[1]<-"BFS_NR"


districts <- as.character(unique(swr_data$GEBIET_NAME))

zhseq13 <- colorRampPalette((zhpal$zhlake[1:7]),space="Lab")(13)[c(seq(1,13,2),seq(0,12,2))]

# Define UI for application 
ui <- fluidPage(
  tags$header(tags$style(".navbar-header { font-family: Arial Black;}"),
              tags$style(" .h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 { font-family: Arial Black;}")),
  navbarPage(theme = shinytheme("cerulean"),
             title=div(img(src="lionwhitemini.png"), "Social Walfare Rates"), 
    tabPanel("Plot", sidebarLayout(
        sidebarPanel(
            h4("Choose Districts, Regions or Municipalities"),
            selectizeInput('e7', label= 'Choose areas to be compared',choices = districts, 
                           multiple = TRUE, selected = "Zürich - ganzer Kanton", options= list(maxItems = 13)),
            hr(),
            includeMarkdown("about.Rmd")),
        mainPanel(h4("Social Welfare Rates, Canton of Zurich"),
                  plotOutput("SWRplot")))),
        tabPanel("Table", titlePanel("Data Table"),
                 # Create a new Row in the UI for selectInputs
                 fluidRow(
                     column(4,
                            selectInput("districtinput4",
                                        "District:",
                                        c("All",
                                          unique(as.character(swr_data$GEBIET_NAME))))),
                     column(4,
                            selectInput("yearinput",
                                        "Year:",
                                        c("All",
                                          unique(as.character(swr_data$INDIKATOR_JAHR)))))),
                 # Create a new row for the table.
                 fluidRow(
                     DT::dataTableOutput("SWRtable"))
        ),
    tabPanel(p(icon("Info"), "About"),
        includeMarkdown("about.Rmd"))
        ))

# Define a server for the Shiny app
server <- function(input, output, session) {
    print(str(swr_data))
    # plot
  
    output$SWRplot <- renderPlot({
        ggplot(data = subset(swr_data, GEBIET_NAME %in% input$e7), 
               aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE, 
                   group = GEBIET_NAME, colour = GEBIET_NAME)) +
            geom_line() +
            geom_point() +
            xlab("Year") +
            ylab("Social welfare rate in %") +
            scale_colour_manual(values=zhseq13) +
            scale_y_continuous(limits=c(0,6))+
            theme_stat() +
            theme(legend.title=element_blank())
    })
    # table
    output$SWRtable <- DT::renderDataTable(DT::datatable({
        dta <- swr_data
        if (input$districtinput4 != "All") {
            dta <- dta[dta$GEBIET_NAME == input$districtinput4,]
        }
        if (input$yearinput != "All") {
            dta <- dta[dta$INDIKATOR_JAHR == input$yearinput,]
        }
        dta
    }, options = list(paging = FALSE))
    )
}


shinyApp(ui = ui, server = server)
