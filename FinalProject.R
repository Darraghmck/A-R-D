#install.packages(c('dbConnect', 'RMySQL', 'SixSigma','shinythemes', 'DBI', 'RMySQL', 'dplyr', 'plotly', 'shinydashboard', 'curl', 'TTR', 'quantmod', 'DT'))
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(DBI)
library(SixSigma)
library(dbConnect)
library(RMySQL)
library(plotly)
library(quantmod)
library(DT)
library(ggplot2)
ui <- dashboardPage(
  dashboardHeader(title = "A-R-D"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Homepage", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("CO Comparisons", icon = icon("th"), 
               menuSubItem("CO Range", tabName = "subitem1"),
               menuSubItem("CO Trends", tabName = "subitem2"),
               menuSubItem("CO Histograms", tabName = "subitem3"),
               menuSubItem("CO Boxplots", tabName = "subitem4")
          ),
      #will only work when downloading from web browser not in R Studio window inherent issue with Shiny
      menuItem("Download Data CSV", icon = icon("download"),
               downloadButton("downloadData", "Download"))
      )
  ),
  dashboardBody(
    tabItems( 
      
      tabItem(tabName = "dashboard",
              fluidRow(
                helpText("Use the search bar to navigate data, search by room name, date, time and room ID."),
                helpText("Click the column header to sort a column."),
                DT::dataTableOutput("mytable3")
              )
      ),
      tabItem(tabName = "subitem1",
              fluidRow(
                h1(strong("Comparisons Between Carbon Monoxide Levels in Different Rooms"),hr(),
                   plotlyOutput("hist"),
                   h2(strong("legend")),
                   column(3,p(tags$em(strong("1 = Bedroom")))), column(3,p(tags$em(strong("2 = Living Room")))), column(3,p(tags$em(strong("3 = Shed")))), column(3,p(tags$em(strong("4 = kitchen"))))
                )
              )
      ),
      tabItem(tabName="subitem2",
              fluidRow(
                h1(strong("Comparisons Between Carbon Monoxide Levels in Different Rooms"),hr(),
                   plotOutput("Trend")
                )
              )
      ),
      tabItem(tabName="subitem3",
              fluidRow(
                h1(strong("Comparisons Between Carbon Monoxide Levels in Different Rooms"),hr(),
                   plotlyOutput("hist1")
                )
              )
      ),
      tabItem(tabName="subitem4",
              fluidRow(
                h1(strong("Comparisons Between Carbon Monoxide Levels in Different Rooms"),hr(),
                   plotlyOutput("box")
                ),
                fluidRow(
                  column(4,h2("bedroom"),verbatimTextOutput("summary1")),
                  column(4,h2("kitchen"),verbatimTextOutput("summary2")),
                  column(4,h2("living room"),verbatimTextOutput("summary3")),
                  column(4,h2("shed"),verbatimTextOutput("summary4"))
                )
              )
      ))
    
  )
)

server <- function(input, output) {
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "readings",
    host = "**.**.**.**",
    username = "****",
    password = "***********")
  on.exit(dbDisconnect(conn), add = TRUE)
  suppressWarnings(arduino <- dbGetQuery(conn, paste0("SELECT * FROM readings;")))
  arduino$room_id <- as.factor(arduino$room_id)
  arduino$room <- as.factor(arduino$room)
  #arduino$time <- strptime(arduino$time, format="%Y-%m-%d %H:%M:%S", tz='GMT')
  shed <- arduino$reading[grep(3, arduino$room_id)]
  liv <- arduino$reading[grep(2, arduino$room_id)]
  bed <- arduino$reading[grep(1, arduino$room_id)]
  kit <- arduino$reading[grep(4, arduino$room_id)]
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(arduino, options = list(lengthMenu = c(50, 100, 500), pageLength = 50))
  })
  #arduino$t <- strftime(arduino$time, format="%H:%M:%S")
  output$Trend <- renderPlot({ggplot(arduino, aes(x=id, y=reading)) +    
      geom_point(shape=1) + 
      facet_wrap( ~ room) + 
      scale_x_discrete(drop=FALSE) + 
      geom_smooth(method=lm , color="red", se=TRUE) + 
      theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())})
  output$read <- renderDataTable(arduino)
  
  killDbConnections <- function () {
    
    all_cons <- dbListConnections(MySQL())
    
    print(all_cons)
    
    for(con in all_cons)
      +  dbDisconnect(con)
    
    print(paste(length(all_cons), " connections killed."))
    
  }
  output$summary1 <- renderPrint({
    summary(bed)
  })
  output$summary2 <- renderPrint({
    summary(kit)
  })
  output$summary3 <- renderPrint({
    summary(liv)
  })
  output$summary4 <- renderPrint({
    summary(shed)
  })
  ax <- list(
    title = ""
  )
  output$hist <- renderPlotly({plot_ly(arduino, x = ~room_id, y = ~reading, type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~reading, mode = 'lines+markers') %>%
      add_trace(y = ~reading, mode = 'markers')%>%
      layout(showlegend = FALSE)})
  output$hist1 <- renderPlotly({plot_ly(alpha = 0.6) %>%
      add_histogram(x = ~bed, name = "bedroom" ) %>%
      add_histogram(x = ~liv, name = "Living Room") %>%
      add_histogram(x = ~shed, name = "Shed") %>%
      add_histogram(x = ~kit, name = "Kitchen") %>%
      layout(barmode = "overlay", xaxis = ax)})
  output$box <- renderPlotly({plot_ly(arduino, y = ~reading, color = ~room, type = "box")})
  output$downloadData <- downloadHandler(
    
    filename = function() { paste("readings", '.csv', sep = '') },
    
    content = function(file) {
      
      write.csv(arduino, file, row.names = FALSE)
      
    }
    
  )
  
  outputOptions(output, 'downloadData', suspendWhenHidden=FALSE)
}

shinyApp(ui, server = server)