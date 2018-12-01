# A test to create a dashboard

library(shinydashboard)
library(shinyWidgets)
library(ggplot2)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Test Dashboard"),
  dashboardSidebar(radioButtons("visualType", "Visual Type:", c("Scatter Plot", "Line Plot", "Box Plot"))),
  dashboardBody(
    
    fluidRow(
      
      tags$style(".nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
     background-color: transparent;
     border-color: transparent;
}

.nav-tabs-custom .nav-tabs li.active {
     border-top-color: red;
}"),
      chooseSliderSkin("Shiny", color = "red"),
      
      tabBox(
        title = "Refined PM Plots",
        width = 12,
        id = "tabset1",
        tabPanel(
          "PM 1",
          plotOutput("plot1"),
          sliderInput("slider11", "Start:", 1, 7381, 1, width = 2000),
          sliderInput("slider12", "End:", 1, 7381, 7381, width = 2000)
        ),
        tabPanel(
          "PM 2.5",
          plotOutput("plot2"),
          sliderInput("slider251", "Start:", 1, 7381, 1, width = 2000),
          sliderInput("slider252", "End:", 1, 7381, 7381, width = 2000)
        ),
        tabPanel(
          "PM 10",
          plotOutput("plot3"),
          sliderInput("slider101", "Start:", 1, 7381, 1, width = 2000),
          sliderInput("slider102", "End:", 1, 7381, 7381, width = 2000)
        )
      )
    )
    
  )
)

server <- function(input, output) {
  
  homeData <- read.csv("Purple_air_Grober Home (33.87271449552452 -84.24118335671386) Primary 08_09_2018 08_16_2018.csv", header = TRUE, sep = ",")
  timedata <- as.POSIXct(strptime(homeData$created_at, "%Y-%m-%d %H:%M:%S UTC"))
  PM1data <- homeData$PM1.0_CF_ATM_ug.m3
  PM2.5data <- homeData$PM2.5_CF_ATM_ug.m3
  PM10data <- homeData$PM10.0_CF_ATM_ug.m3
  
  dataSets <- list(PM1data, PM2.5data, PM10data)
  rem <- vector(mode='numeric', length=0)
  
  for (set in dataSets) {
    setstats <- summary(set)
    setIQR <- IQR(set)
    
    for (i in 1:length(set)) {
      if ((set[i] < setstats[2] - 1.5*(setIQR)) || (set[i] > setstats[5] + 1.5*(setIQR))) {
        if (!(i %in% rem)) {
          rem <- c(rem, i)
        }
      }
    }
  }
  
  print(rem)
  
  PM1data <- PM1data[-rem]
  PM2.5data <- PM2.5data[-rem]
  PM10data <- PM10data[-rem]
  
  output$plot1 <- renderPlot({
    PM1 <- PM1data[seq(input$slider11, input$slider12)]
    time <- timedata[seq(input$slider11, input$slider12)]
    Data <- paste("Data from", toString(input$slider11), "to",toString(input$slider12), sep=" ")
    if (input$visualType == "Scatter Plot") {
      qplot(time, PM1) + theme_minimal() 
    } else if (input$visualType == "Line Plot") {
      qplot(time, PM1, geom="line") + theme_minimal()
    } else if (input$visualType == "Box Plot") {
      qplot(Data, PM1, geom=c("boxplot")) + theme_minimal()
    }else {
      qplot(time, PM1) + theme_minimal()
    }
  })
  
  output$plot2 <- renderPlot({
    PM2.5 <- PM2.5data[seq(input$slider251, input$slider252)]
    time2 <- timedata[seq(input$slider251, input$slider252)]
    Data <- paste("Data from", toString(input$slider11), "to",toString(input$slider12), sep=" ")
    if (input$visualType == "Scatter Plot") {
      qplot(time2, PM2.5) + theme_minimal() 
    } else if (input$visualType == "Line Plot") {
      qplot(time2, PM2.5, geom="line") + theme_minimal()
    } else if (input$visualType == "Box Plot") {
      qplot(Data, PM2.5, geom=c("boxplot")) + theme_minimal()
    }else {
      qplot(time2, PM2.5) + theme_minimal()
    }
  })
  
  output$plot3 <- renderPlot({
    PM10 <- PM10data[seq(input$slider101, input$slider102)]
    time3 <- timedata[seq(input$slider101, input$slider102)]
    Data <- paste("Data from", toString(input$slider11), "to",toString(input$slider12), sep=" ")
    if (input$visualType == "Scatter Plot") {
      qplot(time3, PM10) + theme_minimal() 
    } else if (input$visualType == "Line Plot") {
      qplot(time3, PM10, geom="line") + theme_minimal()
    } else if (input$visualType == "Box Plot") {
      qplot(Data, PM10, geom=c("boxplot")) + theme_minimal()
    }else {
      qplot(time3, PM10) + theme_minimal()
    }
  })
  
}

shinyApp(ui, server)
