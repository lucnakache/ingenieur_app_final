# User interface
source("packages.R")
source("global.R")


ui <- fluidPage(
  titlePanel("Demo of leaflet.minicharts"),
  p("test leaflet.mincharts r package"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(inputId ="year",
                  label = "year",
                  min = 1801,
                  max = 1810,
                  value = 1801,
                  step = 1,
                  animate = animationOptions(interval = 5000,
                                             loop = FALSE)),
      verbatimTextOutput("click_value")
    ),
    
    mainPanel(
      leafletOutput("map")
    )
    
  )
)