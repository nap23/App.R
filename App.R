data("LakeHuron")
dat <- data.frame(Year = as.numeric(time(LakeHuron)), 
                  Elevation = as.numeric(LakeHuron))

uiHuron <- shinyUI(fluidPage(
  
  titlePanel("Water level of Lake Huron"),
  
  sidebarPanel(
    sliderInput(
      ### <b> 
      inputId = "Aruba",
      ### </b>
       label = "Span (loess):", 
       min = .1, max = 1, value = .3, step = .1),
    sliderInput(
      ### <b> 
      inputId = "Jamaica",
      ### </b>
       label = "K (GAM):",
       min = 1, max = 50, value = 5, step = 1)
  ),
  
  mainPanel(
    plotOutput("lPlot")
  )
))

serverHuron <- shinyServer(function(input, output) {
  
  LHmod <- reactive({ gam(Elevation ~ s(Year, bs = "cr", ### <b> 
 k = input$Jamaica), 
### </b> 
                          data = dat) })
    pred <- reactive({ predict(LHmod()) })
  
  output$lPlot <- renderPlot({
    ggplot(data = dat, aes(x = Year, y = Elevation)) +
      geom_line(aes(color = "black"), size = 1) +
      geom_smooth(method = "loess", aes(color = "blue"), ### <b>
 span = input$Aruba,
 ### </b>
                  fill = NA, size = 1) +
      geom_line(aes(y = pred(), color = "red"), size = 1) +
      ggtitle("Measurements of the level of Lake Huron") +
      labs(y = "Elevation (ft)") +
      scale_colour_manual(name = "", values = c("black", "blue", "red"),
                          labels = c("Lake level", "Loess fit", "Spline fit")) +
      theme(legend.position = "top")
  })
})

shinyApp(ui = uiHuron, server = serverHuron)
