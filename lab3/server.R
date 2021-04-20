library(shiny)
library(ggplot2)
library(data.table)
library(dplyr)

shinyServer(function(input, output) {
  DT <- reactive({
    DT <- data.table(mtcars)
    if (input$Cond == "am == 1") {
      #mechanic
      DT <- DT[DT$am == 1, ]
    } else if (input$Cond == "am == 0") {
      #automatic
      DT <- DT[DT$am == 0, ]
    } else if (input$Cond == "vs == 0") {
      #v-shaped
      DT <- DT[DT$vs == 0, ]
    } else if (input$Cond == "vs == 1") {
      #straight
      DT <- DT[DT$vs == 1, ]
    }
    select(DT, input$Y.var, input$X.var)
  })
  
  output$gplot <- renderPlot({
    gp <- ggplot(data = DT(), aes_string(x = input$X.var,
                                         y = input$Y.var))
    gp <- gp + geom_point() + geom_smooth(method = "lm")
    gp
  })
  output$XY.summary <- renderPrint({
    summary(DT())
  })
  output$lm.result <- renderPrint({
    eq <- as.formula(paste(input$Y.var, ' ~ ', input$X.var))
    mod <- lm(eq, data = DT())
    summary(mod)
  })
})
