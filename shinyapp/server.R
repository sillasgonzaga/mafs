shinyServer(function(input, output) {
  
  fit <- reactive({
    forecast(
    apply_selected_model(AirPassengers, model_name = input$modelName, horizon = input$horizonLength),
    h = input$horizonLength)
    })

  output$fcast_plot <- renderPlot({
    plot(fit())
    })
  
  output$fcast_table <- renderTable(
    as.data.frame(fit())
    )
  
  output$acc_table <- renderTable(
    as.data.frame(accuracy(fit()))
  )
  
})