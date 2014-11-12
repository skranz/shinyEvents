
example.how.it.should.look.like = function() {
  library(shiny)
  
  
  app = eventsApp(set.app=TRUE)
  
  # Main page
  ui = fluidPage(
    actionButton("textBtn", "text"),
    actionButton("plotBtn", "plot"),
    actionButton("uiBtn", "ui"),
    actionButton("handlerBtn", "make handler"),
    actionButton("laterBtn", "later"),    
    selectInput("varInput", "Variable:",
        c("Cylinders" = "cyl",
          "Transmission" = "am",
          "Gears" = "gear")
    ),
    textOutput("myText"),
    plotOutput("myPlot"),
    uiOutput('myUI')
  )
  setAppUI(ui)
  
  addButtonHandler("handlerBtn", function(...) {
    updateText("myText", paste0("handler Button ", sample(1:1000,1)))
    
    cat("addButtonHandler laterBtn...")
    addButtonHandler("laterBtn", function(...) {
      cat("addButtonHandler laterBtn")
      updateText("myText", paste0("now we rock!! ", sample(1:1000,1)))
    })
  })

  # user changes value of an input
  addChangeHandler("varInput",on.create=!TRUE, function(id, value,...) {
    updateText("myText",paste0(value," ", sample(1:1000,1)))
  })

  
  text.button.handlers = function(id, value, ...) {
    updateText("myText", paste0("Hello world :",id," ", value," ", sample(1:1000,1)))    
  }
  plot.button.handlers = function(id, value, ...) {
    updateText("myText", paste0("Hello world :",id," ", value," ",
                sample(1:1000,1)))
    library(ggplot2)
    #p = qplot(mpg, wt, data=mtcars)
    #updatePlot("myPlot", p)
    updatePlot("myPlot", plot(runif(10)))
  }
  
  num = 1
  # Dynamical UI that will be shown
  dynUI= fluidRow(
    actionButton("dynBtn", paste0("dynamic ", num)),
    actionButton("waitBtn", paste0("wait ", num))
  )
  
  
  addButtonHandler("textBtn", text.button.handlers)
  addButtonHandler("plotBtn", plot.button.handlers)
  addButtonHandler("uiBtn", function(...) {
    updateUI("myUI", dynUI)
  })
  addButtonHandler("dynBtn", function(...) {
    updateText("myText", paste0("dynamic ", sample(1:1000,1)))
    
    addButtonHandler("waitBtn", function(...) {
      updateText("myText", paste0("now we rock!! ", sample(1:1000,1)))
    })
  })

  # user presses a key
  #add.hotkey.handler("varInput", fun_name)

  runEventsApp(app,launch.browser=rstudio::viewer)

}


