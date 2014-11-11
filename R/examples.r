
example.how.it.should.look.like = function() {
  library(shiny)
  
  
  app = shiny.app()
  set.app(app)
  
  # Main page
  ui = fluidPage(
    actionButton("textBtn", "text"),
    actionButton("plotBtn", "plot"),
    actionButton("uiBtn", "ui"),
    selectInput("varInput", "Variable:",
        c("Cylinders" = "cyl",
          "Transmission" = "am",
          "Gears" = "gear")
    ),
    textOutput("myText"),
    plotOutput("myPlot"),
    uiOutput('dynUI')
  )
  app$ui=ui
  
  # user changes value of an input
  add.change.handler("varInput",on.create=!TRUE, function(id, value,...) {
    updateText("myText",paste0(value," ", sample(1:1000,1)))
  })

  button.handlers = function(id, value, ...) {
    updateText("myText", paste0("Hello world :",id," ", value," ", sample(1:1000,1)))    
  }
  add.button.handler("textBtn", button.handlers)
  add.button.handler("plotBtn", button.handlers)


  
  num = 1
  # Dynamical UI that will be shown
  dynUI= fluidRow(
    actionButton("dynBtn", paste0("dynamic ", num))
  )
  
  
  
  # user presses a key
  #add.hotkey.handler("varInput", fun_name)

  run.app(app,launch.browser=rstudio::viewer)

}



example.how.it.should.look.like = function() {
  library(shiny)
  
  
  app = shiny.app()
  set.app(app)
  
  # Main page
  ui = fluidPage(
    actionButton("staticBtn", "static button"),
    uiOutput('dynUI')
  )
  app.main.ui(ui)
  
  # Dynamical UI that will be shown
  dynUI= fluidRow(
    actionButton("dynBtn", paste0("dynamic ", num)),
    selectInput("varInput", "Variable:",
        c("Cylinders" = "cyl",
          "Transmission" = "am",
          "Gears" = "gear")
    ),
    textOutput("varOutput"),
    plotOutput("myPlot")
  )
  
  # user clicks
  add.click.handler("dynBtn", function(...) {
    update.ui("dynUI", dynUI)  
  })
  
  
  
  # user changes value of an input
  add.change.handler("varInput",varInput.handler)
  
  varInput.handler = function(id, value,..., app=get.app()) {
    val = ui.value(id) # get value of an ui element
    if (!identical(val,value))
      stop("Values should be equal.")
    
    update.ui("varOutput", value)  
  }
  
  # user presses a key
  #add.hotkey.handler("varInput", fun_name)

  runApp(app,launch.browser=rstudio::viewer)

}

#example1.noevents()