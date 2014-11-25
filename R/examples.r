
nested.ui.example = function() {
  library(shinyEvents)
  library(shinyAce)
  
  app = eventsApp()

  
  main.ui = fluidPage(
    actionButton("Btn0", "Main Button"),
    textOutput("Text0"),
    uiOutput("ui1")
  )
  ui1 = fluidRow(
    actionButton("Btn1", "Button 1"),
    textOutput("Text1"),
    uiOutput("ui2")
  )
  ui2 = fluidRow(
    actionButton("Btn2", "Button 2"),
    textOutput("Text2"),
    uiOutput("ui3")
  )
  updateUI("ui2",ui2)
  updateUI("ui1",ui1)
 
  press = function(id, level,...) {
    txt = paste0(id, " ", sample(1:1000,1))
    updateText(paste0("Text",level),txt)
  }
  
  buttonHandler("Btn0", press, level=0)
  buttonHandler("Btn1", press, level=1)
  buttonHandler("Btn2", press, level=2)

  
  runEventsApp(app,ui=main.ui)  
}


hotkey.shiny.events.example = function() {
  library(shinyEvents)
  library(shinyAce)
  
  app = eventsApp()

  
  ui = fluidPage(
    aceEditor("myEdit",value = "Lorris ipsum",
              hotkeys = list(runLine="Ctrl-Enter")),
    actionButton("myBtn", "Press..."),
    textOutput("myText")
  )
  
  
  buttonHandler("myBtn", user.name="Sebastian",
    function(id,session,user.name,...) {
      updateAceEditor(session, "myEdit", value = paste0("Lorris ipsum", sample(1:1000,1), " ", user.name))
      updateText("myText","I pressed a button...")
    }
  )
  
  aceHotkeyHandler("runLine", custom.var = "Omega",function(text,...) {
    cat("Hotkey handler:\n")
    print(list(...))
    print(text)
  })
  
  # I can update outputs before the app is started to set
  # initial values.
  updateText("myText","This is the start text...")
  
  runEventsApp(app,ui=ui)  
}


basic.shinyEvents.example = function() {
  library(shiny)
  
  
  app = eventsApp()
  
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
  
  buttonHandler("handlerBtn", function(...) {
    updateText("myText", paste0("handler Button ", sample(1:1000,1)))
    
    buttonHandler("laterBtn", function(...) {
      cat("buttonHandler laterBtn")
      updateText("myText", paste0("now we rock!! ", sample(1:1000,1)))
    })
  })

  # user changes value of an input
  changeHandler("varInput",on.create=!TRUE, function(id, value,...) {
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
  
  
  buttonHandler("textBtn", text.button.handlers)
  buttonHandler("plotBtn", plot.button.handlers)
  buttonHandler("uiBtn", function(...) {
    updateUI("myUI", dynUI)
  })
  buttonHandler("dynBtn", function(...) {
    updateText("myText", paste0("dynamic ", sample(1:1000,1)))
    
    buttonHandler("waitBtn", function(...) {
      updateText("myText", paste0("now we rock!! ", sample(1:1000,1)))
    })
  })

  # user presses a key
  #add.hotkey.handler("varInput", fun_name)

  runEventsApp(app,launch.browser=rstudio::viewer)

}
