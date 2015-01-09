
nested.ui.example = function() {
  library(shinyEvents)
  library(shinyAce)
  library(restorepoint)
  set.restore.point.options(display.restore.point = TRUE)

  app = eventsApp()

  session=NULL

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
  updateUI(session,"ui2",ui2)
  updateUI(session,"ui1",ui1)

  press = function(id, level,session,...) {
    restore.point("press", dots=NULL)
    txt = paste0(id, " ", sample(1:1000,1))
    cat("before updateText")
    updateText(session,paste0("Text",level),txt)
    cat("after updateText")
    removeEventHandler(session,"Btn1")
    cat("\n finished press....")
  }

  buttonHandler(session,"Btn0", press, level=0)
  buttonHandler(session,"Btn1", press, level=1)
  buttonHandler(session,"Btn2", press, level=2)

  runEventsApp(app,ui=main.ui)
}


hotkey.shiny.events.example = function() {
  library(shinyEvents)
  library(shinyAce)

  app = eventsApp()
  session=NULL

  ui = fluidPage(
    aceEditor("myEdit",value = "Lorris ipsum",
              hotkeys = list(runLine="Ctrl-Enter")),
    actionButton("myBtn", "Press..."),
    textOutput("myText")
  )


  buttonHandler(session,"myBtn", user.name="Sebastian",
    function(id,session,user.name,...) {
      updateAceEditor(session, "myEdit", value = paste0("Lorris ipsum", sample(1:1000,1), " ", user.name))
      updateText(session,"myText","I pressed a button...")
    }
  )

  aceHotkeyHandler(session,"runLine", custom.var = "Omega",function(text,...) {
    cat("Hotkey handler:\n")
    print(list(...))
    print(text)
  })

  # I can update outputs before the app is started to set
  # initial values.
  updateText(session,"myText","This is the start text...")

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

  buttonHandler(session,"handlerBtn", function(...) {
    updateText(session,"myText", paste0("handler Button ", sample(1:1000,1)))

    buttonHandler(session,"laterBtn", function(...) {
      cat("buttonHandler laterBtn")
      updateText(session,"myText", paste0("now we rock!! ", sample(1:1000,1)))
    })
  })

  # user changes value of an input
  changeHandler(session,"varInput",on.create=!TRUE, function(id, value,...) {
    updateText(session,"myText",paste0(value," ", sample(1:1000,1)))
  })


  text.button.handlers = function(id, value, ...) {
    updateText(session,"myText", paste0("Hello world :",id," ", value," ", sample(1:1000,1)))
  }
  plot.button.handlers = function(id, value, ...) {
    updateText(session,"myText", paste0("Hello world :",id," ", value," ",
                sample(1:1000,1)))
    library(ggplot2)
    #p = qplot(mpg, wt, data=mtcars)
    #updatePlot("myPlot", p)
    updatePlot(session,"myPlot", plot(runif(10)))
  }

  num = 1
  # Dynamical UI that will be shown
  dynUI= fluidRow(
    actionButton("dynBtn", paste0("dynamic ", num)),
    actionButton("waitBtn", paste0("wait ", num))
  )


  buttonHandler(session,"textBtn", text.button.handlers)
  buttonHandler(session,"plotBtn", plot.button.handlers)
  buttonHandler(session,"uiBtn", function(session,...) {
    updateUI(session,"myUI", dynUI)
  })
  buttonHandler(session,"dynBtn", function(session,...) {
    updateText(session,"myText", paste0("dynamic ", sample(1:1000,1)))

    buttonHandler(session,"waitBtn", function(session,...) {
      updateText(session,"myText", paste0("now we rock!! ", sample(1:1000,1)))
    })
  })

  # user presses a key
  #add.hotkey.handler("varInput", fun_name)

  runEventsApp(app,launch.browser=rstudio::viewer)
}


chat.example = function() {
  library(shinyEvents)
  library(shinyAce)

  app = eventsApp()
  app$glob$txt = "Conversation so far"
  app$initHandler = function(session,...) {
    updateTextInput(session,"userName",value=paste0("guest", sample.int(10000,1)) )
    updateAceEditor(session,editorId = "convAce",value = app$glob$txt)
  }
  ui = fluidPage(
    textInput("userName","User Name",""),
    aceEditor("convAce",value = app$glob$txt, height="200px",showLineNumbers = FALSE, debounce=100),    
    aceEditor("enterAce",value = "Your text",height="30px",showLineNumbers = FALSE,debounce = 100,hotkeys = list(addTextKey="Ctrl-Enter")),
    fluidRow(
      actionButton("addBtn", "add"),
      actionButton("refreshBtn", "refresh")
    )
  )

  addChatText = function(session,app,...) {
    restore.point("addChatText")
    user = isolate(session$input$userName)
    str = isolate(session$input$enterAce)
    app$glob$txt = paste0(app$glob$txt,"\n",user, ": ",paste0(str,collapse="\n"))
    #updateAceEditor(session, "enterAce", value = "")
    updateAceEditor(session, "convAce", value = app$glob$txt)
  }
  
  buttonHandler(NULL,id="addBtn",addChatText)
  aceHotkeyHandler(NULL,"addTextKey",addChatText)
  buttonHandler(NULL,id="refreshBtn", function(session,app,...) {
    updateAceEditor(session, "convAce", value = app$glob$txt)
  })
  timerHandler(NULL,"refreshChatWindow",1000, function(session,app,...) {
    txt = isolate(session$input$convAce)
    if (!identical(txt, app$glob$txt)) {
      cat("Refresh chat window...")
      updateAceEditor(session, "convAce", value = app$glob$txt)
    }
  })
  app$handlers[["refreshChatWindow"]]
  runEventsApp(app,ui=ui)
}


find.current.app.example = function() {
  library(shinyEvents)
  library(shinyAce)

  app = eventsApp()
  app$glob$txt = "Conversation so far"
  app$id = 0
  app$glob$id = 0
  app$initHandler = function(session,app,...) {
    app$glob$id = app$glob$id+1
    app$id = app$glob$id
  }
  ui = fluidPage(
    actionButton("btn","Click me"),
    textOutput("out")
  )

  buttonHandler(NULL,"btn", function(...) {
    session = getCurrentSession()
    app = getApp(session)
    txt = paste0("id = ", app$id,"  ", sample.int(10000,1))
    updateText(session,"out",txt)
  })
  runEventsApp(app,ui=ui)
}

