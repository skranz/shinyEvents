
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
  setUI("ui2",ui2)
  setUI("ui1",ui1)

  press = function(id, level,session,...) {
    restore.point("press", dots=NULL)
    txt = paste0(id, " ", sample(1:1000,1))
    cat("before setText")
    setText(paste0("Text",level),txt)
    cat("after setText")
    removeEventHandler("Btn1")
    cat("\n finished press....")
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
  session=NULL

  ui = fluidPage(
    aceEditor("myEdit",value = "Lorris ipsum",
              hotkeys = list(runLine="Ctrl-Enter")),
    actionButton("myBtn", "Press..."),
    textOutput("myText")
  )


  buttonHandler("myBtn", user.name="Sebastian",
    function(id,session,user.name,...) {
      updateAceEditor(session, "myEdit", value = paste0("Lorris ipsum", sample(1:1000,1), " ", user.name))
      setText("myText","I pressed a button...")
    }
  )

  aceHotkeyHandler("runLine", custom.var = "Omega",function(text,...) {
    cat("Hotkey handler:\n")
    print(list(...))
    print(text)
  })

  # I can set outputs before the app is started to set
  # initial values.
  setText("myText","This is the start text...")

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
    setText("myText", paste0("handler Button ", sample(1:1000,1)))

    buttonHandler("laterBtn", function(...) {
      cat("buttonHandler laterBtn")
      setText("myText", paste0("now we rock!! ", sample(1:1000,1)))
    })
  })

  # user changes value of an input
  changeHandler("varInput",on.create=!TRUE, function(id, value,...) {
    setText("myText",paste0(value," ", sample(1:1000,1)))
  })


  text.button.handlers = function(id, value, ...) {
    setText("myText", paste0("Hello world :",id," ", value," ", sample(1:1000,1)))
  }
  plot.button.handlers = function(id, value, ...) {
    setText("myText", paste0("Hello world :",id," ", value," ",
                sample(1:1000,1)))
    library(ggplot2)
    #p = qplot(mpg, wt, data=mtcars)
    #setPlot("myPlot", p)
    setPlot("myPlot", plot(runif(10)))
  }

  num = 1
  # Dynamical UI that will be shown
  dynUI= fluidRow(
    actionButton("dynBtn", paste0("dynamic ", num)),
    actionButton("waitBtn", paste0("wait ", num))
  )


  buttonHandler("textBtn", text.button.handlers)
  buttonHandler("plotBtn", plot.button.handlers)
  buttonHandler("uiBtn", function(session,...) {
    setUI("myUI", dynUI)
  })
  buttonHandler("dynBtn", function(session,...) {
    setText("myText", paste0("dynamic ", sample(1:1000,1)))

    buttonHandler("waitBtn", function(session,...) {
      setText("myText", paste0("now we rock!! ", sample(1:1000,1)))
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
    setTextInput("userName",value=paste0("guest", sample.int(10000,1)) )
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
    user = getInputValue("userName")
    str = getInputValue("enterAce")
    app$glob$txt = paste0(app$glob$txt,"\n",user, ": ",paste0(str,collapse="\n"))
    #updateAceEditor(session, "enterAce", value = "")
    updateAceEditor(session, "convAce", value = app$glob$txt)
  }
  
  buttonHandler(id="addBtn",addChatText)
  aceHotkeyHandler("addTextKey",addChatText)
  buttonHandler(id="refreshBtn", function(session,app,...) {
    updateAceEditor(session, "convAce", value = app$glob$txt)
  })
  timerHandler("refreshChatWindow",1000, function(session,app,...) {
    txt = getInputValue("convAce")
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
    textInput("name","Name:",paste0("guest")),
    actionButton("btn","Click me"),
    textOutput("out")
  )

  buttonHandler(NULL,"btn", function(...) {
    txt = paste0("name = ", getInputValue("name"), " id = ", app$id,"  ", sample.int(10000,1))
    setText("out",txt)
  })
  runEventsApp(app,ui=ui)
}

