button.image.click = function() {
  library(SeminarMatching)
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  restore.point.options(display.restore.point = FALSE)
  app = StudSeminarsApp(init.userid = "test", init.password="test", lang="de")
  viewApp(app)
}



setui.example = function() {

  library(shinyEventsUI)
  app = eventsApp()
  
  app$ui = fluidPage(
    nestedSelector(id = "rbg",
      selectors=list(
        section = list(
          choices=list("A"="A","B"="B"),
          contents=list("div1","div2")
        )
      )
    )$ui,
    hidden_div(id="div1",uiOutput("out"),uiOutput("out1"),"div1",actionButton("btn1",label="Click")),
    hidden_div(id="div2","div2",actionButton("btn",label="Click")),
    div(id="div3","div3",uiOutput("out3"))
    
  )
  buttonHandler("btn", function(...) {
    txt = as.character(runif(1))
    setUI("out3",txt)
    setUI("out1",txt) # does not work correctly, since out1 is inside a hidden div
    dsetUI("out",txt) # need dsetUI since out is inside a hidden div
    cat("\n setUI to ",txt)
  })
  buttonHandler("btn1", function(...) {
    cat("\n whatever... ")
  })

  viewApp(app)
}



bottom.script.example = function() {
  app = eventsApp()
  app$ui = fluidPage(
    bottomScript(HTML("alert('Hello World');")),
    p("I am a paragraph.")
  )
  moveBottomScripts(app$ui)
  viewApp(app)

}


gotcha.example = function() {
  library(shiny)
  # Create one button per person
  persons = list(
    peter=list(id=1,name="Peter"),
    paul=list(id=2,name="Paul")
  )
  btn.li = lapply(persons, function(person) {
    id = paste0("btn_",person$id)
    actionButton(id,label = person$name)
  })
  #names(btn.li) = NULL
  btn.row = do.call(fluidRow,btn.li)

  
  ui = fluidPage(
    title = 'Gotcha Example',
    btn.row,
    textOutput("mytext")
  )
  
  server = function(input, output, session) {
    output$mytext <- renderText({
      num = input$btn_1+input$btn_2
      txt = paste0("You pressed ", num, " times a button")
      txt
    })
  }
  runApp(list(ui=ui,server=server),launch.browser=rstudio::viewer) 
  
}

plot.example = function() {
  library(knitr)
  library(markdown)
  library(shiny)
  library(shinyEvents)
  ui = fluidPage(
    title = 'Plot Examples',
    plotOutput("myplot"),
    actionButton("btn","show")
  )
  server = function(input, output, session) {
    output$myplot <- renderPlot({
      plot(runif(100),runif(100))
    })
  }

  runApp(list(ui=ui,server=server),launch.browser=rstudio::viewer) 
  
  app = eventsApp()
  app$ui = fluidPage(
    title = 'Plot Examples',
    uiOutput("myui"),
    actionButton("btn","show")
  )
  setUI("myui", plotOutput("myplot"))
  setPlot("myplot",plot(runif(100),runif(100)))
  viewApp(app)

}


html.example = function() {
  library(knitr)
  library(markdown)
  library(shiny)

  stxt = 
    '```{r "static_chunk", eval=TRUE, collapse=TRUE,comment=NA}
    # I am a static chunk
    T = 10
    x = 1:T
    y = x+rnorm(T)

    summary(lm(y~x))


    ```'
  dtxt = 
    '```{r "dynamic_chunk", eval=TRUE, collapse=TRUE,comment=NA}
    # I am a dynamic chunk
    1:5
    "Hello"

    T = 10
    x = 1:T
    y = x+rnorm(T)

    summary(lm(y~x))

    ```'
  ui = fluidPage(
    title = 'Knitr Examples',
    HTML(knitr::knit2html(text=stxt)),
    uiOutput('ex1')
  )
  server = function(input, output, session) {
    output$ex1 <- renderUI({
      dhtml = knitr::knit2html(text=dtxt)
      dhtml = paste0(dhtml,
"\n<script>$('#ex1 pre code').each(function(i, e) {hljs.highlightBlock(e)});</script>")
      HTML(dhtml)
    })
  }

  runApp(list(
    ui=ui,
    server=server))  
}


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
    removeEventHandler(id="Btn1",eventId="buttonHandlerEvent")
    cat("\n finished press....")
  }

  buttonHandler("Btn0", press, level=0)
  buttonHandler("Btn1", press, level=1)
  buttonHandler("Btn2", press, level=2)

  viewApp(app,ui=main.ui)
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


change.handler.example = function() {
  library(shinyEvents)

  app = eventsApp(verbose=FALSE)

  # Main page
  app$ui = fluidPage(
    selectizeInput("varInput", "Variable:",
        c("Cylinders" = "cyl",
          "Transmission" = "am",
          "Gears" = "gear")
    ),
    radioButtons("rad",label="radio", choices=c("A","B","C")),
    textOutput("myText")
  )
  # handler for change of an input value
  selectChangeHandler("varInput",function(id, value,...) {
    cat("change handler called...")
    args = list(...)
    restore.point("changeHandler.inner")
    setText("myText",paste0("You chose the list item ", value,". ", 
                            "A random number: ", sample(1:1000,1)))
  })

  viewApp(app)
}




basic.shinyEvents.example = function() {
  library(shinyEvents)

  app = eventsApp(verbose=FALSE)

  # Main page
  ui = fluidPage(
    actionButton("textBtn", "text"),
    actionButton("plotBtn", "plot"),
    actionButton("uiBtn", "ui"),
    actionButton("handlerBtn", "handler for later"),
    actionButton("laterBtn", "later"),
    selectInput("varInput", "Variable:",
        c("Cylinders" = "cyl",
          "Transmission" = "am",
          "Gears" = "gear")
    ),
    textOutput("myText"),
    uiOutput('myUI'),
    plotOutput("myPlot")
  )
  setAppUI(ui)

  buttonHandler("textBtn", function(session, id, value, ...) {
    setText("myText", paste0("You pressed the button ",id," ", value," times. "))
  })
  
  buttonHandler("plotBtn", function(...) {
    setText("myText", "Show random plot...")
    setPlot("myPlot", plot(runif(10), runif(10)))    
  })

  # handler for change of an input value
  changeHandler("varInput",on.create=TRUE, function(id, value,...) {
    
    setText("myText",paste0("You chose the list item ", value,". ", 
                            "A random number: ", sample(1:1000,1)))
  })

  # A button handler that dynamically generates another handler
  buttonHandler("handlerBtn", function(value,...) {
    setText("myText", paste0("made handler ", value, " for later button."))
    buttonHandler("laterBtn", maker.value = value, function(maker.value,...) {
      setText("myText", paste0("Maker value: ", maker.value,
                               " Random number: ", sample(1:1000,1)))
    })
  })

  
  num = 1
  # Dynamically create UI with button and add handler for it
  buttonHandler("uiBtn", function(session, value,...) {
    
    # Set a new dynamic UI
    dynUI= fluidRow(
      actionButton("dynBtn", paste0("Created button ",value))
    )
    setUI("myUI", dynUI)
    
    # Add handlers for the new button in the UI.
    # Existing handlers for dynBtn are by default replaced
    buttonHandler("dynBtn", function(value,...) {
      setText("myText", paste0("Dynamic button pressed ", value, " times."))
    })
  })

  rapp = app
  rm(app)
  runEventsApp(rapp,launch.browser=rstudio::viewer)
}


chat.example = function() {
  library(shinyEvents)
  library(shinyAce)

  app = eventsApp()
  
  # app$glob can contain "global" variables that are visible
  # for all sessions.
  # app$glob$txt will be the content of the chat window
  app$glob$txt = "Conversation so far"
  
  app$ui = fluidPage(
    textInput("userName","User Name",""),
    
    # Chat window
    aceEditor("convAce",value = app$glob$txt, height="200px",showLineNumbers = FALSE, debounce=100),    
    
    # Enter new text
    aceEditor("enterAce",value = "Your text",height="30px",showLineNumbers = FALSE,debounce = 100,hotkeys = list(addTextKey="Ctrl-Enter")),
    
    actionButton("addBtn", "add")
  )

  addChatText = function(session,app,...) {
    restore.point("addChatText")
    user = getInputValue("userName")
    str = getInputValue("enterAce")
    app$glob$txt = paste0(app$glob$txt,"\n",user, ": ",paste0(str,collapse="\n"))
    updateAceEditor(session,"convAce", value = app$glob$txt)
    updateAceEditor(session,"enterAce", value = " ")
  }
  
  # Add chat text when button or Ctrl-Enter is pressed 
  buttonHandler(id="addBtn",addChatText)
  aceHotkeyHandler("addTextKey",addChatText)
  
  # refresh chat window each second
  timerHandler("refreshChatWindow",1000, function(session,app,...) {
    txt = getInputValue("convAce")
    if (!identical(txt, app$glob$txt)) {
      cat("Refresh chat window...")
      updateAceEditor(session, "convAce", value = app$glob$txt)
    }
  })
  

  # Initialize each new session with a random user name
  appInitHandler(function(session,app,...) {
    updateTextInput(session,"userName",
                    value=paste0("guest", sample.int(10000,1)) )
    updateAceEditor(session,editorId = "convAce",value = app$glob$txt)
  })


  runEventsApp(app, launch.browser=TRUE)
  # To test chat function, open several browser tabs
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

selectize.example = function() {
  library(shinyEvents)
  set.restore.point.options(display.restore.point = TRUE)

  app = eventsApp()

  li = as.list(1:5)
  names(li) = paste0("item:", 1:5)
  app$ui = fluidPage(
    selectizeInput("mult","Choose multiple", choices = li, selected=NULL, multiple=TRUE),
    textOutput("text")
  )
  changeHandler("mult", function(app, value,...) {
    restore.point("mult.changeHandler")
    #browser()
    print(value)
    val = getInputValue("mult")
    print(val)
    setText("text", paste0(value, collapse=","))
  })
  runEventsApp(app)
}


panel.no.update.example = function() {

  ui.left = fluidRow(
    textInput("textInput",label = "Text:",value = "Choice A"),
    actionButton("setBtn",label = "Set right")
  )
  ui.right = fluidRow(
    selectInput("selectInput",label = "Text:",choices = list())
  )
  
  ui = fluidPage(title = "PanelTest",
    tabsetPanel(id="panels",
      tabPanel("Left",ui.left,value="leftTab"),
      tabPanel("Right",uiOutput("rightUI"),value="rightTab")
    )                  
  )
  server = function(session, input, output,...) {
    output$rightUI <- renderUI({
      cat("Render rightUI")
      ui.right
    })
    observeEvent(input$setBtn, {
      cat("Btn was pressed.")
      txt = isolate(input$textInput)
      updateSelectInput(session,inputId = "selectInput",choices = as.list(txt))
    })
  }
  runApp(list(ui=ui,server=server), launch.browser=rstudio::viewer)

}


