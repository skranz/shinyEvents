rtutor.example = function() {
  restore.point.options(display.restore.point = TRUE)
  library(RTutor3)
  setwd("D:/libraries/RTutor3/")
  file = "test.Rmd"
  ps = create.ps(file=file)
  app = rtutorApp(ps)
  viewApp(app,launch.browser = rstudio::viewer)

}

form.example = function() {
  app = eventsApp()
  app$ui = bootstrapPage(
    p(id="myp","My page"),
    textInput("mytext",label = "Text",value = "Start text"),
    textInput("mytext2",label = "Text",value = "Start text2"),
    
    actionButton("btn","Click me", "data-form-selector"=ids2sel(c("mytext","mytext2")))
  )
  buttonHandler("btn", function(...) {
    args = list(...)
    restore.point("btn.click")
    print(args$formValues)
    cat("button was clicked")
    
  })
  viewApp(app)
}



callJS.example = function() {
  app = eventsApp()
  app$ui = bootstrapPage(
    p(id="myp","My page"),
    actionButton("btn","Click me")
  )
  buttonHandler("btn", function(...) {
    appendToHTML("Extra!" ,"#myp")
    callJS('$("#myp").css',list("font-size" = 20, "color" = "blue"))
    callJS("alert","Hi!")
  })
  viewApp(app)
}

svg.show.example = function() {
  svg = '
<svg width="420" height="300" id="ps_panequiz_5_test" class="clickable_svg">
<defs>
      <filter x="0" y="0" width="1" height="1" id="label_box">
        <feFlood flood-color="white" flood-opacity="0.85"></feFlood>
        <feComposite in="SourceGraphic"></feComposite>
      </filter>
    <marker id="arrow_head" class="arrow_head" markerWidth="10" markerHeight="10" refX="0" refY="3" orient="auto" markerUnits="strokeWidth">
      <path d="M0,0 L0,6 L9,3 z" style="fill: black;"></path>
    </marker>
    
</defs>
<polyline points="60,174 400,174" id="test_curve_xcurve_1" class="curve" style="fill: none; stroke: #ff0000" visibility="visible" display="yes">
<title></title></polyline>
<polyline points="60,64 400,64" id="test_curve_xcurve_2" class="curve" style="fill: none; stroke: #ff7777" visibility="hidden">
<title></title></polyline>
<g id="xaxis" class="axis x-axis">
<line x1="60" x2="410" y1="250" y2="250" class="axis-main"></line>
<line x1="60" x2="60" y1="250" y2="260" class="axis-tick"></line>
<line x1="128" x2="128" y1="250" y2="260" class="axis-tick"></line>
<line x1="196" x2="196" y1="250" y2="260" class="axis-tick"></line>
<line x1="264" x2="264" y1="250" y2="260" class="axis-tick"></line>
<line x1="332" x2="332" y1="250" y2="260" class="axis-tick"></line>
<line x1="400" x2="400" y1="250" y2="260" class="axis-tick"></line>
<text x="60" y="275" class="axis-ticklabel" text-anchor="middle">0</text>
<text x="128" y="275" class="axis-ticklabel" text-anchor="middle">20</text>
<text x="196" y="275" class="axis-ticklabel" text-anchor="middle">40</text>
<text x="264" y="275" class="axis-ticklabel" text-anchor="middle">60</text>
<text x="332" y="275" class="axis-ticklabel" text-anchor="middle">80</text>
<text x="400" y="275" class="axis-ticklabel" text-anchor="middle">100</text></g>
<g id="yaxis" class="axis y-axis">
<line x1="50" x2="50" y1="240" y2="20" class="axis-main"></line>
<line x1="40" x2="50" y1="240" y2="240" class="axis-tick"></line>
<line x1="40" x2="50" y1="196" y2="196" class="axis-tick"></line>
<line x1="40" x2="50" y1="152" y2="152" class="axis-tick"></line>
<line x1="40" x2="50" y1="108" y2="108" class="axis-tick"></line>
<line x1="40" x2="50" y1="64" y2="64" class="axis-tick"></line>
<line x1="40" x2="50" y1="20" y2="20" class="axis-tick"></line>
<text x="37" y="240" class="axis-ticklabel" text-anchor="end" alignment-baseline="middle">0</text>
<text x="37" y="196" class="axis-ticklabel" text-anchor="end" alignment-baseline="middle">20</text>
<text x="37" y="152" class="axis-ticklabel" text-anchor="end" alignment-baseline="middle">40</text>
<text x="37" y="108" class="axis-ticklabel" text-anchor="end" alignment-baseline="middle">60</text>
<text x="37" y="64" class="axis-ticklabel" text-anchor="end" alignment-baseline="middle">80</text>
<text x="37" y="20" class="axis-ticklabel" text-anchor="end" alignment-baseline="middle">100</text></g>
<text x="60" y="174" id="geomlabel_test_curve_xcurve_1" class="boxed-label" text-anchor="start" visibility="visible" display="yes">
<title></title>
x1</text>
<text x="417" y="64" id="geomlabel_test_curve_xcurve_2" class="boxed-label" text-anchor="end" visibility="hidden" display="none">
<title></title>
x2</text>
</svg>  
  '
app = eventsApp()
app$ui = fluidPage(
  actionButton("btn","Go"),
  actionButton("btn1","Show"),
  uiOutput("out") 
)
buttonHandler("btn", function(...) {
  html = HTML(svg)
  setUI("out",html)
  svgClickHandler(id=NULL, fun=function(...) {
    args = list(...)
    restore.point("svgclick")
    cat("\nSVG has been clicked!")
  })
  setHtmlAttribute("test_curve_xcurve_2",list(display="yes"))
})
buttonHandler("btn1", function(...) {
  cat("make visible...")
  setHtmlAttribute("test_curve_xcurve_2",list(visibility="visible", display="yes"))
})
  
viewApp(app)
}

ace.code.completion.example = function() {
  devtools::install_github("skranz/shinyAce")
  
  library(shinyAce)
  app = eventsApp()
  df = data.frame(colx=1:10,cola=2:11,colb=5)
  var.env = as.environment(list(df=df))
  
  app$ui = fluidPage(
    tags$script(HTML(autocomp.js())),
    aceEditor("edit",mode = "r",autoComplete = "live",debounce = 10)
  )
  
  
  
  appInitHandler(function(session,...) {
    acob = shiny::observe({
      inputId = "edit"
      #value = getInputValue(paste0("shinyAce_", inputId, "_hint"))
      value <- session$input[[paste0("shinyAce_", inputId, "_hint")]]
      restore.point("ac.observer")
      cat("\nstart observer", round(runif(1)*100))
      if(is.null(value)) return(NULL)
      str = substring(value$linebuffer,1,value$cursorPosition)
      words = NULL
      arg.words =  autcomp.function.args(str)
      var.words = autocomp.vars(str,var.env=var.env)
      col.words = autocomp.cols(str,var.env=var.env)
      meta = c(rep("arg",length(arg.words)),rep("var",length(var.words)),rep("col",length(col.words)))
      score = c(rep(103,length(arg.words)),rep(102,length(var.words)),rep(101,length(col.words)))      
      words = c(arg.words,var.words,col.words)
      df = data.frame(name=words,value=words,meta=meta, score=score)
      set.autocomplete.list(inputId,df=df)

      
    })
    
    #acob$resume()
  })
  viewApp(app)
}

auth.example = function() {
  app = eventsApp(need.authentication = TRUE)
  app$ui = fluidPage(
    actionButton("authBtn","Authenticate me"),
    actionButton("btn1","Press me only after authentication..."),
    selectInput("sel","Dont change before authentication",choices = 1:10),
    selectInput("sel2","Change me whenever you like",choices = 1:10),
    uiOutput("out")
  )
  buttonHandler("authBtn",no.authentication.required = TRUE,function(...,app=getApp()) {
    app$is.authenticated = TRUE
    setUI("out","You are authenticated...")
  })
  buttonHandler("btn1",function(...,app=getApp()) {
    setUI("out","Yep the button was pressed...")
  })
  changeHandler("sel",function(...){})
  changeHandler("sel2",no.authentication.required = TRUE,function(...){})
  viewApp(app)  
  
}

image.click.example = function() {
  library(EconCurves)
  setwd("D:/libraries/EconCurves/")
  filename="test.png"
  res = plot.png.with.coordmap(plot(1:10), width.px = 400, height.px=300, dir=getwd(), filename=filename)
  library(shinyEvents)
  app = eventsApp()
  addResourcePath("fig", getwd())
  app$ui = fluidPage(
    p("Image"),
    tags$img(src = paste0("fig/",filename),id="myimg")
  )
  imageClickHandler(id="myimg", function(...) {
    args = list(...)
    x = args$x
    y = args$y
    restore.point("my.image.handler")
    cat("\nclicked on image")
  })
  viewApp()

}

button.image.click = function() {
  library(SeminarMatching)
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  restore.point.options(display.restore.point = FALSE)
  app = StudSeminarsApp(init.userid = "test", init.password="test", lang="de")
  viewApp(app)
}

setattr.example = function() {
  app = eventsApp()
  
  app$ui = fluidPage(
    actionButton("btn",label="Hide"),
    div(id="div1",p("I am a div"))
  )
  buttonHandler("btn", function(...) {
    setHtmlCSS("div1",list(visibility="hidden"))
  })
  viewApp(app)
}


setui.example = function() {

  library(shinyEventsUI)
  app = eventsApp()
  
  app$ui = fluidPage(
    radioBtnGroup(id = "rbg",
      labels = c("A","B"),
      show.hide.containers = c("div1","div2")
    ),
    div(id="div1","div1 (shown when A)",uiOutput("out1a"),uiOutput("out1b")),
    hidden_div(id="div2","div2 (shown when B)",actionButton("btn",label="Click"),uiOutput("out2a"),uiOutput("out2b")),
    div(id="div3","div3 (always shown)",uiOutput("out3"))
    
  )
  buttonHandler("btn", function(...) {
    txt = as.character(runif(1))
    setUI("out3",paste0("out 3 ",txt))
    setUI("out1a",paste0("out 1a ",txt)) # does not work correctly, since out1 is inside a hidden div
    setUI("out2a",paste0("out 2a ",txt)) # need dsetUI since out is inside a hidden div
    dsetUI("out2b",paste0("out 2b ",txt)) # need dsetUI since out is inside a hidden div
    dsetUI("out1b",paste0("out 1b ",txt)) # need dsetUI since out is inside a hidden div
    cat("\n setUI to ",txt)
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

dyn.ui.input.error.example = function() {
  library(shiny)

  myUI = function() {
    textInput("mytext",paste0("mytext ", sample.int(10000,1)), value=sample.int(10000,1))
  }
  
  ui = fluidPage(
    title = 'Dynamic UI Input Error',
    uiOutput("myUI"),
    actionButton("myBtn","Click me")
  )
  
  server = function(input, output, session) {
    output$myUI <- renderUI(myUI())
    
    observeEvent(input$myBtn,{
      text = isolate(input$mytext)
      cat("Value of text input:", text)
      output$myUI <- renderUI(myUI())
    })
  }
  runApp(list(ui=ui,server=server),launch.browser=rstudio::viewer) 
  
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


