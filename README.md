# shinyEvents: Alternative way to build shiny apps based on event handlers

Sebastian Kranz, Ulm University

### NEW Version that is not fully compatible with old apps

Starting from version 2.0 shiny events is considerably rewritten. Instead of just building event handling on top of shiny's reactive framework, in many cases it now directly uses javascript events, circumventing many steps of shiny's reactivity. It still is possible to combine shinyEvents with standard shiny reactivity based programming.

I tried to be backward compatible, but there may be some incompatibilities with the old version, however. The most important requirement of the new version is that you add the command
```r
appReadyToRun(app)
```
at the end of your global.R file in a shiny app (see below) to get your app approbiatedly configured.

### Key idea

RStudio's shiny is a great framework to generate web applications with R. In a classical shiny app, interactivity is not generated via event handlers but by `reactive programming`. For details, see the shiny documentation and tutorials under [http://shiny.rstudio.com/](http://shiny.rstudio.com/).

While shiny's reactive programming model is great for smaller apps, I personally found it less useful for bigger applications that create a lot of interactive dynamic UI.

For example, when writing the initial shiny interface for my package RTutor [https://github.com/skranz/RTutor](https://github.com/skranz/RTutor), I felt that some observers or render functions were triggered too frequently, and I was not sure where to best put the 'server code' of newly dynamically created objects. Of course, it is definitely possible to write large applications with reactivity, but given my limited understanding of the reactivity model, it just was hard for me...

Anyway, I generated the package `shinyEvents` to emulate the classical event-handling paradigm for shiny applications and find it personally quite useful... 

The shinyEvents package allows to write shiny applications that use classical event handlers, e.g. for button clicks, value changes, etc. One does not write an explicit server function, but just adds event handlers to an `app` object. Widgets will be updated with explicit calls to updateXXX or setXXXX functions, like e.g. `setText(id, "New text")`. Widget values and event handlers can be set in a similar fashion for an app that has not yet started as for an already running app.

More recently, I added features like custom event handlers for arbitrary jQuery events or better support for input forms. Take a look at the examples below.

## Installation

To install the package run the following code:

```r
if (!require(devtools)) install.packages("devtools")

devtools::install_github(repo="skranz/restorepoint")
devtools::install_github(repo="skranz/shinyEvents")
```


## Examples

### A simple static app

Here is a simple example app. 

```r
library(shinyEvents)

# Create a new eventsApp
app = eventsApp()

# ui
app$ui = fluidPage(
  actionButton("plotBtn", "plot"),
  selectInput("mySelect", "Select:",
    c("Cylinders" = "cyl","Transmission" = "am","Gears" = "gear")
  ),
  textOutput("myText"),
  plotOutput("myPlot")
)

# Handler for the plot button
buttonHandler("plotBtn", function(id,...) {
  restore.point("plotBtnClick")
  setText("myText", paste0("You pressed the button ",id," at ", Sys.time()))
  setPlot("myPlot", plot(runif(10), runif(10)))    
})

# Handler for change of an input value
selectChangeHandler("mySelect", function(id, value,...) {
  restore.point("selectChange")
  setText("myText",paste0("You chose the list item ", value,". ", 
                          "A random number: ", sample(1:1000,1)))
})

# Set an initial text
setText("myText","This is the start text...")

# Directly launch the events app in the viewer pane
viewApp(app)
```

Note that the handlers only are called when indeed an action is performed, e.g. the user selects a different element in a selectInput. In contrast, I often experienced in the usual approach in shiny to observe an input element, the change events are fired more often, e.g. when the element is first created. 

I put in every handler a <a href="https://github.com/skranz/restorepoint" target="_blank">restore point</a>, which I personally find very helpful to debug shiny applications.

## An app with form input

Often I find it helpful to write apps with traditional input forms, where values from input fields are submitted when a button is pressed. Here is an example:

```r
library(shinyEvents)

# Create a new eventsApp
app = eventsApp()

app$ui = fluidPage(
  textInput("input_name","Your Name"),
  textInput("input_email", "Your Email"),
  simpleButton("btn","Submit", form.ids = c("input_name","input_email")),
  textInput("output_name","Saved Name"),
  textInput("output_email","Saved Email")
)

buttonHandler("btn", function(formValues, app=getApp(),...) {
  restore.point("btn.click")
  vals = formValues
  # Name after input_
  names(vals) = substring(names(vals),7)
  print(vals)
  setWidgetValues(list(
    output_name = vals$name,
    output_email = vals$email
  ))
})

viewApp(app)
```

The `simpleButton` function has an argument `form.ids`, which lists the ids of all form elements whose values shall be passed to the buttonHandler in the list variable `formValues`.

## A more complex example with dynamic UI and custom HTML

The two examples before could also be easily implemented with a traditional shiny app. Here is a more complex example with a dynamically created [HTML table](https://www.scaler.com/topics/html/tables-in-html/) with multiple inputs:

```r
library(shinyEvents)

# Create a new eventsApp
app = eventsApp()

app$ui = fluidPage(
  simpleButton("btn1","Create Excercises"),
  uiOutput("exUI")
)


buttonHandler("btn1", function(...) {
  restore.point("btn1_click")
  # Create manual HTML code
  # A table with 5 random addition exercises
  num1 = sample(1:100,5)
  num2 = sample(1:100,5)
  
  rows = paste0(collapse = "\n",
    "<tr><td>",num1," + ", num2 ," = </td>",
    "<td><input class='ans-input' id='ans-",1:5,"' data-row='",1:5,"',></input></td></tr>"
  )
  tab = paste0("<table>",rows,"</table>")
  ui = tagList(
    HTML(tab),
    uiOutput("ansUI"),
    simpleButton("btn2","Check Solution", form.sel=".ans-input")
  )
  
  # clear ansUI if button is pressed multiple times
  setUI("ansUI","")
    
  buttonHandler("btn2", function(formValues, app=getApp(),...) {
    restore.point("btn2.click")
    vals = as.integer(unlist(formValues))

    correct = sum(vals==num1+num2,na.rm = TRUE)
    setUI("ansUI", p(paste0(correct, " exercises correct.")))
  })

  
  setUI("exUI", ui)
})

viewApp(app)
```

When we press `btn1`, we generate some custom HTML table from pure HTML code, not using any specific shiny widget. The dynamically generated simpleButton `btn2` uses the argument `form.sel=".ans-input"`. This is a <a href="https://www.w3schools.com/cssref/css_selectors.asp">CSS selector</a> and specifies that the values of all inputs with class `ans-input` shall be passed in the variable `formValues` to the button handler of `btn2`.

Note that the button handler for `btn1` is defined before the app runs, while the button handler for `btn2` is dynamically created during runtime. `shinyEvents` is designed to allow both. There is a difference in so far that all handlers that will be generated before the app starts will be available in every instance of the app. In contrast, handlers that will be added dynamically will only be available for the specific instance of the app, in which the handler was generated. 

As a general observation, I tend to use more custom HTML and Javascript code the more shiny apps I have programmed. `shinyEvents` has correspondingly evolved to allow simple interfaces to such custom HTML, as the example above illustrates.

## Custom Event Handlers

For all sorts of <a href="https://api.jquery.com/category/events/">jQuery Events</a>, you can add a custom event handler with shinyEvents. Here is a custom handler, you can add to the previous example:

```r
customEventHandler("ans_edit", css.locator=".ans-input",event = "keyup", function(id, value, data, ...) {
  restore.point("ans-edit")
  row = data$row
  setUI("ansUI", p(paste0("You typed ", value, " in row ", row)))
}) 
```

The handler is called when a jQuery `keyup` event is triggered on our manually generated input fields specified by the css class `ans-input`. In the HTML code that created our input fields (previous example), you see that we added a field `data-row`. By default all data fields of the element that triggers an event will be passed to the handler in the variable `data`. Here we stored the row number, and can thus access it in the handler inside R.



## A small chat app

The code below generates a small chat application as a shiny events app in which multiple users can interact. Open multiple browser windows to see how chatting among multiple clients works.

```r
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
    aceEditor("convAce",value = app$glob$txt, height="200px",
              showLineNumbers = FALSE, debounce=100),    
    
    # Enter new text
    aceEditor("enterAce",value = "Your text",height="30px",
              showLineNumbers = FALSE,debounce = 100,
              hotkeys = list(addTextKey="Ctrl-Enter")),
    
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
  appInitHandler(function(input, output, session,app,...) {
    updateTextInput(session,"userName",value=paste0("guest", sample.int(10000,1)) )
    updateAceEditor(session,editorId = "convAce",value = app$glob$txt)
  })


  viewApp(app, launch.browser=TRUE)
  # To test chat function, open several browser tabs
```

We use some new handlers in this example:

  - `aceHotkeyHandler(...)` can handle hotkeys in an aceEditor input
  
  - `timerHandler(...)` specifies a function that will be called in fixed time intervals
  
  - `appInitHandler(...)` specifies a function to customize a newly initiated session of the app. When using R Studio's functionality to run apps, you may have to leave out the argument `app` in the function passed to `appInitHandler`.

The app object has a field `glob` that can be used to store variables that will be shared among sessions. (Of course, you could alternatively just use a global variable directly in R.)

# Deploying as a shiny app

The example run the generated app locally. Of course you can also deploy an event app via shiny server. Just generate in the usual fashion an app folder with files `ui.R`, `server.R`, and `global.R`.

I would recommend to ui.R and server.R to be the following one-liners:


```r
# ui.R
shinyUI(app$ui)
```
and

```r
# server.R
shinyServer(app$server)
```
The generation of the app can then be put into global.R. If our first example, we would put into global.R:

```r
# global.R for a simply shiny events app

library(shinyEvents)
app = eventsApp()
app$ui = fluidPage(p("Hello World!"))

# Important that you add this line
appReadyToRun(app)
```
