# Manually trigger shiny events when some value changes
# While this is also the key idea of reactive programming
# Shiny (at least in current versions as of October 2014)
# tends to trigger events too often.

# What we need:
#   1. Call a function or render output when a button is clicked
#   2. Call a function or render output when an input value has changed
#   3. Update input values when an input value or variable has changed without triggering further events

.SHINY.EVENTS.ENV = new.env()

getDefaultAppEvents = function() {
  list(
    buttonHandlerEvent=list(jscript=buttonHandlerJS()),
    selectChangeHandlerEvent=list(jscript=selectChangeHandlerJS())
  )
}

setAppHasBottomScript = function(has.bottom.script=FALSE, app=getApp()) {
  app$glob$..HAS.BOTTOM.SCRIPT = has.bottom.script 
}

#' If app is not running, mark script to be added at the bottom and return NULL
#' If app is already running return script directly
bottomScript = function(..., app=getApp()) {
  tag=tags$script(...)
  attr(tag,"isBottomScript") <- TRUE
  if (!isTRUE(app$is.running)) app$glob$..HAS.BOTTOM.SCRIPT = TRUE
  tag
}

#' If app is not running, mark script to be added at the bottom and return NULL
#' If app is already running return script directly
singletonBottomScript = function(..., app=getApp()) {
  tag=singleton(tags$script(...)) 
  attr(tag,"isBottomScript") <- TRUE
  if (!isTRUE(app$is.running)) app$glob$..HAS.BOTTOM.SCRIPT = TRUE
  tag
}


#' Given a tag object, extract out any children of tags$head
#' and return them separate from the body.
moveBottomScripts <- function(ui, reset.app=FALSE) {
  restore.point("moveBottomScripts")
  
  bottomItems <- list()
  result <- htmltools:::rewriteTags(ui, function(uiObj) {
    if (htmltools:::isTag(uiObj) && isTRUE(attr(uiObj,"isBottomScript"))) {
      bottomItems <<- append(bottomItems, list(uiObj))
      return(NULL)
    }
    return(uiObj)
  }, FALSE)

  if (reset.app) {
    app=getApp()
    app$glob$..HAS.BOTTOM.SCRIPT = FALSE
  }
  return(tagList(result, bottomItems))
}

#' Generate an empty shiny events app
eventsApp = function(set.as.default=TRUE, verbose=TRUE, single.instance=FALSE, add.events = getDefaultAppEvents(), no.events=FALSE, need.authentication=FALSE, adapt.ui = TRUE) {
  app = new.env()
  glob = new.env(parent=globalenv())
  
  app$glob = glob

  app$.adapt.ui = adapt.ui 
  app$need.authentication = need.authentication
  app$events.without.authentication = NULL
  app$is.authenticated = FALSE
  app$no.events = no.events
  app$eventList = list()
  app$single.instance = single.instance
  app$is.running = FALSE
  app$handlers = list()
  app$values = list()
  app$aceHotKeyRandNum = list()
  app$do.update = list()
  app$verbose=verbose
  app$output = list()
  app$trigger.list = list()
  app$collect.triggers = TRUE
  
  app$initHandler = function(...) {}
  
  app$server = function(session, input, output) {  
    app = getApp()
    app$is.running = TRUE
    app$in.init = TRUE
    cat(paste0("\nStarted new session at ", Sys.time()))
    #stop("Stop in app$server")
    app = setAppSession(session,app)
    session = app$session
    addEventsToSession(app=app)
    addHandlersToSession(app=app)
    app$initHandler(session=session, input=input, output=output, app=app)
    app$in.init = FALSE
  }
  if (set.as.default)
    setApp(app)
  
  # register default events
  for (eventId in names(add.events)) {
    registerEvent(eventId=eventId,jscript=add.events[[eventId]]$jscript,app=app)
  }
  
  app
}

#' set the current app
setApp = function(app) {
  .SHINY.EVENTS.ENV$app = app
}


#' get the current app object
#' 
#' If the app is already running, gets by default the local app copy
#' corresponding to the current session
getApp = function(session=NULL) {
  gapp = .SHINY.EVENTS.ENV$app
  if (is.null(gapp)) return(NULL)
  if (is.null(session)) {
    if (gapp$is.running)
      session = getCurrentSession()
  }
  if (is.null(session))
    return(.SHINY.EVENTS.ENV$app)
  
  app = attr(session,"eventsApp")
  if (!is.null(app)) return(app)
  gapp
}

setAppSession = function(session, app=getApp(global=TRUE)) {
  #restore.point("setAppSession")
  
  if (!app$single.instance) {
    # create a copy of app
    app = as.environment(as.list(app, all.names=TRUE))
    app$isSessionEventApp = TRUE
    attr(session,"eventsApp")=app
  }
  
  app$session = session
  app$input = session$input

  #browser()
  
  
  # Copy initially defined output renderers
  # into session$output object
  if (is.null(app[["initial.output"]]))
    app$initial.output = app$output
  app$output = session$output
  for (id in names(app$initial.output)) {
    try(app$output[[id]] <- app$initial.output[[id]])
  }
    
  session.env = new.env()
  session.env$session = session
  session.env$input = session$input
  session.env$output = session$output
  
  app$session.env = session.env 
  app
}

setAppIsAuthenticated = function(is.authenticated, app=getApp()) {
  app$is.authenticated = is.authenticated
}

#' set the main ui object for the app
setAppUI = function(ui, app=getApp()) {
  app$ui = ui
}

#' view shiny events app in RStudio viewer
viewApp = function(app=getApp(),ui=NULL,launch.browser=rstudio::viewer,...) {
  runEventsApp(app,ui, launch.browser=launch.browser,...)
}

sc = function(..., collapse=NULL, sep="") {
  args = list(...)
  lens = sapply(args, length)
  if (any(lens==0)) return(NULL)
  paste0(..., collapse=collapse, sep=sep)
}

#' set the app ready to run
appReadyToRun = function(app=getApp(), ui=app$ui) {
  restore.point("appReadyToRun")
  
  if (isTRUE(app$.adapt.ui)) {
    addShinyEventsRessourcePath()
    # js code for dsetUI
    js = '
  Shiny.addCustomMessageHandler("shinyEvalJS", function(message) {
    eval(message.code);
  });

  Shiny.addCustomMessageHandler("shinyEventsAppend", function(message) {
    $(message.selector).append(message.html);
  });
  Shiny.addCustomMessageHandler("shinyEventsPrepend", function(message) {
    $(message.selector).prepend(message.html);
  });
  
  Shiny.addCustomMessageHandler("shinyEventsSetInnerHTML", function(message) {
    //alert("selector: "+ message.selector + " html: "+ message.html);
    $(message.selector).html(message.html);
  });
  
  Shiny.addCustomMessageHandler("shinyEventsSetAttribute",function(message) {
      $(message.selector).attr(message.attr);
  });
  Shiny.addCustomMessageHandler("shinyEventsSetCSS", function(message) {
      $(message.selector).css(message.attr);
  });
  '
  
    
    if (!app$no.events) {
      script.tags = lapply(app$eventList, function(event) {
        event$jscript
      })
      ui = tagList(
        singleton(tags$head(tags$script(src="shinyEvents/shinyEvents.js"))),
        ui,
        script.tags,
        tags$script(HTML(js))
      )
    }
    
    
    if (isTRUE(app$glob$..HAS.BOTTOM.SCRIPT))
      ui = moveBottomScripts(ui)
    
  }
  
  app$ui = ui
  app$is.running = TRUE
}

addShinyEventsRessourcePath = function() {
  www.dir = system.file('www', package='shinyEvents')
  # init ressource paths
  shiny::addResourcePath(
    prefix = 'shinyEvents',
    directoryPath = www.dir
  )
}


#' run shiny events app
runEventsApp = function(app=getApp(),ui=NULL,...) {
  #add.ui.renderer(app=app)
  if (!is.null(ui))
    setAppUI(ui=ui, app=app)
  setApp(app)
  appReadyToRun(app)
  on.exit(app$is.running <- FALSE)
  runApp(list(ui=app$ui, server=app$server),...)
}

display = function(...) {
  cat(...)  
}

#' Get the current session object
getCurrentSession =  function() {
  getDefaultReactiveDomain()
}

#' Get the session associated with the app object
getAppSession = function(app=NULL) {
  if (is.null(app))
    return(getCurrentSession())
  app$session
}

#' Get an input value from the current session
getInputValue = function(id, session=getAppSession(app),app=getApp()) {
  isolate(session$input[[id]])
}

#' Set values for shiny or html inputs or other widgets
#' 
#' We may have to adapt the function shinyEventSetValue
#' to include new, not yet supported widget types
#' @param values a named list. The names are the 
#'        id of the html widget. The values are the
#'        values.
setWidgetValues = function(values) {
  callJS("shinyEventsSetValues", values)
}
