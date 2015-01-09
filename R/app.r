# Manually trigger shiny events when some value changes
# While this is also the key idea of reactive programming
# Shiny (at least in current versions as of October 2014)
# tends to trigger events too often.

# What we need:
#   1. Call a function or render output when a button is clicked
#   2. Call a function or render output when an input value has changed
#   3. Update input values when an input value or variable has changed without triggering further events

.SHINY.EVENTS.ENV = new.env()

#' Generate an empty shiny events app
eventsApp = function(set.as.default=TRUE, verbose=FALSE, single.instance=FALSE) {
  app = new.env()
  app$glob = new.env(parent=globalenv())
  app$single.instance = single.instance
  app$isSessionEventApp = FALSE
  app$is.running = FALSE
  app$handlers = list()
  app$values = list()
  app$aceHotKeyRandNum = list()
  app$run.event.handlers=FALSE
  app$do.update = list()
  app$verbose=TRUE
  app$output = list()
  app$trigger.list = list()
  app$collect.triggers = TRUE
  
  app$initHandler = function(...) {}
  
  app$server = function(session, input, output) {
    app = getApp()
    app = setAppSession(session,app)
    session = app$session
    #browser()
    addEventHandlersToSession(session=session,app=app)
    app$initHandler(session=session, input=input, output=output, app=app)
  }
  if (set.as.default)
    setApp(app)
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
    app = as.environment(as.list(app))
    app$isSessionEventApp = TRUE
    attr(session,"eventsApp")=app
  }
  
  app$session = session
  app$input = session$input

  #browser()
  
  
  # Copy initially defined output renderers
  # into session$output object
  app$initial.output = app$output
  app$output = session$output
  for (id in names(app$initial.output)) {
    app$output[[id]] = app$initial.output[[id]]
  }
    
  session.env = new.env()
  session.env$session = session
  session.env$input = session$input
  session.env$output = session$output
  
  app$session.env = session.env 
  app
}

#' set the main ui object for the app
setAppUI = function(ui, app=getApp()) {
  app$ui = ui
}

#' run shiny events app
runEventsApp = function(app=getApp(),ui=NULL,...) {
  #add.ui.renderer(app=app)
  if (!is.null(ui))
    setAppUI(ui=ui, app=app)
  
  app$is.running = TRUE
  runApp(list(ui=app$ui, server=app$server),...)
  #app$is.running = FALSE  
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