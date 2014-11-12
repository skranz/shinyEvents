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
eventsApp = function(set.app=FALSE, verbose=FALSE) {
  app = new.env()
  app$is.running = FALSE
  app$handlers = list()
  app$values = list()
  app$run.event.handlers=FALSE
  app$do.update = list()
  app$verbose=TRUE
  app$server = function(session, input, output) {
    app = getApp()
    setAppSession(session,app)
    addEventHandlersToSession(app$handlers, app)
  }
  if (set.app)
    setApp(app)
  app
}

#' set the current app
setApp = function(app) {
  .SHINY.EVENTS.ENV$app = app
}

#' get the current app
getApp = function() {
  .SHINY.EVENTS.ENV$app
}

setAppSession = function(session, app=getApp()) {
  app$session = session
  app$input = session$input
  app$output = session$output
  
  session.env = new.env()
  session.env$session = session
  session.env$input = session$input
  session.env$output = session$output
  
  app$session.env = session.env
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
