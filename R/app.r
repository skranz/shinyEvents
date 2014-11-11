# Manually trigger shiny events when some value changes
# While this is also the key idea of reactive programming
# Shiny (at least in current versions as of October 2014)
# tends to trigger events too often.

# What we need:
#   1. Call a function or render output when a button is clicked
#   2. Call a function or render output when an input value has changed
#   3. Update input values when an input value or variable has changed without triggering further events

events.env = new.env()

shiny.app = function() {
  app = new.env()
  app$is.running = FALSE
  app$handlers = list()
  app$values = list()
  app$run.event.handlers=FALSE
  app$do.update = list()
  app$server = function(session, input, output) {
    app = get.app()
    set.app.session(session,app)
    add.handlers.to.session(app$handlers, app)
    #add.renderer.to.session(app$renderer, app)
  }
  app
}

set.app = function(app) {
  events.env$app = app
}

get.app = function() {
  events.env$app
}

set.app.session = function(session, app=get.app()) {
  app$session = session
  app$input = session$input
  app$output = session$output
  
  session.env = new.env()
  session.env$session = session
  session.env$input = session$input
  session.env$output = session$output
  
  app$session.env = session.env
}

run.app = function(app=get.app(),...) {
  #add.ui.renderer(app=app)
  runApp(list(ui=app$ui, server=app$server),...)
}

display = function(...) {
  
}

updater.exists = function(id, app=get.app()) {
  id %in% names(app$do.update)
}

perform.update = function(id, app=get.app()) {
  app$do.update[[id]]$counter = isolate(app$do.update[[id]]$counter+1)
}
