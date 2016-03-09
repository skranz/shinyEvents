examples.click = function() {
  app = eventsApp()
  app$ui = tagList(
    fluidPage(
      actionButton("btn1","btn1"),
      actionButton("btn2","btn2"),
      actionButton("btn3","btn3")
    )
  )
  buttonHandler("btn1",var=45,function(id, var,...) {
    print(var)
  })
  
  viewApp(app)
  eventsList = app$eventList
}


addEventsToSession = function(session.env=app$session.env, app=getApp()) {
  restore.point("addEventsToSession")
  for (i in seq_along(app$eventList)) {
    app$eventList[[i]]$observer = eval(app$eventList[[i]]$observer.call,session.env)
  }
}


# TO DO: Need to think about were to store
# app$glob$..eventHandlersList[[eventId]]
# what shall be global, what shall be local...

# A general event handler for all sorts of java script events
# that are bound to a reactive shiny variable via
# Shiny.onInputChange("{{eventId}}", {id: e.target.id, tag: e.target.nodeName, nonce: Math.random()});
eventHandler = function(eventId,id=eventId, fun, ...,jscript=NULL, app=getApp()) {
  args = list(...)
  restore.point("eventHandler")
  registerEvent(eventId,jscript=jscript,app=app)
  value=list(fun=fun,args=args)
  if (!is.null(id)) {
    app$eventList[[eventId]]$handlers[[id]] = value
  } else {
    app$eventList[[eventId]]$glob.handler = value
  }
}

js.event.triggered = function(eventId,value,..., app=getApp()) {
  restore.point("js.event.triggered")
  event = app$eventList[[eventId]]
  id = value$id
  #cat("\njs event triggered eventId = ",eventId," target id = ",id)
  h = event$handlers[[id]]
  if (is.null(h)) {
    h = event$glob.handler
  }
  if (is.null(h)) {
    cat("\nNo event handler for eventId =",eventId," target id = ", id," registered.")
    return()
  }
  do.call(h$fun,c(value,h$args))
}

# register a java script eventId and create handler list
registerEvent = function(eventId, jscript=NULL, app=getApp()) {
  # eventId already exists
  event = app$eventList[[eventId]]
  if (!is.null(event))
    return(event)

  restore.point("registerEvent")
  
  if (app$is.running) {
    warning(paste0("The event ", eventId, " has been registered after the app has been running. Please register the event before you run the app, since the corresponding javascript must be added to the UI."))
    eval(event$observer.call, app$session.env)
  }
  
  
  # add event
  ca = substitute(env = list(eventId=eventId),
  observeEvent(input[[eventId]],{
    value = input[[eventId]]
    js.event.triggered(eventId=eventId,value)
  })
  )
  
  if (is.character(jscript)) {
    jscript = tags$script(HTML(jscript))
  }
  event = list(
    eventId = eventId,
    jscript = jscript,
    handlers = list(),
    glob.handler = NULL,
    observer.call = ca,
    observer = NULL
  )
  app$eventList[[eventId]] = event
  return(event)
}

getAppEvent = function(eventId,app=getApp()) {
  app$eventList[[eventId]]  
}

idEventHandler = function(id, event="change", css.locator="", inner.js.code=NULL, shiny.value.code=NULL, eventId=paste0(id,"_id_",event,"_event"),...) {
  restore.point("classEventHandler")
  if (nchar(css.locator)>0) {
    css.locator=paste0(css.locator," #",id)
  } else {
    css.locator=paste0("#",id)
  }
  customEventHandler(eventId=eventId, css.locator=css.locator, event=event, inner.js.code=inner.js.code, shiny.value.code=shiny.value.code, id=NULL,...)
}

classEventHandler = function(class, event="change", css.locator="", inner.js.code=NULL, shiny.value.code=NULL, eventId=paste0(class,"_class_",event,"_event"), class.prefix=".",...) {
  restore.point("classEventHandler")
  if (nchar(css.locator)>0) {
    css.locator=paste0(css.locator," ",class.prefix,class)
  } else {
    css.locator=paste0(class.prefix,class)
  }
  customEventHandler(eventId=eventId, css.locator=css.locator, event=event, inner.js.code=inner.js.code, shiny.value.code=shiny.value.code, id=NULL,...)
}

customEventHandler = function(eventId, css.locator, event="change", inner.js.code=NULL, shiny.value.code=NULL, id=NULL,...) {
  restore.point("customEventHandler")

  if (is.null(inner.js.code)) {
    inner.js.code = 'var value = $(this).val();'
  }
  if (is.null(shiny.value.code)) {
    shiny.value.code = paste0('{eventId:"',eventId,'",id: this.id, value: $(this).val()})');
  }

  jscript = paste0('
$("',css.locator,'").', event,'(function() {
  ',inner.js.code,'
  Shiny.onInputChange("',eventId,'", ', shiny.value.code,');
  }
});
')
  eventHandler(eventId=eventId,id=id,...,jscript=jscript)
}


# more efficient version of button handler via global eventId handler
buttonHandler = function(id, fun, ..., eventId="buttonHandlerEvent",jscript=buttonHandlerJS(eventId), app=getApp()) {
  restore.point("buttonHandler")
  eventHandler(eventId=eventId,id=id,fun=fun,...,jscript=jscript, app=app)
}



buttonHandlerJS = function(eventId="buttonHandlerEvent") {
  restore.point("buttonHandlerJS")
  res = tags$script(paste0('
  $(document).on("click", function (e) {
    var tag = e.target.nodeName;
    if (tag === "BUTTON") {
      Shiny.onInputChange("',eventId,'", {eventId: "',eventId,'", id: e.target.id, tag: tag, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
    }
  });'))
  return(res)
}

documentClickHandler = function(fun,...,eventId="documentClickHandlerEvent", jscript=documentClickHandlerJS(eventId), id=NULL) {
  eventHandler(eventId = eventId, id=id,fun=fun,...jscript=jscript)  
}

# Add javascript to deal with clicks on free html area,
# i.e. not on inputs, buttons, links or images
# can be used to proceed with slides
documentClickHandlerJS = function(eventId="documentClickHandlerEvent") {
  res = tags$script(paste0('
  $(document).on("click", function (e) {
    var nn = e.target.nodeName;

    if (nn === "BUTTON" || nn === "IMG" || nn === "INPUT" || nn === "A") {
      return;
    }
    var pn = e.target.parentNode;
    if (pn.className === "radio" || pn.className === "checkbox") {
      return;
    }
    var gpn = pn.parentNode;
    if (gpn.className === "radio" || gpn.className === "checkbox") {
      return;
    }

    Shiny.onInputChange("',eventId,'", {id: e.target.id, tag: nn, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
  });'))
}

