# Manually trigger shiny events when some value changes
# While this is also the key idea of reactive programming
# Shiny (at least in current versions as of October 2014)
# tends to trigger events too often.

# What we need:
#   1. Call a function or render output when a button is clicked
#   2. Call a function or render output when an input value has changed
#   3. Update input values when an input value or variable has changed without triggering further events


resetEventHandlers = function(app = getApp()) {
  app$values=list()  
}

addEventHandlersToSession = function(session.env=app$session.env, app=getApp()) {
  restore.point("addEventHandlersToSession")
  for (i in seq_along(app$handlers)) {
    app$handlers[[i]]$observer = eval(app$handlers[[i]]$call, session.env)
  }
}

removeEventHandler = function(id, ind, app = getApp()) {
  #cat("\nremoveEventHandler")

  if (!missing(id)) {
    ind = which(names(app$handlers) %in% id)
  }

  if (app$is.running) {
    for (i in ind) {
      app$handlers[[i]]$observer$destroy()    
    }
  }
  if (length(ind)>0) {
    app$handlers = app$handlers[-ind]
  }
  #cat("\nend removeEventHandler")

}

addEventHandlerToApp = function(id, call, type="unknown", app = getApp(),session.env=app$session.env, if.handler.exists = c("replace","add","skip")[1], intervalMs=NULL, session=getAppSession(app)) {
  #restore.point("addEventHandlerToApp")
  has.handler = id %in% names(app$handlers) 
  
  if ( (!has.handler) | if.handler.exists == "add") {
    n = length(app$handlers)+1
    app$handlers[[n]] = list(id=id, call=call, type=type, observer=NULL)
    names(app$handlers)[n] <- id
    if (app$is.running) {
      app$handlers[[n]]$observer = eval(call,app$session.env)
    }
  } else if (if.handler.exists=="replace") {
    if (app$is.running) {
      if (is.function(app$handlers[[id]]$observer$destroy)) {
        try(app$handlers[[id]]$observer$destroy())
      }
    }
    app$handlers[[id]] = list(id=id, call=call, type=type, observer=NULL)
    if (app$is.running) {
      app$handlers[[id]]$observer = eval(call,app$session.env)
    }

  } else {
    # don't add handler
    return()
  }
  if (type == "timer") {
    app$handlers[[id]]$timer = reactiveTimer(intervalMs = intervalMs, session)
  }
}

#' Add an handler to an input that is called when the input value changes
#' 
#' @param id name of the input element
#' @param fun function that will be called if the input value changes. The function will be called with the arguments: 'id', 'value' and 'session'. One can assign the same handler functions to several input elements.
#' @param ... extra arguments that will be passed to fun when the event is triggered.  
changeHandler = function(id, fun,...,app=getApp(), on.create=FALSE, if.handler.exists = c("replace","add","skip")[1], session=getAppSession(app)) {
  #browser()
  if (app$verbose)
    display("\nadd changeHandler for ",id)

  fun = substitute(fun)
  # Create dynamic observer
  args = list(...)
  
   ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args, s_on.create=on.create),
    observe({
      if (app$verbose)
        display("called event handler for ",s_id)
      input[[s_id]]
      if (hasWidgetValueChanged(s_id, input[[s_id]], on.create=s_on.create)) {
        if (app$verbose)
          display(" run handler...")
        do.call(s_fun, c(list(id=s_id, value=input[[s_id]], session=session,app=app),s_args))
      }
    })
  )
  
  addEventHandlerToApp(id=id,call=ca,type="change",app=app, if.handler.exists=if.handler.exists)
}


#' Add an handler that triggers every intervalMs milliseconds
#' 
#' @param id name of the input element
#' @param fun function that will be called if the input value changes. The function will be called with the arguments: 'id', 'value' and 'session'. One can assign the same handler functions to several input elements.
#' @param ... extra arguments that will be passed to fun when the event is triggered.  
timerHandler = function(id,intervalMs, fun,...,app=getApp(), on.create=FALSE, if.handler.exists = c("replace","add","skip")[1], verbose=FALSE, session=getAppSession(app)) {
  #browser()
  if (verbose)
    display("\nadd timerHandler ",id)

  fun = substitute(fun)
  # Create dynamic observer
  args = list(...)
  
  ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args, s_on.create=on.create,s_verbose=verbose),
    observe({
      if (s_verbose)
        display("\ncalled timer handler ",s_id)
      cURReNTTime = app$handlers[[s_id]]$timer()
      do.call(s_fun, c(list(id=s_id, value=cURReNTTime, session=session,app=app),s_args))
    })
  )
  
  addEventHandlerToApp(id=id,call=ca,type="timer",app=app, if.handler.exists=if.handler.exists, intervalMs=intervalMs)
}


#' Add an handler to a button
#' 
#' @param id name of the button
#' @param fun function that will be called if button is pressed. The function will be called with the arguments: 'id', 'value' and 'session'. One can assign the same handler functions to several buttons.
#' @param ... extra arguments that will be passed to fun when the event is triggered.  
buttonHandler = function(id, fun,..., app = getApp(),if.handler.exists = c("replace","add","skip")[1], session=getAppSession(app)) {
  
  if (app$verbose)
    display("\nadd buttonHandler for ",id)
  
  fun = substitute(fun)
  args = list(...)

  ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args),
    observe({
      #browser()
      if (hasButtonCounterIncreased(s_id, input[[s_id]])) {
        if (app$verbose)
          display(s_id, " has been clicked...")
        do.call(s_fun, c(list(id=s_id, value=input[[s_id]],
                              session=session,app=app),s_args))
      }
    })
  )
  addEventHandlerToApp(id=id,call=ca,type="button",app=app, if.handler.exists=if.handler.exists)
}


#' Add an handler to a hotkey in an aceEditor component
#' 
#' @param id name of the button
#' @param fun function that will be called if button is pressed. The function will be called with the following arguments:
#' 
#'  keyId: the id assigned to the hotkey
#'  editorId: the id of the aceEditor widget
#'  selection: if a text is selected, this selection
#'  text: the text of the aceEditor widget
#'  cursor: a list with the current cursor position:
#'          row and column with index starting with 0
#'  session: the current session object
#' @param ... extra arguments that will be passed to fun when the event is triggered.  
aceHotkeyHandler = function(id, fun,..., app = getApp(),if.handler.exists = c("replace","add","skip")[1], session=getAppSession(app)) {
  
  if (app$verbose)
    display("\nadd aceHotkeyHandler for ",id)
  
  fun = substitute(fun)
  args = list(...)

  ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args),
    observe({
      #restore.point("jdjfdgbfhdbgh")
      #browser()
      if (wasAceHotkeyPressed(s_id, input[[s_id]])) {
        display(s_id, " has been pressed...")
        res = input[[s_id]]
        text = isolate(input[[res$editorId]])
        li = c(list(keyId=s_id),res,
               list(text=text,session=session,app=app),s_args)
        do.call(s_fun,li)
      }
    })
  )
  addEventHandlerToApp(id=id,call=ca,type="button",app=app, if.handler.exists=if.handler.exists)
}



#' Checks whether the value of an input item has been changed (internal function)
hasWidgetValueChanged = function(id, new.value,on.create=FALSE, app = getApp()) {
  restore.point("hasWidgetValueChanged")
  #cat("\nid=",id)
  #cat("\napp$values[[id]]=",app$values[[id]])
  #cat("\nnew.value=",new.value)
  
  if (!id %in% names(app$values)) {
    if (is.null(new.value)) {
      app$values[id] = list(NULL)
    } else {
      app$values[[id]] = new.value
    }
    changed = on.create
  } else {
    changed = !identical(app$values[[id]],new.value)
    if (changed) {
      if (is.null(new.value)) {
        app$values[id] = list(NULL)
      } else {
        app$values[[id]] = new.value
      }
    }
  }
  return(changed)
}

#' Checks whether a button has been pressed again (internal function)
hasButtonCounterIncreased = function(id, counter, app = getApp()) {
  restore.point("hasButtonCounterIncreased")
  if (isTRUE(counter == 0) | is.null(counter) | isTRUE(counter<=app$values[[id]])) {
    app$values[[id]] = counter
    if (app$verbose)
      cat("\nno counter increase: ", id, " ",counter)
    return(FALSE)
  }
  app$values[[id]] = counter
  if (app$verbose)
    cat("\ncounter has increased: ", id, " ",counter)
  return(TRUE)  
}


#' Checks whether a button has been pressed again (internal function)
wasAceHotkeyPressed = function(keyId, value, app = getApp()) {
  restore.point("wasAceHotkeyPressed")

  if (is.null(value))
    return(FALSE)
  old.rand = app$aceHotKeyRandNum[[keyId]]
  app$aceHotKeyRand[[keyId]] = value$randNum
  
  was.pressed = !identical(value$randNum, old.rand)
  was.pressed
}

#' Set a function that will be called when a new session of 
#' an app is initialized.
#' 
#'  @param initHandler a function that takes parameters
#'    session, input, output and app. It will be called from
#'    the app$server function whenever a new session is created.
#'    It allows to initialize session specific variables and e.g.
#'    store them within the app object. The passed app object is already
#'    the local copy created for the new session.    
appInitHandler = function(initHandler,app=getApp()) {
  app$initHandler = initHandler
}
