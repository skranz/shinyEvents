# Manually trigger shiny events when some value changes
# While this is also the key idea of reactive programming
# Shiny (at least in current versions as of October 2014)
# tends to trigger events too often.

# What we need:
#   1. Call a function or render output when a button is clicked
#   2. Call a function or render output when an input value has changed
#   3. Update input values when an input value or variable has changed without triggering further events


resetEventHandlers = function(session=NULL, app = getApp(session)) {
  app$values=list()  
}

addEventHandlersToSession = function(session=NULL,session.env=app$session.env, app=getApp(session)) {
  for (i in seq_along(app$handlers)) {
    app$handlers[[i]]$observer = eval(app$handlers[[i]]$call, session.env)
  }
}

removeEventHandler = function(session=NULL, id, ind, app = getApp(session)) {
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

addEventHandlerToApp = function(session=NULL, id, call, type="unknown", app = getApp(session),session.env=app$session.env, if.handler.exists = c("replace","add","skip")[1], intervalMs=NULL) {
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
changeHandler = function(session=NULL, id, fun,...,app=getApp(session), on.create=FALSE, if.handler.exists = c("replace","add","skip")[1]) {
  #browser()
  if (app$verbose)
    display("\nadd changeHandler for ",id)

  fun = substitute(fun)
  # Create dynamic observer
  args = list(...)
  
   ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args, s_on.create=on.create),
    observe({
      display("called event handler for ",s_id)
      input[[s_id]]
      if (hasWidgetValueChanged(session,s_id, input[[s_id]], on.create=s_on.create)) {
        display("run event handler for ",s_id)
        do.call(s_fun, c(list(id=s_id, value=input[[s_id]], session=session,app=app),s_args))
      }
    })
  )
  
  addEventHandlerToApp(id=id,call=ca,type="change",session=session,app=app, if.handler.exists=if.handler.exists)
}


#' Add an handler that triggers every intervalMs milliseconds
#' 
#' @param id name of the input element
#' @param fun function that will be called if the input value changes. The function will be called with the arguments: 'id', 'value' and 'session'. One can assign the same handler functions to several input elements.
#' @param ... extra arguments that will be passed to fun when the event is triggered.  
timerHandler = function(session=NULL, id,intervalMs, fun,...,app=getApp(session), on.create=FALSE, if.handler.exists = c("replace","add","skip")[1], verbose=FALSE) {
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
  
  addEventHandlerToApp(id=id,call=ca,type="timer",session=session,app=app, if.handler.exists=if.handler.exists, intervalMs=intervalMs)
}


#' Add an handler to a button
#' 
#' @param id name of the button
#' @param fun function that will be called if button is pressed. The function will be called with the arguments: 'id', 'value' and 'session'. One can assign the same handler functions to several buttons.
#' @param ... extra arguments that will be passed to fun when the event is triggered.  
buttonHandler = function(session=NULL, id, fun,..., app = getApp(session),if.handler.exists = c("replace","add","skip")[1]) {
  
  if (app$verbose)
    display("\nadd buttonHandler for ",id)
  
  fun = substitute(fun)
  args = list(...)

  ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args),
    observe({
      if (hasButtonCounterIncreased(session,s_id, input[[s_id]])) {
        display(s_id, " has been clicked...")
        do.call(s_fun, c(list(id=s_id, value=input[[s_id]],
                              session=session,app=app),s_args))
      }
    })
  )
  addEventHandlerToApp(id=id,call=ca,type="button",session=session,app=app, if.handler.exists=if.handler.exists)
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
aceHotkeyHandler = function(session=NULL, id, fun,..., app = getApp(session),if.handler.exists = c("replace","add","skip")[1]) {
  
  if (app$verbose)
    display("\nadd aceHotkeyHandler for ",id)
  
  fun = substitute(fun)
  args = list(...)

  ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args),
    observe({
      #restore.point("jdjfdgbfhdbgh")
      #browser()
      if (wasAceHotkeyPressed(session,s_id, input[[s_id]])) {
        display(s_id, " has been pressed...")
        res = input[[s_id]]
        text = isolate(input[[res$editorId]])
        li = c(list(keyId=s_id),res,
               list(text=text,session=session,app=app),s_args)
        do.call(s_fun,li)
      }
    })
  )
  addEventHandlerToApp(id=id,call=ca,type="button",session=session,app=app, if.handler.exists=if.handler.exists)
}



#' Checks whether the value of an input item has been changed (internal function)
hasWidgetValueChanged = function(session=NULL, id, new.value,on.create=FALSE, app = getApp(session)) {
  restore.point("hasWidgetValueChanged")
  if (!id %in% names(app$values)) {
    app$values[[id]] = new.value
    changed = on.create
  } else {
    changed = !identical(app$values[[id]],new.value)
    if (changed) {
      app$values[[id]] = new.value
    }
  }
  return(changed)
}

#' Checks whether a button has been pressed again (internal function)
hasButtonCounterIncreased = function(session=NULL, id, counter, app = getApp(session)) {
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
wasAceHotkeyPressed = function(session=NULL,keyId, value, app = getApp(session)) {
  restore.point("wasAceHotkeyPressed")

  if (is.null(value))
    return(FALSE)
  old.rand = app$aceHotKeyRandNum[[keyId]]
  app$aceHotKeyRand[[keyId]] = value$randNum
  
  was.pressed = !identical(value$randNum, old.rand)
  was.pressed
}
