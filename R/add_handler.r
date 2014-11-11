# Manually trigger shiny events when some value changes
# While this is also the key idea of reactive programming
# Shiny (at least in current versions as of October 2014)
# tends to trigger events too often.

# What we need:
#   1. Call a function or render output when a button is clicked
#   2. Call a function or render output when an input value has changed
#   3. Update input values when an input value or variable has changed without triggering further events


reset.event.handlers = function(app = get.app()) {
  app$values=list()  
}

add.handlers.to.session = function(handlers, session.env=app$session.env, app=get.app()) {
  for (el in handlers) {
    call = el$call
    eval(call, session.env)
  }
}

add.handler = function(id, call, type="unknown", app = get.app()) {
  if (app$is.running) {
    eval(app$handler.env, call)
  } else {
    app$handlers[[length(app$handlers)+1]] = list(id=id, call=call, type=type)
  }
}

add.change.handler = function(id, fun,...,app=get.app(), on.create=FALSE) {
  fun = substitute(fun)
  # Create dynamic observer
  args = list(...)
  ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args, s_on.create=on.create),
    observe({
      display("called event handler for ",s_id)
      input[[s_id]]
      if (has.widget.value.changed(s_id, input[[s_id]], on.create=s_on.create)) {
        display("run event handler for ",s_id)
        do.call(s_fun, c(list(id=s_id, value=input[[s_id]], session=session),s_args))
      }
    })
  )
  add.handler(id=id,call=ca,type="change",app=app)
}



add.button.handler = function(id, fun,..., app = get.app()) {
  
  fun = substitute(fun)
  args = list(...)

  ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args),
    observe({
      if (has.button.counter.increased(s_id, input[[s_id]])) {
        display(s_id, " has been clicked...")
        do.call(s_fun, c(list(id=s_id, value=input[[s_id]], session=session),s_args))
      }
    })
  )
  add.handler(id=id,call=ca,type="button",app=app)
}

has.widget.value.changed = function(id, new.value,on.create=FALSE, app = get.app()) {
  restore.point("has.widget.value.changed")
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


has.button.counter.increased = function(id, counter, app=get.app()) {
  restore.point("has.widget.counter.increased")
  if (isTRUE(counter == 0) | is.null(counter) | isTRUE(counter<=app$values[[id]])) {
    app$values[[id]] = counter
    cat("\nno counter increase: ", id, " ",counter)
    return(FALSE)
  }
  app$values[[id]] = counter
  cat("\ncounter has increased: ", id, " ",counter)
  return(TRUE)  
}
