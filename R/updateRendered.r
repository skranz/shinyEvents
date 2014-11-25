updateRenderer = function(id, expr.object, renderFunc,
                          update.env=parent.frame(),app=app,
                          overwrite.renderer=FALSE, level=0) {
  restore.point("updateRenderer")
  # add a renderer that is triggered by performUpdate
  addUpdateRenderer(id=id, renderFunc=renderFunc, app=app, 
                    overwrite=overwrite.renderer) 
  app$update.expr = expr.object
  app$update.env = update.env
  triggerWidgetUpdate(id, app, level=level) 
}

#' Update an dataTableOutput object. Can be used instead of renderDataTable
updateDataTable = function(id, expr,update.env=parent.frame(), app=getApp(),...) {
  expr.object = substitute(expr)
  if (app$verbose)
    cat("\n updateDataTable: ", id)
  app$output[[id]] <- renderDataTable(env=update.env, quoted=TRUE, expr=expr.object,...)
}

#' Update an output object. Can be used instead of renderImage
updateImage = function(id, expr,update.env=parent.frame(), app=getApp()) {
  expr.object = substitute(expr)
  updateRenderer(id, expr.object, renderImage, update.env, app)
}

#' Update an textOutput object. Can be used instead of renderPrint
updatePrint = function(id, expr, app=getApp(), update.env=parent.frame()) {
  expr.object = substitute(expr)
  updateRenderer(id, expr.object, renderPrint, update.env, app)
}

#' Update an tableOutput object. Can be used instead of renderTable
updateTable = function(id, expr, app=getApp(), update.env=parent.frame()) {
  expr.object = substitute(expr)
  updateRenderer(id, expr.object, renderTable, update.env, app)
}

#' Update an textOutput object. Can be used instead of renderText
updateText = function(id, expr,update.env=parent.frame(), app=getApp()) {
  expr.object = substitute(expr)
  updateRenderer(id, expr.object, renderText, update.env, app)
}

#' Update an uiOutput object. Can be used instead of renderUI
updateUI = function(id, expr,update.env=parent.frame(), app=getApp()) {  
  expr.object = substitute(expr)
  restore.point("updateUI")
  if (app$verbose)
    cat("\n updateUI: ", id)
  app$output[[id]] <- renderUI(env=update.env, quoted=TRUE, expr=expr.object)
  #updateRenderer(id, expr.object, renderUI, update.env, app)
}


hasUpdater = function(id, app=getApp()) {
  id %in% names(app$do.update)
}

triggerWidgetUpdate = function(id, app=getApp(), level=0) {
  restore.point("triggerWidgetUpdate")
  app$do.update[[id]]$counter = isolate(app$do.update[[id]]$counter+1)
}

#updatePlot = function(id, expr, app=getApp(), update.env=parent.frame()) {
#  #browser()
#  expr.object = substitute(expr)
#  updateRenderer(id, expr.object, myRenderPlot, update.env, app)
#}

#' update an plotOutput object. Can be used instead of renderPlot.
updatePlot = function(id, expr, app=getApp(), update.env=parent.frame()) {
  # Note: code is much simpler than other update code
  # Maybe we can always use this code
  if (app$verbose)
    cat("\n updatePlot: ", id)

  expr.object = substitute(expr)
  app$output[[id]] <- renderPlot(env=update.env, quoted=TRUE, expr=expr.object)
}

addUpdateRenderer = function(id, renderFunc, app=getApp(),overwrite=FALSE) {
  #restore.point("addUpdateRenderer")
  
  if (overwrite | !hasUpdater(id,app)) {
    app$do.update[[id]] = reactiveValues(counter=0)
    app$output[[id]] <- renderFunc({
      #cat("Inside the render function...")
      app$do.update[[id]]$counter
      eval(app$update.expr, app$update.env)
    })
  }
}

