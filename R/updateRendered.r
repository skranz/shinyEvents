updateRenderer = function(session=NULL,id, expr.object, renderFunc,
                          update.env=parent.frame(), 
                          app=getApp(session),
                          overwrite.renderer=FALSE, level=0) {
  restore.point("updateRenderer")
  # add a renderer that is triggered by performUpdate
  addUpdateRenderer(id=id, renderFunc=renderFunc, app=app, 
                    overwrite=overwrite.renderer) 
  app$update.expr = expr.object
  app$update.env = update.env
  triggerWidgetUpdate(id, app=app, session=session, level=level) 
}

#' Update an dataTableOutput object. Can be used instead of renderDataTable
updateDataTable = function(session=NULL,id, val,update.env=parent.frame(), app=getApp(session),...) {
  app$output[[id]] <- renderDataTable(val,...)
}



#' Update an output object. Can be used instead of renderImage
updateImage = function(session=NULL,id, val,app=getApp(session),...) {
  app$output[[id]] <- renderImage(val,...)
}

#' Update an textOutput object. Can be used instead of renderPrint
updatePrint = function(session=NULL,id, expr, app=getApp(session), ...) {
  app$output[[id]] <- renderPrint(val,...)}

#' Update an tableOutput object. Can be used instead of renderTable
updateTable = function(session=NULL,id, val, app=getApp(session),...) {
  app$output[[id]] <- renderTable(val,...)
}

#' Update an textOutput object. Can be used instead of renderText
updateText = function(session=NULL,id, val, app=getApp(session),...) {
  app$output[[id]] <- renderText(val,...)
}

#' Update an uiOutput object. Can be used instead of renderUI
updateUI <- function (session, id, ui, app = getApp(session),...) 
{
    restore.point("updateUI")
    if (app$verbose) 
        cat("\n updateUI: ", id)
    app$output[[id]] <- renderUI(ui,...)
}


hasUpdater = function(session=NULL,id, app=getApp(session)) {
  id %in% names(app$do.update)
}

triggerWidgetUpdate = function(session=NULL,id, app=getApp(session), level=0) {
  restore.point("triggerWidgetUpdate")
  app$do.update[[id]]$counter = isolate(app$do.update[[id]]$counter+1)
  cat("end triggerWidgetUpdate")
}

#updatePlot = function(session=NULL,id, expr, app=getApp(session), update.env=parent.frame()) {
#  #browser()
#  expr.object = substitute(expr)
#  updateRenderer(session,id, expr.object, myRenderPlot, update.env, app)
#}

#' update an plotOutput object. Can be used instead of renderPlot.
updatePlot = function(session=NULL,id, expr, app=getApp(session), update.env=parent.frame()) {
  # Note: code is much simpler than other update code
  # Maybe we can always use this code
  if (app$verbose)
    cat("\n updatePlot: ", id)

  expr.object = substitute(expr)
  app$output[[id]] <- renderPlot(env=update.env, quoted=TRUE, expr=expr.object)
}

addUpdateRenderer = function(session=NULL,id, renderFunc, app=getApp(session),overwrite=FALSE) {
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

