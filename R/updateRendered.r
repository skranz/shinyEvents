hasUpdater = function(session=NULL,id, app=getApp(session)) {
  id %in% names(app$do.update)
}

#' Update an dataTableOutput object. Can be used instead of renderDataTable
updateDataTable = function(session=NULL,id, val,app=getApp(session),...) {
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


#' update an plotOutput object. Can be used instead of renderPlot.
updatePlot = function(session=NULL,id, expr, app=getApp(session), update.env=parent.frame()) {
  # Note: code is much simpler than other update code
  # Maybe we can always use this code
  if (app$verbose)
    cat("\n updatePlot: ", id)

  expr.object = substitute(expr)
  app$output[[id]] <- renderPlot(env=update.env, quoted=TRUE, expr=expr.object)
}


#' Update an dataTableOutput object.
#' 
#' Can be used instead of renderDataTable.
#' Similar to updateDataTable but no need to provide session object
setDataTable = function(id, val,update.env=parent.frame(), app=getApp(),...) {
  updateDataTable(session=app$session, id=id, val=val, update.env=parent.frame(), app=app,...)
}

#' Update an output object. Can be used instead of renderImage
#' 
#' Similar to updateImage but no need to provide session object
setImage = function(id, val,app=getApp(),...) {
  updateImage(session=app$session,id, val,app=app,...)
}

#' Update an textOutput object. Can be used instead of renderPrint
#' 
#' Similar to updatePrint but no need to provide session object
setPrint = function(id, expr, app=getApp(), ...) {
  updatePrint(session=app$session,id, expr,app=app,...)
}

#' Update an tableOutput object. Can be used instead of renderTable
#' 
#' Similar to updateTable but no need to provide session object
setTable = function(id, val, app=getApp(),...) {
  updateTable(session=app$session,id, val,app=app,...)  
}

#' Update an textOutput object. Can be used instead of renderText
#' 
#' Similar to updateText but no need to provide session object
setText = function(id, val, app=getApp(),...) {
  updateText(session=app$session,id, val,app=app,...)  
}

#' Update an uiOutput object. Can be used instead of renderUI
#'
#' Similar to updateUI but no need to provide session object
setUI <- function (id, ui, app = getApp(),...) {
  updateUI(session=app$session,id, ui,app=app,...)  
}

#' update an plotOutput object. Can be used instead of renderPlot.
#' 
#' Similar to updatePlot but no need to provide session object
setPlot = function(id, expr, app=getApp(), update.env=parent.frame(),...) {
  if (app$verbose)
    cat("\n updatePlot: ", id)
  expr.object = substitute(expr)
  app$output[[id]] <- renderPlot(env=update.env, quoted=TRUE, expr=expr.object)
  #updatePlot(session=app$session,id, expr,app=app, update.env=update.env,...)  
}

# #' update an aceEditor object.
# setAceEditor = function(editorId, value, theme, readOnly, mode, fontSize, 
#     wordWrap, border = c("normal", "alert", "flash"), autoComplete = c("disabled", 
#         "enabled", "live"), autoCompleteList = NULL, app=getApp(), ...) {
# 
#   updateAceEditor(session=app$session, editorId=editorId, value=value, theme=theme, readOnly=readOnly, mode=mode, fontSize=fontSize,wordWrap=wordWrap, border = border, autoComplete = autoComplete, autoCompleteList = autoCompleteList,...) 
# }

