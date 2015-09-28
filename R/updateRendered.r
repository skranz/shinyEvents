hasUpdater = function(session=NULL,id, app=getApp(session)) {
  id %in% names(app$do.update)
}

#' Update an dataTableOutput object. Can be used instead of renderDataTable
updateDataTable = function(session=NULL,id, value,app=getApp(session),...) {
  if (require(DT)) {
    app$output[[id]] <- DT::renderDataTable(value,...)
  } else {
    app$output[[id]] <- renderDataTable(value,...)
  }
}

#' Update an output object. Can be used instead of renderImage
updateImage = function(session=NULL,id, value,app=getApp(session),...) {
  app$output[[id]] <- renderImage(value,...)
}

#' Update an textOutput object. Can be used instead of renderPrint
updatePrint = function(session=NULL,id, expr, app=getApp(session), ...) {
  app$output[[id]] <- renderPrint(value,...)}

#' Update an tableOutput object. Can be used instead of renderTable
updateTable = function(session=NULL,id, value, app=getApp(session),...) {
  app$output[[id]] <- renderTable(value,...)
}

#' Update an textOutput object. Can be used instead of renderText
updateText = function(session=NULL,id, value, app=getApp(session),...) {
  app$output[[id]] <- renderText(value,...)
}

#' Update an uiOutput object. Can be used instead of renderUI
updateUI <- function (session, id, ui, app = getApp(session),...) 
{
    
    restore.point("updateUI")
    #cat("\nstart updateUI\n")
    if (app$verbose) 
        cat("\n updateUI: ", id)
    res = try(
      app$output[[id]] <- renderUI({
        #cat("\ncalled renderUI:")
        #cat("\n",as.character(ui))
        ui
      },...)
    )
    
    #cat("\nend updateUI\n")
}


#' update an plotOutput object. Can be used instead of renderPlot.
updatePlot = function(session=NULL,id, expr, app=getApp(session), update.env=parent.frame(),quoted=FALSE) {
  # Note: code is much simpler than other update code
  # Maybe we can always use this code
  if (app$verbose)
    cat("\n updatePlot: ", id)
  if (!quoted) {
    expr.object = substitute(expr)
  } else {
    expr.object = expr
  }
  app$output[[id]] <- renderPlot(env=update.env, quoted=TRUE, expr=expr.object)
}


#' Update an dataTableOutput object.
#' 
#' Can be used instead of renderDataTable.
#' Similar to updateDataTable but no need to provide session object
setDataTable = function(id, value, app=getApp(),...) {
  updateDataTable(session=app$session, id=id, value=value, app=app,...)
}

#' Update an output object. Can be used instead of renderImage
#' 
#' Similar to updateImage but no need to provide session object
setImage = function(id, value,app=getApp(),...) {
  updateImage(session=app$session,id, value,app=app,...)
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
setTable = function(id, value, app=getApp(),...) {
  updateTable(session=app$session,id, value,app=app,...)  
}

#' Update an textOutput object. Can be used instead of renderText
#' 
#' Similar to updateText but no need to provide session object
setText = function(id, value, app=getApp(),...) {
  updateText(session=app$session,id, value,app=app,...)  
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
setPlot = function(id, expr, app=getApp(), update.env=parent.frame(),quoted=FALSE,...) {
  if (app$verbose)
    cat("\n updatePlot: ", id)
  if (!quoted) {
    expr.object = substitute(expr)
  } else {
    expr.object = expr
  }
  app$output[[id]] <- renderPlot(env=update.env, quoted=TRUE, expr=expr.object)
  #updatePlot(session=app$session,id, expr,app=app, update.env=update.env,...)  
}

