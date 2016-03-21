#' Directly setUI , also works for hidden UI
#' @export
dsetUI = function(id, ui, app=getApp(),...) {
  html = as.character(ui)
  app$session$sendCustomMessage(type = 'shinyEventsSetInnerHTML',message = list(id=id, html=html))
}

