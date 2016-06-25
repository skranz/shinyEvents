#' Directly setUI , also works for hidden UI
#' @export
dsetUI = function(id, ui, selector = paste0("#",id, collapse=", "), app=getApp(),...) {
  restore.point("dsetUI")
  
  html = as.character(ui)
  app$session$sendCustomMessage(type = 'shinyEventsSetInnerHTML',message = list(selector=selector, html=html))
}

