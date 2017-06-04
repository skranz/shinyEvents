examples.to.style = function() {
  
}

to.style = function(list) {
  paste0(names(list),": ",list, collapse="; ")
  
}


#' Add a CssClass to an HTML objects
#' @value id the id of the HTML element
#' @value class the class to add
#' @value selector a css selector as string
#' @export 
addCssClass = function(id, class, selector = sc("#",id),app=getApp()) {
  callJS(.fun = paste0('$("',selector,'").addClass'),.args = list(class))
}

#' Add a CssClass to an HTML objects
#' @value id the id of the HTML element
#' @value class the class to add
#' @value selector a css selector as string
#' @export 
removeCssClass = function(id, class, selector = sc("#",id),app=getApp()) {
  callJS(.fun = paste0('$("',selector,'").removeClass'),.args = list(class))
}

#' Add  specified CSS class and remove other CSS class of an HTML objects
#' @value id the id of the HTML element
#' @value add.class the class to add
#' @value remove.class the class to remove
#' @value selector a css selector as string
#' @export 
swapCssClass = function(id, add.class, remove.class, selector = sc("#",id),app=getApp()) {
  js = paste0('$("',selector,'").addClass(',paste0('"',add.class,'"', collapse=","),'.removeClass(',paste0('"',remove.class,'"', collapse=","))
  evalJS(js)
}


#' Set attributes of HTML elements
#' @value selector a css selector as string
#' @value attr a named list of attributes
#' @export 
setHtmlAttribute = function(id=NULL, attr, class=NULL,selector=paste0(c(sc("#",id),sc(".",class)),collapse=", "), app=getApp()) {
  restore.point("setHtmlAttribute")
  app$session$sendCustomMessage(type= 'shinyEventsSetAttribute', message=list(selector=selector, attr=attr))
}

#' Set css style of HTML elements
#' @value selector a css selector as string
#' @value attr a named list of css style attributes
#' @export 
setHtmlCSS = function(id=NULL,attr,class=NULL,selector=paste0(c(sc("#",id),sc(".",class)),collapse=", "), app=getApp()) {
  restore.point("setHtmlCSS")
  app$session$sendCustomMessage(type= 'shinyEventsSetCSS', message=list(selector=selector,attr=attr))
}

#' Hide HTML elements
#' @value selector a css selector as string
#' @value attr a named list of css style attributes
#' @export 
setHtmlHide = function(id=NULL, class=NULL, display="none",selector=paste0(c(sc("#",id),sc(".",class)),collapse=", ")) {
  setHtmlCSS(id=id,class=class, attr=list(display = display),selector=selector)
}

#' Show HTML elements
#' @value selector a css selector as string
#' @value attr a named list of css style attributes
#' @export 
setHtmlShow = function(id=NULL, class=NULL, display="block",visibility="visible",selector=paste0(c(sc("#",id),sc(".",class)),collapse=", ")) {
  setHtmlCSS(id=id,class=class, attr=list(display = display, visibility=visibility), selector=selector)
  evalJS(paste0("$('",selector,"').trigger('shown');"))
}

#' Evaluate arbitrary java script code in the client's web browser
#' @value js the java script code to be evaluated
#' @export 
evalJS = function(js,...,.args=list(...), app=getApp()) {
  app$session$sendCustomMessage(type= 'shinyEvalJS', message=c(list(code=js),.args))
}

#' Append HTML code to a DOM element
#' @value html the html code as string
#' @value selector a css selector as string
#' @export 
appendToHTML = function(html, selector="body", app=getApp()) {
  app$session$sendCustomMessage(type= 'shinyEventsAppend', message=list(selector=selector,html=html))  
}

#' Prpend HTML code to a DOM element
#' @value html the html code as string
#' @value selector a css selector as string
#' @export 
prependToHTML = function(html, selector="body", app=getApp()) {
  app$session$sendCustomMessage(type= 'shinyEventsAppend', message=list(selector=selector,html=html))  
}

#' Call a javascript function or method with R arguments
#' @value .fun name of the function or method to be called, e.g. "$("#mydiv").attr"
#' @value ... function argument in the same order as in the javascript function. Names will not be used. 
#' @export 
callJS = function(.fun,..., .args=NULL, .app=getApp()) {
  if (is.null(.args)) .args = list(...)
  restore.point("callJS")
  
  names(.args) = NULL
  args.code = sc("message.args[",seq_along(.args)-1,"]", collapse=",")
  code = paste0(.fun,"(",args.code,");")
  .app$session$sendCustomMessage(type= 'shinyEvalJS', message=list(code=code,args=.args))

  #.app$session$sendCustomMessage(type= 'shinyEventsCallJS', message=list(fun=.fun,args=.args))    
}


open.url.from.app = function(url, token, new.tab=TRUE,target=if (new.tab) "_blank" else "", link.ui.id=NULL) {
  restore.point("open")

  callJS(.fun = "window.open",list(url,target))
  if (!is.null(link.ui.id)) {
    html = paste0('<a href="', url,'" class="button" target="',target,'">Click here if new window does not open automatically.</a>')
    setUI(link.ui.id, HTML(html))
  }
  
}

