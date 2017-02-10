open.url.from.app = function(url, token, new.tab=TRUE,target=if (new.tab) "_blank" else "", link.ui.id=NULL) {
  restore.point("open")

  callJS(.fun = "window.open",list(url,target))
  if (!is.null(link.ui.id)) {
    html = paste0('<a href="', url,'" class="button" target="',target,'">Click here if new window does not open automatically.</a>')
    setUI(link.ui.id, HTML(html))
  }
  
}