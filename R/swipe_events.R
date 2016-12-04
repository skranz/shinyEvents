# Not working
swipeLeftHandler = function(id=NULL, fun,..., eventId="swipeLeftEvent",app=getApp()) {
  restore.point("swipeLeftHandler")
  eventHandler(eventId=eventId,id=id,fun=fun,...,app=app)
}


swipeRightHandler = function(id=NULL, fun,..., eventId="swipeRightEvent",app=getApp()) {
  restore.point("swipeRightHandler")
  eventHandler(eventId=eventId,id=id,fun=fun,...,app=app)
}


swipeEvents = function(swipeLeftId="swipeLeftEvent",swipeRightId="swipeRightEvent", add.handlers=TRUE) {
  restore.point("swipeEvents")
  code = NULL
  if (!is.null(swipeLeftId)) {
    code = paste0(code,'
      $( document ).on("swipeleft", function( e ) {
        Shiny.onInputChange("',swipeLeftId,'", {id: e.target.id, tag: e.target.nodeName, nonce: Math.random()});
      });
    ')
  }
  if (!is.null(swipeRightId)) {
    eventId = swipeRightId
    code = paste0(code,'
      $( document ).on("swiperight", function( e ) {
        Shiny.onInputChange("',eventId,'", {id: e.target.id, e.target.nodeName, eventId:"',eventId,'", nonce: Math.random()});
      });
    ')
  }
  res = list(
    tags$script(src="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.js"),
    tags$script(code)
  )

  events = c(swipeLeftId,swipeRightId)
  for (eventId in events) {
    registerEventId(eventId)
  }
  res
}

