caller.session = function(env = parent.frame(n=2)) {
  session = env$session
  if (is.null(attr(session,"eventsApp"))) return(NULL)
  return(session)
}

# f = function(session="b") {
#   g()
# }
# 
# g = function(session=caller.session()) {
#   session
# }
# 
# f()

# # Internal function that will be called from app$server
# # Never call this it yoursel
# initEventAppServerFramePos = function() {
#   old.pos = .SHINY.EVENTS.ENV$server.frame.pos
#   new.pos = sys.nframe()-1
#   
#   cat("\n*****************************************************")
#   cat("\nserver.frame.pos: ", new.pos)
#   cat("\n*****************************************************")
# 
#   if (is.null(old.pos)) {
#     .SHINY.EVENTS.ENV$server.frame.pos <- new.pos
#     return()
#   }
# 
#   if (is.na(old.pos)) return()  
#   
#   if (old.pos!=new.pos) {
#     # server.pos is not unique set to NA
#     cat("\n*****************************************************")
#     cat("\nserver.frame.pos is not unique")
#     cat("\n*****************************************************")
#     .SHINY.EVENTS.ENV$server.frame.pos <- NA
#   } else {
#     .SHINY.EVENTS.ENV$server.frame.pos <- new.pos
#    
#   }
# }
# 
# getAppFromServerFramePos <- function(server.frame.pos = .SHINY.EVENTS.ENV$server.frame.pos) {
#   #restore.point("getAppFromServerFramePos")
#   browser()
#   if (is.na(server.frame.pos)) return(NULL)
#   sys.frame(server.frame.pos)$app
# }
# 
# #' Search CallStack for an object named app, since it is called in server
# getAppFromCallStack <- function(varname="app", offset=2) {
#   #restore.point("getAppFromCallStack")
#   cat("\ngetAppFromCallStack... ")
#   n = sys.nframe()-offset
#   if (n<1) return(NULL)
#   search.pos = n:1
#   
#   last.pos = .SHINY.EVENTS.ENV$last.app.pos
#   if (!is.null(last.pos)) {
#     if (last.pos < n) {
#       cat(" last.pos = ", last.pos)
# 
#       search.pos = c(last.pos, setdiff(search.pos, last.pos))
#     }
#   }
#   
#   for (pos in search.pos) {
#     env = sys.frame(pos)
#     if (exists(varname, envir = env, inherits = FALSE)) {
#       #browser()
#       app <- NULL
#       app <- try(get(varname,envir=env, inherits=FALSE), silent=TRUE)
#       if (is.environment(app)) {
#         if (isTRUE(app$isSessionEventApp)) {
#           cat(" found app on pos ", pos)
#           .SHINY.EVENTS.ENV$last.app.pos <- pos
#           return(app)          
#         }
#       }
#     }    
#   }
#   cat("\n*****************************************************")
#   cat("\nNo app found in sys.frames...")
#   cat("\n*****************************************************")
#   return(NULL)
# }
# 
# g = function() {
#   f()
# }
# 
# f = function() {
#   sys.status()
#   sys.nframe()
# }
# g()
