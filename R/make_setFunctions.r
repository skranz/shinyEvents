# examples.make.set.for.update.functions = function() {
#   setwd("D:/libraries/shinyEvents/")
#   
#   txt = make.set.for.update.functions("shiny")
#   writeLines(txt,"setFuns_shiny.r")
# 
#   txt = make.set.for.update.functions("shinyAce")
#   writeLines(txt,"setFuns_shinyAce.r")
#   
#   txt = make.set.for.update.functions("shinyBS")
#   writeLines(txt,"setFuns_shinyBS.r")
# }
# 
# # internal functions to automatically generate code
# make.set.for.update.functions = function(pkg="shiny") {
#   
#   funs = ls(paste0("package:",pkg))
#   funs = funs[substring(funs,1,6)=="update"]
#   
# 
#   fun = funs[1]
#   
#   codes = sapply(funs, function(fun) {
#   
#     com = paste0("formals(",pkg,"::",fun,")")
#     args = eval(parse(text=com))
#     args = args[names(args)!="session"]
#     
#     args.char = as.character(args)
#     args.eq = ifelse(nchar(args.char)>0,"=","" )
#     args.quote = ifelse(is.character(args),'"','')
#     
#     args.str = paste0(names(args),args.eq,args.quote,args.char,args.quote, collapse=", ")
#     
#     base.name = substring(fun,7)
#     
#     par.str = paste0(names(args),"=",names(args), collapse=", ")
#     code = paste0("
# # Update an ",base.name," object for an shiny events app
# # 
# # Similar to update",base.name," in the package ", pkg, ",
# # but no need to pass a session object.\n",
#   "set",base.name," = function(",args.str,", app=getApp()) {\n",
#   "  ",pkg,"::update",base.name,"(session=app$session,",par.str,")\n",
#   "}")
#     cat(paste0("\n",code,"\n"))  
#     code
#   })
# }