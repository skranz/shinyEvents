autocomp.js = function() {
  js = '
Shiny.addCustomMessageHandler("setAceAutoCompleteWords", function(msg) {
    var callback = $("#" + msg.id).data("autoCompleteCallback");
    if(callback !== undefined) callback(null, msg.words);
});
'
  js
}

set.autocomplete.list = function(inputId,  words="", score=100, meta="R", df=data.frame(name=words,value=words,score=score,meta=meta),session = app$session, app=getApp()) {
  restore.point("set.autocomplete.list")
  json = jsonlite::toJSON(df)
  session$sendCustomMessage(type = 'setAceAutoCompleteWords',list(id=inputId,words=json))
}

fun.arg.names = function(fun, envir=globalenv()) {
  if (is.character(fun)) {
    fun = try(get(fun, envir), silent=TRUE)
    if (!is.function(fun)) return(NULL)
  }
  names(formals(fun))
}

autocomp.find.current.function = function(str,vec = strsplit(str,"",fixed = TRUE)[[1]]
) {
  open = which(vec=="(")
  if (length(open)==0) return(NULL)
  closed = which(vec==")")
  if (length(closed) >= length(open)) return(NULL)
  end.pos = open[length(open)-length(closed)]-1
  
  vec = vec[1:end.pos]
  noname = which(!vec %in% c(letters,LETTERS,"_",".",0:9)) 
  if (length(noname)==0) {
    start.pos = 1
  } else {
    start.pos = max(noname)+1
  }
  if (start.pos >= end.pos) return(NULL)
  
  substring(str, start.pos, end.pos)
}



find.varname.at.end.of.string = function(str, vec=strsplit(str,"",fixed = TRUE)[[1]]) {
  noname = which(!vec %in% c(letters,LETTERS,"_",".",0:9)) 
  if (length(noname)==0) {
    start.pos = 1
  } else {
    start.pos = max(noname)+1
  }
  substring(str,start.pos)
} 
autcomp.function.args = function(str, fun.env=globalenv()) {
  restore.point("autcomp.function.args")
  
  fun = autocomp.find.current.function(str)
  if (is.null(fun)) return(NULL)
  
  fun.args = fun.arg.names(fun, fun.env)
  if (length(fun.args)>0) {
    if ("..." %in% fun.args) {
      fun.args = c(paste0(setdiff(fun.args,"..."), " = "),"...")
    } else {
      fun.args = paste0(fun.args, " = ")      
    }
  }
  fun.args
}
autocomp.vars = function(str, var.env = NULL) {
  restore.point("autocomp.vars.and.cols")
  if (is.null(var.env)) return(NULL)
  vars = ls(var.env)
  return(vars)
}

autocomp.cols = function(str, var.env = NULL, vars=ls(var.env)) {
  restore.point("autocomp.vars.and.cols")
  words = NULL
  if (is.null(var.env)) return(NULL)
  cols = unique(unlist(lapply(vars, function(var) {
      val = var.env[[var]]
      if (is.data.frame(val)) return(names(val))
      NULL
  })))
  cols
}