# some simple form widgets

examples.form.widgets = function() {
  app = eventsApp()
  app$ui = bootstrapPage(
    checkboxGroupInput("check", "Variables to show:",
                     c("Cylinders" = "cyl",
                       "Transmission" = "am",
                       "Gears" = "gear")),

    myText = textInput("myText","Text:","Hello World"),
    radioButtons("myRadio",label="Make your choice",choices = list("Choice A"="A","Choice B"= "B"), selected=NA),
    smallButton("btn",label="", icon=icon("trash-o")),
    smallButton("btn","Click me", form.ids=c("check", "myRadio","myText"))
  )
  buttonHandler("btn", function(formValues,...) {
    restore.point("jnsifnjsfn")
    print(formValues)
    #cat(formValues$myRadio)
  })
  viewApp(app)
}


examples.widgets = function() {
  app = eventsApp()
  app$ui = bootstrapPage(
    slimCollapsePanel(open=TRUE,"Panel",
      p("I am open!")
    ),
    radioButtons("myRadio",label="Make your choice",choices = c("A","B"), selected="A"),
    simpleCheckbox("myCheck",label="Click me"),
    
    simpleButton("btn","Click me", form.ids=c("myRadio","myCheck"))
  )
  buttonHandler("btn", function(formValues,...) {
    restore.point("jnsifnjsfn")
    cat(formValues$myRadio, formValues$myCheck)
  })
  checkboxChangeHandler("myCheck", function(id,value,checked,...) {
    args = list(...)
    restore.point("jnsifnjs3efrffn")
    cat("Checkbox", id," changed to ", checked)    
  })
  viewApp(app)
}

smallButton = function(id, label,class.add="",class="btn btn-default action-button btn-xs",style="",icon=NULL, form.ids=NULL,form.sel=NULL,...) {
  args = list(...)
  if ("data-form-selector" %in% names(args) | (is.null(form.ids) & is.null(form.sel))) {
    tags$button(id=id, style=style, type="button", class=paste(class,class.add),...,list(icon, label))
  } else {
    if (is.null(form.sel)) {
      form.sel = paste0("#", form.ids,collapse=", ")
    }
    tags$button(id=id, style=style, type="button", class=paste(class,class.add),`data-form-selector`=form.sel,...,list(icon, label))

  }
}

simpleButton = function(id, label,class.add="",class="btn btn-default action-button",style="",icon=NULL, form.ids=NULL,form.sel=NULL,as.tag=TRUE,...) {
  args = list(...)
  class=paste(class,class.add)
  if (as.tag) {
    if ("data-form-selector" %in% names(args) | (is.null(form.ids) & is.null(form.sel))) {
      tags$button(id=id, style=style, type="button", class=class,...,list(icon, label))
    } else {
      if (is.null(form.sel)) {
        form.sel = paste0("#", form.ids,collapse=", ")
      }
      tags$button(id=id, style=style, type="button",class=class ,`data-form-selector`=form.sel,...,list(icon, label))
  
    }
  } else {
    stop("simpleButton so far only implemented as.tag")    
  }
}



slimCollapsePanel = function (title, ..., value = title, bsStyle = NULL, heading.style=if (!is.null(padding)) paste0("padding-top: ", padding,"; padding-bottom: ",padding,";"), extra.class="", open=FALSE, padding=NULL)
{
    content <- list(...)
    id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1,
        1, 1e+06))))
    if (is.null(value)) {
        value = title
    }
    if (is.null(bsStyle)) {
        bsStyle = "default"
    }
    bsTag <- shiny::tags$div(class = paste0("slim-collapse-panel panel panel-", bsStyle," ", extra.class),
        value = value, shiny::tags$div(class = "panel-heading", style=heading.style,role = "tab", id = paste0("heading_", id), shiny::tags$h4(class = "panel-title",
                shiny::tags$a(`data-toggle` = "collapse", href = paste0("#",
                  id), title))), shiny::tags$div(id = id, class = paste0("panel-collapse collapse ", if(open) "in" else ""),
            role = "tabpanel", shiny::tags$div(class = "panel-body",
                content)))
    #htmltools::attachDependencies(bsTag, shinyBSDep)
}


simpleCheckbox = function(id,label="",value=id, checked=FALSE,name=id,as.tag=TRUE,...) {
  restore.point("simple.checkbox")
  args = list(...)
  if (length(args)>0) {
    args_str = paste0(" ",names(args),'="', args,'" ', collapse="")
  } else {
    args_str = ""
  }
  html = paste0('<input type="checkbox" id="', id,'" name="', name,'" value="', value,'"',args_str, ifelse(checked," checked",""),'>', label, "</input>")
  if (!as.tag) return(html)
  return(HTML(html))
}

#' Add one or several checkboxes that will be called if the box is checked or unchecked
#'
#' @param id name of the input element 
#' @param fun function that will be called if the input value changes. The function will be called with the arguments: 'id', 'value' and 'session'. One can assign the same handler functions to several input elements.
#' @param class alternative to id a class selector
#' @param selector a fully specified css selector.
#' @param stop.propagation if TRUE the event will not propagate
#' @param ... extra arguments that will be passed to fun when the event is triggered.
#' @export
checkboxChangeHandler = function(id=NULL, fun, ...,class=NULL, selector=paste0(c(sc("#", id),
  sc(".", class)), collapse = ", "), eventId=makeEventsId("checkBoxChange",id=id, class=class), stop.propagation=FALSE, event="change",  app=getApp()) {
  restore.point("checkboxChangeHandler")
  
  shiny.value.code = paste0('{eventId:"',eventId,'",id: this.id, value: $(this).val(), checked: this.checked,  data: $(this).data(),nonce: Math.random()}')
  customEventHandler(eventId=eventId,css.locator = selector,fun=fun,..., event=event,shiny.value.code = shiny.value.code,stop.propagation = stop.propagation, app=app)
}

makeEventsId = function(prefix, id=NULL, class=NULL) {
  if (!sum(length(id)+length(class))==1) {
    return(paste0(prefix,"-",random.string(1,12)))
  } else if (length(id)==1) {
    return(paste0(prefix,"-",id))
  } else {
    return(paste0(prefix,"-",class))
  }
}

random.string = function(n=1, nchar=12) {
  if (n == 1) {
    paste0(sample(c(LETTERS,letters), nchar, TRUE), collapse="")
  } else {
    unlist(replicate(n,paste0(sample(c(LETTERS,letters), nchar, TRUE),collapse="")))
  }
}

