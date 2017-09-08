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
  changeHandler("myCheck", function(id,value,...) {
    args = list(...)
    restore.point("jnsifnjs3efrffn")
    cat("Checkbox", id," changed to ", value)    
  })
  viewApp(app)
}

smallButton = function(id, label,class.add="",class="btn btn-default action-button btn-xs",style="",form.ids=NULL,form.sel=NULL,...) {
  args = list(...)
  if ("data-form-selector" %in% names(args) | (is.null(form.ids) & is.null(form.sel))) {
    tags$button(id=id, style=style, type="button", class=paste(class,class.add),...,label)
  } else {
    if (is.null(form.sel)) {
      form.sel = paste0("#", form.ids,collapse=", ")
    }
    tags$button(id=id, style=style, type="button", class=paste(class,class.add),`data-form-selector`=form.sel,...,label)

  }
}

simpleButton = function(id, label,class.add="",class="btn btn-default action-button",style="",form.ids=NULL,form.sel=NULL,as.tag=TRUE,...) {
  args = list(...)
  class=paste(class,class.add)
  if (as.tag) {
    if ("data-form-selector" %in% names(args) | (is.null(form.ids) & is.null(form.sel))) {
      tags$button(id=id, style=style, type="button", class=class,...,label)
    } else {
      if (is.null(form.sel)) {
        form.sel = paste0("#", form.ids,collapse=", ")
      }
      tags$button(id=id, style=style, type="button",class=class ,`data-form-selector`=form.sel,...,label)
  
    }
  } else {
    stop("simpleButton so far only implemented as.tag")    
  }
}



slimCollapsePanel = function (title, ..., value = title, bsStyle = NULL, heading.style=paste0("padding-top: ", padding,"; padding-bottom: ",padding,";"), open=FALSE, padding="3px")
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
    bsTag <- shiny::tags$div(class = paste0("panel panel-", bsStyle),
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
