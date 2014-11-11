get.renderer.table = function() {
  mat = matrix(ncol=2,byrow=TRUE, c(
    "renderPlot","shiny-plot-output",
    "renderText","shiny-text-output"
  ))
  colnames(mat) = c("renderer","class")
  as.data.frame(mat, stringsAsFactors=FALSE)  
}

find.ui.renderer = function(ui) {
  library(stringtools)
  txt = sep.lines(as.character(ui))
  
  class = str.between(txt,'class="','"', not.found=NA)
  id = str.between(txt,'id="','"', not.found=NA)
  
  tab = get.renderer.table()
  renderer = tab$renderer[match(class,tab$class)]
  rows = which(!is.na(renderer))
  
  data.frame(id=id[rows], renderer=renderer[rows], stringsAsFactors=FALSE)
}

examples.make.renderer = function() {
  # Dynamical UI that will be shown
  num = 1
  dynUI= fluidRow(
    actionButton("dynBtn", paste0("dynamic ", num)),
    selectInput("varInput", "Variable:",
        c("Cylinders" = "cyl",
          "Transmission" = "am",
          "Gears" = "gear")
    ),
    textOutput("varOutput"),
    plotOutput("myPlot")
  )
  ui = dynUI
  
  ir = find.ui.renderer(ui)
  ir
  
  call = substitute()
  
}

add.renderText = function(id, app=get.app()) {
  if (!id %in% names(app$do.update)) {
    app$do.update[[id]] = reactiveValues(counter=0)
    app$output[[id]] <- renderText({
      #cat("\ndo.update[['",id,"'']]$counter = ", app$do.update[[id]]$counter)
      app$do.update[[id]]$counter
      app$text
    })
  }
}

add.renderer = function(id,  app=get.app()) {
  # nothing done yet
}