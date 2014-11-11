updateText = function(id, text, app=get.app()) {
  if (!updater.exists(id,app)) {
    add.renderText(id, app)
  }
  app$text = text
  perform.update(id, app)
}