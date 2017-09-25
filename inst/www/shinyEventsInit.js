Shiny.addCustomMessageHandler("shinyEvalJS", function(message) {
  eval(message.code);
});

Shiny.addCustomMessageHandler("shinyEventsAppend", function(message) {
  $(message.selector).append(message.html);
});
Shiny.addCustomMessageHandler("shinyEventsPrepend", function(message) {
  $(message.selector).prepend(message.html);
});

Shiny.addCustomMessageHandler("shinyEventsSetInnerHTML", function(message) {
  //alert("selector: "+ message.selector + " html: "+ message.html);
  $(message.selector).html(message.html);
});

Shiny.addCustomMessageHandler("shinyEventsSetAttribute",function(message) {
    $(message.selector).attr(message.attr);
});
Shiny.addCustomMessageHandler("shinyEventsSetCSS", function(message) {
    $(message.selector).css(message.attr);
});
