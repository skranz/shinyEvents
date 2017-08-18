shinyEventsExtractFormValues = function(id) {
  var sel = $("#"+id).data("form-selector");
  var vals = {};
  if (typeof sel === "undefined") return(null);
  $(sel).each(function( index ) {
    var valId = $(this).attr("id");
    var value = shinyEventsWidgetValue($(this));
    vals[valId] = value;
  });

  return vals;
};

// Tries to extract the value of different shiny widgets
// x is a jquery object e.g. $("#myInput")
// for normal inputs we just return x.val()
// special widgets like radio buttons, check boxes or
// ace editors need special treatment, however
shinyEventsWidgetValue = function(x) {
  var id = x.attr("id");
  if (x.hasClass("shiny-input-radiogroup")) {
    csel = "#"+id+" input:radio[name=\'"+id+"\']:checked";
    return $(csel).val();
  } else if (x.hasClass("ace_editor")) {
    var editor = x.data("ace");
    return editor.getValue();
  } else if (x.hasClass("shiny-input-checkboxgroup")) {
    var values = new Array();
    csel = "#"+id+" input:checkbox[name=\'"+id+"\']:checked";
    $.each($(csel), function() {
      values.push($(this).val());
    });
    return values;
  } else if (x.attr('type')==="checkbox") {
    return(x.prop("checked"));
  }
  return x.val();
};
