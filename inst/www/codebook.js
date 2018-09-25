$(function(){

  function urlExists(url, callback){
    $.ajax({
      type: 'GET',
      url: url,
      success: function(){
        callback(true);
      },
      error: function() {
        callback(false);
      }
    });
  }
  var editor = ace.edit("editor");
  editor.setTheme("ace/theme/monokai");
  editor.getSession().setMode("ace/mode/r");
  editor.setFontSize("14px");

  //This app requires OpenCPU 1.0.1 or higher!
  ocpu.seturl("../R")

  function make_codebook(e){
    $("#make_codebook").prop("disabled", true);
    $("#working").removeClass("invisible");
    var req = ocpu.call("load_data_and_render_codebook", {
      file: $("#file")[0].files[0],
      text: editor.getSession().getValue(),
      remove_file: true
    }, function(session){
      $("#error").collapse('hide');
      var html_result = session.getFileURL("codebook.html");
      urlExists(html_result, function(html) {
        if(html) {
          $("#download_codebook").attr('href', html_result).show();
          $("iframe").attr('src', html_result);
        } else {
            var pdf_result = session.getFileURL("codebook.pdf");
            $("#download_codebook").attr('href', pdf_result).show();
            $("iframe").attr('src', pdf_result);
        }
      });
    }).fail(function(text){
      $("#error_message").html("Error: " + req.responseText);
      $("#error").collapse('show');
    }).always(function(){
        $("#make_codebook").prop("disabled", false);
        $("#working").addClass("invisible");
    });
  }

  //init on start
  $("#make_codebook").click(make_codebook);
});
