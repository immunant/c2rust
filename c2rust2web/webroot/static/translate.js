'use strict';

var c_editor = ace.edit("c_src");
c_editor.session.setMode("ace/mode/c_cpp");

var rust_editor = ace.edit("rust_src");
rust_editor.session.setMode("ace/mode/rust");

$('#translate').click(function() {
    var data = { src: c_editor.getValue(), };

    $.post({
        url: '/translate',
        data: data,
        dataType: 'text',
        success: function(resp, status, jqXHR) {
            rust_editor.session.setMode("ace/mode/rust");
            rust_editor.setValue(resp);
            $("#rust_editor > .editor_title").text("Generated Rust source code")
        },
    }).fail(function(e) {
        var errorObj = $.parseJSON(e.responseText);
        rust_editor.session.setMode("ace/mode/text");
        rust_editor.setValue(errorObj.description);

        $("#rust_editor > .editor_title").text("Error message")
    });
});