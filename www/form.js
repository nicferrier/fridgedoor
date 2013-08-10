$(document).ready(
    function () {
        var form_attach = function (selector, callback) {
            var form_queue = new Array();
            var form_queue_process = function (form_queue) {
                var head = form_queue.shift();
                var cbproc = function (cb, data, form_data, jqXHR) {
                    if ($.isFunction(cb)) {
                        var r = cb(data, form_data, jqXHR);
                        if (r) { return r; }
                    }
                    return false;
                };
                while (!(head === undefined)) {
                    var action = head[0];
                    var data = head[1];
                    var cb = head[2];
                    var dataType = "json";
                    $.ajax({
                               type: "POST",
                               url: action,
                               data: data,
                               dataType: dataType,
                               success: function (response_data, status, jqXHR) {
                                   cbproc(cb, response_data, data, jqXHR);
                               },
                           });
                    head = form_queue.shift();
                }
            };
            var action = $(selector).attr("action");
            $(selector).submit(
                function (evt) {
                    var data = $(evt.target).serialize();
                    $(evt.target)[0].reset();
                    form_queue.push([action, data, callback]);
                    form_queue_process(form_queue);
                    return false;
                }
            );
        };
        form_attach(
            "form[name=fridge]", 
            function (response_data, form_data, jqXHR) {
                console.log("response=", response_data, "data=", form_data);
            }
        );
    });
