$(document).ready(
    function () {
        var hashit = function (s) {
            var hash = 0, i, ch;
            if (s.length == 0) return hash;
            for (i = 0, l = s.length; i < l; i++) {
                ch = s.charCodeAt(i);
                hash  = ((hash<<5)-hash) + ch;
                hash |= 0; // Convert to 32bit integer
            }
            return hash;
        };
        var url_id = hashit(document.location.pathname);
        console.log("url_id", url_id);
        $.ajax({ url: document.location.href,
                 dataType: "json",
                 data: "_" + new Date().getTime(),
                 success: function (response_data) {
                     var t;
                     t = $("#thing")[0];
                     $(t.content.querySelector("div.section")).attr("id", url_id);
                     $(t.content.querySelector("p em")).text(
                         response_data["title"]
                     );
                     $(t.content.querySelector("pre")).text(response_data["doc"]);
                     document.body.appendChild(t.content.cloneNode(true));
                     // now we need to scroll to it
                     $("html, body").animate(
                         { scrollTop: $("#" + url_id).offset().top }, 
                         1000
                     );
                 }
               });
        console.log("after ajax");
    });
