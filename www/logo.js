(function() {
     $(document).ready(
         function () {
             $.getScript(
                 "/jquery.lettering-0.6.1.min.js",
                 function() { 
                     $("#logo a").lettering();
                 }
             );
         }
     );
 }
)();
