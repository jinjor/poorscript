var startApp = function (view) {
   return function (e,state) {
      return function (e,state) {
         return function (e,state) {
            return a(b(c()));
         };
      };
   };
};
var main = function (e,state) {
   var a = console.log(e);
   var ret = $if(e === "afterUpdate",
   function () {
      return render(view(state));
      ;
   },
   function () {
      return pipe(update("a",
      "Hello!"),
      function () {
         return action("afterUpdate");
      });
      ;
   });
   return ret;
   ;
};
var view = function (state) {
   return "<h1>" + (state.a + "</h1>");
   ;
};
;