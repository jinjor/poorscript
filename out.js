var startApp = function (view) {
   return $apply(function () {
      return $apply(function () {
         return $apply(function () {
            return b(c(d(e())));
         });
      });
   });
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
   return "<h1>" + state.a + "</h1>";
   ;
};
;