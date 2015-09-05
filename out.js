var main = function (e,state) {
   return $if(e,
   function () {
      return render(html(state));
      ;
   },
   function () {
      return pipe(update("a",
      "Hello!"),
      function () {
         return action("update");
      });
      ;
   });
};
var html = function (state) {
   return state.a;
};
;