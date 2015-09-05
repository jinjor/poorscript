var main = function (e,state) {
   return $if(e,
   function () {
      return render(state.a);
      ;
   },
   function () {
      return pipe(update("a",
      "Hello!"),
      action("render"));
      ;
   });
};
;