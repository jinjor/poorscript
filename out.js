var startApp = function (init,
upd,
view) {
   return function (e,state) {
      var a = console.log(e);
      var ret = $if(e === "afterUpdate",
      function () {
         return render(view(state.model));
         ;
      },
      function () {
         return pipe(update("model",
         init),
         function () {
            return action("afterUpdate");
         });
         ;
      });
      return ret;
      ;
   };
   ;
};
var init = "Hello";
var upd = function (e,model) {
   return model;
};
var view = function (model) {
   return "<h1>" + model + "</h1>";
   ;
};
var main = startApp(init,
upd,
view);
;